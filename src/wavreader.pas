{
  Description: WAV file reader.

  Copyright (C) 2020-2026 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>.
}

unit WavReader;

{$mode objfpc}

interface

uses
  Classes, Common, SysUtils, Math;

const
  WAVE_FORMAT_PCM = $0001;
  WAVE_FORMAT_IEEE_FLOAT = $0003;
  WAVE_FORMAT_ALAW = $0006;
  WAVE_FORMAT_MULAW = $0007;
  WAVE_FORMAT_EXTENSIBLE = $fffe;

type
  // What the analysis needs to know about a WAV file, taken from its fmt,
  // fact and data chunks.
  TWaveFormat = record
    Format: word;              // effective sample format (PCM or IEEE_FLOAT), GUID-resolved
    Channels: word;
    ChannelMask: longword;
    SampleRate: longword;
    ByteRate: longword;
    BlockAlign: word;
    BitsPerSample: word;
    FactSampleLength: longword;  // optional fact chunk, kept for diagnostics
    DataSize: longword;          // bytes of audio in the data chunk
  end;

// Reads the RIFF/WAVE header and leaves the stream at the first audio byte.
// Returns 0 on success, -1 for a malformed or unsupported file, -2 for a file
// without channels or audio data.
function ReadWaveHeader(AStream: TStream; out AFormat: TWaveFormat): longint;

// Reads ASampleCount frames, de-interleaved and normalized to [-1, +1], into
// AChannels (already sized). Returns 0 on success, -1 if the stream ends early.
function ReadWaveSamples(AStream: TStream; const AFormat: TWaveFormat;
  AChannels: TDoubleMatrix; ASampleCount: longint): longint;


implementation

type
  // WAV is formed by the following structures in this order
  // all items are in little endian order, except the char arrays
  // items might be in big endian order if the RIFF identifier is RIFX

  TRiffHeader = packed record
    ckid: array [0..3] of char; // should be "RIFF"
    cksize: longword;             // 4 + (8 + subchunk1size) + (8 + subchunk2size).
    // the entire file size excluding TRiffHeader.id and .size
    waveid: array [0..3] of char; // should be "WAVE"
  end;

  TFmtchunk = packed record
    ckid: array [0..3] of char;          // should be "fmt "
    cksize: longword;                    // subchunk1size: 16, 18 or 40
    FormatTag: word;                     // pcm = 1 (linear quantization), values > 1 indicate a compressed format
    Channels: word;                      // mono = 1, stereo = 2, etc
    samplespersec: longword;             // 8000, 44100, etc
    bytespersec: longword;               // = samplerate * numchannels * bitspersample/8
    blockalign: word;                    // = numchannels * bitspersample/8
    BitsPerSample: word;                 // examples: 8 bits, 16 bits, etc
  end;

  TFmtchunkext = packed record
    cbsize: word;                        // size of the extension (0 or 22)
    validbitspersample: word;            // number of valid bits
    channelmask: longword;               // speaker position mask
    subcode: word;                       // GUID data format code
    subformat: array [0..13] of byte;    // GUID
  end;

  TFactchunk = packed record
    ckid: array [0..3] of char;  // should be "fact"
    cksize: longword;              // chunk size: minimum 4
    samplelength: longword;              // Number of samples (per channel)
  end;

  TDatachunk = packed record
    subck2id: array [0..3] of char; // should be "data"
    subck2size: longword;             // == numsamples * numchannels * bitspersample/8
  end;

const
  idriff = 'RIFF';
  idwave = 'WAVE';
  idfmt = 'fmt ';
  iffact = 'fact';
  iddata = 'data';
  idlist = 'LIST';

function ReadWaveHeader(AStream: TStream; out AFormat: TWaveFormat): longint;
var
  Riff: TRiffHeader;
  Fmt: TFmtchunk;
  Fmtext: TFmtchunkext;
  Factchunk: TFactchunk;
  Datachunk: TDatachunk;
  ckid: array[0..3] of char;
  cksize: longword;
  BodyStart: int64;
  FmtFound, DataFound: boolean;
begin
  Result := 0;
  AFormat := Default(TWaveFormat);
  Fmt := Default(TFmtchunk);
  Fmtext := Default(TFmtchunkext);
  Datachunk := Default(TDatachunk);

  if AStream.Read(Riff, sizeof(Riff)) <> sizeof(Riff) then Result := -1;
  if Riff.ckid  <> idriff then Result := -1;
  if Riff.waveid <> idwave then Result := -1;
  if Result <> 0 then Exit;
  Riff.cksize := leton(Riff.cksize);
  {$ifopt D+}
  writeln('riff.ckid          ', Riff.ckid);
  writeln('riff.cksize        ', Riff.cksize);
  writeln('riff.format        ', Riff.waveid);
  {$endif}

  Factchunk.samplelength := 0;
  FmtFound  := False;
  DataFound := False;
  // Walk the RIFF chunk list. Each chunk is ckID(4) + ckSize(4) + body[ckSize],
  // padded to an even length. Unknown chunks (LIST, fact, bext, cue, ...) are
  // skipped by their declared size: this is the robust, spec-compliant way and
  // replaces the old byte-by-byte scan that could false-match "data" inside a
  // metadata chunk and ignored the chunk boundaries entirely.
  while (Result = 0) and (not DataFound) do
  begin
    if AStream.Read(ckid, sizeof(ckid)) <> sizeof(ckid) then Break;   // clean EOF
    if AStream.Read(cksize, sizeof(cksize)) <> sizeof(cksize) then Result := -1;
    if Result <> 0 then Break;
    cksize    := leton(cksize);
    BodyStart := AStream.Position;

    if ckid = idfmt then
    begin
      // mandatory 16-byte body (FormatTag..BitsPerSample); valid for cksize 16/18/40
      if AStream.Read(Fmt.FormatTag, 16) <> 16 then Result := -1;
      if Result = 0 then
      begin
        Fmt.ckid          := idfmt;
        Fmt.cksize        := cksize;
        Fmt.FormatTag     := leton(Fmt.FormatTag);
        Fmt.Channels      := leton(Fmt.Channels);
        Fmt.samplespersec := leton(Fmt.samplespersec);
        Fmt.bytespersec   := leton(Fmt.bytespersec);
        Fmt.blockalign    := leton(Fmt.blockalign);
        Fmt.BitsPerSample := leton(Fmt.BitsPerSample);
        // full GUID extension is present only with the 40-byte fmt layout
        if cksize >= 40 then
        begin
          if AStream.Read(Fmtext, sizeof(Fmtext)) <> sizeof(Fmtext) then Result := -1;
          if Result = 0 then
          begin
            Fmtext.cbsize             := leton(Fmtext.cbsize);
            Fmtext.validbitspersample := leton(Fmtext.validbitspersample);
            Fmtext.channelmask        := leton(Fmtext.channelmask);
            Fmtext.subcode            := leton(Fmtext.subcode);
          end;
        end;
        FmtFound := True;
      end;
    end
    else if ckid = iffact then
    begin
      // optional; carries the per-channel sample count (kept for diagnostics)
      if AStream.Read(Factchunk.samplelength, sizeof(Factchunk.samplelength)) <> sizeof(Factchunk.samplelength) then Result := -1;
      if Result = 0 then Factchunk.samplelength := leton(Factchunk.samplelength);
    end
    else if ckid = iddata then
    begin
      Datachunk.subck2id   := iddata;
      Datachunk.subck2size := cksize;
      DataFound := True;
      Break;   // leave the stream positioned at the first audio byte for ReadWaveSamples
    end;

    // jump to the next chunk header: body length plus the pad byte if odd
    if (Result = 0) and (not DataFound) then
      AStream.Position := BodyStart + cksize + (cksize and 1);
  end;

  if Result <> 0 then Exit;
  if not FmtFound  then Exit(-1);
  if not DataFound then Exit(-1);

  // Resolve the effective format. For WAVE_FORMAT_EXTENSIBLE the real format is
  // the leading code of the GUID (Fmtext.subcode), not FormatTag.
  if Fmt.FormatTag = WAVE_FORMAT_EXTENSIBLE then
    AFormat.Format := Fmtext.subcode
  else
    AFormat.Format := Fmt.FormatTag;

  if Fmt.Channels  = 0 then Result := -2;
  if Fmt.blockalign = 0 then Result := -1;
  // Every meter divides by the sample rate.
  if Fmt.samplespersec = 0 then Result := -1;
  // ReadWaveSamples de-interleaves tightly packed frames: reject layouts whose
  // block alignment disagrees with channels * bytes per sample.
  if Fmt.blockalign <> Fmt.Channels * (Fmt.BitsPerSample div 8) then Result := -1;
  // Accept integer PCM and IEEE float; reject A-law, mu-law, ADPCM and the rest.
  case AFormat.Format of
    WAVE_FORMAT_PCM:        if not (Fmt.BitsPerSample in [8, 16, 24, 32]) then Result := -1;
    WAVE_FORMAT_IEEE_FLOAT: if not (Fmt.BitsPerSample in [32, 64])        then Result := -1;
  else
    Result := -1;
  end;
  if Result <> 0 then Exit;

  {$ifopt D+}
  writeln('ffmt.subckid       ', Fmt.ckid);
  writeln('ffmt.subcksize     ', Fmt.cksize);
  writeln('ffmt.formattag     ', Fmt.FormatTag);
  writeln('ffmt.effformat     ', AFormat.Format);
  writeln('ffmt.channels      ', Fmt.Channels);
  writeln('ffmt.samplerate    ', Fmt.samplespersec);
  writeln('ffmt.byterate      ', Fmt.bytespersec);
  writeln('ffmt.blockalign    ', Fmt.blockalign);
  writeln('ffmt.bitspersample ', Fmt.BitsPerSample);
  writeln('fact.samplelength  ', Factchunk.samplelength);
  writeln('data.subck2id      ', Datachunk.subck2id);
  writeln('data.subck2size    ', Datachunk.subck2size);
  {$endif}

  AFormat.Channels         := Fmt.Channels;
  AFormat.ChannelMask      := Fmtext.channelmask;
  AFormat.SampleRate       := Fmt.samplespersec;
  AFormat.ByteRate         := Fmt.bytespersec;
  AFormat.BlockAlign       := Fmt.blockalign;
  AFormat.BitsPerSample    := Fmt.BitsPerSample;
  AFormat.FactSampleLength := Factchunk.samplelength;
  AFormat.DataSize         := Datachunk.subck2size;

  if Datachunk.subck2size = 0 then Result := -2;
end;

function ReadWaveSamples(AStream: TStream; const AFormat: TWaveFormat;
  AChannels: TDoubleMatrix; ASampleCount: longint): longint;
const
  // Frames decoded per stream read: bounds the byte buffer to a few MB and
  // keeps every single Read below the Longint count limit of TStream.Read.
  ChunkFrames = 65536;
var
  i, j, k, Index: longint;
  First, Last, ChunkBytes: longint;
  Buffer: array of byte;
  InvNorm: TDouble;
begin
  Result := 0;
  Buffer := nil;
  SetLength(Buffer, ChunkFrames * AFormat.BlockAlign);

  First := 0;
  while First < ASampleCount do
  begin
    Last := Min(First + ChunkFrames, ASampleCount) - 1;
    ChunkBytes := (Last - First + 1) * AFormat.BlockAlign;
    if AStream.Read(Buffer[0], ChunkBytes) <> ChunkBytes then
      Exit(-1);

    // Dispatch once per chunk on the (format, bit depth) pair, then run a
    // single, type-specialized pass that de-interleaves AND normalizes in one
    // go (no second sweep over the matrix). The integer paths multiply by
    // 1/full-scale: that scale is a power of two, so its reciprocal is exactly
    // representable and the product is bit-identical to the division.
    Index := 0;
    case AFormat.Format of
      WAVE_FORMAT_PCM:
        case AFormat.BitsPerSample of
          8:
            begin
              InvNorm := 1.0 / 128.0;          // exact (2^-7); unsigned, zero at 128
              for i := First to Last do
                for j := 0 to AFormat.Channels - 1 do
                begin
                  AChannels[j][i] := (pbyte(@Buffer[Index])^ - 128.0) * InvNorm;
                  Inc(Index, 1);
                end;
            end;
          16:
            begin
              InvNorm := 1.0 / 32768.0;        // exact (2^-15)
              for i := First to Last do
                for j := 0 to AFormat.Channels - 1 do
                begin
                  AChannels[j][i] := psmallint(@Buffer[Index])^ * InvNorm;
                  Inc(Index, 2);
                end;
            end;
          24:
            begin
              InvNorm := 1.0 / 8388608.0;      // exact (2^-23)
              for i := First to Last do
                for j := 0 to AFormat.Channels - 1 do
                begin
                  if Buffer[Index + 2] > 127 then
                    k := longint($FFFFFFFF)
                  else
                    k := 0;

                  k := (k shl 8) or Buffer[Index + 2];
                  k := (k shl 8) or Buffer[Index + 1];
                  k := (k shl 8) or Buffer[Index + 0];

                  AChannels[j][i] := k * InvNorm;
                  Inc(Index, 3);
                end;
            end;
          32:
            begin
              InvNorm := 1.0 / 2147483648.0;   // exact (2^-31)
              for i := First to Last do
                for j := 0 to AFormat.Channels - 1 do
                begin
                  AChannels[j][i] := plongint(@Buffer[Index])^ * InvNorm;
                  Inc(Index, 4);
                end;
            end;
        end;
      WAVE_FORMAT_IEEE_FLOAT:
        // Raw little-endian IEEE samples, already in [-1, +1] (overs may exceed
        // it and are kept on purpose for true-peak). Widening single -> double
        // and reading double are both exact on an LE host, so no scaling.
        case AFormat.BitsPerSample of
          32:
            for i := First to Last do
              for j := 0 to AFormat.Channels - 1 do
              begin
                AChannels[j][i] := psingle(@Buffer[Index])^;
                Inc(Index, 4);
              end;
          64:
            for i := First to Last do
              for j := 0 to AFormat.Channels - 1 do
              begin
                AChannels[j][i] := pdouble(@Buffer[Index])^;
                Inc(Index, 8);
              end;
        end;
    end;
    First := Last + 1;
  end;
end;

end.
