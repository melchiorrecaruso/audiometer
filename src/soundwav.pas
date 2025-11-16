{
  Description: Sound routines.

  Copyright (C) 2020-2025 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit SoundWav;

{$mode objfpc}

interface

uses
  Classes, Common, DynamicRange, Spectrum, SysUtils, Math, Loudness;

  // WAVE Utils

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
    ckid: array [0..3] of char;  // should be "fmt "
    cksize: longword;              // subchunk1size: 16, 18 or 40
    FormatTag: word;
    // pcm = 1 (linear quantization), values > 1 indicate a compressed format
    Channels: word;                  // mono = 1, stereo = 2, etc
    samplespersec: longword;              // 8000, 44100, etc
    bytespersec: longword;
    // = samplerate * numchannels * bitspersample/8
    blockalign: word;                  // = numchannels * bitspersample/8
    BitsPerSample: word;                  // examples: 8 bits, 16 bits, etc
  end;

  TFmtchunkext = packed record
    cbsize: word;                  // size of the extension (0 or 22)
    validbitspersample: word;                  // number of valid bits
    channelmask: longword;              // speaker position mask
    subcode: word;                  // GUID data format code
    subformat: array [0..13] of byte; // GUID
  end;

  TFactchunk = packed record
    ckid: array [0..3] of char;  // should be "fact"
    cksize: longword;              // chunk size: minimum 4
    samplelength: longword;              // Number of samples (per channel)
  end;

const
  WAVE_FORMAT_PCM = $0001;
  WAVE_FORMAT_IEEE_FLOAT = $0003;
  WAVE_FORMAT_ALAW = $0006;
  WAVE_FORMAT_MULAW = $0007;
  WAVE_FORMAT_EXTENSIBLE = $fffe;

type
  TDatachunk = packed record
    subck2id: array [0..3] of char; // should be "data"
    subck2size: longword;             // == numsamples * numchannels * bitspersample/8
  end;

  // TTrack

  TTrack = class
  private
    FFilename: string;
    FAlbum: string;
    FNumber: longint;
    FSampleRate: longword;
    FSampleCount: longint;
    FChannelCount: longint;
    FChannels: TDoubleMatrix;
    FBitsPerSample: longint;
    FByterate: longint;
    FDuration: longint;
    // Spectrum
    FSpectrums: TSpectrums;
    // Loudness
    FLoudness: TLoudnessMeter;
    // Dynamicrange
    FDRMeter: TDynamicRangeMeter;
  public
    constructor Create(const AFilename: string);
    destructor Destroy; override;
    procedure ClearChannels;
  public
    property Filename: string read FFilename;
    property Album: string read FAlbum;
    property Number: longint read FNumber;
    property SampleCount: longint read FSampleCount;
    property SampleRate: longword read FSampleRate;
    property ChannelCount: longint read FChannelCount;
    property Channels: TDoubleMatrix read FChannels;
    property BitsPerSample: longint read FBitsPerSample;
    property Byterate: longint read FByterate;
    property Duration: longint read FDuration;

    property Spectrums: TSpectrums read FSpectrums;
    property Loudness: TLoudnessMeter read FLoudness;
    property DRMeter: TDynamicRangeMeter read FDRMeter;
  end;

  { TTrackAnalyzer }

  TTrackAnalyzer = class(TThread)
  private
    FFmt: TFmtchunk;
    FFmtext: TFmtchunkext;
    FDatachunk: TDatachunk;
    //---
    FTrack: TTrack;
    //---
    FStatus: longint;
    FStream: tstream;
    FPercentage: longint;
    FOnStart: tthreadmethod;
    FOnStop: tthreadmethod;
    FOnTick: tthreadmethod;
    FTick, FTickCount: longint;
    FFFTOn: boolean;
    procedure DoTick;
    procedure ReadHeader(AStream: TStream);
    procedure ReadStream(AStream: TStream);
    function ReadChannels(AStream: TStream;
      AChannels: TDoubleMatrix; ASampleCount: longint): longint;
    procedure PrepareSamplesForAnalysis;
  public
    constructor Create(ATrack: TTrack; AStream: TStream; AFFTOn: boolean);
    destructor Destroy; override;
    procedure Execute; override;
  public
    property OnStart: tthreadmethod write FOnStart;
    property OnStop: tthreadmethod write FOnStop;
    property OnTick: tthreadmethod write FOnTick;
    property Percentage: longint read FPercentage;
    property Status: longint read FStatus;
  end;

  // ttracklist

  TTrackList = class
  private
    FList: TList;
    function GetCount: longint;
    function GetTrack(AIndex: longint): TTrack;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const ATrackname: string);
    procedure Delete(AIndex: longint);
    procedure Clear;
    procedure Sort;
    procedure Save(S: TStrings);
  public
    property Tracks[AIndex: longint]: TTrack read GetTrack; default;
    property Count: longint read GetCount;
  end;

function IsFileSupported(AFileExtension: string): boolean;
function OpenDialogFileFilter: string;

var
  AudioAnalyzer: TTrackAnalyzer = nil;


implementation

uses
  DateUtils;

const
  idriff = 'RIFF';
  idwave = 'WAVE';
  idfmt = 'fmt ';
  iffact = 'fact';
  iddata = 'data';
  idlist = 'LIST';

// Usefull routines

function IsFileSupported(AFileExtension: string): boolean;
begin
  AFileExtension := LowerCase(AFileExtension);
  if AFileExtension = '.wav'  then exit(True);
  if AFileExtension = '.flac' then exit(True);
  if AFileExtension = '.mp3'  then exit(True);
  if AFileExtension = '.ape'  then exit(True);
  if AFileExtension = '.ogg'  then exit(True);
  if AFileExtension = '.m4a'  then exit(True);
  if AFileExtension = '.ac3'  then exit(True);
  Result := False;
end;

function OpenDialogFileFilter: string;
begin
  Result := 'Supported files|*.wav;*.flac;*.mp3;*.ape;*.ogg;*.m4a;*.ac3;|All files|*.*;';
end;

// TTrack

constructor TTrack.Create(const AFilename: string);
begin
  inherited Create;
  FFilename      := AFilename;
  FAlbum         := '';
  FNumber        := 0;
  FSampleRate    := 0;
  FBitsPerSample := 0;
  FByterate      := 0;
end;

destructor TTrack.Destroy;
begin
  ClearChannels;
  FDRMeter.Finalize;
  FLoudness.Finalize;
  FSpectrums.Finalize;
  inherited Destroy;
end;

procedure TTrack.ClearChannels;
begin
  SetLength(FChannels, 0, 0);
  FSpectrums.Finalize
end;

// TTrackAnalyzer

constructor TTrackAnalyzer.Create(ATrack: TTrack; AStream: TStream; AFFTOn: boolean);
begin
  FFFTOn := AFFTOn;
  FTrack := ATrack;
  FStream := AStream;
  FreeOnTerminate := True;
  inherited Create(True);
end;

destructor TTrackAnalyzer.Destroy;
begin
  inherited Destroy;
end;

procedure TTrackAnalyzer.Execute;
{$ifopt D+}
var
  ch: longint;
{$endif}
begin
  FPercentage := 0;
  if Assigned(FOnStart) then
    Synchronize(FOnStart);

  ReadStream(FStream);
  {$ifopt D+}
  writeln;
  writeln('LOUDNESS:');
  writeln('Track.Rms         ', FTrack.Loudness.Rms                :2:2);
  writeln('Track.Peak        ', FTrack.Loudness.Peak               :2:2);
  writeln('Track.TruePeak    ', FTrack.Loudness.TruePeak           :2:2);
  writeln('Track.Lk          ', FTrack.Loudness.IntegratedLoudness :2:2);
  writeln('Track.LRA         ', FTrack.Loudness.LoudnessRange      :2:2);
  writeln('Track.PLR         ', FTrack.Loudness.PeakToLoudnessRatio:2:2);

  writeln;
  writeln('DYNAMIC RANGE:');
  writeln('Track.DR:         ', FTrack.DRMeter.DR                  :2:2);
  for ch := 0 to FTrack.FChannelCount -1 do
  begin
    writeln(ChannelName(ch, FTrack.FChannelCount),'.Rms         ', Decibel(FTrack.DRMeter.Rms (ch)):2:2);
    writeln(ChannelName(ch, FTrack.FChannelCount),'.Peak        ', Decibel(FTrack.DRMeter.Peak(ch)):2:2);
    writeln(ChannelName(ch, FTrack.FChannelCount),'.DR          ', FTrack.DRMeter.DR(ch):2:2);
  end;
  writeln;
  {$endif}

  FPercentage := 100;
  if Assigned(FOnStop) then
    Synchronize(FOnStop);
end;

procedure TTrackAnalyzer.DoTick;
begin
  Inc(FTick);
  FPercentage := Trunc(100 * FTick / FTickCount);
  if Assigned(FOnTick) then
  begin
    Synchronize(FOnTick);
  end;
end;

procedure TTrackAnalyzer.ReadStream(AStream: TStream);
begin
  {$ifopt D+}
  writeln('Track.Filename     ', FTrack.Filename);
  {$endif}
  //Read headers
  ReadHeader(AStream);
  if Status <> 0 then Exit;
  // Update track details
  FTrack.FAlbum := '';
  FTrack.FNumber := 0;
  FTrack.FSampleRate := FFmt.samplespersec;
  FTrack.FBitsPerSample := FFmt.BitsPerSample;
  FTrack.FChannelCount := FFmt.Channels;
  FTrack.FSampleRate := FFmt.samplespersec;
  FTrack.FByterate := FFmt.bytespersec;
  FTrack.FDuration := 0;

  if FTrack.FSampleRate > 0 then
  begin
    FTrack.FDuration := (FDatachunk.subck2size div FFmt.blockalign) div FTrack.FSampleRate;
  end;
  FTrack.FSampleCount := FDatachunk.subck2size div FFmt.blockalign;

  SetLength(FTrack.FChannels, FTrack.FChannelCount, FTrack.FSampleCount);
  ReadChannels(AStream, FTrack.FChannels, FTrack.FSampleCount);

  // Init
  FTrack.FDRMeter .Init(@DoTick);
  FTrack.FLoudness.Init(@DoTick);
  FTrack.Spectrums.Init(DEFAULTWINDOWSIZE, DEFAULTHOPSIZE, @DoTick);

  // Estimate TickCount
  FTick := 0;
  FTickCount := 0;
  Inc(FTickCount, FTrack.FDRMeter.EstimatedTicks(FTrack.FChannelCount, FTrack.FSampleCount, FTrack.FSampleRate));
  Inc(FTickCount, FTrack.FLoudness.EstimatedTicks(FTrack.FChannelCount, FTrack.FSampleCount, FTrack.FSampleRate));
  if FFFTOn then
  begin
    Inc(FTickCount, FTrack.Spectrums.EstimatedTicks(FTrack.FChannelCount, FTrack.FSampleCount, FTrack.FSampleRate));
  end;

  // Process
  FTrack.FDRMeter.Process(FTrack.FChannels, FTrack.FSampleCount, FTrack.FSampleRate);
  FTrack.FLoudness.Process(FTrack.FChannels, FTrack.FSampleCount, FTrack.FSampleRate);
  if FFFTOn then
  begin
    FTrack.Spectrums.Process(FTrack.FChannels, FTrack.FSampleCount, FTrack.FSampleRate);
  end;
end;

procedure TTrackAnalyzer.ReadHeader(AStream: tstream);
var
  Marker: array[0..3] of char;
  Riff: TRiffHeader;
begin
  if AStream.Read(Riff, sizeof(Riff)) <> sizeof(Riff) then FStatus := -1;
  if Riff.ckid <> idriff then FStatus := -1;
  if Riff.waveid <> idwave then FStatus := -1;
  Riff.cksize := leton(Riff.cksize);
  {$ifopt D+}
  writeln('riff.ckid          ', Riff.ckid);
  writeln('riff.cksize        ', Riff.cksize);
  writeln('riff.format        ', Riff.waveid);
  {$endif}
  if AStream.Read(FFmt, sizeof(FFmt)) <> sizeof(FFmt) then FStatus := -1;
  if FFmt.ckid <> idfmt then FStatus := -1;

  FFmt.cksize := leton(FFmt.cksize);
  FFmt.FormatTag := leton(FFmt.FormatTag);
  FFmt.Channels := leton(FFmt.Channels);
  FFmt.samplespersec := leton(FFmt.samplespersec);
  FFmt.bytespersec := leton(FFmt.bytespersec);
  FFmt.blockalign := leton(FFmt.blockalign);
  FFmt.BitsPerSample := leton(FFmt.BitsPerSample);

  if FFmt.Channels = 0 then FStatus := -2;
  if FFmt.BitsPerSample = 0 then FStatus := -1;
  if FFmt.BitsPerSample = 32 then FStatus := -1;

  {$ifopt D+}
  writeln('ffmt.subckid       ', FFmt.ckid);
  writeln('ffmt.subcksize     ', FFmt.cksize);
  writeln('ffmt.formattag     ', FFmt.FormatTag);
  writeln('ffmt.channels      ', FFmt.Channels);
  writeln('ffmt.samplerate    ', FFmt.samplespersec);
  writeln('ffmt.byterate      ', FFmt.bytespersec);
  writeln('ffmt.blockalign    ', FFmt.blockalign);
  writeln('ffmt.bitspersample ', FFmt.BitsPerSample);
  {$endif}

  if FFmt.cksize = 40 then
  begin
    if AStream.Read(FFmtext, sizeof(FFmtext)) <> sizeof(FFmtext) then FStatus := -1;

    FFmtext.cbsize := leton(FFmtext.cbsize);
    FFmtext.validbitspersample := leton(FFmtext.validbitspersample);
    FFmtext.channelmask := leton(FFmtext.channelmask);
    {$ifopt D+}
    writeln;
    writeln('ffmtext.cbsize             ', FFmtext.cbsize);
    writeln('ffmtext.validbitspersample ', FFmtext.validbitspersample);
    writeln('ffmtext.channelmask        ', FFmtext.channelmask);
    writeln;
    {$endif}
  end;

  // search data section by scanning
  FillChar(Marker, sizeof(Marker), ' ');
  while AStream.Read(Marker[3], 1) = 1 do
  begin
    if Marker = iddata then
    begin
      FDatachunk.subck2id := iddata;
      AStream.Read(FDatachunk.subck2size,
        sizeof(FDatachunk.subck2size));
      break;
    end;
    move(Marker[1], Marker[0], sizeof(Marker) - 1);
  end;

  if FDatachunk.subck2id <> iddata then FStatus := -1;
  FDatachunk.subck2size := leton(FDatachunk.subck2size);
  {$ifopt D+}
  writeln('data.subck2id      ', FDatachunk.subck2id);
  writeln('data.subck2size    ', FDatachunk.subck2size);
  {$endif}
  if FDatachunk.subck2size = 0 then FStatus := -2;
end;

function TTrackAnalyzer.ReadChannels(AStream: TStream;
  AChannels: TDoubleMatrix; ASampleCount: longint): longint;
var
  i, j, k: longint;
  Sample: array[0..3] of byte;
begin
  Result := 0;
  for i := 0 to ASampleCount - 1 do
  begin
    for j := 0 to FFmt.Channels - 1 do
    begin
      if FFmt.BitsPerSample = 8 then
      begin
        if AStream.Read(Sample[0], 1) <> 1 then FStatus := -1;
        AChannels[j][i] := pbyte(@Sample[0])^;
      end
      else
      if FFmt.BitsPerSample = 16 then
      begin
        if AStream.Read(Sample[0], 2) <> 2 then FStatus := -1;
        AChannels[j][i] := psmallint(@Sample[0])^;
      end
      else
      if FFmt.BitsPerSample = 24 then
      begin
        if AStream.Read(Sample[0], 3) <> 3 then FStatus := -1;

        if Sample[2] > 127 then
          k := longint($FFFFFFFF)
        else
          k := 0;

        k := (k shl 8) or Sample[2];
        k := (k shl 8) or Sample[1];
        k := (k shl 8) or Sample[0];

        AChannels[j][i] := k;
      end
      else
      if FFmt.BitsPerSample = 32 then
      begin
        if AStream.Read(Sample[0], 4) <> 4 then FStatus := -1;
        AChannels[j][i] := plongint(@Sample[0])^;
      end;
    end;
  end;
  PrepareSamplesForAnalysis;
end;

procedure TTrackAnalyzer.PrepareSamplesForAnalysis;
var
  i, j: longint;
  MinValue, MaxValue: TDouble;
  MeanValue: TDouble;
  Norm: TDouble;
begin
  Norm := 1 shl (FTrack.FBitsPerSample - 1);

  for i := Low(FTrack.FChannels) to High(FTrack.FChannels) do
  begin
    MinValue :=  MaxFloat;
    MaxValue := -MaxFloat;
    for j := Low(FTrack.FChannels[i]) to High(FTrack.FChannels[i]) do
    begin
      MinValue := Min(MinValue, FTrack.FChannels[i][j]);
      MaxValue := Max(MaxValue, FTrack.FChannels[i][j]);
    end;

    MeanValue := (MaxValue + MinValue) / 2;
    for j := Low(FTrack.FChannels[i]) to High(FTrack.FChannels[i]) do
    begin
      FTrack.FChannels[i][j] := (FTrack.FChannels[i][j] - MeanValue) / Norm;
    end;
  end;
end;

// TTrackList

function CompareTrackName(Item1, Item2: pointer): longint;
begin
  Result := AnsiCompareFileName(TTrack(Item1).FFilename, TTrack(Item2).FFilename);
end;

constructor TTrackList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TTrackList.Destroy;
begin
  Clear;
  FList.Destroy;
  inherited Destroy;
end;

procedure TTrackList.Add(const ATrackname: string);
var
  Track: TTrack;
begin
  Track := TTrack.Create(ATrackname);
  FList.add(Track);
end;

procedure TTrackList.Delete(AIndex: longint);
begin
  TTrack(FList[AIndex]).Destroy;
  FList.Delete(AIndex);
end;

procedure TTrackList.Clear;
var
  i: longint;
begin
  for i := 0 to FList.Count - 1 do
    TTrack(FList[i]).Destroy;
  FList.Clear;
end;

procedure TTrackList.Sort;
begin
  FList.Sort(@CompareTrackname);
end;

function TTrackList.GetCount: longint;
begin
  Result := FList.Count;
end;

function TTrackList.GetTrack(AIndex: longint): TTrack;
begin
  Result := TTrack(FList[AIndex]);
end;

procedure TTrackList.Save(S: TStrings);
const
  Splitter = '--------------------------------------------------------------------------------';
var
  DR: double;
  i: longint;
  Track: TTrack;
begin
  S.Clear;
  S.Add('AudioMeter 0.5.2 - Dynamic Range Meter');
  S.Add(Splitter);
  S.Add(Format('Log date : %s', [DateTimeToStr(now)]));
  S.Add(Splitter);
  S.Add('');

  DR := 0;
  if Count > 0 then
  begin
    Track := GetTrack(0);
    S.Add('DR      Peak        RMS     bps  chs      SR  Duration  Track');
    S.Add(Splitter);
    for i := 0 to Count - 1 do
    begin
      Track := GetTrack(i);
      S.Add(Format('DR%2.0f %7.2f dB %7.2f dB %4.0d %4.0d %7.0d %-s     %s',
        [Track.DRMeter.DR,
         Decibel(Track.DRMeter.Peak),
         Decibel(Track.DRMeter.Rms),
         Track.Bitspersample,
         Track.Channelcount,
         Track.Samplerate,
         Format('%3.2d:%2.2d', [
           Track.FDuration div (60),
           Track.FDuration mod (60)]),
           ExtractFileName(Track.FFilename)]));

      DR := DR + Track.DRMeter.DR;
    end;
    DR := Trunc(DR / Count);

    S.Add(Splitter);
    S.Add('');
    S.Add('Number of tracks:  %d', [Count]);
    if DR >  0 then S.Add('Official DR value: DR%1.0f', [DR]);
    if DR <= 0 then S.Add('Official DR value: ---');

    S.Add('');
    S.Add(Splitter);
  end;
end;

end.
