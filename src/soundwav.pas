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

unit Soundwav;

{$mode objfpc}

interface

uses
  Classes, Common, dynamicrange, spectrum, SysUtils, Math, loudness, ufft, utypes;

  // WAVE utils

type
  // WAV is formed by the following structures in this order
  // all items are in little endian order, except the char arrays
  // items might be in big endian order if the RIFF identifier is RIFX

  TRiffheader = packed record
    ckid: array [0..3] of char; // should be "RIFF"
    cksize: longword;             // 4 + (8 + subchunk1size) + (8 + subchunk2size).
    // the entire file size excluding TRiffheader.id and .size
    waveid: array [0..3] of char; // should be "WAVE"
  end;

  TFmtchunk = packed record
    ckid: array [0..3] of char;  // should be "fmt "
    cksize: longword;              // subchunk1size: 16, 18 or 40
    formattag: word;
    // pcm = 1 (linear quantization), values > 1 indicate a compressed format
    channels: word;                  // mono = 1, stereo = 2, etc
    samplespersec: longword;              // 8000, 44100, etc
    bytespersec: longword;
    // = samplerate * numchannels * bitspersample/8
    blockalign: word;                  // = numchannels * bitspersample/8
    bitspersample: word;                  // examples: 8 bits, 16 bits, etc
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
    // spectrum
    FSpectrums: TSpectrums;
    // loudness
    FLoudness: TLoudnessMeter;
    // dynamicrange
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

  TTrackAnalyzer = class(tthread)
  private
    FFmt: TFmtchunk;
    FFmtext: TFmtchunkext;
    FDatachunk: TDatachunk;
    //---
    FTrack: TTrack;
    //---
    FStatus: longint;
    FStream: tstream;
    FPercentage: double;
    FOnStart: tthreadmethod;
    FOnStop: tthreadmethod;
    FOnTick: tthreadmethod;
    FFFTOn: boolean;
    function GetPercentage: longint;
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
    property Percentage: longint read GetPercentage;
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
    procedure SaveToFile(const AFilename: string);
  public
    property Count: longint read GetCount;
    property Tracks[AIndex: longint]: TTrack read GetTrack; default;
  end;

function IsFileSupported(AFileExtension: string): boolean;

var
  AudioAnalyzer: TTrackAnalyzer = nil;


implementation

uses
  dateutils;

const
  idriff = 'RIFF';
  idwave = 'WAVE';
  idfmt = 'fmt ';
  iffact = 'fact';
  iddata = 'data';
  idlist = 'LIST';

// usefull routines

function IsFileSupported(AFileExtension: string): boolean;
begin
  AFileExtension := lowercase(AFileExtension);
  if AFileExtension = '.wav'  then exit(True);
  if AFileExtension = '.flac' then exit(True);
  if AFileExtension = '.mp3'  then exit(True);
  if AFileExtension = '.ape'  then exit(True);
  if AFileExtension = '.ogg'  then exit(True);
  if AFileExtension = '.m4a'  then exit(True);
  if AFileExtension = '.ac3'  then exit(True);
  Result := False;
end;

function CompareTrackName(item1, item2: pointer): longint;
begin
  Result := AnsiCompareFileName(TTrack(item1).FFilename, TTrack(item2).FFilename);
end;

// TTrack

constructor TTrack.Create(const AFilename: string);
begin
  inherited Create;
  FFilename := AFilename;
  FAlbum := '';
  FNumber := 0;
  FSampleRate := 0;
  FBitsPerSample := 0;
  FByterate := 0;

  FDRMeter.Init;
  FLoudness.Init;
  FSpectrums.Init(DEFAULTWINDOWSIZE, DEFAULTWINDOWSIZE div 2);
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
end;

// TTrackAnalyzer

constructor TTrackAnalyzer.Create(atrack: TTrack; astream: tstream; aFFTOn: boolean);
begin
  FFFTOn := affton;
  FTrack := atrack;
  FStream := astream;
  FreeOnTerminate := False;

  inherited Create(True);
end;

destructor TTrackAnalyzer.Destroy;
begin
  inherited Destroy;
end;

procedure TTrackAnalyzer.Execute;
var
  ch, i, j: longint;
begin
  FPercentage := 0;
  if assigned(FOnStart) then
    synchronize(FOnStart);

  ReadStream(FStream);
  if FStatus = 0 then
    if FFmt.channels > 0 then
    begin
      for i := 0 to FFmt.channels - 1 do
      begin
        (*
        FTrack.fchannels[i].dr       := getdr(i);
        FTrack.fchannels[i].rms      := getrms(i);
        FTrack.fchannels[i].peak     := getpeak(i);
        FTrack.fchannels[i].truepeak := gettruepeak(i);
        FTrack.fchannels[i].lufs     := lufs(FTrack.fchannels[i].samples, FTrack.Samplerate);
        FTrack.fchannels[i].lra      := lra (FTrack.fchannels[i].samples, FTrack.Samplerate);
        {$ifopt D+}
        writeln;
        writeln('track.DR  [', i,'] ',         FTrack.fchannels[i].dr       :2:1);
        writeln('track.Rms [', i,'] ', decibel(FTrack.fchannels[i].rms     ):2:2);
        writeln('track.Peak[', i,'] ', decibel(FTrack.fchannels[i].peak    ):2:2);
        writeln('track.TPL [', i,'] ', decibel(FTrack.fchannels[i].truepeak):2:2);
        {$endif}
        *)
      end;

      {$ifopt D+}
      writeln;
      writeln('track.DR:         ', FTrack.drmeter.DR       :2:2);
      writeln('track.Rms         ', FTrack.loudness.Rms                :2:2);
      writeln('track.Peak        ', FTrack.loudness.Peak               :2:2);
      writeln('track.TruePeak    ', FTrack.loudness.TruePeak           :2:2);
      writeln('track.Lk          ', FTrack.loudness.IntegratedLoudness :2:2);
      writeln('track.LRA         ', FTrack.loudness.LoudnessRange      :2:2);
      writeln('track.PLR         ', FTrack.loudness.PeakToLoudnessRatio:2:2);
      writeln;

      for ch := 0 to FTrack.Channelcount -1 do
        writeln('track.Peak', ch, '       ', FTrack.loudness.Peak(ch):2:2);
      for ch := 0 to FTrack.Channelcount -1 do
        writeln('track.TruePeak', ch, '   ', FTrack.loudness.TruePeak(ch):2:2);

      for i := 0 to FTrack.Duration * 10 + 1 do
        writeln('track.S@',i * 100, 'ms       ', FTrack.loudness.ShortTermLoudness(i*100):2:2);

      writeln;
      {$endif}
    end;

  FPercentage := 100;
  if assigned(FOnStop) then
    synchronize(FOnStop);
end;

procedure TTrackAnalyzer.ReadStream(AStream: TStream);
var
  ch, i, j, k: longint;
  step, steps: longint;
  ticktime: tdatetime;
begin
  {$ifopt D+}
  writeln('track.name         ', FTrack.Filename);
  {$endif}
  //read headers
  ReadHeader(astream);
  if Status <> 0 then exit;
  // update track details
  FTrack.FAlbum := '';
  FTrack.FNumber := 0;
  FTrack.FSampleRate := FFmt.samplespersec;
  FTrack.FBitsPerSample := FFmt.bitspersample;
  FTrack.FChannelCount := FFmt.channels;
  FTrack.FSampleRate := FFmt.samplespersec;
  FTrack.FByterate := FFmt.bytespersec;
  FTrack.FDuration := 0;

  if FTrack.FSampleRate > 0 then
  begin
    FTrack.FDuration := (FDatachunk.subck2size div FFmt.blockalign) div
      FTrack.FSampleRate;
  end;
  FTrack.FSampleCount := FDatachunk.subck2size div FFmt.blockalign;

  SetLength(FTrack.FChannels, FTrack.FChannelCount, FTrack.FSampleCount);
  ReadChannels(AStream, FTrack.FChannels, FTrack.FSampleCount);

  FTrack.FDRMeter.Process(FTrack.FChannels, FTrack.FSampleCount, FTrack.FSampleRate);
  FTrack.FLoudness.Process(FTrack.FChannels, FTrack.FSampleCount, FTrack.FSampleRate);

  // calculate spectrum (FFT)
  if FFFTOn then
  begin
    FTrack.Spectrums.Process(FTrack.FChannels, FTrack.FSampleCount, FTrack.FSampleRate);
  end;
end;

procedure TTrackAnalyzer.ReadHeader(AStream: tstream);
var
  marker: array[0..3] of char;
  riff: TRiffheader;
begin
  if AStream.Read(riff, sizeof(riff)) <> sizeof(riff) then FStatus := -1;
  if riff.ckid <> idriff then FStatus := -1;
  if riff.waveid <> idwave then FStatus := -1;
  riff.cksize := leton(riff.cksize);
  {$ifopt D+}
  writeln('riff.ckid          ', riff.ckid);
  writeln('riff.cksize        ', riff.cksize);
  writeln('riff.format        ', riff.waveid);
  {$endif}
  if AStream.Read(FFmt, sizeof(FFmt)) <> sizeof(FFmt) then FStatus := -1;
  if FFmt.ckid <> idfmt then FStatus := -1;

  FFmt.cksize := leton(FFmt.cksize);
  FFmt.formattag := leton(FFmt.formattag);
  FFmt.channels := leton(FFmt.channels);
  FFmt.samplespersec := leton(FFmt.samplespersec);
  FFmt.bytespersec := leton(FFmt.bytespersec);
  FFmt.blockalign := leton(FFmt.blockalign);
  FFmt.bitspersample := leton(FFmt.bitspersample);

  if FFmt.channels = 0 then FStatus := -2;
  if FFmt.bitspersample = 0 then FStatus := -1;
  if FFmt.bitspersample = 32 then FStatus := -1;

  {$ifopt D+}
  writeln('ffmt.subckid       ', FFmt.ckid);
  writeln('ffmt.subcksize     ', FFmt.cksize);
  writeln('ffmt.formattag     ', FFmt.formattag);
  writeln('ffmt.channels      ', FFmt.channels);
  writeln('ffmt.samplerate    ', FFmt.samplespersec);
  writeln('ffmt.byterate      ', FFmt.bytespersec);
  writeln('ffmt.blockalign    ', FFmt.blockalign);
  writeln('ffmt.bitspersample ', FFmt.bitspersample);
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
  fillchar(marker, sizeof(marker), ' ');
  while AStream.Read(marker[3], 1) = 1 do
  begin
    if marker = iddata then
    begin
      FDatachunk.subck2id := iddata;
      AStream.Read(FDatachunk.subck2size,
        sizeof(FDatachunk.subck2size));
      break;
    end;
    move(marker[1], marker[0], sizeof(marker) - 1);
  end;

  if FDatachunk.subck2id <> iddata then FStatus := -1;
  FDatachunk.subck2size := leton(FDatachunk.subck2size);
  {$ifopt D+}
  writeln('data.subck2id      ', FDatachunk.subck2id);
  writeln('data.subck2size    ', FDatachunk.subck2size);
  {$endif}
  if FDatachunk.subck2size = 0 then FStatus := -2;
end;

function TTrackAnalyzer.ReadChannels(AStream: TStream; AChannels: TDoubleMatrix;
  ASampleCount: longint): longint;
var
  i, j, k: longint;
  dt: array[0..3] of byte;
begin
  Result := 0;
  for i := 0 to ASampleCount - 1 do
  begin
    for j := 0 to FFmt.channels - 1 do
    begin
      if FFmt.bitspersample = 8 then
      begin
        if AStream.Read(dt[0], 1) <> 1 then FStatus := -1;
        AChannels[j][i] := pbyte(@dt[0])^;
      end
      else
      if FFmt.bitspersample = 16 then
      begin
        if AStream.Read(dt[0], 2) <> 2 then FStatus := -1;
        AChannels[j][i] := psmallint(@dt[0])^;
      end
      else
      if FFmt.bitspersample = 24 then
      begin
        if AStream.Read(dt[0], 3) <> 3 then FStatus := -1;

        if dt[2] > 127 then
          k := longint($FFFFFFFF)
        else
          k := 0;

        k := (k shl 8) or dt[2];
        k := (k shl 8) or dt[1];
        k := (k shl 8) or dt[0];

        AChannels[j][i] := k;
      end
      else
      if FFmt.bitspersample = 32 then
      begin
        if AStream.Read(dt[0], 4) <> 4 then FStatus := -1;
        AChannels[j][i] := plongint(@dt[0])^;
      end;
    end;
  end;
  PrepareSamplesForAnalysis;
end;

procedure TTrackAnalyzer.PrepareSamplesForAnalysis;
var
  i, j: longint;
  MinValue, MaxValue: double;
  Meanvalue: double;
  Morm: double;
begin
  Morm := 1 shl (FTrack.FBitsPerSample - 1);

  for i := low(FTrack.FChannels) to high(FTrack.FChannels) do
  begin
    MinValue := maxfloat;
    MaxValue := -maxfloat;
    for j := low(FTrack.FChannels[i]) to high(FTrack.FChannels[i]) do
    begin
      MinValue := min(MinValue, FTrack.FChannels[i][j]);
      MaxValue := max(MaxValue, FTrack.FChannels[i][j]);
    end;

    Meanvalue := (MaxValue + MinValue) / 2;
    for j := low(FTrack.FChannels[i]) to high(FTrack.FChannels[i]) do
    begin
      FTrack.FChannels[i][j] := (FTrack.FChannels[i][j] - Meanvalue) / Morm;
    end;
  end;
end;

function TTrackAnalyzer.GetPercentage: longint;
begin
  Result := Round(FPercentage);
end;

// TTrackList

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
  track: TTrack;
begin
  track := TTrack.Create(ATrackname);
  FList.add(track);
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
  FList.sort(@comparetrackname);
end;

function TTrackList.GetCount: longint;
begin
  Result := FList.Count;
end;

function TTrackList.GetTrack(AIndex: longint): TTrack;
begin
  Result := TTrack(FList[AIndex]);
end;

procedure TTrackList.SaveToFile(const AFilename: string);
const
  splitter = '--------------------------------------------------------------------------------';
var
  dr: double;
  i: longint;
  s: TStringList;
  track: TTrack;
begin
  s := TStringList.Create;
  s.add('AudioMeter 0.5.0 - Dynamic Range Meter');
  s.add(splitter);
  s.add(format('Log date : %s', [datetimetostr(now)]));
  s.add(splitter);
  s.add('');

  dr := 0;
  if Count > 0 then
  begin
    track := GetTrack(0);
    s.add('DR      Peak        RMS     bps  chs      SR  Duration  Track');
    s.add(splitter);
    for i := 0 to Count - 1 do
    begin
      track := GetTrack(i);
      s.add(format('DR%2.0f %7.2f dB %7.2f dB %4.0d %4.0d %7.0d %-s     %s',
        [(track.drmeter.DR), decibel(track.loudness.Peak),
        decibel(track.loudness.Rms), track.Bitspersample,
        track.Channelcount, track.Samplerate,
        format('%3.2d:%2.2d', [track.FDuration div (60), track.FDuration mod (60)]),
        extractfilename(track.FFilename)]));

      dr := dr + track.drmeter.DR;
    end;
    dr := dr / Count;

    s.add(splitter);
    s.add('');
    s.add('Number of tracks:  %d', [Count]);
    if dr > 0 then s.add('Official DR value: %1.0f', [dr]);
    if dr <= 0 then s.add('Official DR value: ---');

    s.add('');
    s.add(splitter);
  end;
  s.savetofile(AFilename);
  s.Destroy;
end;

end.
