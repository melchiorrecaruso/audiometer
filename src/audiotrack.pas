{
  Description: Audio track and track list.

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

unit AudioTrack;

{$mode objfpc}{$H+}

interface

uses
  Classes, Common, DynamicRange, Spectrum, SysUtils, Math, Loudness, WavReader;

type
  // TTrack

  TTrack = class
  private
    FFilename: string;
    FSampleRate: longword;
    FSampleCount: longint;
    FChannelCount: longint;
    FChannelMask: longword;
    FChannels: TDoubleMatrix;
    FBitsPerSample: longint;
    FDuration: longint;
    // Cached min/max waveform envelope for the requested render width.
    FWaveCacheWidth: longint;
    FWaveMin: TDoubleMatrix;
    FWaveMax: TDoubleMatrix;
    // Spectrum
    FSpectrums: TSpectrums;
    // Loudness
    FLoudness: TLoudnessMeter;
    // Dynamicrange
    FDRMeter: TDynamicRangeMeter;
    function GetDurationSeconds: double;
  public
    constructor Create(const AFilename: string);
    destructor Destroy; override;
    // Requires positive sample rate/channel count and nonnegative frame count.
    // Allocates fresh zeroed samples and resets analysis/cache state. Invalid
    // arguments or allocation failure leave the current track unchanged.
    procedure SetFormat(const AFormat: TWaveFormat; ASampleCount: longint);
    procedure ClearChannels;
    // Call after changing samples through Channels, before rendering again.
    // Like other track mutations, this must not run concurrently with a drawer.
    procedure InvalidateWaveformCache;
    procedure PrepareWaveformCache(AWidth: longint);
    function WaveformMin(AChannel, AX: longint): double;
    function WaveformMax(AChannel, AX: longint): double;
  public
    property Filename: string read FFilename;
    property SampleCount: longint read FSampleCount;
    property SampleRate: longword read FSampleRate;
    property ChannelCount: longint read FChannelCount;
    property ChannelMask: longword read FChannelMask;
    property Channels: TDoubleMatrix read FChannels;
    property BitsPerSample: longint read FBitsPerSample;
    property Duration: longint read FDuration;
    property DurationSeconds: double read GetDurationSeconds;

    property Spectrums: TSpectrums read FSpectrums;
    property Loudness: TLoudnessMeter read FLoudness;
    property DRMeter: TDynamicRangeMeter read FDRMeter;
  end;

  // TTrackList

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
  public
    property Tracks[AIndex: longint]: TTrack read GetTrack; default;
    property Count: longint read GetCount;
  end;


implementation

// TTrack

constructor TTrack.Create(const AFilename: string);
begin
  inherited Create;
  FFilename      := AFilename;
  FSampleRate    := 0;
  FBitsPerSample := 0;
  FWaveCacheWidth := 0;
end;

destructor TTrack.Destroy;
begin
  ClearChannels;
  FDRMeter.Finalize;
  FLoudness.Finalize;
  inherited Destroy;
end;

function TTrack.GetDurationSeconds: double;
begin
  if FSampleRate > 0 then Result := FSampleCount / FSampleRate
  else Result := 0;
end;

procedure TTrack.SetFormat(const AFormat: TWaveFormat; ASampleCount: longint);
var
  NewChannels: TDoubleMatrix;
begin
  if ASampleCount < 0 then
    raise EArgumentException.Create('Sample count must not be negative');
  if AFormat.SampleRate = 0 then
    raise EArgumentException.Create('Sample rate must be positive');
  if AFormat.Channels = 0 then
    raise EArgumentException.Create('Channel count must be positive');

  // Allocate first, so failure cannot leave metadata and sample arrays out
  // of sync. Fresh arrays also avoid retaining samples from an earlier file.
  NewChannels := nil;
  SetLength(NewChannels, AFormat.Channels, ASampleCount);
  ClearChannels;
  FDRMeter.Finalize;
  FLoudness.Finalize;
  FChannels := NewChannels;
  FSampleRate    := AFormat.SampleRate;
  FBitsPerSample := AFormat.BitsPerSample;
  FChannelCount  := AFormat.Channels;
  FChannelMask   := AFormat.ChannelMask;
  FSampleCount   := ASampleCount;
  FDuration      := Trunc(GetDurationSeconds);
end;

procedure TTrack.ClearChannels;
begin
  SetLength(FChannels, 0, 0);
  InvalidateWaveformCache;
  FSpectrums.Finalize;
end;

procedure TTrack.InvalidateWaveformCache;
begin
  FWaveCacheWidth := 0;
  FWaveMin := nil;
  FWaveMax := nil;
end;

procedure TTrack.PrepareWaveformCache(AWidth: longint);
var
  ch, i, X: longint;
  FirstSample, LastSample: longint;
  ZMin, ZMax: double;
  NewMin, NewMax: TDoubleMatrix;
begin
  if (AWidth <= 0) or (FSampleCount <= 0) or
     (Length(FChannels) = 0) then
  begin
    InvalidateWaveformCache;
    Exit;
  end;

  for ch := Low(FChannels) to High(FChannels) do
    if Length(FChannels[ch]) < FSampleCount then
      raise EArgumentException.Create('Sample count exceeds channel length');

  if (FWaveCacheWidth = AWidth) and
     (Length(FWaveMin) = Length(FChannels)) then Exit;

  NewMin := nil;
  NewMax := nil;
  SetLength(NewMin, Length(FChannels), AWidth);
  SetLength(NewMax, Length(FChannels), AWidth);

  for ch := Low(FChannels) to High(FChannels) do
    for X := 0 to AWidth - 1 do
    begin
      ZMin := Infinity;
      ZMax := -Infinity;
      // Partition the entire track, including its remainder. When there
      // are more columns than samples, repeat the corresponding sample.
      FirstSample := int64(X) * FSampleCount div AWidth;
      LastSample := Max(FirstSample,
        int64(X + 1) * FSampleCount div AWidth - 1);
      for i := FirstSample to LastSample do
      begin
        ZMin := Min(ZMin, FChannels[ch][i]);
        ZMax := Max(ZMax, FChannels[ch][i]);
      end;
      NewMin[ch][X] := ZMin;
      NewMax[ch][X] := ZMax;
    end;

  FWaveMin := NewMin;
  FWaveMax := NewMax;
  FWaveCacheWidth := AWidth;
end;

function TTrack.WaveformMin(AChannel, AX: longint): double;
begin
  if (AChannel >= 0) and (AChannel < Length(FWaveMin)) and
     (AX >= 0) and (AX < Length(FWaveMin[AChannel])) then
    Result := FWaveMin[AChannel][AX]
  else
    Result := 0;
end;

function TTrack.WaveformMax(AChannel, AX: longint): double;
begin
  if (AChannel >= 0) and (AChannel < Length(FWaveMax)) and
     (AX >= 0) and (AX < Length(FWaveMax[AChannel])) then
    Result := FWaveMax[AChannel][AX]
  else
    Result := 0;
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
  try
    FList.Add(Track);
  except
    Track.Free;
    raise;
  end;
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

end.
