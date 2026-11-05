{
  Description: Audio track and track list.

  Copyright (C) 2020-2026 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit AudioTrack;

{$mode objfpc}

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
  public
    constructor Create(const AFilename: string);
    destructor Destroy; override;
    // Takes the format of the WAV file and allocates ASampleCount frames per
    // channel for the samples.
    procedure SetFormat(const AFormat: TWaveFormat; ASampleCount: longint);
    procedure ClearChannels;
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
  FSpectrums.Finalize;
  inherited Destroy;
end;

procedure TTrack.SetFormat(const AFormat: TWaveFormat; ASampleCount: longint);
begin
  FSampleRate    := AFormat.SampleRate;
  FBitsPerSample := AFormat.BitsPerSample;
  FChannelCount  := AFormat.Channels;
  FChannelMask   := AFormat.ChannelMask;
  FDuration      := 0;
  if FSampleRate > 0 then
    FDuration := (AFormat.DataSize div AFormat.BlockAlign) div FSampleRate;
  FSampleCount   := ASampleCount;
  SetLength(FChannels, FChannelCount, FSampleCount);
end;

procedure TTrack.ClearChannels;
begin
  SetLength(FChannels, 0, 0);
  FWaveCacheWidth := 0;
  SetLength(FWaveMin, 0, 0);
  SetLength(FWaveMax, 0, 0);
  FSpectrums.Finalize;
end;

procedure TTrack.PrepareWaveformCache(AWidth: longint);
var
  ch, i, X: longint;
  FirstSample, WindowSize: longint;
  ZMin, ZMax: double;
begin
  if (AWidth <= 0) or (FSampleCount <= 0) or
     (Length(FChannels) = 0) then
  begin
    FWaveCacheWidth := 0;
    SetLength(FWaveMin, 0, 0);
    SetLength(FWaveMax, 0, 0);
    Exit;
  end;

  if (FWaveCacheWidth = AWidth) and
     (Length(FWaveMin) = Length(FChannels)) then Exit;

  SetLength(FWaveMin, Length(FChannels), AWidth);
  SetLength(FWaveMax, Length(FChannels), AWidth);
  WindowSize := FSampleCount div AWidth;

  for ch := Low(FChannels) to High(FChannels) do
    for X := 0 to AWidth - 1 do
    begin
      ZMin := Infinity;
      ZMax := -Infinity;
      if WindowSize > 0 then
      begin
        FirstSample := X * WindowSize;
        // Preserve the original drawer's fixed-size windows exactly. Any
        // remainder at the end of the track is intentionally not redistributed.
        for i := 0 to WindowSize - 1 do
        begin
          ZMin := Min(ZMin, FChannels[ch][FirstSample + i]);
          ZMax := Max(ZMax, FChannels[ch][FirstSample + i]);
        end;
      end else
      begin
        // A very short clip (or an unusually wide configured backing bitmap)
        // has fewer samples than columns. The old loop produced infinities in
        // this case; repeat the nearest sample to keep coordinates finite.
        FirstSample := longint(Int64(X) * FSampleCount div AWidth);
        FirstSample := EnsureRange(FirstSample, 0, FSampleCount - 1);
        ZMin := FChannels[ch][FirstSample];
        ZMax := ZMin;
      end;
      FWaveMin[ch][X] := ZMin;
      FWaveMax[ch][X] := ZMax;
    end;

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

end.
