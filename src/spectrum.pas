{
  Description: Spectrum routines.

  Copyright (C) 2026 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit Spectrum;

{$mode objfpc}{$h+}
{$modeswitch advancedrecords}

interface

uses
  Common, SysUtils, ufft, utypes;

const
  DEFAULTWINDOWSIZE = 1024;
  DEFAULTHOPSIZE    = DEFAULTWINDOWSIZE div 2;

type
  TSpectrums = record
  private
    FChannels: TDoubleMatrix;
    FWindowCount: longint;
    FWindowSize: longint;
    FHopSize: longint;
    FOutBins: longint;
    FTick: TTickMethod;
    FWindow: TDoubleVector;   // coefficienti di Hann precalcolati
    FBuff: TCompVector;       // buffer di input FFT riutilizzabile
    procedure BuildWindow;
    procedure GetSpectrum(ASamples: PDouble; ASpectrum: PDouble);
    procedure SmoothSpectrogram;
  public
    procedure Init(AWindowSize: longint = DEFAULTWINDOWSIZE;
      AHopSize: LongInt = DEFAULTHOPSIZE; const ATick: TTickMethod = nil);
    procedure Finalize;

    function EstimatedTicks(AChannelCount, ASampleCount, ASampleRate: longint): longint;
    procedure Process(const AChannels: TDoubleMatrix; ASampleCount, ASampleRate: longint);

    property WindowSize: longint read FWindowSize;
    property WindowCount: longint read FWindowCount;
    property HopSize: longint read FHopSize;
    property OutBins: longint read FOutBins;

    property Channels: TDoubleMatrix read FChannels;
  end;


implementation

uses
  Math;

procedure TSpectrums.BuildWindow;
var
  i: longint;
begin
  SetLength(FWindow, FWindowSize);
  SetLength(FBuff,   FWindowSize);
  for i := 0 to FWindowSize - 1 do
    FWindow[i] := 0.5 - 0.5 * cos(2 * pi * i / (FWindowSize - 1));
end;

procedure TSpectrums.GetSpectrum(ASamples: PDouble; ASpectrum: PDouble);
var
  i, n: longint;
  Freq: TCompVector;
  scale, scaleEdge: TDouble;
begin
  n := FWindowSize;
  for i := 0 to n - 1 do
  begin
    FBuff[i].x := FWindow[i] * ASamples^;
    FBuff[i].y := 0;
    Inc(ASamples);
  end;
  Freq := FFT(n, FBuff);

  scaleEdge := 1 / n;
  scale     := 2 / n;

  // bin 0 (DC)
  ASpectrum^ := Abs(Freq[0].x) * scaleEdge;
  Inc(ASpectrum);
  // bin da 1 a N/2 - 1
  for i := 1 to (n div 2) - 1 do
  begin
    ASpectrum^ := Sqrt(Sqr(Freq[i].x) + Sqr(Freq[i].y)) * scale;
    Inc(ASpectrum);
  end;
  // bin N/2 (Nyquist), se N è pari
  if (n and 1) = 0 then
    ASpectrum^ := Abs(Freq[n div 2].x) * scaleEdge;
end;

// TSpectrums

procedure TSpectrums.Init(AWindowSize: longint = DEFAULTWINDOWSIZE;
  AHopSize: LongInt = DEFAULTHOPSIZE; const ATick: TTickMethod = nil);
begin
  Finalize;
  FTick := ATick;

  if AWindowSize <= 0 then
    AWindowSize := DEFAULTWINDOWSIZE;
  if AHopSize <= 0 then
    AHopSize := DEFAULTHOPSIZE;

  FWindowSize := AWindowSize;
  FHopSize    := AHopSize;
  BuildWindow;
end;

procedure TSpectrums.Finalize;
begin
  FWindowCount := 0;
  FWindowSize  := 0;
  FHopSize     := 0;
  FOutBins     := 0;
  SetLength(FWindow, 0);
  SetLength(FBuff, 0);
  SetLength(FChannels, 0, 0);
end;

function TSpectrums.EstimatedTicks(AChannelCount, ASampleCount, ASampleRate: longint): longint;
begin
  if ASampleCount >= FWindowSize then
    result := ((ASampleCount - FWindowSize) div FHopSize + 1) * AChannelCount
  else
    result := 0;
end;

procedure TSpectrums.Process(const AChannels: TDoubleMatrix; ASampleCount, ASampleRate: longint);
var
  ch, i: longint;
begin
  if ASampleCount >= FWindowSize then
    FWindowCount := (ASampleCount - FWindowSize) div FHopSize + 1
  else
    FWindowCount := 0;

  if FWindowCount > 0 then
  begin
    FOutBins := FWindowSize div 2 + 1;

    SetLength(FChannels, Length(AChannels), FWindowCount * OutBins);
    for ch := Low(AChannels) to High(AChannels) do
    begin
      for i := 0 to FWindowCount - 1 do
      begin
        GetSpectrum(@AChannels[ch][(i * FHopSize)], @FChannels[ch][(i * OutBins)]);

        if Assigned(FTick) then FTick;
      end;
    end;
    SmoothSpectrogram;
  end else
    Finalize;
end;

procedure TSpectrums.SmoothSpectrogram;
var
  ch, i, j, index: longint;
  Tmp: TDoubleVector;
begin
  SetLength(Tmp, FWindowCount * FOutBins);

  // --- Smoothing sull'asse delle frequenze ---
  for ch := Low(FChannels) to High(FChannels) do
  begin
    Move(FChannels[ch][0], Tmp[0], Length(FChannels[ch]) * SizeOf(TDouble));

    for i := 0 to FWindowCount - 1 do
    begin
      for j := 1 to FOutBins - 2 do
      begin
        index := i * FOutBins + j;

        FChannels[ch][index] := (Tmp[index - 1] + 2 * Tmp[index] + Tmp[index + 1]) / 4;
      end;
    end;
  end;

  // --- Smoothing sull'asse del tempo ---
  for ch := Low(FChannels) to High(FChannels) do
  begin
    Move(FChannels[ch][0], Tmp[0], Length(FChannels[ch]) * SizeOf(TDouble));

    for i := 1 to FWindowCount - 2 do
    begin
      for j := 0 to FOutBins - 1 do
      begin
        index := i * FOutBins + j;

        FChannels[ch][index] := (Tmp[index - FOutBins] + 2 * Tmp[index] + Tmp[index + FOutBins]) / 4;
      end;
    end;
  end;
end;

end.
