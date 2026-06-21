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
    FWindow: TDoubleVector;   // precomputed Hann coefficients
    FBuff: TCompVector;       // reusable FFT input buffer
    FFreq: TCompVector;       // reusable FFT output buffer
    FScale: TDouble;          // magnitude scale for interior bins (2/N)
    FScaleEdge: TDouble;      // magnitude scale for DC and Nyquist (1/N)
    procedure BuildWindow;
    function  ComputeWindowCount(ASampleCount: longint): longint;
    procedure GetSpectrum(ASamples: PDouble; ASpectrum: PDouble);
    procedure GetSpectrumPair(ASamplesA, ASamplesB: PDouble;
      ASpectrumA, ASpectrumB: PDouble);
    procedure SmoothSpectrogram;
  public
    procedure Init(AWindowSize: longint = DEFAULTWINDOWSIZE;
      AHopSize: LongInt = DEFAULTHOPSIZE; const ATick: TTickMethod = nil);
    procedure Finalize;

    function EstimatedTicks(AChannelCount, ASampleCount: longint): longint;
    procedure Process(const AChannels: TDoubleMatrix; ASampleCount: longint);

    property WindowSize: longint read FWindowSize;
    property WindowCount: longint read FWindowCount;
    property HopSize: longint read FHopSize;
    property OutBins: longint read FOutBins;

    property Channels: TDoubleMatrix read FChannels;
  end;


implementation

uses
  Math;

function Magnitude(const ARe, AIm: TDouble): TDouble; inline;
begin
  result := Sqrt(ARe * ARe + AIm * AIm);
end;

procedure TSpectrums.BuildWindow;
var
  i: longint;
begin
  SetLength(FWindow, FWindowSize);
  SetLength(FBuff,   FWindowSize);
  SetLength(FFreq,   FWindowSize);
  for i := 0 to FWindowSize - 1 do
  begin
    FWindow[i] := 0.5 - 0.5 * cos(2 * pi * i / (FWindowSize - 1));
    FBuff[i].y := 0;   // cleared once; only the single-window path overwrites it
  end;
  FScaleEdge := 1 / FWindowSize;
  FScale     := 2 / FWindowSize;
end;

function TSpectrums.ComputeWindowCount(ASampleCount: longint): longint;
begin
  if ASampleCount >= FWindowSize then
    result := (ASampleCount - FWindowSize) div FHopSize + 1
  else
    result := 0;
end;

procedure TSpectrums.GetSpectrum(ASamples: PDouble; ASpectrum: PDouble);
var
  i, n: longint;
begin
  n := FWindowSize;
  for i := 0 to n - 1 do
  begin
    FBuff[i].x := FWindow[i] * ASamples^;
    FBuff[i].y := 0;   // runs at most once per channel
    Inc(ASamples);
  end;
  FFT(n, FBuff, FFreq);

  ASpectrum^ := Abs(FFreq[0].x) * FScaleEdge;
  Inc(ASpectrum);
  for i := 1 to (n div 2) - 1 do
  begin
    ASpectrum^ := Magnitude(FFreq[i].x, FFreq[i].y) * FScale;
    Inc(ASpectrum);
  end;
  if (n and 1) = 0 then
    ASpectrum^ := Abs(FFreq[n div 2].x) * FScaleEdge;
end;

procedure TSpectrums.GetSpectrumPair(ASamplesA, ASamplesB: PDouble;
  ASpectrumA, ASpectrumB: PDouble);
var
  i, k, n, half: longint;
  ar, ai, br, bi: TDouble;
  zk, znk: Complex;
begin
  // Two real signals packed into a single complex FFT:
  //   z[n] = a[n] + i*b[n],  Z = FFT(z)
  //   A[k] = ( Z[k] + conj(Z[N-k]) ) / 2
  //   B[k] = ( Z[k] - conj(Z[N-k]) ) / (2i)
  // Cross-validated against the direct path to machine epsilon.
  n    := FWindowSize;
  half := n div 2;

  for i := 0 to n - 1 do
  begin
    FBuff[i].x := FWindow[i] * ASamplesA^;
    FBuff[i].y := FWindow[i] * ASamplesB^;
    Inc(ASamplesA);
    Inc(ASamplesB);
  end;
  FFT(n, FBuff, FFreq);

  // DC: A0 = Re(Z0), B0 = Im(Z0)
  ASpectrumA^ := Abs(FFreq[0].x) * FScaleEdge;
  ASpectrumB^ := Abs(FFreq[0].y) * FScaleEdge;
  Inc(ASpectrumA);
  Inc(ASpectrumB);

  for k := 1 to half - 1 do
  begin
    zk  := FFreq[k];
    znk := FFreq[n - k];

    ar := (zk.x + znk.x) * 0.5;
    ai := (zk.y - znk.y) * 0.5;
    br := (zk.y + znk.y) * 0.5;
    bi := (znk.x - zk.x) * 0.5;

    ASpectrumA^ := Magnitude(ar, ai) * FScale;
    ASpectrumB^ := Magnitude(br, bi) * FScale;
    Inc(ASpectrumA);
    Inc(ASpectrumB);
  end;

  // Nyquist (even N): A = Re(Z[N/2]), B = Im(Z[N/2])
  if (n and 1) = 0 then
  begin
    ASpectrumA^ := Abs(FFreq[half].x) * FScaleEdge;
    ASpectrumB^ := Abs(FFreq[half].y) * FScaleEdge;
  end;
end;

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
  FScale       := 0;
  FScaleEdge   := 0;
  SetLength(FWindow, 0);
  SetLength(FBuff, 0);
  SetLength(FFreq, 0);
  SetLength(FChannels, 0, 0);
end;

function TSpectrums.EstimatedTicks(AChannelCount, ASampleCount: longint): longint;
begin
  result := ComputeWindowCount(ASampleCount) * AChannelCount;
end;

procedure TSpectrums.Process(const AChannels: TDoubleMatrix; ASampleCount: longint);
var
  ch, i: longint;
begin
  FWindowCount := ComputeWindowCount(ASampleCount);

  if FWindowCount > 0 then
  begin
    FOutBins := FWindowSize div 2 + 1;

    SetLength(FChannels, Length(AChannels), FWindowCount * OutBins);
    for ch := Low(AChannels) to High(AChannels) do
    begin
      i := 0;
      // process windows in pairs: one FFT per two windows
      while i + 1 < FWindowCount do
      begin
        GetSpectrumPair(
          @AChannels[ch][(i    ) * FHopSize],
          @AChannels[ch][(i + 1) * FHopSize],
          @FChannels[ch][(i    ) * OutBins],
          @FChannels[ch][(i + 1) * OutBins]);

        if Assigned(FTick) then begin FTick; FTick; end;
        Inc(i, 2);
      end;
      // leftover odd window, if any
      if i < FWindowCount then
      begin
        GetSpectrum(@AChannels[ch][i * FHopSize], @FChannels[ch][i * OutBins]);
        if Assigned(FTick) then FTick;
      end;
    end;
    SmoothSpectrogram;
  end else
    Finalize;
end;

procedure TSpectrums.SmoothSpectrogram;
var
  ch, i, j, base, index: longint;
  Tmp, row: TDoubleVector;
begin
  SetLength(Tmp, FWindowCount * FOutBins);

  // frequency axis
  for ch := Low(FChannels) to High(FChannels) do
  begin
    row := FChannels[ch];
    Move(row[0], Tmp[0], Length(row) * SizeOf(TDouble));

    for i := 0 to FWindowCount - 1 do
    begin
      base := i * FOutBins;
      for j := 1 to FOutBins - 2 do
      begin
        index := base + j;
        row[index] := (Tmp[index - 1] + 2 * Tmp[index] + Tmp[index + 1]) * 0.25;
      end;
    end;
  end;

  // time axis
  for ch := Low(FChannels) to High(FChannels) do
  begin
    row := FChannels[ch];
    Move(row[0], Tmp[0], Length(row) * SizeOf(TDouble));

    for i := 1 to FWindowCount - 2 do
    begin
      base := i * FOutBins;
      for j := 0 to FOutBins - 1 do
      begin
        index := base + j;
        row[index] := (Tmp[index - FOutBins] + 2 * Tmp[index] + Tmp[index + FOutBins]) * 0.25;
      end;
    end;
  end;
end;

end.
