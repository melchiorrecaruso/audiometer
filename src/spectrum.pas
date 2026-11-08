{
  Description: Spectrum routines.

  Copyright (C) 2025-2026 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit Spectrum;

{$mode objfpc}{$h+}
{$modeswitch advancedrecords}

interface

uses
  Common, SysUtils, ufft, utypes;

const
  DEFAULTWINDOWSIZE = 2048;
  DEFAULTHOPSIZE    = DEFAULTWINDOWSIZE div 2;
  // Time rows kept for the spectrogram: each row holds the per-bin maximum
  // of the windows that fall in it, so short events stay visible while the
  // memory no longer grows with the track length.
  DEFAULTMAXROWS    = 2048;

type
  TSpectrums = record
  private
    FChannels: TSingleMatrix;     // spectrogram, [channel][row * OutBins + bin]
    FRowCount: longint;
    FWindowCount: longint;
    FWindowSize: longint;
    FHopSize: longint;
    FOutBins: longint;
    FTick: TTickMethod;
    FWindow: TDoubleVector;   // precomputed Hann coefficients
    FBuff: TCompVector;       // reusable FFT input buffer
    FFreq: TCompVector;       // reusable FFT output buffer
    FScale: TDouble;          // magnitude scale for interior bins (2/sum(w))
    FScaleEdge: TDouble;      // magnitude scale for DC and Nyquist (1/sum(w))
    FSpectrumRms: TDoubleVector;  // aggregate across all windows/channels
    FSpectrumPeak: TDoubleVector;
    procedure BuildWindow;
    procedure AddSpectrum(AChannel, AWindow: longint; ASpectrum: PDouble);
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
    function SpectrumRms(ABin: longint): double;
    function SpectrumPeak(ABin: longint): double;

    property WindowSize: longint read FWindowSize;
    property WindowCount: longint read FWindowCount;
    property HopSize: longint read FHopSize;
    property OutBins: longint read FOutBins;
    property RowCount: longint read FRowCount;

    property Channels: TSingleMatrix read FChannels;
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
  Sum: TDouble;
begin
  SetLength(FWindow, FWindowSize);
  SetLength(FBuff,   FWindowSize);
  SetLength(FFreq,   FWindowSize);
  Sum := 0;
  for i := 0 to FWindowSize - 1 do
  begin
    FWindow[i] := 0.5 - 0.5 * cos(2 * pi * i / (FWindowSize - 1));
    FBuff[i].y := 0;   // cleared once; only the single-window path overwrites it
    Sum := Sum + FWindow[i];
  end;
  // The window attenuates a sine by sum(w)/N (about 1/2 for Hann): dividing
  // by sum(w) instead of N gives back its amplitude, so a full-scale sine
  // reads 0 dBFS.
  FScaleEdge := 1 / Sum;
  FScale     := 2 / Sum;
end;

function TSpectrums.ComputeWindowCount(ASampleCount: longint): longint;
begin
  if (FWindowSize > 0) and (FHopSize > 0) and (ASampleCount >= FWindowSize) then
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
  if AWindowSize <= 0 then
    AWindowSize := DEFAULTWINDOWSIZE;
  if AHopSize <= 0 then
    AHopSize := DEFAULTHOPSIZE;

  // The FFT backend only accepts powers of two; Hann needs at least 3 taps.
  if (AWindowSize < 4) or ((AWindowSize and (AWindowSize - 1)) <> 0) then
    raise EArgumentException.Create('FFT window size must be a power of two >= 4');

  Finalize;
  FTick := ATick;
  FWindowSize := AWindowSize;
  FHopSize    := AHopSize;
  BuildWindow;
end;

procedure TSpectrums.Finalize;
begin
  FRowCount    := 0;
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
  SetLength(FSpectrumRms, 0);
  SetLength(FSpectrumPeak, 0);
end;

procedure TSpectrums.AddSpectrum(AChannel, AWindow: longint; ASpectrum: PDouble);
var
  k, Base: longint;
  Row: TSingleVector;
begin
  // spectrogram: maximum of the windows falling in the same row
  Base := (int64(AWindow) * FRowCount div FWindowCount) * FOutBins;
  Row := FChannels[AChannel];
  for k := 0 to FOutBins - 1 do
  begin
    if ASpectrum[k] > Row[Base + k] then
      Row[Base + k] := ASpectrum[k];
    // frequency spectrum, from the unsmoothed windows: energy sum (RMS once
    // divided) and peak hold
    FSpectrumRms[k] := FSpectrumRms[k] + Sqr(ASpectrum[k]);
    if ASpectrum[k] > FSpectrumPeak[k] then
      FSpectrumPeak[k] := ASpectrum[k];
  end;
end;

function TSpectrums.SpectrumRms(ABin: longint): double;
begin
  if (ABin >= 0) and (ABin < Length(FSpectrumRms)) then
    Result := FSpectrumRms[ABin]
  else
    Result := 0;
end;

function TSpectrums.SpectrumPeak(ABin: longint): double;
begin
  if (ABin >= 0) and (ABin < Length(FSpectrumPeak)) then
    Result := FSpectrumPeak[ABin]
  else
    Result := 0;
end;

function TSpectrums.EstimatedTicks(AChannelCount, ASampleCount: longint): longint;
var
  Count: int64;
begin
  if AChannelCount < 0 then
    raise EArgumentException.Create('Channel count must not be negative');
  Count := int64(ComputeWindowCount(ASampleCount)) * AChannelCount;
  // Keep the LongInt API used by the progress counter, without wrapping.
  if Count > High(longint) then
    raise ERangeError.Create('Estimated spectrum ticks exceed LongInt capacity');
  result := longint(Count);
end;

procedure TSpectrums.Process(const AChannels: TDoubleMatrix; ASampleCount: longint);
var
  ch, i, k: longint;
  SpectrumA, SpectrumB: TDoubleVector;
begin
  if (FWindowSize = 0) or (FHopSize = 0) then
    raise EInvalidOpException.Create('Spectrum must be initialized before processing');

  // Clear results only: empty/short input must not destroy the FFT setup.
  FWindowCount := 0;
  FRowCount := 0;
  FOutBins := 0;
  FChannels := nil;
  FSpectrumRms := nil;
  FSpectrumPeak := nil;
  if (Length(AChannels) = 0) or (ASampleCount <= 0) then Exit;
  for ch := Low(AChannels) to High(AChannels) do
    if Length(AChannels[ch]) < ASampleCount then
      raise EArgumentException.Create('Sample count exceeds channel length');
  FWindowCount := ComputeWindowCount(ASampleCount);

  if FWindowCount > 0 then
  begin
    FOutBins  := FWindowSize div 2 + 1;
    FRowCount := Min(FWindowCount, DEFAULTMAXROWS);

    // Arrays were released above, so all newly allocated elements start at zero.
    SetLength(FChannels, Length(AChannels), int64(FRowCount) * OutBins);
    SetLength(FSpectrumRms, FOutBins);
    SetLength(FSpectrumPeak, FOutBins);
    SetLength(SpectrumA, FOutBins);
    SetLength(SpectrumB, FOutBins);
    for ch := Low(AChannels) to High(AChannels) do
    begin
      i := 0;
      // process windows in pairs: one FFT per two windows
      while i + 1 < FWindowCount do
      begin
        GetSpectrumPair(
          @AChannels[ch][(i    ) * FHopSize],
          @AChannels[ch][(i + 1) * FHopSize],
          @SpectrumA[0],
          @SpectrumB[0]);
        AddSpectrum(ch, i,     @SpectrumA[0]);
        AddSpectrum(ch, i + 1, @SpectrumB[0]);

        if Assigned(FTick) then begin FTick; FTick; end;
        Inc(i, 2);
      end;
      // leftover odd window, if any
      if i < FWindowCount then
      begin
        GetSpectrum(@AChannels[ch][i * FHopSize], @SpectrumA[0]);
        AddSpectrum(ch, i, @SpectrumA[0]);
        if Assigned(FTick) then FTick;
      end;
    end;

    for k := 0 to FOutBins - 1 do
      FSpectrumRms[k] := Sqrt(FSpectrumRms[k] / (int64(FWindowCount) * Length(AChannels)));
    SmoothSpectrogram;
  end;
end;

procedure TSpectrums.SmoothSpectrogram;
var
  ch, i, j, base, index: longint;
  Tmp, row: TSingleVector;
begin
  SetLength(Tmp, int64(FRowCount) * FOutBins);

  // frequency axis
  for ch := Low(FChannels) to High(FChannels) do
  begin
    row := FChannels[ch];
    Move(row[0], Tmp[0], Length(row) * SizeOf(TSingle));

    for i := 0 to FRowCount - 1 do
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
    Move(row[0], Tmp[0], Length(row) * SizeOf(TSingle));

    for i := 1 to FRowCount - 2 do
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
