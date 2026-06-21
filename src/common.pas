{
  Description: Common routines.

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

unit Common;

{$mode objfpc}{$h+}

interface

uses
  Classes, Fgl, SysUtils;

type
  TSingle       = single;
  TSingleVector = array of TSingle;
  TSingleMatrix = array of TSingleVector;

  TDouble = double;
  TDoubleVector = array of TDouble;
  TDoubleMatrix = array of TDoubleVector;

  TListOfDouble = specialize TFPGList<double>;

  TTickMethod = procedure of object;

function Decibel(const AAmplitude: TDouble): TDouble;
function EnergyToDecibel(const AEnergy: TDouble): TDouble;
function EnergyToLufs(const AEnergy: TDouble): TDouble;
function EnergyToLu(const AEnergy: TDouble): TDouble;

function Peak(ASamples: PDouble; ASampleCount: longint): TDouble; inline;
function Rms2(ASamples: PDouble; ASampleCount: longint): TDouble; inline;
function Rms (ASamples: PDouble; ASampleCount: longint): TDouble; inline;
function TruePeak(ASamples: PDouble; ASampleCount, ASampleRate: longint): TDouble;
function CrestFactor(const APeak, ARms2: TDouble): TDouble; inline;

procedure QuickSort(var AValues: TDoubleVector; Low, High: longint);
function Percentile(var AValues: TListOfDouble; P: double): TDouble;

function ChannelName(AChannelIndex, AChannelCount: longint): string;
function GetAppFile(const FileName: string): string;


implementation

uses
  Math;

function GetAppFile(const FileName: string): string;
begin
  Result := ExtractFilePath(ParamStr(0)) + FileName;
  if not FileExists(Result) then
  begin
    ForceDirectories(GetAppConfigDir(False));
    Result := IncludeTrailingBackSlash(GetAppConfigDir(False)) + FileName;
  end;
end;

function ChannelName(AChannelIndex, AChannelCount: longint): string;
begin
  case AChannelCount of
    1: Result := 'Mono';
    2: case AChannelIndex of
         0: Result := 'Left Channel';
         1: Result := 'Right Channel';
       end;
    5: case AChannelIndex of
         0: Result := 'Left Channel';
         1: Result := 'Right Channel';
         2: Result := 'Center Channel';
         3: Result := 'Left Surround Channel';
         4: Result := 'Right Surround Channel';
       end;
    6: case AChannelIndex of
        0: Result := 'Left Channel';
        1: Result := 'Right Channel';
        2: Result := 'Center Channel';
        3: Result := 'LFE Channel';
        4: Result := 'Left Surround Channel';
        5: Result := 'Right Surround Channel';
       end;
  else Result := '';
  end;
end;

procedure QuickSort(var AValues: TDoubleVector; Low, High: longint);
var
  i, j: Integer;
  pivot, temp: TDouble;
begin
  i := Low;
  j := High;
  pivot := AValues[(Low + High) div 2];

  repeat
    while AValues[i] < pivot do
      Inc(i);
    while AValues[j] > pivot do
      Dec(j);
    if i <= j then
    begin
      temp := AValues[i];
      AValues[i] := AValues[j];
      AValues[j] := temp;
      Inc(i);
      Dec(j);
    end;
  until i > j;

  if Low < j then
    QuickSort(AValues, Low, j);
  if i < High then
    QuickSort(AValues, i, High);
end;

function Compare(const AValue1, AValue2: double): longint;
begin
  result := Sign(AValue1 - AValue2);
end;

function  Percentile(var AValues: TListOfDouble; P: double): TDouble;
var
  index: double;
  Lower, Upper: longint;
  Fraction: double;
begin
  if AValues.Count = 0 then Exit(NegInfinity);

  AValues.Sort(@Compare);

  // Compute the exact index
  index := P * (AValues.Count - 1);
  Lower := Floor(index);
  Upper := Ceil(index);
  Fraction := index - Lower;

  if Upper >= AValues.Count then
    Upper := AValues.Count - 1;

  // Linear interpolation
  if Lower = Upper then
    result := AValues[Lower]
  else
    result := AValues[Lower] + Fraction * (AValues[Upper] - AValues[Lower]);
end;

function Decibel(const AAmplitude: TDouble): TDouble;
begin
  if AAmplitude > 1e-10 then
    result := 20 * Log10(AAmplitude)
  else
    result := NegInfinity;
end;

function EnergyToDecibel(const AEnergy: TDouble): TDouble;
begin
  if AEnergy > 1e-10 then
    result := 10 * Log10(AEnergy)
  else
    result := NegInfinity;
end;

function EnergyToLufs(const AEnergy: TDouble): TDouble;
begin
  if AEnergy > 1e-10 then
    result := -0.691 + 10*Log10(AEnergy)
  else
    result := NegInfinity;
end;

function EnergyToLu(const AEnergy: TDouble): TDouble;
begin
  if AEnergy > 1e-10 then
    result := 23 + EnergyToLufs(AEnergy)
  else
    result := NegInfinity;
end;

function Peak(ASamples: PDouble; ASampleCount: longint): TDouble;
var
  i: longint;
begin
  result := 0;
  for i := 0 to ASampleCount -1  do
  begin
    result := Max(result, Abs(ASamples^));
    Inc(ASamples);
  end;
end;

function Rms2(ASamples: PDouble; ASampleCount: longint): TDouble; inline;
var
  i: longint;
begin
  if ASampleCount = 0 then Exit(0);

  result := 0;
  for i := 0 to ASampleCount -1 do
  begin
    result := result + Sqr(ASamples^);
    Inc(ASamples);
  end;
  result := result / ASampleCount;
end;

function Rms(ASamples: PDouble; ASampleCount: longint): TDouble; inline;
begin
  result := Sqrt(Rms2(ASamples, ASampleCount));
end;

function TruePeak(ASamples: PDouble; ASampleCount, ASampleRate: longint): TDouble;
const
  Coeffs: array[0..3, 0..23] of TDouble = (
  (-0.001780280772489,  0.003253283030257, -0.005447293390376,  0.008414568116553,
   -0.012363296099675,  0.017436805871070, -0.024020143876810,  0.032746828420101,
   -0.045326602900760,  0.066760686868173, -0.120643370377371,  0.989429605248410,
    0.122160009958442, -0.046376232812786,  0.022831393004364, -0.011580897261667,
    0.005358105753167, -0.001834671998839, -0.000103681038815,  0.001002216283171,
   -0.001293611238062,  0.001184842429930, -0.000908719377960,  0.002061304229100),
  (-0.001473218555432,  0.002925336766866, -0.005558126468508,  0.009521159741206,
   -0.015296028027209,  0.023398977482278, -0.034752051245281,  0.050880967772373,
   -0.075227488678419,  0.116949442543490, -0.212471239510148,  0.788420616540440,
    0.460788819545818, -0.166082211358253,  0.092555759769552, -0.057854829231334,
    0.037380809681132, -0.024098441541823,  0.015115653825711, -0.009060645712669,
    0.005033299068467, -0.002511544062471,  0.001030723665756, -0.000694079453823),
  (-0.000694079453823,  0.001030723665756, -0.002511544062471,  0.005033299068467,
   -0.009060645712669,  0.015115653825711, -0.024098441541823,  0.037380809681132,
   -0.057854829231334,  0.092555759769552, -0.166082211358253,  0.460788819545818,
    0.788420616540440, -0.212471239510148,  0.116949442543490, -0.075227488678419,
    0.050880967772373, -0.034752051245281,  0.023398977482278, -0.015296028027209,
    0.009521159741206, -0.005558126468508,  0.002925336766866, -0.001473218555432),
  ( 0.002061304229100, -0.000908719377960,  0.001184842429930, -0.001293611238062,
    0.001002216283171, -0.000103681038815, -0.001834671998839,  0.005358105753167,
   -0.011580897261667,  0.022831393004364, -0.046376232812786,  0.122160009958442,
    0.989429605248410, -0.120643370377371,  0.066760686868173, -0.045326602900760,
    0.032746828420101, -0.024020143876810,  0.017436805871070, -0.012363296099675,
    0.008414568116553, -0.005447293390376,  0.003253283030257, -0.001780280772489));
const
  Taps = 24;
  HalfTaps = Taps div 2;
var
  i, Phase, Phases, PhaseStride, p, PadLen: longint;
  PadSamples: TDoubleVector;
  Sum: TDouble;
begin
  Result := 0;

  // Oversampling factor (FIR phases), per BS.1770: effective rate must reach
  // ~192 kHz. 4x below 96 kHz (44100/48000/88200), 2x from 96 kHz up.
  if ASampleRate < 96000 then
    Phases := 4
  else
    Phases := 2;

  // Phase stride into the 4-phase table: 4x uses phases {0,1,2,3};
  // 2x must use evenly spaced phases {0,2}, not the adjacent {0,1}.
  PhaseStride := 4 div Phases;

  // Add zero padding at both ends to avoid out-of-bound reads
  PadLen := HalfTaps;
  SetLength(PadSamples, ASampleCount + 2 * PadLen);

  FillChar(PadSamples[0], PadLen * SizeOf(TDouble), 0);
  Move(ASamples^, PadSamples[PadLen], ASampleCount * SizeOf(TDouble));
  FillChar(PadSamples[PadLen + ASampleCount], PadLen * SizeOf(TDouble), 0);

  // Compute the true peak value
  for i := PadLen to PadLen + ASampleCount - 1 do
  begin
    for Phase := 0 to Phases - 1 do
    begin
      p := Phase * PhaseStride;

      // Full 24-tap unrolling
      Sum :=
        PadSamples[i-12] * Coeffs[p][ 0] +
        PadSamples[i-11] * Coeffs[p][ 1] +
        PadSamples[i-10] * Coeffs[p][ 2] +
        PadSamples[i- 9] * Coeffs[p][ 3] +
        PadSamples[i- 8] * Coeffs[p][ 4] +
        PadSamples[i- 7] * Coeffs[p][ 5] +
        PadSamples[i- 6] * Coeffs[p][ 6] +
        PadSamples[i- 5] * Coeffs[p][ 7] +
        PadSamples[i- 4] * Coeffs[p][ 8] +
        PadSamples[i- 3] * Coeffs[p][ 9] +
        PadSamples[i- 2] * Coeffs[p][10] +
        PadSamples[i- 1] * Coeffs[p][11] +
        PadSamples[i   ] * Coeffs[p][12] +
        PadSamples[i+ 1] * Coeffs[p][13] +
        PadSamples[i+ 2] * Coeffs[p][14] +
        PadSamples[i+ 3] * Coeffs[p][15] +
        PadSamples[i+ 4] * Coeffs[p][16] +
        PadSamples[i+ 5] * Coeffs[p][17] +
        PadSamples[i+ 6] * Coeffs[p][18] +
        PadSamples[i+ 7] * Coeffs[p][19] +
        PadSamples[i+ 8] * Coeffs[p][20] +
        PadSamples[i+ 9] * Coeffs[p][21] +
        PadSamples[i+10] * Coeffs[p][22] +
        PadSamples[i+11] * Coeffs[p][23];

      if Sum < 0 then Sum := -Sum;
      if Sum > result then result := Sum;
    end;
  end;

  // The true peak can never be below the sample peak (no phase is the identity).
  Result := Max(Result, Peak(ASamples, ASampleCount));
end;

function CrestFactor(const APeak, ARms2: TDouble): TDouble;
begin
  if ARms2 > 1e-10 then
    result := APeak / Sqrt(ARms2)
  else
    result := NegInfinity;
end;

end.
