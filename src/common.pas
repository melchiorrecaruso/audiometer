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
  Taps = 24;
  HalfTaps = Taps div 2;

  Coeffs: array[0..3, 0..23] of TDouble = (
  (-0.002219053376896,  0.002948613536731, -0.004934828274959,  0.008482311536817, -0.013966189197941,  0.021907679957957,
   -0.033139556798674,  0.049198343624066, -0.073392328686107,  0.114431176231724, -0.204560620771758,  0.635244452219038,
    0.635244452219038, -0.204560620771758,  0.114431176231724, -0.073392328686107,  0.049198343624066, -0.033139556798674,
    0.021907679957957, -0.013966189197941,  0.008482311536817, -0.004934828274959,  0.002948613536731, -0.002219053376896),
  (-0.001536414565580,  0.002037414454575, -0.003401509805016,  0.005829157434461, -0.009561327080684,  0.014924048948865,
   -0.022424472405145,  0.032972363481558, -0.048458295901242,  0.073592298946080, -0.124038337433555,  0.299592070903024,
    0.898776212709072, -0.173653672406977,  0.089946143156320, -0.055913418347587,  0.036851465067624, -0.024560136443730,
    0.016117972864774, -0.010220728948318,  0.006182439703216, -0.003585375199882,  0.002136800525530, -0.001604699657383),
  (-0.000000000000000,  0.000000000000000, -0.000000000000000,  0.000000000000000, -0.000000000000000,  0.000000000000000,
   -0.000000000000000,  0.000000000000000, -0.000000000000000,  0.000000000000000, -0.000000000000000,  0.000000000000000,
    1.000000000000000,  0.000000000000000, -0.000000000000000,  0.000000000000000, -0.000000000000000,  0.000000000000000,
   -0.000000000000000,  0.000000000000000, -0.000000000000000,  0.000000000000000, -0.000000000000000,  0.000000000000000),
  ( 0.001481922844456, -0.001957720660232,  0.003253627849449, -0.005544820666195,  0.009031945916049, -0.013972297700045,
    0.020745574234394, -0.029998516911380,  0.042995784204457, -0.062617698643783,  0.097012314568961, -0.180757765774346,
    0.903788828871731,  0.291036943706882, -0.116290011767025,  0.066448030134161, -0.041997923675932,  0.027296808203149,
   -0.017617244926144,  0.011039045008504, -0.006618011762878,  0.003811392623640, -0.002258908454114,  0.001688702776240));

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

function TruePeak_LIBEBUR128(ASamples: PDouble; ASampleCount, ASampleRate: longint): TDouble;
// True-peak via the libebur128 interpolator: 49-tap Hann-windowed sinc,
// 4x oversampling (2x at >= 96 kHz), polyphase split by (tap mod factor).
const
  Taps = 49;
var
  Factor, Delay, PadLen, f, t, n, j: longint;
  m, c, s: double;
  Proto: array[0..Taps-1] of double;
  Pad: TDoubleVector;
begin
  Result := 0;
  if ASampleCount <= 0 then Exit;

  if ASampleRate < 96000 then Factor := 4 else Factor := 2;

  for j := 0 to Taps - 1 do
  begin
    m := j - (Taps - 1) / 2.0;
    if Abs(m) > 1e-12 then
      c := Sin(m * Pi / Factor) / (m * Pi / Factor)
    else
      c := 1.0;
    Proto[j] := c * 0.5 * (1.0 - Cos(2.0 * Pi * j / (Taps - 1)));
  end;

  Delay  := (Taps + Factor - 1) div Factor;
  PadLen := Delay;
  SetLength(Pad, ASampleCount + PadLen);
  FillChar(Pad[0], PadLen * SizeOf(TDouble), 0);
  Move(ASamples^, Pad[PadLen], ASampleCount * SizeOf(TDouble));

  for n := 0 to ASampleCount - 1 do
    for f := 0 to Factor - 1 do
    begin
      s := 0;
      for t := 0 to Delay - 1 do
      begin
        j := f + Factor * t;
        if j < Taps then
          s := s + Proto[j] * Pad[PadLen + n - t];
      end;
      if Abs(s) > Result then Result := Abs(s);
    end;

  Result := Max(Result, Peak(ASamples, ASampleCount));
end;

function TruePeak_BS1770(ASamples: PDouble; ASampleCount, ASampleRate: longint): TDouble;
// True-peak via the ITU-R BS.1770 Annex 2 *example* 48-tap, 4-phase filter
// (coefficients for 48 kHz, taken verbatim from the Recommendation).
const
  Taps = 12;
  Coeffs: array[0..3, 0..Taps-1] of TDouble = (
  ( 0.0017089843750,  0.0109863281250, -0.0196533203125,  0.0332031250000,
   -0.0594482421875,  0.1373291015625,  0.9721679687500, -0.1022949218750,
    0.0476074218750, -0.0266113281250,  0.0148925781250, -0.0083007812500),
  (-0.0291748046875,  0.0292968750000, -0.0517578125000,  0.0891113281250,
   -0.1665039062500,  0.4650878906250,  0.7797851562500, -0.2003173828125,
    0.1015625000000, -0.0582275390625,  0.0330810546875, -0.0189208984375),
  (-0.0189208984375,  0.0330810546875, -0.0582275390625,  0.1015625000000,
   -0.2003173828125,  0.7797851562500,  0.4650878906250, -0.1665039062500,
    0.0891113281250, -0.0517578125000,  0.0292968750000, -0.0291748046875),
  (-0.0083007812500,  0.0148925781250, -0.0266113281250,  0.0476074218750,
   -0.1022949218750,  0.9721679687500,  0.1373291015625, -0.0594482421875,
    0.0332031250000, -0.0196533203125,  0.0109863281250,  0.0017089843750));
var
  p, k, n, PadLen: longint;
  s: double;
  Pad: TDoubleVector;
begin
  Result := 0;
  if ASampleCount <= 0 then Exit;

  PadLen := Taps;
  SetLength(Pad, ASampleCount + 2 * PadLen);
  FillChar(Pad[0], Length(Pad) * SizeOf(TDouble), 0);
  Move(ASamples^, Pad[PadLen], ASampleCount * SizeOf(TDouble));

  for n := 0 to ASampleCount + PadLen - 1 do
    for p := 0 to 3 do
    begin
      s := 0;
      for k := 0 to Taps - 1 do
        s := s + Coeffs[p][k] * Pad[PadLen + n - k];
      if Abs(s) > Result then Result := Abs(s);
    end;

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
