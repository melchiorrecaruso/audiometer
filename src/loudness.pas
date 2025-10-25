{
  Description: ITU-R BS.1770-5 Loudness Measurement Routines.

  Copyright (C) 2025 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit Loudness;

{$mode objfpc}{$h+}
{$modeswitch advancedrecords}

interface

uses
  Classes, Common, Sysutils;

type
  arrayofdouble = array of double;
  arrayofarrayofdouble = array of arrayofdouble;

  TShelvingFilter = record
  private
    a1, a2: double;
    b0, b1, b2: double;
    x1, x2: double;
    y1, y2: double;
  public
    procedure Init(ASamplerate: longint);
    function Run(const ASample: TSample): TSample;
  end;

  THighpassFilter = record
  private
    a1, a2: double;
    b0, b1, b2: double;
    x1, x2: double;
    y1, y2: double;
  public
    procedure Init(ASamplerate: longint);
    function Run(const ASample: TSample): TSample;
  end;

  TKWeightingFilter = record
  private
    HighpassFilter: THighpassFilter;
    ShelvingFilter: TShelvingFilter;
  public
    procedure Init(ASamplerate: longint);
    function Run(const ASample: TSample): TSample;
  end;

  TLoudnessMeter = class
  private
    FSampleCount: longint;
    FSampleRate: longint;
    FBlockEnergies: arrayofarrayofdouble;
    FBlockSize: longint;
    FNumBlocks: longint;

    FRms2: arrayofdouble;
    FPeak: arrayofdouble;
    FTruePeak: arrayofdouble;
    FWeights: arrayofdouble;

    FMomentaryEnergies: arrayofarrayofdouble;
    FShortTermEnergies: arrayofarrayofdouble;
    FIntegratedEnergies: arrayofdouble;
    FEnergiesRange: arrayofdouble;
    FEnergyRange: double;

    FCrestFactors: arrayofdouble;
    FPeakToLoudnessRatio: arrayofdouble;

    procedure CalculateFIRCoefficents(var ACoeffs: arrayofarrayofdouble; AOverSample, ATaps: longint);
    procedure ClearFIRCoefficents(var ACoeffs: arrayofarrayofdouble);

    procedure CalculateChannelWeights(const AChannels: TChannels);
    procedure CalculateBlockEnergies(const AChannels: TChannels);
    procedure CalculateTruePeak(const AChannels: TChannels);
    procedure CalculatePeak(const AChannels: TChannels);
    procedure CalculateRms2(const AChannels: TChannels);
    procedure CalculateCrestFactor;
    procedure CalculatePeakToLoudnessRatio;
    procedure CalculateMomentaryEnergies;
    procedure CalculateShortTermEnergies;
    procedure CalculateIntegratedEnergies;
    procedure CalculateEnergyRanges;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure Analyze(const AChannels: TChannels; ASampleCount, ASampleRate: longint);

    function Rms2(AChannel: longint): double;
    function Rms2: double;
    function Peak(AChannel: longint): double;
    function Peak: double;
    function TruePeak(AChannel: longint): double;
    function TruePeak: double;

    function CrestFactor(AChannel: longint): double;
    function CrestFactor: double;
    function PeakToLoudnessRatio(AChannel: longint): double;
    function PeakToLoudnessRatio: double;

    function MomentaryLoudness(const AtTime: longint; AChannel: longint): double;
    function MomentaryLoudness(const AtTime: longint): double;
    function ShortTermLoudness(const AtTime: longint; AChannel: longint): double;
    function ShortTermLoudness(const AtTime: longint): double;
    function IntegratedLoudness(AChannel: longint): double;
    function IntegratedLoudness: double;
    function LoudnessRanges(AChannel: longint): double;
    function LoudnessRange: double;
  end;


function EnergyToLufs(const aenergy: double): double;


implementation

uses
  math;

function Percentile(const Values: TListOfDouble; P: double): double;
var
  index: Double;
  Lower, Upper: Integer;
  Fraction: Double;
begin
  Values.Sort(@Common.Compare);

  // Compute the exact index
  index := P * (Values.Count - 1);
  Lower := Floor(index);
  Upper := Ceil(index);
  Fraction := index - Lower;

  if Upper >= Values.Count then
    Upper := Values.Count - 1;

  // Linear interpolation
  if Lower = Upper then
    result := Values[Lower]
  else
    result := Values[Lower] + Fraction * (Values[Upper] - Values[Lower]);
end;

function EnergyToLufs(const AEnergy: double): double;
begin
  if AEnergy > 1e-10 then
    result := -0.691 + 10*Log10(AEnergy)
  else
    result := NegInfinity;
end;

// TShelvingFilter

procedure TShelvingFilter.Init(ASamplerate: longint);
const
  gain = 4.0;
  f0 = 1500.0;
var
  a, w0, alpha, cosw0: double;
begin
  x1 := 0; x2 := 0;
  y1 := 0; y2 := 0;

  a     := power(10, gain / 40);
  w0    := 2 * pi * f0 / ASamplerate;
  cosw0 := cos(w0);
  alpha := sin(w0) / 2 * sqrt((a + 1/a) * (1 / 0.707 - 1) + 2);

  a1 := -2 *     ((a - 1) + (a + 1) * cosw0);
  a2 :=          ((a + 1) + (a - 1) * cosw0 - 2 * sqrt(a) * alpha);
  b0 :=      a * ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
  b1 := -2 * a * ((a - 1) + (a + 1) * cosw0);
  b2 :=      a * ((a + 1) + (a - 1) * cosw0 - 2 * sqrt(a) * alpha);

  // Normalize
  a1 := a1 / ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
  a2 := a2 / ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
  b0 := b0 / ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
  b1 := b1 / ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
  b2 := b2 / ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
end;

function TShelvingFilter.Run(const ASample: TSample): TSample;
begin
  result := b0 * ASample + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2;
  // Shift state
  x2 := x1;
  x1 := ASample;
  y2 := y1;
  y1 := result;
end;

// THighpassFilter

procedure THighpassFilter.Init(ASamplerate: longint);
const
  f0 = 60.0;
var
  w0, c, norm: Double;
begin
  x1 := 0; x2 := 0;
  y1 := 0; y2 := 0;

  w0 := 2 * Pi * f0 / ASamplerate;
  c := 1 / tan(w0 / 2);

  norm := 1 / (1 + sqrt(2) * c + c * c);
  b0 := 1 * norm;
  b1 := -2 * b0;
  b2 := b0;
  a1 := 2 * (c * c - 1) * norm;
  a2 := (1 - sqrt(2) * c + c * c) * norm;
end;

function THighpassFilter.Run(const ASample: TSample): TSample;
begin
  result := b0 * ASample + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2;
  // Shift state
  x2 := x1;
  x1 := ASample;
  y2 := y1;
  y1 := result;
end;

// TKWeightingFilter

procedure TKWeightingFilter.Init(ASamplerate: longint);
begin
  HighpassFilter.Init(ASamplerate);
  ShelvingFilter.Init(ASamplerate);
end;

function TKWeightingFilter.Run(const ASample: TSample): TSample;
begin
  result := ShelvingFilter.Run(HighpassFilter.Run(ASample));
end;

// TLoudnessMeter

constructor TLoudnessMeter.Create;
begin
  inherited Create;
  FSampleCount := 0;
  FSampleRate  := 0;

  FBlockEnergies   := nil;
  FRms2     := nil;
  FPeak     := nil;
  FTruePeak := nil;
  FWeights  := nil;


  FMomentaryEnergies  := nil;
  FShortTermEnergies  := nil;
  FIntegratedEnergies := nil;
  FEnergiesRange      := nil;

  FCrestFactors := nil;
  FPeakToLoudnessRatio := nil;
end;

destructor TLoudnessMeter.Destroy;
begin
  Clear;
  inherited destroy;
end;

procedure TLoudnessMeter.Clear;
var
  ch: longint;
begin
  FSampleCount := 0;
  FSampleRate  := 0;
  FBlockSize   := 0;
  FNumBlocks   := 0;
  FEnergyRange := 0;

  for ch := Low(FBlockEnergies) to High(FBlockEnergies) do
  begin
    SetLength(FBlockEnergies[ch], 0);
  end;
  SetLength(FBlockEnergies      , 0);
  SetLength(FMomentaryEnergies  , 0);
  SetLength(FShortTermEnergies  , 0);
  SetLength(FIntegratedEnergies , 0);
  SetLength(FEnergiesRange      , 0);
  SetLength(FPeakToLoudnessRatio, 0);
  SetLength(FCrestFactors       , 0);

  SetLength(FPeak    , 0);
  SetLength(FRms2    , 0);
  SetLength(FTruePeak, 0);
  SetLength(FWeights , 0);
end;

procedure TLoudnessMeter.CalculateFIRCoefficents(var ACoeffs: arrayofarrayofdouble; AOversample, ATaps: longint);
var
  fc, x: double;
  j, Phase, Tap: longint;
  Coeff, Sum: double;
begin
  SetLength(ACoeffs, AOversample);
  for j := Low(ACoeffs) to High(ACoeffs) do
    SetLength(ACoeffs[j], ATaps);

  fc := 0.5 / AOversample;
  // generate FIR ACoeffs
  for Phase := 0 to AOversample -1 do
  begin
    Sum := 0.0;
    for Tap := 0 to ATaps -1 do
    begin
      x := (Tap - (ATaps / 2)) - Phase/AOversample;
      // sinc
      if abs(x) > 1e-10 then
        Coeff := 2.0 * fc * sin(pi * x) / (pi * x)
      else
        Coeff := 2.0 * fc;
      // finestra di hamming
      Coeff := Coeff * (0.54 - 0.46 * cos(2.0 * pi * Tap / (ATaps - 1)));
      ACoeffs[Phase, Tap] := Coeff;
      Sum := Sum + Coeff;
    end;
    // normalize ACoeffs
    for Tap := 0 to ATaps -1 do
      ACoeffs[Phase, Tap] := ACoeffs[Phase, Tap] / Sum;
  end;
end;

procedure TLoudnessMeter.ClearFIRCoefficents(var ACoeffs: arrayofarrayofdouble);
var
  j: longint;
begin
  for j := Low(ACoeffs) to High(ACoeffs) do
    SetLength(ACoeffs[j], 0);
  SetLength(ACoeffs, 0);
end;

procedure TLoudnessMeter.CalculateChannelWeights(const AChannels: TChannels);
var
  ch: longint;
begin
  // Allocate and set the channel Weights based on the number of channels
  Setlength(FWeights, Length(AChannels));
  case Length(AChannels) of
    1: begin
         FWeights[0] := 1.0; // Mono
       end;
    2: begin
         FWeights[0] := 1.0; // Stereo L
         FWeights[1] := 1.0; // Stereo R
       end;
    5: begin
         FWeights[0] := 1.0;   // L
         FWeights[1] := 1.0;   // R
         FWeights[2] := 1.0;   // C
         FWeights[3] := 1.412; // LS
         FWeights[4] := 1.412; // RS
       end;
    6: begin                          // 5.1
         FWeights[0] := 1.0;   // L
         FWeights[1] := 1.0;   // R
         FWeights[2] := 1.0;   // C
         FWeights[3] := 0.0;   // LFE
         FWeights[4] := 1.412; // LS
         FWeights[5] := 1.412; // RS
       end;
  else
    // Default: assign weight 1.0 to all channels
    for ch := Low(AChannels) to High(AChannels) do
      FWeights[ch] := 1.0;
  end;
end;

procedure TLoudnessMeter.CalculateTruePeak(const AChannels: TChannels);
var
  i, ch, index: longint;
  Coeffs: arrayofarrayofdouble;
  Phase, Tap, Taps, Oversample: longint;
  Sample, Sum, TruePk: double;
begin
  Coeffs := nil;
  case FSampleRate of
    44100: begin Oversample := 4; Taps := 12; end;
    48000: begin Oversample := 4; Taps := 12; end;
    88200: begin Oversample := 2; Taps := 12; end;
    96000: begin Oversample := 2; Taps := 12; end;
  else     begin Oversample := 2; Taps := 12; end;
  end;

  SetLength(FTruePeak, Length(AChannels));

  CalculateFIRCoefficents(Coeffs, Oversample, Taps);
  if Length(Coeffs) > 0 then
  begin
    for ch := Low(AChannels) to High(AChannels) do
    begin
      TruePk := 0;
      for i := Low(AChannels[ch]) to High(AChannels[ch]) do
      begin
        for Phase := 0 to Oversample -1 do
        begin
          Sum := 0;
          for Tap := 0 to Taps -1 do
          begin
            index := Tap - (Taps div 2) + i;

            Sample := 0;
            if (index >= Low (AChannels[ch])) and
               (index <= High(AChannels[ch])) then
              Sample :=  AChannels[ch][index];

            Sum := Sum + Sample * Coeffs[Phase, Tap];
          end;
          TruePk := Max(TruePk, Abs(Sum));
        end;
      end;
      FTruePeak[ch] := TruePk;
    end;

  end else
  begin
    for ch := Low(AChannels) to High(AChannels) do
    begin
      FTruePeak[ch] := FPeak[ch];
    end;
  end;
  ClearFIRCoefficents(Coeffs);
end;

procedure TLoudnessMeter.CalculatePeak(const AChannels: TChannels);
var
  ch: longint;
begin
  SetLength(FPeak, Length(AChannels));
  for ch := Low(AChannels) to High(AChannels) do
    FPeak[ch] := Common.Peak(AChannels[ch], 0, FSampleCount);
end;

procedure TLoudnessMeter.CalculateRms2(const AChannels: TChannels);
var
  ch: longint;
begin
  SetLength(FRms2, Length(AChannels));
  for ch := Low(AChannels) to High(AChannels) do
    FRms2[ch] := Common.Rms2(AChannels[ch], 0, FSampleCount);
end;

procedure TLoudnessMeter.CalculateCrestFactor;
var
  ch: longint;
begin
  SetLength(FCrestFactors, Length(FBlockEnergies));
  for ch := Low(FBlockEnergies) to High(FBlockEnergies) do
  begin
    if FRms2[ch] > 1e-10 then
      FCrestFactors[ch] := Decibel(FTruePeak[ch] / Sqrt(FRms2[ch]))
    else
      FCrestFactors[ch] := NegInfinity;
  end;
end;

procedure TLoudnessMeter.CalculatePeakToLoudnessRatio;
var
  ch: longint;
begin
  SetLength(FPeakToLoudnessRatio, Length(FBlockEnergies));
  for ch := low(FBlockEnergies) to high(FBlockEnergies) do
  begin
    if FIntegratedEnergies[ch] > 1e-10 then
      FPeakToLoudnessRatio[ch] := Decibel(FTruePeak[ch]) - EnergyToLufs(FIntegratedEnergies[ch])
    else
      FPeakToLoudnessRatio[ch] := NegInfinity;
  end;
end;

procedure TLoudnessMeter.CalculateBlockEnergies(const AChannels: TChannels);
const
  BlockMs = 100;
var
  ch, i, OffSet: Integer;
begin
  FBlockSize := (FSampleRate * BlockMs) div 1000;

  if FSampleCount < FBlockSize then
    FNumBlocks := 0
  else
    FNumBlocks := ((FSampleCount - FBlockSize) div FBlockSize) + 1;

  SetLength(FBlockEnergies, Length(AChannels));
  for ch := Low(AChannels) to High(AChannels) do
  begin
    SetLength(FBlockEnergies[ch], FNumBlocks);
    OffSet := 0;

    for i := 0 to FNumBlocks - 1 do
    begin
      FBlockEnergies[ch][i] := Common.Rms2(AChannels[ch], OffSet, FBlockSize);
      Inc(Offset, FBlockSize);
    end;
  end;
end;

procedure TLoudnessMeter.CalculateMomentaryEnergies;
const
  NumSteps = 4;
var
  ch, i, j, startIndex, stepCount: longint;
  sumEnergy: double;
begin
  SetLength(FMomentaryEnergies, Length(FBlockEnergies));

  for ch := Low(FBlockEnergies) to High(FBlockEnergies) do
  begin
    SetLength(FMomentaryEnergies[ch], FNumBlocks);

    for i := 0 to FNumBlocks - 1 do
    begin
      startIndex := Max(0, i - NumSteps + 1);
      stepCount  := i - startIndex + 1;

      sumEnergy := 0.0;
      for j := startIndex to i do
        sumEnergy := sumEnergy + FBlockEnergies[ch][j];

      FMomentaryEnergies[ch][i] := sumEnergy / stepCount;
    end;
  end;
end;

procedure TLoudnessMeter.CalculateShortTermEnergies;
const
  NumSteps = 30;
var
  ch, i, j, startIndex, stepCount: longint;
  sumEnergy: double;
begin
  SetLength(FShortTermEnergies, Length(FBlockEnergies));

  for ch := Low(FBlockEnergies) to High(FBlockEnergies) do
  begin
    SetLength(FShortTermEnergies[ch], FNumBlocks);

    for i := 0 to FNumBlocks - 1 do
    begin
      startIndex := Max(0, i - NumSteps + 1);
      stepCount  := i - startIndex + 1;

      sumEnergy := 0.0;
      for j := startIndex to i do
        sumEnergy := sumEnergy + FBlockEnergies[ch][j];

      FShortTermEnergies[ch][i] := sumEnergy / stepCount;
    end;
  end;
end;

procedure TLoudnessMeter.CalculateIntegratedEnergies;
const
  NumSteps = 4;               // 4 × 100 ms = 400 ms window
  AbsoluteGateLufs = -70.0;   // ITU absolute gate threshold
var
  ch, i, j: Integer;
  ValidCount, RelCount: Integer;
  SumEnergy, RelEnergy, AvgEnergy, L0: Double;
  BlockEnergy: Double;
  Energies: arrayofdouble;
begin
  SetLength(FIntegratedEnergies, Length(FBlockEnergies));

  for ch := Low(FBlockEnergies) to High(FBlockEnergies) do
  begin
    // Step 1️⃣ – Build 400 ms blocks by averaging 4 × 100 ms energies
    SetLength(Energies, FNumBlocks);
    for i := 0 to FNumBlocks - 1 do
    begin
      SumEnergy := 0.0;
      for j := Max(0, i - NumSteps + 1) to i do
        SumEnergy := SumEnergy + FBlockEnergies[ch][j];
      Energies[i] := SumEnergy / Min(NumSteps, i + 1);
    end;

    // Step 2️⃣ – Apply absolute gating (-70 LUFS)
    AvgEnergy := 0.0;
    ValidCount := 0;
    for i := 0 to FNumBlocks - 1 do
    begin
      BlockEnergy := Energies[i];
      if EnergyToLufs(BlockEnergy) >= AbsoluteGateLufs then
      begin
        AvgEnergy := AvgEnergy + BlockEnergy;
        Inc(ValidCount);
      end;
    end;

    // If no valid blocks remain, skip channel
    if ValidCount = 0 then
    begin
      FIntegratedEnergies[ch] := NegInfinity;
      Continue;
    end;

    // Step 3️⃣ – Compute L0 (mean loudness above absolute gate)
    L0 := EnergyToLufs(AvgEnergy / ValidCount);

    // Step 4️⃣ – Apply relative gating (-10 LU below L0)
    RelEnergy := 0.0;
    RelCount := 0;
    for i := 0 to FNumBlocks - 1 do
    begin
      BlockEnergy := Energies[i];
      if EnergyToLufs(BlockEnergy) >= L0 - 10.0 then
      begin
        RelEnergy := RelEnergy + BlockEnergy;
        Inc(RelCount);
      end;
    end;

    // Step 5️⃣ – Compute final Integrated Loudness (L_I) in LUFS
    if RelCount > 0 then
      FIntegratedEnergies[ch] := EnergyToLufs(RelEnergy / RelCount)
    else
      FIntegratedEnergies[ch] := NegInfinity;
  end;
end;

procedure TLoudnessMeter.CalculateEnergyRanges;
const
  WindowSteps = 30;
  StepSteps   = 10;
  AbsoluteGateLufs = -70.0;
var
  ch, i, j, index: Integer;
  WindowEnergy: Double;
  ValidBlocks: TListOfDouble;
  p95, p10: Double;
begin
  ValidBlocks := TListOfDouble.Create;
  SetLength(FEnergiesRange, Length(FBlockEnergies));

  // 1️⃣ Compute LRA per channel
  for ch := Low(FBlockEnergies) to High(FBlockEnergies) do
  begin
    ValidBlocks.Clear;

    // 2️⃣ Sliding 3 s window, stepped every 1 s
    i := 0;
    while i < FNumBlocks do
    begin

      // 3️⃣ Average 3 s energy window
      WindowEnergy := 0.0;
      for j := 0 to WindowSteps - 1 do
      begin
        index := i + j;
        if index < FNumBlocks then
        begin
          WindowEnergy := WindowEnergy + FBlockEnergies[ch][index];
        end;
      end;
      WindowEnergy := WindowEnergy / Min(WindowSteps, FNumBlocks - i);

      // 4️⃣ Apply absolute gate
      if EnergyToLufs(WindowEnergy) > AbsoluteGateLufs then
      begin
        ValidBlocks.Add(WindowEnergy);
      end;

      Inc(i, StepSteps);
    end;

    // 5️⃣ Compute 10th and 95th percentiles (ITU-R BS.1770-4 LRA)
    if ValidBlocks.Count >= 2 then
    begin
      p95 := Percentile(ValidBlocks, 0.95);
      p10 := Percentile(ValidBlocks, 0.10);
      FEnergiesRange[ch] := EnergyToLufs(p95) - EnergyToLufs(p10);
    end else
      FEnergiesRange[ch] := EnergyToLufs(NegInfinity);
  end;
  ValidBlocks.Destroy;
end;


  // 6️⃣ Optionally compute global (channel-weighted) range

  {
  var sumWeighted: array of Double;
  SetLength(sumWeighted, 0);

  i := 0;
  while i < FNumBlocks do
  begin
    if i + WindowSteps > FNumBlocks then Break;

    WindowEnergy := 0.0;
    for ch := Low(FBlockEnergies) to High(FBlockEnergies) do
    begin
      var temp: Double := 0.0;
      for j := 0 to WindowSteps - 1 do
        temp := temp + FBlockEnergies[ch][i + j];
      temp := temp / WindowSteps;
      if EnergyToLufs(temp) > AbsoluteGateLufs then
        WindowEnergy := WindowEnergy + FWeights[ch] * temp;
    end;

    if WindowEnergy > 1e-12 then
    begin
      SetLength(sumWeighted, Length(sumWeighted) + 1);
      sumWeighted[High(sumWeighted)] := WindowEnergy;
    end;

    Inc(i, StepSteps);
  end;

  if Length(sumWeighted) >= 2 then
  begin
    p95 := Percentile(sumWeighted, 0.95, Length(sumWeighted));
    p10 := Percentile(sumWeighted, 0.10, Length(sumWeighted));
    FEnergyRange := EnergyToLufs(p95) - EnergyToLufs(p10);
  end
  else
    FEnergyRange := NegInfinity;
  }

function TLoudnessMeter.LoudnessRanges(AChannel: longint): double;
begin
  result := FEnergiesRange[AChannel];
end;

function TLoudnessMeter.LoudnessRange: double;
begin
  result := FEnergyRange;
end;

function TLoudnessMeter.MomentaryLoudness(const AtTime: longint; AChannel: longint): double;
const
  Stepms = 100;
var
  index: longint;
begin
  index := AtTime div Stepms;

  if index > High(FMomentaryEnergies[AChannel]) then
    index := High(FMomentaryEnergies[AChannel]);

  result := EnergyToLufs(FMomentaryEnergies[AChannel][index]);
end;

function TLoudnessMeter.MomentaryLoudness(const AtTime: longint): double;
const
  Stepms = 100;
var
  index, ch: longint;
begin
  result := 0;
  for ch := Low(FMomentaryEnergies) to High(FMomentaryEnergies) do
  begin
    index := AtTime div Stepms;

    if index > High(FMomentaryEnergies[ch]) then
      index := High(FMomentaryEnergies[ch]);

    result := result + FWeights[ch] * FMomentaryEnergies[ch][index];
  end;

  result := EnergyToLufs(result);
end;

function TLoudnessMeter.ShortTermLoudness(const AtTime: longint; AChannel: longint): double;
const
  Stepms = 100;
var
  index: longint;
begin
  index := AtTime div Stepms;

  if index > High(FShortTermEnergies[AChannel]) then
    index := High(FShortTermEnergies[AChannel]);

  result := EnergyToLufs(FShortTermEnergies[AChannel][index]);
end;

function TLoudnessMeter.ShortTermLoudness(const AtTime: longint): double;
const
  Stepms = 100;
var
  index, ch: longint;
begin
  result := 0;
  for ch := Low(FShortTermEnergies) to High(FShortTermEnergies) do
  begin
    index := AtTime div Stepms;

    if index >= Length(FShortTermEnergies[ch]) then
      index := High(FShortTermEnergies[ch]);

    result := result + FWeights[ch] * FShortTermEnergies[ch][index];
  end;
  result := EnergyToLufs(result);
end;

function TLoudnessMeter.IntegratedLoudness(AChannel: longint): double;
begin
  result := EnergyToLufs(FIntegratedEnergies[AChannel]);
end;

function TLoudnessMeter.IntegratedLoudness: double;
var
  ch: longint;
begin
  result := 0;
  for ch := Low(FIntegratedEnergies) to High(FIntegratedEnergies) do
  begin
    result := result + FWeights[ch] * FIntegratedEnergies[ch];
  end;
  result := EnergyToLufs(result);
end;

procedure TLoudnessMeter.Analyze(const AChannels: TChannels; ASampleCount, ASampleRate: longint);
var
  ch, i: longint;
  y: TChannels;
  kFilter: TKWeightingFilter;
begin
  Clear;
  FSampleCount := ASampleCount;
  FSampleRate  := ASampleRate;

  CalculatePeak(AChannels);
  CalculateRms2(AChannels);
  CalculateTruePeak(AChannels);
  CalculateChannelWeights(AChannels);
  // Apply K-weighting filter
  SetLength(y, Length(AChannels));
  for ch := Low(y) to High(y) do
  begin
    kFilter.Init(ASampleRate);

    SetLength(y[ch], ASampleCount);
    for i := Low(y[ch]) to High(y[ch]) do
    begin
      y[ch][i] := kFilter.Run(AChannels[ch][i]);
    end;
  end;
  CalculateBlockEnergies(y);
  CalculateMomentaryEnergies;
  CalculateShortTermEnergies;
  CalculateIntegratedEnergies;
  CalculateEnergyRanges;
  CalculatePeakToLoudnessRatio;
  CalculateCrestFactor;

  for ch := Low(y) to High(y) do
    SetLength(y[ch], 0);
  SetLength(y, 0);
end;

function TLoudnessMeter.Rms2(AChannel: longint): double;
begin
  result := FRms2[AChannel];
end;

function TLoudnessMeter.Rms2: double;
var
  ch: longint;
  SumWeight: double;
begin
  result := 0;
  SumWeight := 0;
  for ch := Low(FRms2) to High(FRms2) do
  begin
    result := result + FWeights[ch] * FRms2[ch];
    SumWeight := SumWeight + FWeights[ch];
  end;

  if SumWeight > 0 then
  begin
    result := result / SumWeight;
  end;
end;

function TLoudnessMeter.Peak(AChannel: longint): double;
begin
  result := FPeak[AChannel];
end;

function TLoudnessMeter.Peak: double;
var
  ch: longint;
begin
  result := 0;
  for ch := Low(FPeak) to High(FPeak) do
  begin
    result := Max(result, FPeak[ch]);
  end;
end;

function TLoudnessMeter.TruePeak(AChannel: longint): double;
begin
  result := FTruePeak[AChannel];
end;

function TLoudnessMeter.TruePeak: double;
var
  ch: longint;
begin
  result := 0;

  for ch := Low(FTruePeak) to High(FTruePeak) do
  begin
    result := Max(result, FTruePeak[ch]);
  end;
end;

function TLoudnessMeter.CrestFactor(AChannel: longint): double;
begin
  result := FCrestFactors[AChannel];
end;

function TLoudnessMeter.CrestFactor: double;
var
  ch: longint;
begin
  result := 0;
  if Length(FCrestFactors) > 0 then
  begin
    for ch := Low(FCrestFactors) to High(FCrestFactors) do
    begin
      result := result + FCrestFactors[ch];
    end;
    result := result / Length(FCrestFactors);
  end;
end;

function TLoudnessMeter.PeakToLoudnessRatio(AChannel: longint): double;
begin
  result := FPeakToLoudnessRatio[AChannel];
end;

function TLoudnessMeter.PeakToLoudnessRatio: double;
var
  ch: longint;
begin
  result := 0;
  if Length(FPeakToLoudnessRatio) > 0 then
  begin
    for ch := Low(FPeakToLoudnessRatio) to High(FPeakToLoudnessRatio) do
    begin
      result := result + FPeakToLoudnessRatio[ch];
    end;
    result := result / Length(FPeakToLoudnessRatio);
  end;
end;

end.

