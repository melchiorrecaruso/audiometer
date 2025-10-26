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
    procedure Clear;
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
    procedure Clear;
  end;

  TKWeightingFilter = record
  private
    HighpassFilter: THighpassFilter;
    ShelvingFilter: TShelvingFilter;
  public
    procedure Init(ASamplerate: longint);
    function Run(const ASample: TSample): TSample;
    procedure Clear;
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

    FMomentaryEnergies: arrayofdouble;
    FShortTermEnergies: arrayofdouble;

    FIntegratedLoudness: double;
    FLoudnessRange: double;
    FCrestFactors: arrayofdouble;

    procedure CalculateFIRCoefficients(var ACoeffs: arrayofarrayofdouble; AOverSample, ATaps: longint);
    procedure ClearFIRCoefficents(var ACoeffs: arrayofarrayofdouble);

    procedure CalculateChannelWeights(const AChannels: TChannels);
    procedure CalculateBlockEnergies(const AChannels: TChannels);
    procedure CalculateTruePeak(const AChannels: TChannels);
    procedure CalculatePeak(const AChannels: TChannels);
    procedure CalculateRms2(const AChannels: TChannels);
    procedure CalculateCrestFactor;
    procedure CalculateMomentaryEnergies;
    procedure CalculateShortTermEnergies;
    procedure CalculateIntegratedLoudness;
    procedure CalculateLoudnessRange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure Analyze(const AChannels: TChannels; ASampleCount, ASampleRate: longint);

    function Rms(AChannel: longint): double;
    function Rms: double;
    function Peak(AChannel: longint): double;
    function Peak: double;
    function TruePeak(AChannel: longint): double;
    function TruePeak: double;

    function CrestFactor(AChannel: longint): double;
    function CrestFactor: double;

    function MomentaryLoudness(AtTime: longint): double;
    function ShortTermLoudness(AtTime: longint): double;
    function IntegratedLoudness: double;
    function PeakToLoudnessRatio: double;
    function LoudnessRange: double;
  end;


function EnergyToLufs(const aenergy: double): double;


implementation

uses
  Math;

function EnergyToLufs(const AEnergy: double): double;
begin
  if AEnergy > 1e-10 then
    result := -0.691 + 10*Log10(AEnergy)
  else
    result := NegInfinity;
end;

// TShelvingFilter

procedure TShelvingFilter.Init(ASampleRate: longint);
const
  f0 = 1681.974450955533;
  G  = 3.999843853973347;
  Q  = 0.7071752369554196;
var
  Omega, Vh, Vb, denom: Double;
begin
  Omega := tan(Pi * f0 / ASampleRate);
  Vh := Power(10.0, G / 20.0);
  Vb := Power(Vh, 0.4996667741545416);

  denom := 1.0 + Omega / Q + Omega * Omega;

  b0 := (Vh + Vb * Omega / Q + Omega * Omega) / denom;
  b1 := 2.0 * (Omega * Omega - Vh) / denom;
  b2 := (Vh - Vb * Omega / Q + Omega * Omega) / denom;
  a1 := 2.0 * (Omega * Omega - 1.0) / denom;
  a2 := (1.0 - Omega / Q + Omega * Omega) / denom;

  {$ifopt D+}
  writeln('TShelvingFilter.b0  ', b0:0:10);
  writeln('TShelvingFilter.b1  ', b1:0:10);
  writeln('TShelvingFilter.b2  ', b2:0:10);
  writeln('TShelvingFilter.a1  ', a1:0:10);
  writeln('TShelvingFilter.a2  ', a2:0:10);
  {$endif}
  Clear;
end;

procedure TShelvingFilter.Clear;
begin
  x1 := 0;
  x2 := 0;
  y1 := 0;
  y2 := 0;
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


procedure THighpassFilter.Init(ASampleRate: longint);
const
  f0 = 38;
  Q  = 0.5;
var
  Omega, denom: Double;
begin
  Omega := tan(Pi * f0 / ASampleRate);
  denom := 1.0 + Omega / Q + Omega * Omega;

  b0 := 1.0 / denom;
  b1 := -2.0 / denom;
  b2 := 1.0 / denom;
  a1 := 2.0 * (Omega * Omega - 1.0) / denom;
  a2 := (1.0 - Omega / Q + Omega * Omega) / denom;

  {$ifopt D+}
  writeln('THighpassFilter.b0  ', b0:0:10);
  writeln('THighpassFilter.b1  ', b1:0:10);
  writeln('THighpassFilter.b2  ', b2:0:10);
  writeln('THighpassFilter.a1  ', a1:0:10);
  writeln('THighpassFilter.a2  ', a2:0:10);
  {$endif}
  Clear;
end;

procedure THighpassFilter.Clear;
begin
  x1 := 0;
  x2 := 0;
  y1 := 0;
  y2 := 0;
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
  ShelvingFilter.Init(ASamplerate);
  HighpassFilter.Init(ASamplerate);
end;

function TKWeightingFilter.Run(const ASample: TSample): TSample;
begin
  result := ShelvingFilter.Run(HighpassFilter.Run(ASample));
end;

procedure TKWeightingFilter.Clear;
begin
  HighpassFilter.Clear;
  ShelvingFilter.Clear;
end;

// TLoudnessMeter

constructor TLoudnessMeter.Create;
begin
  inherited Create;
  Clear;
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
  FSampleCount      := 0;
  FSampleRate       := 0;
  FBlockSize        := 0;
  FNumBlocks        := 0;
  FLoudnessRange      := 0;
  FIntegratedLoudness := 0;

  for ch := Low(FBlockEnergies) to High(FBlockEnergies) do
  begin
    SetLength(FBlockEnergies[ch], 0);
  end;
  SetLength(FBlockEnergies    , 0);
  SetLength(FMomentaryEnergies, 0);
  SetLength(FShortTermEnergies, 0);
  SetLength(FCrestFactors     , 0);

  SetLength(FPeak    , 0);
  SetLength(FRms2    , 0);
  SetLength(FTruePeak, 0);
  SetLength(FWeights , 0);
end;

procedure TLoudnessMeter.CalculateFIRCoefficients(var ACoeffs: arrayofarrayofdouble; AOverSample, ATaps: longint);
var
  fc, x: double;
  i, Phase, Tap: longint;
  Coeff, Sum: double;
begin
  SetLength(ACoeffs, AOversample);
  for i := Low(ACoeffs) to High(ACoeffs) do
    SetLength(ACoeffs[i], ATaps);

  fc := 0.5 / AOversample;

  for Phase := 0 to AOversample - 1 do
  begin
    Sum := 0.0;
    for Tap := 0 to ATaps - 1 do
    begin
      x := (Tap - ((ATaps - 1) / 2)) - Phase / AOversample;
      if Abs(x) > 1e-10 then
        Coeff := 2.0 * fc * Sin(Pi * x) / (Pi * x)
      else
        Coeff := 2.0 * fc;
      // Hamming window
      Coeff := Coeff * (0.54 - 0.46 * Cos(2.0 * Pi * Tap / (ATaps - 1)));
      ACoeffs[Phase, Tap] := Coeff;
      Sum := Sum + Coeff;
    end;
    // Normalize
    for Tap := 0 to ATaps - 1 do
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
         FWeights[0] := 1.0;   // Mono
       end;
    2: begin                   // Stereo
         FWeights[0] := 1.0;   // L
         FWeights[1] := 1.0;   // R
       end;
    5: begin                   // 5.0
         FWeights[0] := 1.0;   // L
         FWeights[1] := 1.0;   // R
         FWeights[2] := 1.0;   // C
         FWeights[3] := 1.412; // LS
         FWeights[4] := 1.412; // RS
       end;
    6: begin                   // 5.1
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

(*
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

  CalculateFIRCoefficients(Coeffs, Oversample, Taps);
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
*)

procedure TLoudnessMeter.CalculateTruePeak(const AChannels: TChannels);
const
  // FIR coefficients
  Coeffs: array[0..3, 0..23] of double = (
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
var
  ch, i, Phase, Tap, Taps, Oversample: longint;
  Sample, Sum, TruePk: double;
  index: longint;
begin
  case FSampleRate of
    44100, 48000: begin Oversample := 4; Taps := 24; end;
    88200, 96000: begin Oversample := 2; Taps := 24; end;
  else            begin Oversample := 2; Taps := 24; end;
  end;

  SetLength(FTruePeak, Length(AChannels));

  for ch := Low(AChannels) to High(AChannels) do
  begin
    TruePk := 0;
    for i := Low(AChannels[ch]) to High(AChannels[ch]) do
    begin
      for Phase := 0 to Oversample -1 do
      begin
        Sum := 0;
        for Tap := 0 to Taps - 1 do
        begin
          index := Tap - (Taps div 2) + i;

          if (index >= Low (AChannels[ch])) and
             (index <= High(AChannels[ch])) then
            Sample := AChannels[ch][index]
          else
            Sample := 0;

          Sum := Sum + Sample * Coeffs[Phase, Tap];
        end;

        if Abs(Sum) > TruePk then
          TruePk := Abs(Sum);
      end;
    end;
    FTruePeak[ch] := TruePk;
  end;
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
      FCrestFactors[ch] := FPeak[ch] / Sqrt(FRms2[ch]) // FTruePeak[ch] / Sqrt(FRms2[ch])
    else
      FCrestFactors[ch] := NegInfinity;
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
      Inc(OffSet, FBlockSize);
    end;
  end;
end;

procedure TLoudnessMeter.CalculateMomentaryEnergies;
const
  NumSteps = 4;
var
  ch, i, j, index: longint;
  SumEnergy: double;
  SumCount: longint;
begin
  SetLength(FMomentaryEnergies, FNumBlocks);

  for i := 0 to FNumBlocks - 1 do
  begin
    FMomentaryEnergies[i] := 0;

    for ch := Low(FBlockEnergies) to High(FBlockEnergies) do
    begin
      SumEnergy := 0;
      SumCount  := 0;
      for j := 0 to NumSteps -1 do
      begin
        index := i + j;
        if index < FNumBlocks then
        begin
          SumEnergy := SumEnergy + FBlockEnergies[ch][index];
          SumCount  := SumCount + 1;
        end;
      end;

      FMomentaryEnergies[i] := FMomentaryEnergies[i] + (SumEnergy / SumCount) * FWeights[ch];
    end;
  end;
end;

procedure TLoudnessMeter.CalculateShortTermEnergies;
const
  NumSteps = 30;
var
  ch, i, j, index: longint;
  SumEnergy: double;
  SumCount: longint;
begin
  SetLength(FShortTermEnergies, FNumBlocks);

  for i := 0 to FNumBlocks - 1 do
  begin
    FShortTermEnergies[i] := 0;

    for ch := Low(FBlockEnergies) to High(FBlockEnergies) do
    begin
      SumEnergy := 0;
      SumCount  := 0;
      for j := 0 to NumSteps -1 do
      begin
        index := i + j;
        if index < FNumBlocks then
        begin
          SumEnergy := SumEnergy + FBlockEnergies[ch][index];
          SumCount  := SumCount + 1;
        end;
      end;

      FShortTermEnergies[i] := FShortTermEnergies[i] + (SumEnergy / SumCount) * FWeights[ch];
    end;
  end;
end;

procedure TLoudnessMeter.CalculateIntegratedLoudness;
const
  AbsoluteGateLufs = -70;
  RelativeGateLufs = -10;
var
  i: longint;
  SumCount: longint;
  SumEnergy, AvgEnergy: double;
  Energies: TListOfDouble;
begin
  Energies := TListOfDouble.Create;

  for i := 0 to FNumBlocks -1 do
  begin
    if EnergyToLufs(FMomentaryEnergies[i]) > AbsoluteGateLufs then
    begin
      Energies.Add(FMomentaryEnergies[i]);
    end;
  end;

  FIntegratedLoudness := NegInfinity;

  if Energies.Count > 0 then
  begin
    AvgEnergy := 0;
    for i := 0 to Energies.Count -1 do
    begin
      AvgEnergy := AvgEnergy + Energies[i];
    end;
    AvgEnergy := EnergyToLufs(AvgEnergy / Energies.Count);

    SumEnergy := 0;
    SumCount  := 0;
    for i := 0 to Energies.Count -1 do
    begin
      if EnergyToLufs(Energies[i]) > AvgEnergy + RelativeGateLufs then
      begin
        SumEnergy := SumEnergy + Energies[i];
        SumCount  := SumCount + 1;
      end;
    end;

    if SumCount > 0 then
      FIntegratedLoudness := EnergyToLufs(SumEnergy / SumCount);
  end;
  Energies.Destroy;
end;

procedure TLoudnessMeter.CalculateLoudnessRange;
const
  AbsoluteGateLufs = -70;
  RelativeGateLufs = -20;
var
  i: longint;
  AvgEnergy: double;
  Energies: TListOfDouble;
  p95, p10: double;
begin
  Energies := TListOfDouble.Create;

  for i := 0 to FNumBlocks -1 do
  begin
    if EnergyToLufs(FShortTermEnergies[i]) > AbsoluteGateLufs then
    begin
      Energies.Add(FShortTermEnergies[i])
    end;
  end;

  FLoudnessRange := NegInfinity;

  if Energies.Count > 0 then
  begin
    AvgEnergy := 0;
    for i := 0 to Energies.Count -1 do
    begin
      AvgEnergy := AvgEnergy + Energies[i];
    end;
    AvgEnergy := EnergyToLufs(AvgEnergy / Energies.Count);

    for i := Energies.Count -1 downto 0 do
    begin
      if EnergyToLufs(Energies[i]) <= AvgEnergy + RelativeGateLufs then
      begin
        Energies.Delete(i);
      end;
    end;

    if Energies.Count >= 2 then
    begin
      p95 := Percentile(Energies, 0.95);
      p10 := Percentile(Energies, 0.10);
      FLoudnessRange := EnergyToLufs(p95) - EnergyToLufs(p10);
    end;
  end;
  Energies.Destroy;
end;

function TLoudnessMeter.LoudnessRange: double;
begin
  result := FLoudnessRange;
end;

function TLoudnessMeter.MomentaryLoudness(AtTime: longint): double;
const
  Stepms = 100;
var
  index: longint;
begin
  index := AtTime div Stepms;

  if index < Length(FMomentaryEnergies) then
    result := EnergyToLufs(FMomentaryEnergies[index])
  else
    result := NegInfinity
end;

function TLoudnessMeter.ShortTermLoudness(AtTime: longint): double;
const
  Stepms = 100;
var
  index: longint;
begin
  index := AtTime div Stepms;

  if index < Length(FShortTermEnergies) then
    result := EnergyToLufs(FShortTermEnergies[index])
  else
    result := NegInfinity
end;

function TLoudnessMeter.IntegratedLoudness: double;
begin
  result := FIntegratedLoudness;
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
  kFilter.Init(ASampleRate);
  for ch := Low(y) to High(y) do
  begin
    SetLength(y[ch], ASampleCount);

    kFilter.Clear;
    for i := Low(y[ch]) to High(y[ch]) do
    begin
      y[ch][i] := kFilter.Run(AChannels[ch][i]);
    end;
  end;
  CalculateBlockEnergies(y);
  CalculateMomentaryEnergies;
  CalculateShortTermEnergies;
  CalculateIntegratedLoudness;
  CalculateLoudnessRange;
  CalculateCrestFactor;

  for ch := Low(y) to High(y) do
    SetLength(y[ch], 0);
  SetLength(y, 0);
end;

function TLoudnessMeter.Rms(AChannel: longint): double;
begin
  result := EnergyToDecibel(FRms2[AChannel]);
end;

function TLoudnessMeter.Rms: double;
var
  ch: longint;
  SumEnergy,
  SumWeight: double;
begin
  SumEnergy := 0;
  SumWeight := 0;
  for ch := Low(FRms2) to High(FRms2) do
  begin
    SumEnergy := SumEnergy + FWeights[ch] * FRms2[ch];
    SumWeight := SumWeight + FWeights[ch];
  end;

  if SumWeight > 0 then
    result := EnergyToDecibel(SumEnergy / SumWeight)
  else
    result := NegInfinity;
end;

function TLoudnessMeter.Peak(AChannel: longint): double;
begin
  result := Decibel(FPeak[AChannel]);
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
  result := Decibel(result);
end;

function TLoudnessMeter.TruePeak(AChannel: longint): double;
begin
  result := Decibel(FTruePeak[AChannel]);
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
  result := Decibel(result);
end;

function TLoudnessMeter.CrestFactor(AChannel: longint): double;
begin
  result := Decibel(FCrestFactors[AChannel]);
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
    result := Decibel(result / Length(FCrestFactors));
  end;
end;

function TLoudnessMeter.PeakToLoudnessRatio: double;
var
  ch: Integer;
  MaxTruePeak: double;
begin
  MaxTruePeak := 0;
  for ch := Low(FTruePeak) to High(FTruePeak) do
  begin
    MaxTruePeak := Max(MaxTruePeak, FTruePeak[ch]);
  end;

  result := Decibel(MaxTruePeak) - FIntegratedLoudness;
end;

end.

