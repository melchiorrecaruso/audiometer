{
  Description: ITU-R BS.1770-5 Loudness Measurement Routines.

  Copyright (C) 2025-2026 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
  Common, SysUtils;

type
  TShelvingFilter = record
  private
    a1, a2: double;
    b0, b1, b2: double;
    x1, x2: double;
    y1, y2: double;
  public
    procedure Init(ASamplerate: longint);
    function Process(const ASample: TDouble): TDouble;
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
    function Process(const ASample: TDouble): TDouble;
    procedure Clear;
  end;

  TKWeightingFilter = record
  private
    FHighpassFilter: THighpassFilter;
    FShelvingFilter: TShelvingFilter;
  public
    procedure Init(ASamplerate: longint);
    function Process(const ASample: TDouble): TDouble;
    procedure Clear;
  end;

  TChannelMetrics = record
  private
    FRms2: double;
    FPeak: double;
    FTruePeak: double;
    FCrestFactor: double;
  public
    procedure Process(const ASamples: TDoubleVector;
      ASampleCount, ASampleRate: longint);
    property Rms2: double read FRms2;
    property Peak: double read FPeak;
    property TruePeak: double read FTruePeak;
    property CrestFactor: double read FCrestFactor;
  end;

  TLoudnessMeter = record
  private
    FSampleRate: longint;
    FSampleCount: longint;

    FBlocks: TDoubleMatrix;
    FBlockSize: longint;
    FBlockCount: longint;

    FWeights: TDoubleVector;
    FMomentaryEnergies: TDoubleVector;
    FShortTermEnergies: TDoubleVector;
    FMaxMomentary: double;
    FMaxShortTerm: double;
    FIntegratedLoudness: double;
    FLoudnessRange: double;

    FTick: TTickMethod;
    FChannelMetrics: array of TChannelMetrics;
    procedure UpdateWeights(const AChannels: TDoubleMatrix);
    procedure UpdateBlocks(const AChannels: TDoubleMatrix);
    procedure UpdateMomentaryEnergies;
    procedure UpdateShortTermEnergies;
    procedure UpdateMaxLoudness(const AkwSamples: TDoubleMatrix);
    procedure UpdateIntegratedLoudness;
    procedure UpdateLoudnessRange;
  public
    procedure Init(const ATick: TTickMethod = nil);
    procedure Finalize;

    function EstimatedTicks(AChannelCount, ASampleCount,
      ASampleRate: longint): longint;

    // Layouts: mono, L/R, L/R/C/LS/RS, L/R/C/LFE/LS/RS.
    // Sample rates: 8000 .. High(LongInt) div 3000 Hz.
    procedure Process(const AChannels: TDoubleMatrix;
      ASampleCount, ASampleRate: longint);

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
    function MaxMomentaryLoudness: double;
    function MaxShortTermLoudness: double;
    function IntegratedLoudness: double;
    function LoudnessRange: double;
    function PeakToLoudnessRatio: double;
  end;

implementation

uses
  Math;

// Supported input range: common audio rates from 8 kHz, bounded to keep
// window-size arithmetic within LongInt. This is an API limit, not an ITU limit.
procedure ValidateSampleRate(ASampleRate: longint);
begin
  if (ASampleRate < 8000) or (ASampleRate > High(longint) div 3000) then
    raise EArgumentException.Create('Unsupported sample rate');
end;

procedure ValidateChannelCount(AChannelCount: longint);
begin
  if not (AChannelCount in [1, 2, 5, 6]) then
    raise EArgumentException.Create(
      'Unsupported channel layout (expected mono, stereo, 5.0 or 5.1)');
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
  ValidateSampleRate(ASampleRate);
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
  // writeln;
  // writeln('TShelvingFilter.b0  ', b0:0:10);
  // writeln('TShelvingFilter.b1  ', b1:0:10);
  // writeln('TShelvingFilter.b2  ', b2:0:10);
  // writeln('TShelvingFilter.a1  ', a1:0:10);
  // writeln('TShelvingFilter.a2  ', a2:0:10);
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

function TShelvingFilter.Process(const ASample: TDouble): TDouble;
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
  f0 = 38.13547087602444;
  Q  = 0.5003270373238773;
var
  Omega, denom: Double;
begin
  ValidateSampleRate(ASampleRate);
  Omega := tan(Pi * f0 / ASampleRate);
  denom := 1.0 + Omega / Q + Omega * Omega;

  // BS.1770 RLB stage: numerator is deliberately NOT gain-normalized.
  b0 := 1.0;
  b1 := -2.0;
  b2 := 1.0;
  a1 := 2.0 * (Omega * Omega - 1.0) / denom;
  a2 := (1.0 - Omega / Q + Omega * Omega) / denom;

  {$ifopt D+}
  // writeln;
  // writeln('THighpassFilter.b0  ', b0:0:10);
  // writeln('THighpassFilter.b1  ', b1:0:10);
  // writeln('THighpassFilter.b2  ', b2:0:10);
  // writeln('THighpassFilter.a1  ', a1:0:10);
  // writeln('THighpassFilter.a2  ', a2:0:10);
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

function THighpassFilter.Process(const ASample: TDouble): TDouble;
begin
  result := b0 * ASample + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2;
  // Shift state
  x2 := x1;
  x1 := ASample;
  y2 := y1;
  y1 := result;
end;

// TKWeightingFilter

procedure TKWeightingFilter.Init(ASampleRate: longint);
begin
  FShelvingFilter.Init(ASampleRate);
  FHighpassFilter.Init(ASampleRate);
end;

function TKWeightingFilter.Process(const ASample: TDouble): TDouble;
begin
  result := FShelvingFilter.Process(FHighpassFilter.Process(ASample));
end;

procedure TKWeightingFilter.Clear;
begin
  FHighpassFilter.Clear;
  FShelvingFilter.Clear;
end;

// TChannelMetrics

procedure TChannelMetrics.Process(const ASamples: TDoubleVector;
  ASampleCount, ASampleRate: longint);
begin
  FRms2 := 0;
  FPeak := 0;
  FTruePeak := 0;
  FCrestFactor := NegInfinity;

  if ASampleCount <= 0 then
    Exit;

  ValidateSampleRate(ASampleRate);
  if ASampleCount > Length(ASamples) then
    raise EArgumentException.Create('Sample count exceeds channel length');

  FRms2 := Common.Rms2(@ASamples[0], ASampleCount);
  FPeak := Common.Peak(@ASamples[0], ASampleCount);
  FTruePeak := Common.TruePeak(@ASamples[0], ASampleCount, ASampleRate);
  FCrestFactor := Common.CrestFactor(FPeak, FRms2);
end;

// TLoudnessMeter

procedure TLoudnessMeter.Init(const ATick: TTickMethod = nil);
begin
  Finalize;
  FTick := ATick;
end;

procedure TLoudnessMeter.Finalize;
begin
  FSampleRate := 0;
  FSampleCount := 0;
  FBlockSize := 0;
  FBlockCount := 0;

  SetLength(FBlocks, 0, 0);

  FWeights := nil;
  FMomentaryEnergies := nil;
  FShortTermEnergies := nil;
  FMaxMomentary := NegInfinity;
  FMaxShortTerm := NegInfinity;
  FIntegratedLoudness := NegInfinity;
  FLoudnessRange := NegInfinity;
  FChannelMetrics := nil;
end;

procedure TLoudnessMeter.UpdateWeights(const AChannels: TDoubleMatrix);
begin
  // Fixed channel order: mono; L/R; L/R/C/LS/RS; L/R/C/LFE/LS/RS.
  ValidateChannelCount(Length(AChannels));
  SetLength(FWeights, Length(AChannels));

  case Length(AChannels) of
    1:
      begin
        FWeights[0] := 1.0;   // Mono
      end;
    2:
      begin                   // Stereo
        FWeights[0] := 1.0;   // L
        FWeights[1] := 1.0;   // R
      end;
    5:
      begin                   // 5.0
        FWeights[0] := 1.0;   // L
        FWeights[1] := 1.0;   // R
        FWeights[2] := 1.0;   // C
        FWeights[3] := 1.41;  // LS (BS.1770 Table 3)
        FWeights[4] := 1.41;  // RS
      end;
    6:
      begin                   // 5.1
        FWeights[0] := 1.0;   // L
        FWeights[1] := 1.0;   // R
        FWeights[2] := 1.0;   // C
        FWeights[3] := 0.0;   // LFE
        FWeights[4] := 1.41;  // LS (BS.1770 Table 3)
        FWeights[5] := 1.41;  // RS
      end;
  end;
end;

procedure TLoudnessMeter.UpdateBlocks(const AChannels: TDoubleMatrix);
const
  BlockMs = 100;
var
  ch, i, OffSet, BlockEnd: longint;
begin
  FBlockSize := (FSampleRate * BlockMs) div 1000;

  // Round absolute grid positions, rather than accumulating truncated 100 ms
  // blocks (11025 Hz would otherwise drift by 5 samples per second).
  FBlockCount := (int64(FSampleCount) * 10) div FSampleRate;

  FBlocks := nil;
  if FBlockCount > 0 then
  begin
    SetLength(FBlocks, Length(AChannels), FBlockCount);
    for ch := Low(AChannels) to High(AChannels) do
    begin
      OffSet := 0;
      for i := 0 to FBlockCount - 1 do
      begin
        BlockEnd := (int64(i + 1) * FSampleRate + 5) div 10;
        // Store energy sums: adjacent grid cells may differ by one sample.
        FBlocks[ch][i] := Common.Rms2(@AChannels[ch][OffSet], BlockEnd - OffSet)
          * (BlockEnd - OffSet);
        OffSet := BlockEnd;
      end;
    end;
  end;
end;

procedure TLoudnessMeter.UpdateMomentaryEnergies;
const
  NumSteps = 4;
var
  ch, i, j: longint;
  SumEnergy: double;
begin
  SetLength(FMomentaryEnergies, FBlockCount);

  for i := 0 to FBlockCount - 1 do
  begin
    FMomentaryEnergies[i] := 0;

    if i < NumSteps - 1 then
      Continue;

    for ch := Low(FBlocks) to High(FBlocks) do
    begin
      SumEnergy := 0;
      for j := 0 to NumSteps - 1 do
      begin
        SumEnergy := SumEnergy + FBlocks[ch][i - j];
      end;
      FMomentaryEnergies[i] := FMomentaryEnergies[i] + SumEnergy * FWeights[ch] /
        (((int64(i + 1) * FSampleRate + 5) div 10) -
         ((int64(i + 1 - NumSteps) * FSampleRate + 5) div 10));
    end;
  end;
end;

procedure TLoudnessMeter.UpdateShortTermEnergies;
const
  NumSteps = 30;
var
  ch, i, j: longint;
  SumEnergy: double;
begin
  SetLength(FShortTermEnergies, FBlockCount);

  for i := 0 to FBlockCount - 1 do
  begin
    FShortTermEnergies[i] := 0;

    if i < NumSteps - 1 then
      Continue;

    for ch := Low(FBlocks) to High(FBlocks) do
    begin
      SumEnergy := 0;
      for j := 0 to NumSteps - 1 do
      begin
        SumEnergy := SumEnergy + FBlocks[ch][i - j];
      end;
      FShortTermEnergies[i] := FShortTermEnergies[i] + SumEnergy * FWeights[ch] /
        (((int64(i + 1) * FSampleRate + 5) div 10) -
         ((int64(i + 1 - NumSteps) * FSampleRate + 5) div 10));
    end;
  end;
end;

procedure TLoudnessMeter.UpdateMaxLoudness(const AkwSamples: TDoubleMatrix);
const
  MomentaryMs = 400;
  ShortTermMs = 3000;
var
  ch, p, MomentarySize, ShortTermSize: longint;
  Square: double;
  MomentaryEnergy, ShortTermEnergy: double;
  MaxMomentaryEnergy, MaxShortTermEnergy: double;
  MomentarySum, ShortTermSum: TDoubleVector;
begin
  // Sample-accurate sliding-window maxima for momentary (400 ms) and short-term
  // (3 s) loudness. Same energy/weight accumulation as UpdateMomentaryEnergies /
  // UpdateShortTermEnergies, but over the K-weighted sample stream: the maxima
  // need finer resolution than the 100 ms block grid to meet the EBU peak tests
  // (3341 cases 10/11/13/14). The running sums are the O(N) equivalent of the
  // per-block window loop used there; EnergyToLufs is monotonic, so the maximum
  // energy is tracked and converted to LUFS only once at the end.
  FMaxMomentary := NegInfinity;
  FMaxShortTerm := NegInfinity;

  if (Length(AkwSamples) = 0) or (FSampleCount <= 0) then
    Exit;

  MomentarySize := (int64(FSampleRate) * MomentaryMs + 500) div 1000;
  ShortTermSize := (int64(FSampleRate) * ShortTermMs + 500) div 1000;

  SetLength(MomentarySum, Length(AkwSamples));
  SetLength(ShortTermSum, Length(AkwSamples));
  for ch := Low(AkwSamples) to High(AkwSamples) do
  begin
    MomentarySum[ch] := 0;
    ShortTermSum[ch] := 0;
  end;

  MaxMomentaryEnergy := 0;
  MaxShortTermEnergy := 0;

  for p := 0 to FSampleCount - 1 do
  begin
    for ch := Low(AkwSamples) to High(AkwSamples) do
    begin
      Square := Sqr(AkwSamples[ch][p]);
      MomentarySum[ch] := MomentarySum[ch] + Square;
      ShortTermSum[ch] := ShortTermSum[ch] + Square;
      if p >= MomentarySize then
        MomentarySum[ch] := MomentarySum[ch] - Sqr(AkwSamples[ch][p - MomentarySize]);
      if p >= ShortTermSize then
        ShortTermSum[ch] := ShortTermSum[ch] - Sqr(AkwSamples[ch][p - ShortTermSize]);
    end;

    if p >= MomentarySize - 1 then
    begin
      MomentaryEnergy := 0;
      for ch := Low(AkwSamples) to High(AkwSamples) do
        MomentaryEnergy := MomentaryEnergy +
          (MomentarySum[ch] / MomentarySize) * FWeights[ch];
      if MomentaryEnergy > MaxMomentaryEnergy then
        MaxMomentaryEnergy := MomentaryEnergy;
    end;

    if p >= ShortTermSize - 1 then
    begin
      ShortTermEnergy := 0;
      for ch := Low(AkwSamples) to High(AkwSamples) do
        ShortTermEnergy := ShortTermEnergy +
          (ShortTermSum[ch] / ShortTermSize) * FWeights[ch];
      if ShortTermEnergy > MaxShortTermEnergy then
        MaxShortTermEnergy := ShortTermEnergy;
    end;
  end;

  if MaxMomentaryEnergy > 0 then
    FMaxMomentary := EnergyToLufs(MaxMomentaryEnergy);
  if MaxShortTermEnergy > 0 then
    FMaxShortTerm := EnergyToLufs(MaxShortTermEnergy);
end;

procedure TLoudnessMeter.UpdateIntegratedLoudness;
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
  try
    for i := 0 to FBlockCount - 1 do
    begin
      if EnergyToLufs(FMomentaryEnergies[i]) > AbsoluteGateLufs then
        Energies.Add(FMomentaryEnergies[i]);
    end;

    FIntegratedLoudness := NegInfinity;
    if Energies.Count > 0 then
    begin
      AvgEnergy := 0;
      for i := 0 to Energies.Count - 1 do
        AvgEnergy := AvgEnergy + Energies[i];
      AvgEnergy := EnergyToLufs(AvgEnergy / Energies.Count);

      SumEnergy := 0;
      SumCount := 0;
      for i := 0 to Energies.Count - 1 do
      begin
        if EnergyToLufs(Energies[i]) > AvgEnergy + RelativeGateLufs then
        begin
          SumEnergy := SumEnergy + Energies[i];
          SumCount := SumCount + 1;
        end;
      end;

      if SumCount > 0 then
        FIntegratedLoudness := EnergyToLufs(SumEnergy / SumCount);
    end;
  finally
    Energies.Free;
  end;
end;

function Compare(const Item1, Item2: TDouble): longint;
begin
  result := Sign(Item1 - Item2);
end;

procedure TLoudnessMeter.UpdateLoudnessRange;
const
  AbsoluteGateLufs = -70;
  RelativeGateLufs = -20;
var
  i, iLow, iHigh: longint;
  AvgEnergy: double;
  Loudness: TListOfDouble;
  Energies: TListOfDouble;
begin
  Energies := TListOfDouble.Create;
  Loudness := nil;
  try
    // Absolute gate on the short-term blocks
    for i := 0 to FBlockCount - 1 do
      if EnergyToLufs(FShortTermEnergies[i]) >= AbsoluteGateLufs then
        Energies.Add(FShortTermEnergies[i]);

    FLoudnessRange := NegInfinity;
    if Energies.Count > 0 then
    begin
      // Relative gate: -20 LU below the mean loudness of the abs-gated set
      AvgEnergy := 0;
      for i := 0 to Energies.Count - 1 do
        AvgEnergy := AvgEnergy + Energies[i];
      AvgEnergy := EnergyToLufs(AvgEnergy / Energies.Count);

      // Build the relative-gated distribution as LOUDNESS values (LUFS),
      // not energies: EBU Tech 3342 takes percentiles of the loudness
      // distribution, so the log conversion must happen before sorting.
      Loudness := TListOfDouble.Create;
      for i := 0 to Energies.Count - 1 do
        if EnergyToLufs(Energies[i]) >= AvgEnergy + RelativeGateLufs then
          Loudness.Add(EnergyToLufs(Energies[i]));

      if Loudness.Count > 0 then
      begin
        Loudness.Sort(@Compare);
        // EBU Tech 3342 percentiles: nearest-rank, round-half-up, P10 and P95.
        iLow := Trunc((Loudness.Count - 1) * 0.10 + 0.5);
        iHigh := Trunc((Loudness.Count - 1) * 0.95 + 0.5);
        FLoudnessRange := Loudness[iHigh] - Loudness[iLow];
      end;
    end;
  finally
    Loudness.Free;
    Energies.Free;
  end;
end;

function TLoudnessMeter.MomentaryLoudness(AtTime: longint): double;
const
  Stepms = 100;
var
  index: longint;
begin
  // Entry 0 ends at 100 ms, not at time zero. Never expose future audio.
  index := AtTime div Stepms - 1;

  if (index >= 0) and (index < Length(FMomentaryEnergies)) then
    result := EnergyToLufs(FMomentaryEnergies[index])
  else
    result := NegInfinity;
end;

function TLoudnessMeter.ShortTermLoudness(AtTime: longint): double;
const
  Stepms = 100;
var
  index: longint;
begin
  index := AtTime div Stepms - 1;

  if (index >= 0) and (index < Length(FShortTermEnergies)) then
    result := EnergyToLufs(FShortTermEnergies[index])
  else
    result := NegInfinity;
end;

function TLoudnessMeter.MaxMomentaryLoudness: double;
begin
  result := FMaxMomentary;
end;

function TLoudnessMeter.MaxShortTermLoudness: double;
begin
  result := FMaxShortTerm;
end;

function TLoudnessMeter.IntegratedLoudness: double;
begin
  result := FIntegratedLoudness;
end;

function TLoudnessMeter.LoudnessRange: double;
begin
  result := FLoudnessRange;
end;

function TLoudnessMeter.EstimatedTicks(AChannelCount, ASampleCount,
  ASampleRate: longint): longint;
begin
  if (AChannelCount <= 0) or (ASampleCount <= 0) then
    Exit(0);

  ValidateSampleRate(ASampleRate);
  ValidateChannelCount(AChannelCount);

  // One tick for metrics and filtering per channel, then six analysis stages.
  result := 2 * AChannelCount + 6;
end;

procedure TLoudnessMeter.Process(const AChannels: TDoubleMatrix;
  ASampleCount, ASampleRate: longint);
var
  kwBlocks: TDoubleMatrix;
  kFilter: TKWeightingFilter;
  ch, i: longint;
begin
  Finalize;

  if (Length(AChannels) = 0) or (ASampleCount <= 0) then
    Exit;

  ValidateSampleRate(ASampleRate);
  ValidateChannelCount(Length(AChannels));
  for ch := Low(AChannels) to High(AChannels) do
    if Length(AChannels[ch]) < ASampleCount then
      raise EArgumentException.Create('Sample count exceeds channel length');

  FSampleCount := ASampleCount;
  FSampleRate := ASampleRate;

  // Single channel metrics
  SetLength(FChannelMetrics, Length(AChannels));
  for ch := Low(FChannelMetrics) to High(FChannelMetrics) do
  begin
    FChannelMetrics[ch].Process(AChannels[ch], ASampleCount, ASampleRate);
    if Assigned(FTick) then
      FTick;
  end;

  // Loudness metrics
  kwBlocks := nil;
  SetLength(kwBlocks, Length(AChannels), ASampleCount);
  kFilter.Init(ASampleRate);
  for ch := Low(kwBlocks) to High(kwBlocks) do
  begin
    kFilter.Clear;
    for i := Low(kwBlocks[ch]) to High(kwBlocks[ch]) do
    begin
      kwBlocks[ch][i] := kFilter.Process(AChannels[ch][i]);
    end;
    if Assigned(FTick) then
      FTick;
  end;

  UpdateWeights(AChannels);
  UpdateBlocks(kwBlocks);
  if Assigned(FTick) then
    FTick;

  UpdateMomentaryEnergies;
  if Assigned(FTick) then
    FTick;

  UpdateShortTermEnergies;
  if Assigned(FTick) then
    FTick;

  UpdateIntegratedLoudness;
  if Assigned(FTick) then
    FTick;

  UpdateLoudnessRange;
  if Assigned(FTick) then
    FTick;

  UpdateMaxLoudness(kwBlocks);
  if Assigned(FTick) then
    FTick;

  for ch := Low(kwBlocks) to High(kwBlocks) do
    kwBlocks[ch] := nil;
  kwBlocks := nil;
end;

function TLoudnessMeter.Rms(AChannel: longint): double;
begin
  result := EnergyToDecibel(FChannelMetrics[AChannel].Rms2);
end;

function TLoudnessMeter.Rms: double;
var
  ch: longint;
  SumEnergy: double;
  SumWeight: double;
begin
  SumEnergy := 0;
  SumWeight := 0;
  for ch := Low(FChannelMetrics) to High(FChannelMetrics) do
  begin
    SumEnergy := SumEnergy + FChannelMetrics[ch].Rms2 * FWeights[ch];
    SumWeight := SumWeight + FWeights[ch];
  end;

  if SumWeight > 0 then
    result := EnergyToDecibel(SumEnergy / SumWeight)
  else
    result := NegInfinity;
end;

function TLoudnessMeter.Peak(AChannel: longint): double;
begin
  result := Decibel(FChannelMetrics[AChannel].Peak);
end;

function TLoudnessMeter.Peak: double;
var
  ch: longint;
begin
  result := 0;
  for ch := Low(FChannelMetrics) to High(FChannelMetrics) do
  begin
    result := Max(result, FChannelMetrics[ch].Peak);
  end;
  result := Decibel(result);
end;

function TLoudnessMeter.TruePeak(AChannel: longint): double;
begin
  result := Decibel(FChannelMetrics[AChannel].TruePeak);
end;

function TLoudnessMeter.TruePeak: double;
var
  ch: longint;
begin
  result := 0;
  for ch := Low(FChannelMetrics) to High(FChannelMetrics) do
  begin
    result := Max(result, FChannelMetrics[ch].TruePeak);
  end;
  result := Decibel(result);
end;

function TLoudnessMeter.CrestFactor(AChannel: longint): double;
begin
  result := Decibel(FChannelMetrics[AChannel].CrestFactor);
end;

function TLoudnessMeter.CrestFactor: double;
var
  ch, ActiveChannels: longint;
begin
  // Retain the mean of channel amplitude ratios. Silence has no defined
  // ratio and must not poison the result of the other channels.
  result := 0;
  ActiveChannels := 0;
  for ch := Low(FChannelMetrics) to High(FChannelMetrics) do
    if FChannelMetrics[ch].Rms2 > 0 then
    begin
      result := result + FChannelMetrics[ch].CrestFactor;
      Inc(ActiveChannels);
    end;

  if ActiveChannels > 0 then
    result := Decibel(result / ActiveChannels)
  else
    result := NegInfinity;
end;

function TLoudnessMeter.PeakToLoudnessRatio: double;
var
  ch: longint;
  MaxTruePeak: double;
begin
  MaxTruePeak := 0;
  for ch := Low(FChannelMetrics) to High(FChannelMetrics) do
  begin
    MaxTruePeak := Max(MaxTruePeak, FChannelMetrics[ch].TruePeak);
  end;
  // PLR is undefined without a finite integrated measurement (silence,
  // sub-gate material, or less than one complete gating window).
  if (MaxTruePeak <= 0) or IsInfinite(FIntegratedLoudness) then
    result := NegInfinity
  else
    result := Decibel(MaxTruePeak) - FIntegratedLoudness;
end;

end.
