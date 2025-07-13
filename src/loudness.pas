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

unit loudness;

{$mode objfpc}{$h+}
{$modeswitch advancedrecords}

interface

uses
  classes, common, fgl, sysutils;

type
  arrayofdouble = array of double;
  arrayofarrayofdouble = array of arrayofdouble;

  tshelvingfilter = record
  private
    a1, a2: double;
    b0, b1, b2: double;
    x1, x2: double;
    y1, y2: double;
  public
    procedure init(asamplerate: longint);
    function run(const asample: tsample): tsample;
  end;

  thighpassfilter = record
  private
    a1, a2: double;
    b0, b1, b2: double;
    x1, x2: double;
    y1, y2: double;
  public
    procedure init(asamplerate: longint);
    function run(const asample: tsample): tsample;
  end;

  tkweightingfilter = record
  private
    highpassfilter: thighpassfilter;
    shelvingfilter: tshelvingfilter;
  public
    procedure init(asamplerate: longint);
    function run(const asamples: tsamples): tsamples;
    function run(const asample: tsample): tsample;
  end;

  tebur128analyzer = class
  private
    fsamplerate: longint;
    fenergies: arrayofarrayofdouble;

    ftruepeakperchannel: arrayofdouble;
    fpeakperchannel: arrayofdouble;
    frms2perchannel: arrayofdouble;
    FMomentaryLoudnessPerChannel: arrayofarrayofdouble;
    FShortTermLoudnessPerChannel: arrayofarrayofdouble;
    FIntegratedLoudnessPerChannel: arrayofdouble;
    fcrestperchannel: arrayofdouble;
    fplrperchannel: arrayofdouble;
    fmperchannel: arrayofdouble;
    fsperchannel: arrayofdouble;
    fiperchannel: arrayofdouble;
    flraperchannel: arrayofdouble;

    ftruepeakglobal: double;
    fpeakglobal: double;
    frms2global: double;
    fcrestglobal: double;
    fplrglobal: double;
    fmglobal: double;
    fsglobal: double;
    figlobal: double;
    flraglobal: double;

    procedure ClearFIRCoefficents(var ACoeffs: arrayofarrayofdouble);
    procedure GenerateFIRCoefficents(var ACoeffs: arrayofarrayofdouble; AOversample, ATaps: longint);

    procedure CalculateTruePeak(const achannels: tchannels);
    procedure CalculatePeak(const achannels: tchannels);
    procedure CalculateRms2(const achannels: tchannels);
    procedure CalculateCrestFactor(const achannels: tchannels);
    procedure CalculatePeakToLoudnessRatio(const achannels: tchannels);
    procedure CalculateMomentaryLoudness(const achannels: tchannels);
    procedure CalculateShortTermLoudness(const achannels: tchannels);
    procedure CalculateIntegratedLoudness(const achannels: tchannels);
    procedure CalculateLoudnessRange(const achannels: tchannels);
  public
    constructor create;
    destructor destroy; override;
    procedure analyze(const achannels: tchannels; asamplerate: integer);
    procedure clear;

    property truepeakperchannel: arrayofdouble read ftruepeakperchannel;
    property peakperchannel: arrayofdouble read fpeakperchannel;
    property rms2perchannel: arrayofdouble read frms2perchannel;
    property crestperchannel: arrayofdouble read fcrestperchannel;
    property plrperchannel: arrayofdouble read fplrperchannel;
    property mperchannel: arrayofdouble read fmperchannel;
    property sperchannel: arrayofdouble read fsperchannel;
    property iperchannel: arrayofdouble read fiperchannel;
    property lraperchannel: arrayofdouble read flraperchannel;

    property truepeakglobal: double read ftruepeakglobal;
    property peakglobal: double read fpeakglobal;
    property rms2global: double read frms2global;
    property crestglobal: double read fcrestglobal;
    property plrglobal: double read fplrglobal;
    property mglobal: double read fmglobal;
    property sglobal: double read fsglobal;
    property iglobal: double read figlobal;
    property lraglobal: double read flraglobal;
  end;


function rms2tolufs(const aenergy: double): double;
function truepeak(const asamples: tsamples; aoversample, ataps: longint): tsample;
function lufs(const asamples: tsamples; asamplerate: longint): double;
function lra(const asamples: tsamples; asamplerate: longint): double;
function plr(const truepeak: tsample; const energy: double): double;

implementation

uses
  math;

function rms2tolufs(const aenergy: double): double;
begin
  if aenergy > 1e-10 then
    result := 10*log10(aenergy)
  else
    result := neginfinity;
end;

function plr(const truepeak: tsample; const energy: double): double;
begin
  result := decibel(truepeak) - rms2tolufs(energy);
end;

function truepeak(const asamples: tsamples; aoversample, ataps: longint): double;
begin

end;

function lufs(const asamples: tsamples; asamplerate: integer): double;
begin

end;

function percentile(const values: arrayofdouble; p: double): double;
var
  i: longint;
  index: double;
  list: tlistofdouble;
begin
  if length(values) = 0 then
    exit(neginfinity);

  list := tlistofdouble.create;
  for i := low(values) to high(values) do
  begin
    list.add(values[i]);
  end;
  list.sort(@compare);

  index := p * (list.count - 1);
  i := floor(index);

  if i >= (list.count - 1) then
    result := list[list.count - 1]
  else
    result := list[i] + (index - i) * (list[i + 1] - list[i]);

  list.free;
end;

function lra(const asamples: tsamples; asamplerate: integer): double;
begin

end;

// tshelvingfilter

procedure tshelvingfilter.init(asamplerate: longint);
const
  gain = 4.0;
  f0 = 1500.0;
var
  a, w0, alpha, cosw0: double;
begin
  x1 := 0; x2 := 0;
  y1 := 0; y2 := 0;

  a := power(10, gain / 40);
  w0 := 2 * pi * f0 / asamplerate;
  cosw0 := cos(w0);
  alpha := sin(w0) / 2 * sqrt((a + 1/a) * (1 / 0.707 - 1) + 2); // q ≈ 0.707

  a1 := -2 *     ((a - 1) + (a + 1) * cosw0);
  a2 :=          ((a + 1) + (a - 1) * cosw0 - 2 * sqrt(a) * alpha);
  b0 :=      a * ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
  b1 := -2 * a * ((a - 1) + (a + 1) * cosw0);
  b2 :=      a * ((a + 1) + (a - 1) * cosw0 - 2 * sqrt(a) * alpha);

  // normalize
  a1 := a1 / ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
  a2 := a2 / ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
  b0 := b0 / ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
  b1 := b1 / ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
  b2 := b2 / ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
end;

function tshelvingfilter.run(const asample: tsample): tsample;
begin
  result := b0 * asample + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2;
  // shift state
  x2 := x1;
  x1 := asample;
  y2 := y1;
  y1 := result;
end;

// thighpassfilter

procedure thighpassfilter.init(asamplerate: longint);
const
  f0 = 60.0;
var
  w0, c, norm: Double;
begin
  x1 := 0; x2 := 0;
  y1 := 0; y2 := 0;

  w0 := 2 * Pi * f0 / asamplerate;
  c := 1 / tan(w0 / 2);

  norm := 1 / (1 + sqrt(2) * c + c * c);
  b0 := 1 * norm;
  b1 := -2 * b0;
  b2 := b0;
  a1 := 2 * (c * c - 1) * norm;
  a2 := (1 - sqrt(2) * c + c * c) * norm;
end;

function thighpassfilter.run(const asample: tsample): tsample;
begin
  result := b0 * asample + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2;
  // shift state
  x2 := x1;
  x1 := asample;
  y2 := y1;
  y1 := result;
end;

// tkweightingfilter

procedure tkweightingfilter.init(asamplerate: longint);
begin
  highpassfilter.init(asamplerate);
  shelvingfilter.init(asamplerate);
end;

function tkweightingfilter.run(const asample: tsample): tsample;
begin
  result := shelvingfilter.run(highpassfilter.run(asample));
end;

function tkweightingfilter.run(const asamples: tsamples): tsamples;
var
  i: integer;
begin
  result := nil;
  setlength(result, length(asamples));
  for i := low(result) to high(result) do
    result[i] := run(asamples[i]);
end;

// tebur128analyzer

constructor tebur128analyzer.Create;
begin
  inherited Create;
  ftruepeakperchannel := nil;
  fpeakperchannel     := nil;
  frms2perchannel     := nil;

  FMomentaryLoudnessPerChannel  := nil;
  FShortTermLoudnessPerChannel  := nil;
  FIntegratedLoudnessPerChannel := nil;

  fcrestperchannel    := nil;
  fplrperchannel      := nil;
  fmperchannel        := nil;
  fsperchannel        := nil;
  fiperchannel        := nil;
  flraperchannel      := nil;
end;

destructor tebur128analyzer.Destroy;
begin
  Clear;
  inherited destroy;
end;

procedure tebur128analyzer.ClearFIRCoefficents(var ACoeffs: arrayofarrayofdouble);
var
  i: longint;
begin
  for i := low(ACoeffs) to high(ACoeffs) do
  begin
    setlength(ACoeffs[i], 0);
  end;
  setlength(ACoeffs, 0);
end;

procedure tebur128analyzer.GenerateFIRCoefficents(var ACoeffs: arrayofarrayofdouble; AOversample, ATaps: longint);
var
  fc, x: double;
  i, Phase, Tap: longint;
  Coeff, Sum: double;
begin
  Setlength(ACoeffs, AOversample);
  for i := low(ACoeffs) to high(ACoeffs) do
    Setlength(ACoeffs[i], ATaps);

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

procedure tebur128analyzer.CalculateTruePeak(const AChannels: tchannels);
var
  i, j, index: longint;
  Coeffs: arrayofarrayofdouble;
  Phase, Tap, Taps, Oversample: longint;
  Sum, TruePeak: double;
begin
  Coeffs := nil;
  for i := low(AChannels) to high(AChannels) do
    case fsamplerate of
      44100: begin Oversample := 4; Taps := 12; GenerateFIRCoefficents(Coeffs, Oversample, Taps); end;
      48000: begin Oversample := 4; Taps := 12; GenerateFIRCoefficents(Coeffs, Oversample, Taps); end;
      88200: begin Oversample := 2; Taps := 12; GenerateFIRCoefficents(Coeffs, Oversample, Taps); end;
      96000: begin Oversample := 2; Taps := 12; GenerateFIRCoefficents(Coeffs, Oversample, Taps); end;
    end;

  SetLength(FTruePeakPerChannel, Length(AChannels));
  if Length(Coeffs) > 0 then
  begin
    for i := low(AChannels) to high(AChannels) do
    begin
      TruePeak := 0;
      for j := low(AChannels[i]) to high(AChannels[i]) do
      begin
        for Phase := 0 to Oversample -1 do
        begin
          Sum := 0;
          for Tap := 0 to Taps -1 do
          begin
            index := Tap - (Taps div 2) + j;
            if index in [low(AChannels[i]), high(AChannels[i])] then
              Sum := Sum + AChannels[i][index] * Coeffs[Phase, Tap];
          end;
          TruePeak := Max(TruePeak, Abs(Sum));
        end;
      end;
      FTruePeakPerChannel[i] := TruePeak;
    end;
    ClearFIRCoefficents(Coeffs);
  end else
  begin
    for i := low(AChannels) to high(AChannels) do
    begin
      FTruePeakPerChannel[i] := FPeakPerChannel[i];
    end;
  end;
end;

procedure tebur128analyzer.CalculatePeak(const AChannels: tchannels);
var
  i: longint;
begin
  SetLength(FPeakPerChannel, Length(AChannels));
  for i := Low(FPeakPerChannel) to High(FPeakPerChannel) do
    FPeakPerChannel[i] := Rms2(AChannels[i], 0, Length(AChannels[i]));
end;

procedure tebur128analyzer.CalculateRms2(const AChannels: tchannels);
var
  i: longint;
begin
  SetLength(FRms2PerChannel, Length(AChannels));
  for i := Low(FRms2PerChannel) to High(FRms2PerChannel) do
    FRms2PerChannel[i] := Rms2(AChannels[i], 0, Length(AChannels[i]));
end;

procedure tebur128analyzer.CalculateCrestFactor(const AChannels: TChannels);
var
  i: longint;
begin
  SetLength(FCrestPerChannel, Length(AChannels));
  for i := Low(FCrestPerChannel) to High(FCrestPerChannel) do
    FCrestPerChannel[i] := Decibel(FTruePeakPerChannel[i]/Sqrt(FRms2PerChannel[i]));
end;

procedure tebur128analyzer.CalculatePeakToLoudnessRatio(const achannels: tchannels);
var
  i: longint;
begin
  setlength(fplrperchannel, length(achannels));
  for i := low(fplrperchannel) to high(fplrperchannel) do
    fplrperchannel[i] := decibel(ftruepeakperchannel[i]) - FIntegratedLoudnessPerChannel[i];
end;

procedure tebur128analyzer.CalculateMomentaryLoudness(const AChannels: TChannels);
const
  Blockms = 400;
  Stepms  = 100;
var
  BlockSize, StepSize, i, ch: longint;
begin
  BlockSize := (FSamplerate * Blockms) div 1000;
  StepSize  := (FSamplerate * Stepms ) div 1000;

  SetLength(FMomentaryLoudnessPerChannel, Length(AChannels));
  for ch := Low(AChannels) to High(AChannels) do
  begin
    SetLength(FMomentaryLoudnessPerChannel[ch], Length(AChannels[ch]) div StepSize);

    for i := Low(FMomentaryLoudnessPerChannel[ch]) to High(FMomentaryLoudnessPerChannel[ch]) do
    begin
      FMomentaryLoudnessPerChannel[ch][i] := -0.691 + Rms2ToLufs(Rms2(AChannels[ch], i * StepSize, BlockSize));
    end;
  end;
end;

procedure tebur128analyzer.CalculateShortTermLoudness(const achannels: tchannels);
const
  Blockms = 3000;
  Stepms  = 100;
var
  BlockSize, StepSize, i, ch: longint;
begin
  BlockSize := (FSamplerate * Blockms) div 1000;
  StepSize  := (FSamplerate * Stepms ) div 1000;

  SetLength(FShortTermLoudnessPerChannel, Length(AChannels));
  for ch := Low(AChannels) to High(AChannels) do
  begin
    SetLength(FShortTermLoudnessPerChannel[ch], Length(AChannels[ch]) div StepSize);

    for i := Low(FShortTermLoudnessPerChannel[ch]) to High(FShortTermLoudnessPerChannel[ch]) do
    begin
      FShortTermLoudnessPerChannel[ch][i] := -0.691 + Rms2ToLufs(Rms2(AChannels[ch], i * StepSize, BlockSize));
    end;
  end;
end;

procedure tebur128analyzer.CalculateIntegratedLoudness(const achannels: tchannels);
const
  Blockms = 400;
  Stepms  = 100;
  AbsoluteGateLufs = -70.0;
var
  BlockSize, StepSize, i, ch: longint;
  Energy: double;
  Energies: arrayofdouble = nil;
  EnergyCount: integer;
  e0, e1: double;
  e0Count, e1Count: longint;
begin
  BlockSize := (FSamplerate * Blockms) div 1000;
  StepSize  := (FSamplerate * Stepms ) div 1000;

  SetLength(FIntegratedLoudnessPerChannel, Length(AChannels));
  for ch := Low(AChannels) to High(AChannels) do
  begin
    SetLength(Energies, Length(AChannels[ch]) div StepSize);

    EnergyCount := 0;

    i := Low(AChannels[ch]);
    while (i + BlockSize) < Length(AChannels[ch]) do
    begin
      Energy := Rms2(AChannels[ch], i, BlockSize);

      if Rms2ToLufs(Energy) > AbsoluteGateLufs then
      begin
        Energies[EnergyCount] := Energy;
        inc(EnergyCount);
      end;
      inc(i, StepSize);
    end;

    e0 := 0.0;
    e0Count := 0;
    for i := 0 to EnergyCount - 1 do
    begin
      e0 := e0 + Energies[i];
      inc(e0Count);
    end;
    if e0Count > 0 then e0 := e0 / e0Count;

    e1 := 0.0;
    e1Count := 0;
    for i := 0 to EnergyCount - 1 do
      if Energies[i] >= (e0 / 10.0) then
      begin
        e1 := e1 + Energies[i];
        inc(e1Count);
      end;
    if e1Count > 0 then e1 := e1 / e1Count;

    if (e0Count > 0) and (e1Count > 0) then
      FIntegratedLoudnessPerChannel[ch] := Rms2ToLufs(e1)
    else
      FIntegratedLoudnessPerChannel[ch] := neginfinity;
  end;
end;

procedure tebur128analyzer.CalculateLoudnessRange(const achannels: tchannels);
const
  blockms = 3000;
  stepms  = 1000;
  absolutegatelufs = -70.0;
var
  blocksize, stepsize, i: longint;
  energy: double;
  energies: arrayofdouble = nil;
  energycount: integer;
  p95, p10: double;
begin
  blocksize := (asamplerate * blockms) div 1000;
  stepsize  := (asamplerate * stepms ) div 1000;




  setlength(energies, length(asamples) div stepsize);
  energycount := 0;

  i := 0;
  while (i + blocksize) < length(asamples) do
  begin
    energy := rms2(asamples, i, blocksize);

    if rms2tolufs(energy) > absolutegatelufs then
    begin
      energies[energycount] := energy;
      inc(energycount);
    end;
    inc(i, stepsize);
  end;

  if energycount >= 2 then
  begin
    p95 := percentile(energies, 0.95);
    p10 := percentile(energies, 0.10);
    result := p95 - p10;
  end else
    result := 0;
end;

procedure tebur128analyzer.Analyze(const AChannels: TChannels; ASamplerate: integer);
var
  i, j: longint;
  kWeighted: TChannels;
  kWeightingFilter: TkWeightingFilter;
begin
  FSamplerate := ASamplerate;

  Clear;
  CalculatePeak(AChannels);
  CalculateRms2(AChannels);
  CalculateTruePeak(AChannels);

  kWeighted := nil;
  SetLength(kWeighted, Length(AChannels));
  for i := low(kWeighted) to high(kWeighted) do
  begin
    kWeightingFilter.Init(ASamplerate);

    SetLength(kWeighted[i], Length(AChannels[i]));
    for j := low(kWeighted[i]) to high(kWeighted[i]) do
    begin
      kweighted[i][j] := kWeightingFilter.Run(AChannels[i][j]);
    end;
  end;

  CalculateMomentaryLoudness(kWeighted);
  CalculateShortTermLoudness(kWeighted);
  CalculateIntegratedLoudness(kWeighted);








end;

procedure tebur128analyzer.Clear;
begin
  setlength(ftruepeakperchannel, 0);
  setlength(fpeakperchannel    , 0);
  setlength(frms2perchannel    , 0);
  setlength(FMomentaryLoudnessPerChannel    , 0);
  setlength(fcrestperchannel   , 0);
  setlength(fplrperchannel     , 0);
  setlength(fmperchannel       , 0);
  setlength(fsperchannel       , 0);
  setlength(fiperchannel       , 0);
  setlength(flraperchannel     , 0);
end;

end.

