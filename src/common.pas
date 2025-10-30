unit Common;

{$mode objfpc}{$h+}

interface

uses
  Classes, Fgl, SysUtils;

type
  PSample   = ^TSample;
  TSample   = double;
  TSamples  = array of TSample;
  TChannels = array of TSamples;

  TValue    = double;
  TVector   = array of TValue;
  TMatrix   = array of TVector;

  // TAudioSample = Single;
  //PAudioSample = ^TAudioSample;

  TAudioMetric = Double;
  PAudioMetric = ^TAudioMetric;


  arrayofdouble = array of double;
  arrayofarrayofdouble = array of arrayofdouble;

  listofdouble = specialize tfpglist<double>;

function Decibel(const AAmplitude: double): double;
function EnergyToDecibel(const AEnergy: double): double;
function EnergyToLufs(const AEnergy: double): double;

function Peak(ASamples: PSample; ASamplecount: longint): double;
function Rms2(ASamples: PSample; ASamplecount: longint): double;
function Rms(ASamples: PSample; ASamplecount: longint): double;
function TruePeak(ASamples: PSample; ASamplecount, ASamplerate: longint): double;

procedure QuickSort(var A: arrayofdouble; Low, High: Integer);
function Percentile(var AValues: listofdouble; P: double): double;


implementation

uses
  Math;

procedure QuickSort(var A: arrayofdouble; Low, High: Integer);
var
  i, j: Integer;
  pivot, temp: Double;
begin
  i := Low;
  j := High;
  pivot := A[(Low + High) div 2];

  repeat
    while A[i] < pivot do
      Inc(i);
    while A[j] > pivot do
      Dec(j);
    if i <= j then
    begin
      temp := A[i];
      A[i] := A[j];
      A[j] := temp;
      Inc(i);
      Dec(j);
    end;
  until i > j;

  if Low < j then
    QuickSort(A, Low, j);
  if i < High then
    QuickSort(A, i, High);
end;

function Compare(const AValue1, AValue2: double): longint;
begin
  result := Sign(AValue1 - AValue2);
end;

function  Percentile(var AValues: listofdouble; P: double): double;
var
  index: double;
  Lower, Upper: longint;
  Fraction: double;
begin
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

function Decibel(const AAmplitude: double): double;
begin
  if AAmplitude > 1e-10 then
    result := 20 * Log10(AAmplitude)
  else
    result := NegInfinity;
end;

function EnergyToDecibel(const AEnergy: double): double;
begin
  if AEnergy > 1e-10 then
    result := 10 * Log10(AEnergy)
  else
    result := NegInfinity;
end;

function EnergyToLufs(const AEnergy: double): double;
begin
  if AEnergy > 1e-10 then
    result := -0.691 + 10*Log10(AEnergy)
  else
    result := NegInfinity;
end;

function Peak(ASamples: PSample; ASamplecount: longint): double;
var
  i: longint;
begin
  result := 0.0;
  for i := 0 to ASamplecount -1  do
  begin
    result := Max(result, Abs(ASamples^));
    Inc(ASamples);
  end;
end;

function Rms2(ASamples: PSample; ASamplecount: longint): double;
var
  i: longint;
begin
  if ASamplecount = 0 then Exit(0);

  result := 0;
  for i := 0 to ASamplecount -1 do
  begin
    result := result + Sqr(ASamples^);
    Inc(ASamples);
  end;
  result := result / ASamplecount;
end;

function Rms(ASamples: PSample; ASamplecount: longint): double;
begin
  result := sqrt(Rms2(ASamples, ASamplecount));
end;

function TruePeak(ASamples: PSample; ASamplecount, ASamplerate: longint): double;
const
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
const
     Taps  = 24;
  HalfTaps = Taps div 2;
var
  i, Phase, Phases, PadLen, Tap: longint;
  PadSamples: arrayofdouble;
  Sum, Sample: double;
begin
  Result := NegInfinity;

  // Determine oversampling factor (number of FIR phases)
  case ASampleRate of
    44100, 48000: Phases := 4;  // 4x oversampling
    88200, 96000: Phases := 2;  // 2x oversampling
  else
    Phases := 2;
  end;

  // Add zero padding at both ends to avoid out-of-bound reads
  PadLen := HalfTaps;
  SetLength(PadSamples, ASamplecount + 2 * PadLen);

  // Copy the selected range of samples (AIndex .. AIndex+ASamplecount-1)
  FillChar(PadSamples[0], PadLen * SizeOf(double), 0);
  Move(ASamples^, PadSamples[PadLen], ASamplecount * SizeOf(double));
  FillChar(PadSamples[PadLen + ASamplecount], PadLen * SizeOf(double), 0);

  // Compute the true peak value
  for i := PadLen to PadLen + ASamplecount - 1 do
  begin
    for Phase := 0 to Phases - 1 do
    begin
      Sum := 0.0;
      (*
      // Loop unrolling: 4 taps per iterazione, 24 taps -> 6 iterazioni
      Tap := 0;
      while Tap < Taps do
      begin
        Sum := Sum + PadSamples[i + Tap     - HalfTaps] * Coeffs[Phase][Tap    ]
                   + PadSamples[i + Tap + 1 - HalfTaps] * Coeffs[Phase][Tap + 1]
                   + PadSamples[i + Tap + 2 - HalfTaps] * Coeffs[Phase][Tap + 2]
                   + PadSamples[i + Tap + 3 - HalfTaps] * Coeffs[Phase][Tap + 3];
        Inc(Tap, 4);
      end;
      *)

      // Full 24-tap unrolling
      Sum :=
        PadSamples[i-12] * Coeffs[Phase][ 0] +
        PadSamples[i-11] * Coeffs[Phase][ 1] +
        PadSamples[i-10] * Coeffs[Phase][ 2] +
        PadSamples[i- 9] * Coeffs[Phase][ 3] +
        PadSamples[i- 8] * Coeffs[Phase][ 4] +
        PadSamples[i- 7] * Coeffs[Phase][ 5] +
        PadSamples[i- 6] * Coeffs[Phase][ 6] +
        PadSamples[i- 5] * Coeffs[Phase][ 7] +
        PadSamples[i- 4] * Coeffs[Phase][ 8] +
        PadSamples[i- 3] * Coeffs[Phase][ 9] +
        PadSamples[i- 2] * Coeffs[Phase][10] +
        PadSamples[i- 1] * Coeffs[Phase][11] +
        PadSamples[i   ] * Coeffs[Phase][12] +
        PadSamples[i+ 1] * Coeffs[Phase][13] +
        PadSamples[i+ 2] * Coeffs[Phase][14] +
        PadSamples[i+ 3] * Coeffs[Phase][15] +
        PadSamples[i+ 4] * Coeffs[Phase][16] +
        PadSamples[i+ 5] * Coeffs[Phase][17] +
        PadSamples[i+ 6] * Coeffs[Phase][18] +
        PadSamples[i+ 7] * Coeffs[Phase][19] +
        PadSamples[i+ 8] * Coeffs[Phase][20] +
        PadSamples[i+ 9] * Coeffs[Phase][21] +
        PadSamples[i+10] * Coeffs[Phase][22] +
        PadSamples[i+11] * Coeffs[Phase][23];

      if Sum < 0 then Sum := -Sum;
      if Sum > result then result := Sum;
    end;
  end;
end;

end.

