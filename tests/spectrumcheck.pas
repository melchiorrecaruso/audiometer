{
  Description: Spectrum checks on synthetic signals.

  Feeds TSpectrums with signals whose spectrum is known in advance and
  checks the results:

    - a sine centred on a bin reads its level in dBFS (0 and -20 dBFS);
    - a sine between two bins stays within the Hann scalloping loss;
    - a 20 ms burst in a 10-minute track survives in the spectrogram rows;
    - white noise gives a flat long-term spectrum;
    - the spectrogram memory does not grow with the track length.

  Copyright (C) 2026 Melchiorre Caruso <melchiorrecaruso@gmail.com>
  GPL v2 or later.
}

unit SpectrumCheck;

{$mode objfpc}{$h+}

interface

// Returns the number of failed checks (0 = all passed).
function RunSpectrumSuite: longint;


implementation

uses
  SysUtils, Math, Common, Spectrum;

const
  SampleRate = 44100;
  // Hann scalloping loss for a sine half-way between two bins: 1.42 dB
  ScallopingDB = 1.5;

var
  Failed: longint;

procedure Check(const AName: string; AValue, ALow, AHigh: double;
  const AUnit: string);
var
  Pass: boolean;
begin
  Pass := (AValue >= ALow) and (AValue <= AHigh);
  if not Pass then Inc(Failed);
  Writeln(Format('  %-48s %9.3f %-3s [%.2f, %.2f]  %s',
    [AName, AValue, AUnit, ALow, AHigh, BoolToStr(Pass, 'PASS', 'FAIL')]));
end;

function NewSignal(ASeconds: double): TDoubleMatrix;
begin
  Result := nil;
  SetLength(Result, 1, Round(ASeconds * SampleRate));
end;

procedure AddSine(var ASignal: TDoubleMatrix; AFrequency, ALevelDB: double;
  AFirst, ACount: longint);
var
  i: longint;
  Amplitude: double;
begin
  Amplitude := Power(10, ALevelDB / 20);
  for i := AFirst to AFirst + ACount - 1 do
    ASignal[0][i] := ASignal[0][i] + Amplitude * Sin(2 * Pi * AFrequency * i / SampleRate);
end;

function Analyze(const ASignal: TDoubleMatrix): TSpectrums;
begin
  Result := Default(TSpectrums);
  Result.Init;
  Result.Process(ASignal, Length(ASignal[0]));
end;

function BinFrequency(ABin: longint): double;
begin
  Result := ABin * SampleRate / DEFAULTWINDOWSIZE;
end;

function MaxBinLevel(var ASpectrums: TSpectrums): double;
var
  k: longint;
begin
  Result := 0;
  for k := 0 to ASpectrums.OutBins - 1 do
    Result := Max(Result, ASpectrums.SpectrumPeak(k));
  Result := Decibel(Result);
end;

procedure CheckSineLevels;
const
  Bin = 46;   // 990.5 Hz, centred on a bin
var
  Signal: TDoubleMatrix;
  S: TSpectrums;
  Level: double;
begin
  Signal := NewSignal(10);
  AddSine(Signal, BinFrequency(Bin), 0, 0, Length(Signal[0]));
  S := Analyze(Signal);
  Check('sine 990.5 Hz 0 dBFS: peak level', Decibel(S.SpectrumPeak(Bin)), -0.05, 0.05, 'dB');
  Check('sine 990.5 Hz 0 dBFS: RMS level', Decibel(S.SpectrumRms(Bin)), -0.05, 0.05, 'dB');
  S.Finalize;

  Signal := NewSignal(10);
  AddSine(Signal, BinFrequency(Bin), -20, 0, Length(Signal[0]));
  S := Analyze(Signal);
  Check('sine 990.5 Hz -20 dBFS: peak level', Decibel(S.SpectrumPeak(Bin)), -20.05, -19.95, 'dB');
  S.Finalize;

  // half-way between two bins: worst case of the Hann window
  Signal := NewSignal(10);
  AddSine(Signal, BinFrequency(Bin) + 0.5 * SampleRate / DEFAULTWINDOWSIZE, 0, 0, Length(Signal[0]));
  S := Analyze(Signal);
  Level := MaxBinLevel(S);
  Check('sine between two bins 0 dBFS: highest bin', Level, -ScallopingDB, 0.05, 'dB');
  S.Finalize;
end;

procedure CheckBurst;
const
  Bin = 232;   // 4995.7 Hz
var
  Signal: TDoubleMatrix;
  S: TSpectrums;
  Row, Best, Burst: longint;
  Level, Background: double;
begin
  // 20 ms at 0 dBFS in the middle of 10 minutes of silence: about 12 windows
  // fall in each spectrogram row, so picking a single window per row would
  // usually miss it.
  Signal := NewSignal(600);
  Burst := Length(Signal[0]) div 2 + 777;
  AddSine(Signal, BinFrequency(Bin), 0, Burst, Round(0.020 * SampleRate));
  S := Analyze(Signal);

  Check('10 min track: spectrogram rows', S.RowCount, DEFAULTMAXROWS, DEFAULTMAXROWS, '');

  Level := 0;
  Best := -1;
  Background := 0;
  for Row := 0 to S.RowCount - 1 do
  begin
    if S.Channels[0][Row * S.OutBins + Bin] > Level then
    begin
      Level := S.Channels[0][Row * S.OutBins + Bin];
      Best := Row;
    end;
    if Abs(Row - S.RowCount div 2) > 4 then
      Background := Max(Background, S.Channels[0][Row * S.OutBins + Bin]);
  end;
  // the burst is shorter than a window and the rows are smoothed (1-2-1 in
  // time and frequency), so it reads below 0 dBFS but well above silence
  Check('20 ms burst 0 dBFS: level in spectrogram', Decibel(Level), -20, 0.05, 'dB');
  Check('20 ms burst: row (expected around the middle)', Best,
    S.RowCount div 2 - 2, S.RowCount div 2 + 2, '');
  Check('20 ms burst: silence elsewhere', Max(-200, Decibel(Background)), -200, -150, 'dB');
  S.Finalize;
end;

procedure CheckWhiteNoise;
var
  Signal: TDoubleMatrix;
  S: TSpectrums;
  i, k, n: longint;
  Level, Sum, SumSq, Mean, Deviation: double;
begin
  Signal := NewSignal(60);
  RandSeed := 1;
  for i := 0 to High(Signal[0]) do
    Signal[0][i] := 0.25 * (2 * Random - 1);
  S := Analyze(Signal);

  Sum := 0;
  SumSq := 0;
  n := 0;
  // away from DC and Nyquist, which are scaled differently
  for k := 10 to S.OutBins - 10 do
  begin
    Level := Decibel(S.SpectrumRms(k));
    Sum := Sum + Level;
    SumSq := SumSq + Sqr(Level);
    Inc(n);
  end;
  Mean := Sum / n;
  Deviation := Sqrt(Max(0, SumSq / n - Sqr(Mean)));
  Check('white noise: RMS spectrum flatness (std dev)', Deviation, 0, 0.5, 'dB');
  S.Finalize;
end;

function RunSpectrumSuite: longint;
begin
  Failed := 0;
  Writeln('Spectrum checks on synthetic signals');
  Writeln(Format('window %d, hop %d, %d Hz', [DEFAULTWINDOWSIZE, DEFAULTHOPSIZE, SampleRate]));
  Writeln(StringOfChar('-', 96));
  CheckSineLevels;
  CheckBurst;
  CheckWhiteNoise;
  Writeln(StringOfChar('-', 96));
  if Failed = 0 then
    Writeln('RESULT: all spectrum checks passed.')
  else
    Writeln(Format('RESULT: %d check(s) failed.', [Failed]));
  Result := Failed;
end;

end.
