unit LoudnessRegression;
{$mode objfpc}{$H+}

interface

procedure LoudnessTests;

implementation

uses SysUtils, Math, Common, Loudness, RegressionSupport;

type
  TTickCounter = class
    Count: integer;
    procedure Tick;
  end;

procedure TTickCounter.Tick;
begin
  Inc(Count);
end;

procedure LoudnessTests;
const N=192000; Rate=48000;
var L: TLoudnessMeter; A: TDoubleMatrix; i,ch: integer;
  Mono: double; H: THighpassFilter; S: TShelvingFilter;
  K: TKWeightingFilter; M: TChannelMetrics;
  Raised: boolean;
  Counter: TTickCounter;
  TestRate: longint;
begin
  // Published BS.1770 48 kHz impulse coefficients, independent of filter code.
  H.Init(Rate); S.Init(Rate);
  Check('BS1770 highpass b0', H.Process(1), 1, 1e-12);
  Check('BS1770 highpass impulse[1]', H.Process(0), -0.009952545166020, 1e-12);
  Check('BS1770 shelving b0', S.Process(1), 1.53512485958697, 1e-12);
  H.Clear; Check('highpass clear', H.Process(0), 0, 0);
  L:=Default(TLoudnessMeter); L.Init;
  SetLength(A, 1, N);
  for i:=0 to N-1 do A[0][i]:=0.1*Sin(2*Pi*1000*i/Rate);
  L.Process(A,N,Rate); Mono:=L.IntegratedLoudness;
  Check('1kHz mono LUFS', Mono, -23.0036, 0.01);
  Check('sine RMS dBFS', L.Rms(0), -23.01029995664, 1e-9);
  Check('sine crest', L.CrestFactor(0), 3.01029995664, 1e-9);
  Check('constant tone LRA', L.LoudnessRange, 0, 0.01);
  Check('negative momentary time', L.MomentaryLoudness(-100), NegInfinity, 0);
  Check('negative short term time', L.ShortTermLoudness(-100), NegInfinity, 0);
  SetLength(A,2,N);
  for i:=0 to N-1 do A[1][i]:=A[0][i];
  L.Process(A,N,Rate);
  Check('stereo energy sum', L.IntegratedLoudness-Mono, 3.01029995664, 1e-9);
  SetLength(A,6,N);
  for ch:=0 to 5 do for i:=0 to N-1 do A[ch][i]:=0;
  for i:=0 to N-1 do A[3][i]:=0.1*Sin(2*Pi*1000*i/Rate);
  L.Process(A,N,Rate);
  Check('LFE excluded', L.IntegratedLoudness, NegInfinity, 0);
  for i:=0 to N-1 do begin A[3][i]:=0; A[4][i]:=0.1*Sin(2*Pi*1000*i/Rate); end;
  L.Process(A,N,Rate);
  Check('surround energy weight', L.IntegratedLoudness-Mono, 10*Log10(1.41), 1e-9);
  // Exactly one valid 3 s window has identical P10 and P95.
  SetLength(A,1,Rate*3);
  for i:=0 to High(A[0]) do A[0][i]:=0.1*Sin(2*Pi*997*i/Rate);
  L.Process(A,Length(A[0]),Rate);
  Check('997 Hz calibration',L.IntegratedLoudness,-23.01,0.005);
  Check('single short term window LRA',L.LoudnessRange,0,0);
  SetLength(A,2,Rate*3);
  for i:=0 to High(A[1]) do A[1][i]:=0;
  L.Process(A,Length(A[0]),Rate);
  Check('silent channel does not poison crest',L.CrestFactor,L.CrestFactor(0),1e-12);
  for i:=0 to High(A[0]) do A[0][i]:=0;
  L.Process(A,Length(A[0]),Rate);
  Check('silence PLR undefined',L.PeakToLoudnessRatio,NegInfinity,0);
  Check('silence crest undefined',L.CrestFactor,NegInfinity,0);
  // Tech 3341 section 2.9 / Tech 3343 section 8.1 electrical alignment.
  for i:=0 to High(A[0]) do begin
    A[0][i]:=Power(10.0,-18.0/20.0)*Sin(2*Pi*1000*i/Rate);
    A[1][i]:=A[0][i];
  end;
  L.Process(A,Length(A[0]),Rate);
  Check('stereo -18 dBFS alignment',L.IntegratedLoudness,-18,0.1);
  // BS.1770 sums channel energies, not waveforms: antiphase cannot cancel.
  Mono:=L.IntegratedLoudness;
  for i:=0 to High(A[1]) do A[1][i]:=-A[1][i];
  L.Process(A,Length(A[0]),Rate);
  Check('stereo phase independence',L.IntegratedLoudness,Mono,1e-10);
  SetLength(A,1,0); L.Process(A,0,Rate);
  Check('empty loudness resets', L.IntegratedLoudness, NegInfinity, 0);
  Check('empty M maximum resets',L.MaxMomentaryLoudness,NegInfinity,0);
  Check('empty S maximum resets',L.MaxShortTermLoudness,NegInfinity,0);
  Check('empty crest undefined',L.CrestFactor,NegInfinity,0);
  Check('empty PLR undefined',L.PeakToLoudnessRatio,NegInfinity,0);
  // Public entry points reject unsupported rates, including standalone filters.
  for i := 0 to 4 do
  begin
    case i of
      0: TestRate := 0;
      1: TestRate := -1;
      2: TestRate := 7999;
      3: TestRate := 1000;
      else TestRate := High(longint);
    end;
    Raised := False;
    try H.Init(TestRate); except on E: EArgumentException do Raised := True; end;
    Check('highpass rejects rate '+IntToStr(TestRate), Ord(Raised), 1, 0);
    Raised := False;
    try S.Init(TestRate); except on E: EArgumentException do Raised := True; end;
    Check('shelving rejects rate '+IntToStr(TestRate), Ord(Raised), 1, 0);
    Raised := False;
    try K.Init(TestRate); except on E: EArgumentException do Raised := True; end;
    Check('K filter rejects rate '+IntToStr(TestRate), Ord(Raised), 1, 0);
    SetLength(A, 1, 32);
    Raised := False;
    try L.Process(A, 32, TestRate); except on E: EArgumentException do Raised := True; end;
    Check('meter rejects rate '+IntToStr(TestRate), Ord(Raised), 1, 0);
    Raised := False;
    try M.Process(A[0], 32, TestRate); except on E: EArgumentException do Raised := True; end;
    Check('metrics rejects rate '+IntToStr(TestRate), Ord(Raised), 1, 0);
  end;
  Raised := False;
  try M.Process(A[0], 33, Rate); except on E: EArgumentException do Raised := True; end;
  Check('standalone metrics rejects truncated vector', Ord(Raised), 1, 0);
  Check('invalid metrics reset energy', M.Rms2, 0, 0);
  for ch := 3 to 8 do
    if not (ch in [5, 6]) then
    begin
      SetLength(A, ch, 32);
      Raised := False;
      try L.Process(A, 32, Rate); except on E: EArgumentException do Raised := True; end;
      Check('reject unsupported layout '+IntToStr(ch), Ord(Raised), 1, 0);
      Check('invalid layout resets loudness', L.IntegratedLoudness, NegInfinity, 0);
      Check('invalid layout resets RMS', L.Rms, NegInfinity, 0);
    end;
  Counter := TTickCounter.Create;
  try
    L.Init(@Counter.Tick);
    for ch := 1 to 6 do
      if ch in [1, 2, 5, 6] then
      begin
        SetLength(A, 0); SetLength(A, ch, 3200);
        Counter.Count := 0;
        L.Process(A, 3200, 8000);
        Check('tick count layout '+IntToStr(ch), Counter.Count,
          L.EstimatedTicks(ch, 3200, 8000), 0);
        Check('8k silence', L.IntegratedLoudness, NegInfinity, 0);
      end;
    Counter.Count := 0;
    L.Process(A, 0, Rate);
    Check('empty input ticks', Counter.Count, L.EstimatedTicks(6, 0, Rate), 0);
    Raised := False;
    try L.EstimatedTicks(8, 3200, Rate); except on E: EArgumentException do Raised := True; end;
    Check('tick estimate rejects layout', Ord(Raised), 1, 0);
    Raised := False;
    try L.EstimatedTicks(1, 3200, 0); except on E: EArgumentException do Raised := True; end;
    Check('tick estimate rejects rate', Ord(Raised), 1, 0);
  finally
    L.Init; // Detach the callback before destroying its target.
    Counter.Free;
  end;
  L.Finalize;
end;

end.
