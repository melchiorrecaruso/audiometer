unit GatingRegression;
{$mode objfpc}{$H+}

interface

procedure GatingTests;

implementation

uses Math, Common, Loudness, RegressionSupport;

procedure GatingTests;
const Rate=48000; N=Rate*40;
var A: TDoubleMatrix; L: TLoudnessMeter; i: integer;
  Level, Reference: double;
begin
  L:=Default(TLoudnessMeter); L.Init;
  SetLength(A,1,N);
  for i:=0 to N-1 do begin
    if i<N div 2 then Level:=0.1 else Level:=0.01;
    A[0][i]:=Level*Sin(2*Pi*1000*i/Rate);
  end;
  L.Process(A,N,Rate);
  Check('two plateau LRA 20 LU', L.LoudnessRange, 20, 0.05);
  // Relative integrated gate must remove the -40 LU plateau.
  for i:=N div 2 to N-1 do A[0][i]:=0.001*Sin(2*Pi*1000*i/Rate);
  L.Process(A,N,Rate);
  Check('relative integrated gate', L.IntegratedLoudness, -23.0036, 0.05);
  // An audible mathematical signal below the absolute gate still has finite M.
  for i:=0 to N-1 do A[0][i]:=1e-5*Sin(2*Pi*1000*i/Rate);
  L.Process(A,N,Rate);
  Check('absolute integrated gate',L.IntegratedLoudness,NegInfinity,0);
  Check('quiet momentary remains finite',L.MomentaryLoudness(1000),-103.0036,0.01);
  Reference:=L.MomentaryLoudness(1000);
  Check('quiet short term',L.ShortTermLoudness(3500),Reference,0.001);
  // Tech 3341: incomplete final gating blocks must be discarded.
  SetLength(A,1,Rate);
  for i:=0 to Rate-1 do A[0][i]:=0.1*Sin(2*Pi*1000*i/Rate);
  L.Process(A,Rate,Rate); Reference:=L.IntegratedLoudness;
  SetLength(A,1,Rate+Rate div 20);
  for i:=Rate to High(A[0]) do A[0][i]:=0.9*Sin(2*Pi*1000*i/Rate);
  L.Process(A,Length(A[0]),Rate);
  Check('incomplete 50 ms loud tail excluded from I',L.IntegratedLoudness,Reference,1e-12);
  // Tech 3342: whole-program repetition and gain changes above the gate.
  SetLength(A,1,N*2);
  for i:=0 to N*2-1 do begin
    if (i mod N)<N div 2 then Level:=0.1 else Level:=0.01;
    A[0][i]:=Level*Sin(2*Pi*1000*i/Rate);
  end;
  L.Process(A,N*2,Rate);
  Check('LRA repeated programme',L.LoudnessRange,20,0.05);
  Reference:=L.LoudnessRange;
  for i:=0 to High(A[0]) do A[0][i]:=A[0][i]*0.5;
  L.Process(A,N*2,Rate);
  Check('LRA gain invariance above absolute gate',L.LoudnessRange,Reference,1e-9);
  L.Finalize;
end;

end.
