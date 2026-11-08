unit CommonRegression;
{$mode objfpc}{$H+}

interface

procedure CommonTests;

implementation

uses Math, Common, RegressionSupport;

procedure CommonTests;
var V: TDoubleVector; i, r, phase: integer;
  Rate: integer; Envelope, TP: double;
begin
  Check('amplitude dB', Decibel(0.5), -6.020599913279624, 1e-12);
  Check('energy dB', EnergyToDecibel(0.25), -6.020599913279624, 1e-12);
  Check('quiet energy', EnergyToDecibel(1e-12), -120, 1e-10);
  Check('quiet amplitude', Decibel(1e-12), -240, 1e-10);
  Check('quiet LUFS', EnergyToLufs(1e-12), -120.691, 1e-10);
  Check('quiet LU', EnergyToLu(1e-12), -97.691, 1e-10);
  Check('silence dB', Decibel(0), NegInfinity, 0);
  Check('silence energy', EnergyToDecibel(0), NegInfinity, 0);
  Check('crest scale invariance', CrestFactor(2e-6, 1e-12), 2, 1e-12);
  SetLength(V, 4); V[0] := -1; V[1] := 0; V[2] := 0.5; V[3] := -0.5;
  Check('peak', Common.Peak(@V[0], 4), 1, 0);
  Check('mean square', Common.Rms2(@V[0], 4), 0.375, 1e-15);
  Check('RMS', Common.Rms(@V[0], 4), Sqrt(0.375), 1e-15);
  Check('empty RMS', Common.Rms2(nil, 0), 0, 0);
  Check('empty true peak', Common.TruePeak(nil, 0, 48000), 0, 0);
  SetLength(V, 0); QuickSort(V, 0, -1);
  SetLength(V, 5); V[0]:=3; V[1]:=-1; V[2]:=3; V[3]:=0; V[4]:=2;
  QuickSort(V, 0, 4);
  for i:=1 to 4 do Check('sorted values', Ord(V[i]>=V[i-1]), 1, 0);
  // fs/4 sine, 45 degrees: all samples miss the reconstructed peak.
  SetLength(V, 4096);
  for i:=0 to High(V) do V[i]:=0.5*Sin(Pi*i/2+Pi/4);
  Check('true peak 48k', Decibel(Common.TruePeak(@V[0], Length(V), 48000)),
    -6.020599913279624, 0.4);
  Check('true peak 96k', Decibel(Common.TruePeak(@V[0], Length(V), 96000)),
    -6.020599913279624, 0.4);
  // Analytic band-limited tones with fades: amplitude 0.5 is the reference,
  // independent of the implementation's FIR. Exercise phase and sample rate.
  for r:=0 to 2 do begin
    case r of 0: Rate:=44100; 1: Rate:=48000; else Rate:=96000; end;
    SetLength(V,Rate div 5);
    for phase:=0 to 7 do begin
      for i:=0 to High(V) do begin
        Envelope:=Min(1.0,Min(i,High(V)-i)/(Rate*0.01));
        V[i]:=0.5*Envelope*Sin(2*Pi*12000*i/Rate+phase*Pi/8);
      end;
      TP:=Common.TruePeak(@V[0],Length(V),Rate);
      Check('12 kHz true peak phase/rate sweep',20*Log10(TP),
        -6.020599913279624,0.4);
      Check('true peak >= sample peak',Ord(TP>=Common.Peak(@V[0],Length(V))),1,0);
    end;
  end;
end;

end.
