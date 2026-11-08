unit SpectrumRegression;
{$mode objfpc}{$H+}

interface

procedure SpectrumTests;

implementation

uses SysUtils, Math, Common, Spectrum, RegressionSupport;

procedure SpectrumTests;
const N=32; Hop=16; Count=64;
var S: TSpectrums; A: TDoubleMatrix; i,k,w: integer;
  Re,Im,Win,SumWin,Mag,Energy,Peak: double;
  Raised: boolean;
  Ticks: longint;
begin
  S:=Default(TSpectrums); S.Init(N,Hop);
  SetLength(A,2,Count);
  for i:=0 to Count-1 do begin
    A[0][i]:=0.3+0.2*Sin(2*Pi*5*i/N)+0.1*Cos(Pi*i);
    A[1][i]:=0.4*Cos(2*Pi*7*i/N);
  end;
  S.Process(A,Count);
  Check('spectrum window count',S.WindowCount,3,0);
  // Direct O(N^2) DFT oracle: tests packed pair, odd leftover, DC, Nyquist,
  // and channel/window energy aggregation without calling the FFT library.
  for k:=0 to N div 2 do begin
    Energy:=0; Peak:=0;
    for w:=0 to 5 do begin
      Re:=0; Im:=0; SumWin:=0;
      for i:=0 to N-1 do begin
        Win:=0.5-0.5*Cos(2*Pi*i/(N-1)); SumWin:=SumWin+Win;
        Re:=Re+A[w div 3][(w mod 3)*Hop+i]*Win*Cos(2*Pi*k*i/N);
        Im:=Im-A[w div 3][(w mod 3)*Hop+i]*Win*Sin(2*Pi*k*i/N);
      end;
      Mag:=Sqrt(Sqr(Re)+Sqr(Im))/SumWin;
      if (k>0) and (k<N div 2) then Mag:=2*Mag;
      Energy:=Energy+Sqr(Mag); Peak:=Max(Peak,Mag);
    end;
    Check('DFT RMS bin '+IntToStr(k), S.SpectrumRms(k), Sqrt(Energy/6), 1e-12);
    Check('DFT peak bin '+IntToStr(k), S.SpectrumPeak(k), Peak, 1e-12);
  end;
  S.Process(A,1);
  Check('short input preserves window',S.WindowSize,N,0);
  S.Process(A,Count);
  Check('reuse after short input',S.WindowCount,3,0);
  S.Process(nil,Count);
  Check('empty spectrum',S.WindowCount,0,0);
  // Processing a full-size silence after nonzero input must reset every bin,
  // not merely the metadata (same allocation sizes on purpose).
  S.Process(A,Count);
  for w:=0 to 1 do for i:=0 to Count-1 do A[w][i]:=0;
  S.Process(A,Count);
  for k:=0 to S.OutBins-1 do begin
    Check('reused spectrum RMS silence',S.SpectrumRms(k),0,0);
    Check('reused spectrum peak silence',S.SpectrumPeak(k),0,0);
  end;
  // Invalid reinitialization must preserve both setup and existing results.
  A[0][1]:=0.25;
  S.Process(A,Count);
  Peak:=S.SpectrumPeak(1);
  Raised:=False;
  try S.Init(30,1); except on E: EArgumentException do Raised:=True; end;
  Check('invalid reinit raises',Ord(Raised),1,0);
  Check('invalid reinit preserves window',S.WindowSize,N,0);
  Check('invalid reinit preserves hop',S.HopSize,Hop,0);
  Check('invalid reinit preserves results',S.SpectrumPeak(1),Peak,0);
  S.Process(A,Count);
  Check('reuse after invalid reinit',S.WindowCount,3,0);

  S.Init(4,1);
  Check('minimum FFT size accepted',S.WindowSize,4,0);
  Check('large valid tick estimate',S.EstimatedTicks(1,High(longint)),
    High(longint)-3,0);
  Raised:=False;
  try Ticks:=S.EstimatedTicks(2,High(longint));
  except on E: ERangeError do Raised:=True; end;
  Check('tick overflow rejected',Ord(Raised),1,0);
  Raised:=False;
  try Ticks:=S.EstimatedTicks(-1,Count);
  except on E: EArgumentException do Raised:=True; end;
  Check('negative channel count rejected',Ord(Raised),1,0);
  Check('zero channels estimate',S.EstimatedTicks(0,Count),0,0);
  S.Finalize;
  Check('finalized tick estimate',S.EstimatedTicks(2,Count),0,0);
  Raised:=False;
  try S.Process(A,Count); except on E: EInvalidOpException do Raised:=True; end;
  Check('process after finalize rejected',Ord(Raised),1,0);
  S:=Default(TSpectrums);
  Raised:=False;
  try S.Process(nil,0); except on E: EInvalidOpException do Raised:=True; end;
  Check('process before init rejected',Ord(Raised),1,0);
  S.Init(0,0);
  Check('default window preserved',S.WindowSize,DEFAULTWINDOWSIZE,0);
  Check('default hop preserved',S.HopSize,DEFAULTHOPSIZE,0);
  S.Finalize;
end;

end.
