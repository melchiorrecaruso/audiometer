unit DynamicRangeRegression;
{$mode objfpc}{$H+}

interface

procedure DRTests;

implementation

uses SysUtils, Math, Common, DynamicRange, RegressionSupport;

procedure DRTests;
var D: TDynamicRangeMeter; A: TDoubleMatrix; i: integer;
  ExpectedLeft, ExpectedRight: double;
  Raised: boolean;
begin
  D := Default(TDynamicRangeMeter); D.Init;
  // Low sample rate is sufficient for these purely arithmetic block tests.
  SetLength(A, 1, 4);
  for i:=0 to 3 do A[0][i]:=0.5;
  D.Process(A, 4, 10);
  Check('DR short channel count', D.ChannelCount, 1, 0);
  Check('DR short block count', D.BlockCount, 1, 0);
  if D.ChannelCount>0 then Check('DR short energy', D.Rms2, 0.25, 1e-15);
  SetLength(A, 1, 31);
  for i:=0 to 29 do A[0][i]:=0.5;
  A[0][30]:=1;
  D.Process(A, 31, 10);
  Check('DR weighted tail energy', D.Rms2, double(8.5)/31, 1e-15);
  Check('DR tail peak', D.Peak, 1, 0);
  // Five blocks: top 20% energy and second highest peak determine DR.
  SetLength(A, 1, 150);
  for i:=0 to 149 do A[0][i]:=0;
  A[0][0]:=1; A[0][30]:=0.5; A[0][60]:=0.25;
  D.Process(A, 150, 10);
  Check('DR top energy / second peak', D.DR(0),
    20*Log10(0.5/Sqrt(2/30)), 1e-12);
  for i:=0 to 149 do A[0][i]:=0;
  D.Process(A, 150, 10);
  Check('DR silence', D.DR, 0, 0);
  // Actual 44.1 kHz boundary, not only the progress estimate.
  SetLength(A,1,132481);
  for i:=0 to High(A[0]) do A[0][i]:=0;
  A[0][132479]:=0.5; A[0][132480]:=1;
  D.Process(A,Length(A[0]),44100);
  Check('44.1k first block peak',D.Peak(0,0),0.5,0);
  Check('44.1k tail peak',D.Peak(0,1),1,0);
  Check('44.1k exact block energy',D.Rms2(0,0),double(0.25)/132480,1e-15);
  // Ten blocks: the upper 20% consists of TWO energies. Channels have
  // deliberately different crest ratios; average dB only at the last step.
  SetLength(A,0); SetLength(A,2,300);
  A[0][0]:=1; A[0][30]:=0.5;
  A[1][0]:=1; A[1][30]:=0.8;
  ExpectedLeft:=20*Log10(0.5/Sqrt(double(1.25)/30));
  ExpectedRight:=20*Log10(0.8/Sqrt(double(1.64)/30));
  D.Process(A,300,10);
  Check('DR upper two energies left',D.DR(0),ExpectedLeft,1e-12);
  Check('DR upper two energies right',D.DR(1),ExpectedRight,1e-12);
  Check('DR channel mean then rounding',D.DR,Floor((ExpectedLeft+ExpectedRight)/2+0.5),0);
  // Invalid input must leave an empty, readable meter, even after success.
  Raised := False;
  try D.Process(A, 300, 0);
  except on E: EArgumentException do Raised := True; end;
  Check('DR rejects invalid rate', Ord(Raised), 1, 0);
  Check('DR channels after invalid rate', D.ChannelCount, 0, 0);
  Check('DR after invalid rate', D.DR, 0, 0);
  D.Process(A, 300, 10);
  Raised := False;
  try D.Process(A, 301, 10);
  except on E: EArgumentException do Raised := True; end;
  Check('DR rejects short channel', Ord(Raised), 1, 0);
  Check('DR channels after short channel', D.ChannelCount, 0, 0);
  Check('DR blocks after short channel', D.BlockCount, 0, 0);
  Check('DR after short channel', D.DR, 0, 0);
  Check('DR RMS after short channel', D.Rms, 0, 0);
  Check('DR peak after short channel', D.Peak, 0, 0);
  D.Process(A, 300, 10);
  Check('DR reuse after validation failure', D.ChannelCount, 2, 0);
  Check('DR ticks negative channels', D.EstimatedTicks(-1, 300, 10), 0, 0);
  Check('DR ticks zero rate', D.EstimatedTicks(2, 300, 0), 0, 0);
  Check('DR ticks normal', D.EstimatedTicks(2, 301, 10), 20, 0);
  Check('DR ticks 44.1k boundary', D.EstimatedTicks(2, 132480, 44100), 2, 0);
  Check('DR ticks largest result', D.EstimatedTicks(High(longint), 3, 1), High(longint), 0);
  Raised := False;
  try D.EstimatedTicks(1, 300, High(longint));
  except on E: EArgumentException do Raised := True; end;
  Check('DR ticks rejects excessive rate', Ord(Raised), 1, 0);
  Raised := False;
  try D.EstimatedTicks(High(longint), 6, 1);
  except on E: EArgumentException do Raised := True; end;
  Check('DR ticks rejects overflow', Ord(Raised), 1, 0);
  // The tail intentionally has full block weight in DR, not in global RMS.
  SetLength(A, 0); SetLength(A, 1, 31);
  for i := 0 to 29 do A[0][i] := 0.5;
  A[0][30] := 1;
  D.Process(A, 31, 10);
  Check('DR one-sample tail convention', D.DR(0), 20*Log10(0.5/Sqrt(2)), 1e-12);
  D.Finalize;
end;

end.
