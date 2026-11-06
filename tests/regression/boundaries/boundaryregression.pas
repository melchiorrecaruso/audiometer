unit BoundaryRegression;
{$mode objfpc}{$H+}

interface

procedure BoundaryTests;

implementation

uses SysUtils, Math, Common, DynamicRange, Loudness, Spectrum, RegressionSupport;

procedure BoundaryTests;
var S: TSpectrums; D: TDynamicRangeMeter; L: TLoudnessMeter;
  A: TDoubleMatrix; Raised: boolean; Rate,i,p: integer;
  K: TKWeightingFilter; Energy: TDoubleVector; Sum: double;
begin
  S:=Default(TSpectrums); D:=Default(TDynamicRangeMeter);
  L:=Default(TLoudnessMeter);
  for i:=0 to 2 do begin
    Raised:=False;
    try
      case i of 0: S.Init(1,1); 1: S.Init(2,1); 2: S.Init(30,1); end;
    except on E: EArgumentException do Raised:=True; end;
    Check('invalid FFT size '+IntToStr(i),Ord(Raised),1,0);
  end;
  S.Init(32,16); D.Init; L.Init;
  SetLength(A,1,32);
  Raised:=False;
  try S.Process(A,33); except on E: EArgumentException do Raised:=True; end;
  Check('FFT rejects truncated channel',Ord(Raised),1,0);
  Raised:=False;
  try D.Process(A,33,48000); except on E: EArgumentException do Raised:=True; end;
  Check('DR rejects truncated channel',Ord(Raised),1,0);
  Raised:=False;
  try L.Process(A,33,48000); except on E: EArgumentException do Raised:=True; end;
  Check('loudness rejects truncated channel',Ord(Raised),1,0);
  Raised:=False;
  try D.Process(A,32,0); except on E: EArgumentException do Raised:=True; end;
  Check('DR rejects zero rate',Ord(Raised),1,0);
  Raised:=False;
  try L.Process(A,32,0); except on E: EArgumentException do Raised:=True; end;
  Check('loudness rejects zero rate',Ord(Raised),1,0);
  Check('44.1k DR tick estimate',D.EstimatedTicks(2,264600,44100),2,0);
  for i:=0 to 1 do begin
    if i=0 then Rate:=44100 else Rate:=96000;
    SetLength(A,1,Rate*4);
    for p:=0 to Rate*4-1 do A[0][p]:=0.1*Sin(2*Pi*1000*p/Rate);
    L.Process(A,Rate*4,Rate);
    Check('1kHz LUFS rate '+IntToStr(Rate),L.IntegratedLoudness,-23.0036,0.03);
  end;
  // Isolate grid arithmetic from K-filter accuracy: compare the window at
  // 4 seconds with an explicit sum over the SAME filtered stream. At 11025 Hz
  // truncating each 100 ms cell loses 20 samples over this interval.
  Rate:=11025; SetLength(A,1,Rate*4); SetLength(Energy,Rate*4);
  K.Init(Rate);
  for p:=0 to High(A[0]) do begin
    if p<Rate*3 then A[0][p]:=0.01*Sin(2*Pi*997*p/Rate)
    else A[0][p]:=0.2*Sin(2*Pi*997*p/Rate);
    Energy[p]:=Sqr(K.Process(A[0][p]));
  end;
  L.Process(A,Length(A[0]),Rate);
  Sum:=0;
  for p:=Rate*4-4410 to Rate*4-1 do Sum:=Sum+Energy[p];
  Check('11025 Hz exact M grid at EOF',L.MomentaryLoudness(4000),
    -0.691+10*Log10(Sum/4410),1e-9);
  Sum:=0;
  for p:=Rate to Rate*4-1 do Sum:=Sum+Energy[p];
  Check('11025 Hz exact S grid at EOF',L.ShortTermLoudness(4000),
    -0.691+10*Log10(Sum/(Rate*3)),1e-9);
  S.Finalize; D.Finalize; L.Finalize;
end;

end.
