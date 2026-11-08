unit SlidingMaxRegression;
{$mode objfpc}{$H+}

interface

procedure SlidingMaxTests;

implementation

uses SysUtils, Math, Common, Loudness, RegressionSupport;

procedure SlidingMaxTests;
const Rate=48000; N=Rate*4;
var A: TDoubleMatrix; L: TLoudnessMeter; i,j,Width: integer;
  X,Y,Sum,MaxEnergy: double; Energy,Prefix: TDoubleVector;
  // Canonical BS.1770 48 kHz biquads, not coefficients from the implementation.
  X1,X2,Y1,Y2,H1,H2,Z1,Z2: double;
begin
  SetLength(A,1,N); SetLength(Energy,N); SetLength(Prefix,N+1);
  X1:=0; X2:=0; Y1:=0; Y2:=0; H1:=0; H2:=0; Z1:=0; Z2:=0;
  for i:=0 to N-1 do begin
    // Off-grid burst: a 100 ms grid alone misses the true maximum.
    if (i>=12345) and (i<12345+Rate div 2) then
      X:=0.2*Sin(2*Pi*1000*i/Rate) else X:=0;
    A[0][i]:=X;
    Y:=1.53512485958697*X-2.69169618940638*X1+1.19839281085285*X2
      +1.69065929318241*Y1-0.73248077421585*Y2;
    X2:=X1; X1:=X; Y2:=Y1; Y1:=Y;
    X:=Y-2*H1+H2+1.99004745483398*Z1-0.99007225036621*Z2;
    H2:=H1; H1:=Y; Z2:=Z1; Z1:=X;
    Energy[i]:=Sqr(X); Prefix[i+1]:=Prefix[i]+Energy[i];
  end;
  L:=Default(TLoudnessMeter); L.Init; L.Process(A,N,Rate);
  for j:=0 to 1 do begin
    if j=0 then Width:=Rate*4 div 10 else Width:=Rate*3;
    MaxEnergy:=0;
    for i:=Width to N do begin
      Sum:=(Prefix[i]-Prefix[i-Width])/Width;
      MaxEnergy:=Max(MaxEnergy,Sum);
    end;
    if j=0 then Y:=L.MaxMomentaryLoudness else Y:=L.MaxShortTermLoudness;
    Check('sample accurate maximum '+IntToStr(j),Y,EnergyToLufs(MaxEnergy),1e-7);
  end;
  // Timestamp queries must refer to windows ending at that time, including
  // the final window at EOF, not to the following 100 ms grid cell.
  Check('M before full window',L.MomentaryLoudness(399),NegInfinity,0);
  Check('S before full window',L.ShortTermLoudness(2999),NegInfinity,0);
  for j:=4 to 40 do begin
    i:=j*Rate div 10; Width:=Rate*4 div 10;
    Sum:=(Prefix[i]-Prefix[i-Width])/Width;
    if Sum>1e-10 then
      Check('M timestamp '+IntToStr(j*100),L.MomentaryLoudness(j*100),
        -0.691+10*Log10(Sum),1e-7);
  end;
  for j:=30 to 40 do begin
    i:=j*Rate div 10; Width:=Rate*3;
    Sum:=(Prefix[i]-Prefix[i-Width])/Width;
    if Sum>1e-10 then
      Check('S timestamp '+IntToStr(j*100),L.ShortTermLoudness(j*100),
        -0.691+10*Log10(Sum),1e-7);
  end;
  L.Finalize;
end;

end.
