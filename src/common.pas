unit Common;

{$mode objfpc}{$h+}

interface

uses
  classes, fgl, sysutils;

type
  TSample   = double;
  TSamples  = array of TSample;
  TChannels = array of TSamples;

  tarrayofdouble = array of double;
  tarrayofarrayofdouble = array of tarrayofdouble;

  tlistofdouble  = specialize tfpglist<double>;

function Decibel(const AAmplitude: double): double;
function EnergyToDecibel(const AEnergy: double): double;

function Compare(const AValue1, AValue2: double): longint;
function Peak(const ASamples: TSamples; AIndex, ACount: longint): double;
function Rms2(const ASamples: TSamples; AIndex, ACount: longint): double;
function Rms(const ASamples: TSamples; AIndex, ACount: longint): double;

implementation

uses
  math;

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

function Compare(const AValue1, AValue2: double): longint;
begin
  if AValue2 > AValue1 then
    result := +1
  else
    if AValue2 < AValue1 then
      result := -1
    else
      result := 0;
end;

function Rms2(const ASamples: TSamples; AIndex, ACount: longint): double;
var
  i: longint;
begin
  result := 0;
  if ACount > 0 then
  begin
    for i := AIndex to (AIndex + ACount) -1 do
    begin
      result := result + Sqr(ASamples[i]);
    end;
    result := result / ACount;
  end;
end;

function Rms(const ASamples: TSamples; AIndex, ACount: longint): double;
begin
  result := sqrt(rms2(ASamples, AIndex, ACount));
end;

function Peak(const ASamples: TSamples; AIndex, ACount: longint): double;
var
  i: integer;
begin
  result := 0.0;
  for i := AIndex to (AIndex + ACount) -1 do
  begin
    result := Max(result, Abs(ASamples[i]));
  end;
end;

end.

