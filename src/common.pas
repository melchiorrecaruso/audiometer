unit common;

{$mode objfpc}{$h+}

interface

uses
  classes, fgl, sysutils;

type
  tsample   = double;
  tsamples  = array of tsample;
  tchannels = array of tsamples;

  tarrayofdouble = array of double;
  tlistofdouble  = specialize tfpglist<double>;

function decibel(const asample: double): double;
function compare(const sample1, sample2: double): longint;
function peak(const asamples: tsamples; aindex, acount: longint): double;
function rms2(const asamples: tsamples; aindex, acount: longint): double;
function rms(const asamples: tsamples; aindex, acount: longint): double;

implementation

uses
  math;

function decibel(const asample: double): double;
begin
  if asample > 1e-10 then
    result := 20*log10(asample)
  else
    result := neginfinity;
end;

function compare(const sample1, sample2: double): longint;
begin
  if sample2 > sample1 then
    result := +1
  else
    if sample2 < sample1 then
      result := -1
    else
      result := 0;
end;

function rms2(const asamples: tsamples; aindex, acount: longint): double;
var
  i: integer;
begin
  result := 0.0;
  if acount > 0 then
  begin
    for i := aindex to (aindex + acount) -1 do
    begin
      result := result + sqr(asamples[i]);
    end;
    result := result / acount;
  end;
end;

function rms(const asamples: tsamples; aindex, acount: longint): double;
begin
  result := sqrt(rms2(asamples, aindex, acount));
end;

function peak(const asamples: tsamples; aindex, acount: longint): double;
var
  i: integer;
begin
  result := 0.0;
  for i := aindex to (aindex + acount) -1 do
  begin
    result := max(result, abs(asamples[i]));
  end;
end;

end.

