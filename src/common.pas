unit common;

{$mode objfpc}{$h+}

interface

uses
  classes, fgl, sysutils;

type
  tchannel  = array of double;
  tchannels = array of tchannel;

  arrayofdouble = array of double;

  tdoublelist = specialize tfpglist<double>;


function compare(const value1, value2: double): longint;
function decibel(const asample: double): double;

implementation

uses
  math;

function compare(const value1, value2: double): longint;
begin
  if value2 > value1 then
    result := +1
  else
    if value2 < value1 then
      result := -1
    else
      result := 0;
end;

function decibel(const asample: double): double;
begin
  if asample > 1e-10 then
    result := 20*log10(asample)
  else
    result := neginfinity;
end;

end.

