{
  Description: Sound utils routines.

  Copyright (C) 2025 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

unit soundutils;

{$mode objfpc}{$h+}
{$modeswitch advancedrecords}

interface

uses
  classes, fgl, sysutils;

type
  arrayofdouble = array of double;

  tshelvingfilter = record
  private
    a1, a2: double;
    b0, b1, b2: double;
    x1, x2: double;
    y1, y2: double;
  public
    procedure init(samplerate: longint);
    function run(const sample: double): double;
  end;

  thighpassfilter = record
  private
    a1, a2: double;
    b0, b1, b2: double;
    x1, x2: double;
    y1, y2: double;
  public
    procedure init(samplerate: longint);
    function run(const sample: double): double;
  end;

  tkweightingfilter = record
  private
    highpassfilter: thighpassfilter;
    shelvingfilter: tshelvingfilter;
  public
    procedure init(samplerate: longint);
    procedure run(var samples: arrayofdouble);
    function run(const sample: double): double;
  end;

  tdoublelist  = specialize tfpglist<double>;


function Decibel(const asample: double): double;
function Peak(const asamples: arrayofdouble; aindex, acount: longint): double;
function Rms2(const asamples: arrayofdouble; aindex, acount: longint): double;
function Rms2ToLuFS(const aenergy: double): double;

function TruePeak(const asamples: arrayofdouble; aoversample, ataps: longint): double;
function LuFS(const asamples: arrayofdouble; asamplerate: longint): double;
function Lu(const asamples: arrayofdouble; asamplerate: longint): double;
function LRA(const asamples: arrayofdouble; asamplerate: longint): double;
function PLR(const sample, energy: double): double;

function compare(const value1, value2: double): longint;

implementation

uses
  math;

function Decibel(const asample: double): double;
begin
  if asample > 1e-10 then
    result := 20*log10(asample)
  else
    result := neginfinity;
end;

function rms2tolufs(const aenergy: double): double;
begin
  if aenergy > 1e-10 then
    result := 10*log10(aenergy)
  else
    result := neginfinity;
end;

function rms2(const asamples: arrayofdouble; aindex, acount: longint): double;
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

function peak(const asamples: arrayofdouble; aindex, acount: longint): double;
var
  i: integer;
begin
  result := 0.0;
  for i := aindex to (aindex + acount) -1 do
  begin
    result := max(result, abs(asamples[i]));
  end;
end;

function PLR(const sample, energy: double): double;
begin
  result := Decibel(sample) - rms2tolufs(energy);
end;

function TruePeak(const asamples: arrayofdouble; aoversample, ataps: longint): double;
var
  fc, x: double;
  i, phase, tap: longint;
  coeff, sum: double;
  coeffs: array of array of double = nil;
begin
  setlength(coeffs, aoversample);
  for i := low(coeffs) to high(coeffs) do
    setlength(coeffs[i], ataps);

  fc := 0.5 / aoversample;
  // generate fir coeffs
  for phase := 0 to aoversample -1 do
  begin
    sum := 0.0;
    for tap := 0 to ataps -1 do
    begin
      x := (tap - (ataps / 2)) - phase/aoversample;
      // sinc
      if abs(x) > 1e-10 then
        coeff := 2.0 * fc * sin(pi * x) / (pi * x)
      else
        coeff := 2.0 * fc;
      // finestra di hamming
      coeff := coeff * (0.54 - 0.46 * cos(2.0 * pi * tap / (ataps - 1)));
      coeffs[phase, tap] := coeff;
      sum := sum + coeff;
    end;
    // normalize coeffs
    for tap := 0 to ataps -1 do
      coeffs[phase, tap] := coeffs[phase, tap] / sum;
  end;

  result := 0;
  for i := (low(asamples) + ataps div 2) to (high(asamples) - ataps div 2) do
  begin
    for phase := 0 to aoversample -1 do
    begin
      sum := 0;
      for tap := 0 to ataps -1 do
        sum := sum + asamples[tap - (ataps div 2) + i] * coeffs[phase, tap];
      result := max(result, abs(sum));
    end;
  end;

  for i := low(coeffs) to high(coeffs) do
    setlength(coeffs[i], 0);
  setlength(coeffs, 0);
end;

function LuFS(const ASamples: arrayofdouble; ASamplerate: integer): double;
const
  blockms = 400;
  stepms  = 100;
  absolutegatelufs = -70.0;
var
  blocksize, stepsize, i: longint;
  energy: double;
  energies: arrayofdouble = nil;
  energycount: integer;
  e0, e1: double;
  e0count, e1count: longint;
begin
  blocksize := (asamplerate * blockms) div 1000;
  stepsize  := (asamplerate * stepms ) div 1000;

  setlength(energies, length(asamples) div stepsize);
  energycount := 0;

  i := 0;
  while (i + blocksize) < length(asamples) do
  begin
    energy := rms2(asamples, i, blocksize);

    if rms2tolufs(energy) > absolutegatelufs then
    begin
      energies[energycount] := energy;
      inc(energycount);
    end;
    inc(i, stepsize);
  end;

  // gating relativo: calcola e0
  e0 := 0.0;
  e0count := 0;
  for i := 0 to energycount - 1 do
  begin
    e0 := e0 + energies[i];
    inc(e0count);
  end;
  if e0count = 0 then exit(neginfinity);
  e0 := e0 / e0count;

  // gating relativo: scarta blocchi < e0 / 10
  e1 := 0.0;
  e1count := 0;
  for i := 0 to energycount - 1 do
    if energies[i] >= (e0 / 10.0) then
    begin
      e1 := e1 + energies[i];
      inc(e1count);
    end;
  if e1count = 0 then exit(neginfinity);
  e1 := e1 / e1count;

  result := rms2tolufs(e1);
end;

function LU(const asamples: arrayofdouble; asamplerate: integer): double;
begin
  result := LuFS(ASamples, ASamplerate) + 23;
end;

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

function percentile(const values: arrayofdouble; p: double): double;
var
  i: longint;
  index: double;
  list: tdoublelist;
begin
  if length(values) = 0 then
    exit(neginfinity);

  list := tdoublelist.create;
  for i := low(values) to high(values) do
  begin
    list.add(values[i]);
  end;
  list.sort(@compare);

  index := p * (list.count - 1);
  i := floor(index);

  if i >= (list.count - 1) then
    result := list[list.count - 1]
  else
    result := list[i] + (index - i) * (list[i + 1] - list[i]);

  list.free;
end;

function lra(const asamples: arrayofdouble; asamplerate: integer): double;
const
  blockms = 3000;
  stepms  = 1000;
  absolutegatelufs = -70.0;
var
  blocksize, stepsize, i: longint;
  energy: double;
  energies: arrayofdouble = nil;
  energycount: integer;
  p95, p10: double;
begin
  blocksize := (asamplerate * blockms) div 1000;
  stepsize  := (asamplerate * stepms ) div 1000;

  setlength(energies, length(asamples) div stepsize);
  energycount := 0;

  i := 0;
  while (i + blocksize) < length(asamples) do
  begin
    energy := rms2(asamples, i, blocksize);

    if rms2tolufs(energy) > absolutegatelufs then
    begin
      energies[energycount] := energy;
      inc(energycount);
    end;
    inc(i, stepsize);
  end;

  if energycount >= 2 then
  begin
    p95 := percentile(energies, 0.95);
    p10 := percentile(energies, 0.10);
    result := p95 - p10;
  end else
    result := 0;
end;

// tshelvingfilter

procedure tshelvingfilter.init(samplerate: longint);
const
  gain = 4.0;
  f0 = 1500.0;
var
  a, w0, alpha, cosw0: double;
begin
  x1 := 0; x2 := 0;
  y1 := 0; y2 := 0;

  a := power(10, gain / 40);
  w0 := 2 * pi * f0 / samplerate;
  cosw0 := cos(w0);
  alpha := sin(w0) / 2 * sqrt((a + 1/a) * (1 / 0.707 - 1) + 2); // q ≈ 0.707

  a1 := -2 *     ((a - 1) + (a + 1) * cosw0);
  a2 :=          ((a + 1) + (a - 1) * cosw0 - 2 * sqrt(a) * alpha);
  b0 :=      a * ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
  b1 := -2 * a * ((a - 1) + (a + 1) * cosw0);
  b2 :=      a * ((a + 1) + (a - 1) * cosw0 - 2 * sqrt(a) * alpha);

  // normalize
  a1 := a1 / ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
  a2 := a2 / ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
  b0 := b0 / ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
  b1 := b1 / ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);
  b2 := b2 / ((a + 1) + (a - 1) * cosw0 + 2 * sqrt(a) * alpha);

//a1 := -1.69065929318241;
//a2 :=  0.73248077421585;
//b0 :=  1.53512485958697;
//b1 := -2.69169618940638;
//b2 :=  1.19839281085285;
end;

function tshelvingfilter.run(const sample: double): double;
begin
  result := b0 * sample + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2;
  // shift state
  x2 := x1;
  x1 := sample;
  y2 := y1;
  y1 := result;
end;

// thighpassfilter

procedure thighpassfilter.init(samplerate: longint);
const
  f0 = 60.0;
var
  w0, c, norm: Double;
begin
  x1 := 0; x2 := 0;
  y1 := 0; y2 := 0;

  w0 := 2 * Pi * f0 / samplerate;
  c := 1 / tan(w0 / 2);

  norm := 1 / (1 + sqrt(2) * c + c * c);
  b0 := 1 * norm;
  b1 := -2 * b0;
  b2 := b0;
  a1 := 2 * (c * c - 1) * norm;
  a2 := (1 - sqrt(2) * c + c * c) * norm;

//a1 := -1.99004745483398;
//a2 :=  0.99007225036621;
//b0 :=  1.0;
//b1 := -2.0;
//b2 :=  1.0;
end;

function thighpassfilter.run(const sample: double): double;
begin
  result := b0 * sample + b1 * x1 + b2 * x2 - a1 * y1 - a2 * y2;
  // shift state
  x2 := x1;
  x1 := sample;
  y2 := y1;
  y1 := result;
end;

// tkweightingfilter

procedure tkweightingfilter.init(samplerate: longint);
begin
  highpassfilter.init(samplerate);
  shelvingfilter.init(samplerate);
end;

function tkweightingfilter.run(const sample: double): double;
begin
  result := shelvingfilter.run(highpassfilter.run(sample));
end;

procedure tkweightingfilter.run(var samples: arrayofdouble);
var
  i: integer;
begin
  for i := low(samples) to high(samples) do
    samples[i] := run(samples[i]);
end;

end.

