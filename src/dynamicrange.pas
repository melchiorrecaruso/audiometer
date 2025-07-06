{
  Description: Dynamic Range Measurement Routines.

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

unit dynamicrange;

{$mode ObjFPC}{$H+}
{$modeswitch advancedrecords}

interface

uses
  classes, common, fgl, sysutils;

type
  tblock = record
    rms2: tarrayofdouble;
    peak: tarrayofdouble;
  end;

  tblocks = record
  private
    fblocks: array of tblock;
    fblocknum: longint;
    fblocksize: longint;
    fdynamicrange: tarrayofdouble;
    function getblock(index: longint): tblock;
    function getdynamicrange: double;
  public
    procedure clear;
    procedure execute(const achannels: tchannels; asamplerate: longint);
    property blocks[index: longint]: tblock read getblock;
    property blocknum: longint read fblocknum;
    property dynamicrange: double read getdynamicrange;
  end;


implementation

uses
  math;

// tblocks

function tblocks.getblock(index: longint): tblock;
begin
  result := fblocks[index];
end;

function tblocks.getdynamicrange: double;
var
  i: longint;
begin
  result := 0;
  if length(fdynamicrange) > 0 then
  begin
    for i := low(fdynamicrange) to high(fdynamicrange) do
    begin
      result := result + fdynamicrange[i];
    end;
    result := trunc(result / length(fdynamicrange) + 1);
  end;
end;

procedure tblocks.clear;
var
  i: longint;
begin
  setlength(fdynamicrange, 0);
  for i := low(fblocks) to high(fblocks) do
  begin
    setlength(fblocks[i].rms2, 0);
    setlength(fblocks[i].peak, 0);
  end;
  setlength(fblocks, 0);
  fblocksize := 0;
  fblocknum  := 0;
end;

procedure tblocks.execute(const achannels: tchannels; asamplerate: longint);
var
  i, j, ch: longint;
  sum2, peak, peak2nd: double;
  rmslist: tlistofdouble;
  peaklist: tlistofdouble;
  channelcount, num: longint;
begin
  clear;
  channelcount := length(achannels);
  if channelcount = 0 then exit;

  fblocksize := 3 * asamplerate;
  fblocknum  := length(achannels[0]) div fblocksize;

  if fblocknum < 2 then exit;

  setlength(fblocks, fblocknum);
  for i := low(fblocks) to high(fblocks) do
  begin
    setlength(fblocks[i].rms2, channelcount);
    setlength(fblocks[i].peak, channelcount);
  end;

  for ch := 0 to channelcount -1 do
  begin
    for i := low(fblocks) to high(fblocks) do
    begin
      sum2 := 0.0;
      peak := 0.0;
      for j := (i * fblocksize) to ((i + 1) * fblocksize -1) do
      begin
        sum2 := sum2 + sqr(achannels[ch][j]);
        peak := max(peak, abs(achannels[ch][j]));
      end;
      fblocks[i].rms2[ch] := sum2 / fblocksize;
      fblocks[i].peak[ch] := peak;
    end;
  end;

  setlength(fdynamicrange, channelcount);
  for ch := 0 to channelcount -1 do
  begin
    rmslist  := tlistofdouble.create;
    peaklist := tlistofdouble.create;
    for i := low(fblocks) to high(fblocks) do
    begin
      rmslist.add(sqrt(2*fblocks[i].rms2[ch]));
      peaklist.add(fblocks[i].peak[ch]);
    end;
    rmslist.sort(@compare);
    peaklist.sort(@compare);

    num  := trunc(0.2 * rmslist.count);
    if num > 1 then
    begin
      peak2nd := peaklist[1];

      sum2 := 0;
      for j := 0 to num -1 do
      begin
        sum2 := sum2 + sqr(rmslist[i]);
      end;
      fdynamicrange[ch] := decibel(sqrt(sum2/num) / peak2nd);
    end;
    rmslist.destroy;
    peaklist.destroy
  end;
end;

end.
