{
  Description: Sound routines.

  Copyright (C) 2020 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit soundwav;

{$mode objfpc}

interface

uses
  classes, sysutils, math, dialogs, tasources, fgl;

// WAVE UTILS

type
  // WAV is formed by the following structures in this order
  // All items are in little endian order, except the char arrays
  // Items might be in big endian order if the RIFF identifier is RIFX

  triffheader = packed record
    ckid   : array [0..3] of char; // should be RIFF
    cksize : longword;             // 4 + (8 + SubChunk1Size) + (8 + SubChunk2Size).
                                   // The entire file size excluding TRiffHeader.ID and .Size
    format : array [0..3] of char; // should be WAVE
  end;

  tfmt = packed record
    subckid       : array [0..3] of char; // Should be "fmt "
    subcksize     : longword;             // SubChunk1Size
    format        : word;                 // PCM = 1 (Linear quantization), values > 1 indicate a compressed format
    channels      : word;                 // Mono = 1, Stereo = 2, etc
    samplerate    : longword;             // 8000, 44100, etc
    byterate      : longword;             // = SampleRate * NumChannels * BitsPerSample/8
    blockalign    : word;                 // = NumChannels * BitsPerSample/8
    bitspersample : word;                 // examples: 8 bits, 16 bits, etc
  end;

  tdatachunk = packed record
    subck2id   : array [0..3] of char; // should be "data"
    subck2size : longword;             // == NumSamples * NumChannels * BitsPerSample/8
  end;
  // And after this header the actual data comes, which is an array of samples

  tdata = packed record
    channelvalues : array of longint;
    blockid       : longword;
  end;

  tdoublelist = specialize tfpglist<double>;

  { TWaveReader }
  
  twavereader = class(tthread)
  private
    ffmt: tfmt;
    fdatachunk: tdatachunk;
    fdata: array of tdata;
    fblocksize: longword;
    fblocknum: longword;
    frmssrc:  tlistchartsource;
    fpeaksrc: tlistchartsource;

    fstatus: longint;
    fstream : tstream;
    fdravg: longint;
    fpercentage: double;
    fonstart:    tthreadmethod;
    fonstop:     tthreadmethod;
    fonprogress: tthreadmethod;

    function getpercentage: longint;
    function getchannels: word;
    function getbitspersample: word;
    function getsamplerate: longword;
    procedure setblocksize(value: longword);
    procedure readheaders(astream: tstream);
    procedure readsamples(astream: tstream);
    procedure readfromstream(astream: tstream);
    function rms(blockid: longword; channel: word): double;
    function peak(blockid: longword; channel: word): double;
    function db(value: double): double;
    function dr(channel: word): double;
    procedure clear;
  public
    constructor create(astream: tstream);
    destructor destroy; override;
    procedure execute; override;
  public
    property channels: word read getchannels;
    property bitspersample: word read getbitspersample;
    property samplerate: longword read getsamplerate;
    property blocksize: longword read fblocksize;
    property blocknum: longword read fblocknum;
    property dravg: longint read fdravg;

    property rmssources:  tlistchartsource write frmssrc;
    property peaksources: tlistchartsource write fpeaksrc;

    property onstart:    tthreadmethod write fonstart;
    property onstop:     tthreadmethod write fonstop;
    property onprogress: tthreadmethod write fonprogress;
    property percentage: longint read getpercentage;
    property status:     longint read fstatus;
  end;


implementation

const
  idriff = 'RIFF';
  idwave = 'WAVE';
  idfmt  = 'fmt ';
  iddata = 'data';

function doublecompare(const item1, item2: double): longint;
begin
  if item1 < item2 then result := +1 else
  if item1 > item2 then result := -1 else result := 0;
end;

function twavereader.rms(blockid: longword; channel: word): double;
var
  i     : longint;
  num   : longint;
  smpi2 : int64;
begin
  num   := 0;
  smpi2 := 0;
  for i := low(fdata) to high(fdata) do
  begin
    if fdata[i].blockid = blockid then
    begin
      smpi2 := smpi2 + sqr(fdata[i].channelvalues[channel]);
      inc(num);
    end;
  end;

  result := 0;
  if num > 0 then
  begin
    result := sqrt(2*(smpi2/num));
  end;
end;

function twavereader.peak(blockid: longword; channel: word): double;
var
  i : longint;
begin
  result := 0;
  for i := low(fdata) to high(fdata) do
    if fdata[i].blockid = blockid then
    begin
      result := max(result, abs(fdata[i].channelvalues[channel]));
    end;
end;

function twavereader.db(value: double): double;
begin
  result := 20*log10(value);
end;

function twavereader.dr(channel: word): double;
var
  i        : longint;
  n        : longint;
  rmsi     : double;
  rmslist  : tdoublelist;
  peaki    : double;
  peaklist : tdoublelist;
  peak2nd  : double;
  tick     : double;
begin
  result := 0;
  if fstatus = -1 then exit;

  tick     := 100 / (fblocknum * ffmt.channels);
  rmslist  := tdoublelist.create;
  peaklist := tdoublelist.create;
  for i := 1 to fblocknum do
  begin
    fpercentage := fpercentage + tick;
    if assigned(fonprogress) then
      synchronize(fonprogress);

    rmsi := rms(i, channel);
    rmslist.add(rmsi);

    peaki := peak(i, channel);
    peaklist.add(peaki);

    if assigned(frmssrc)  then frmssrc .add(i, db(rmsi ));
    if assigned(fpeaksrc) then fpeaksrc.add(i, db(peaki));
  end;
  rmslist. sort(@doublecompare);
  peaklist.sort(@doublecompare);

  n := round(0.2 * fblocknum);
  if n > 1 then
  begin
    peak2nd := peaklist[1];

    for i := 0 to n -1 do
    begin
      result := result + sqr(rmslist[i]);
    end;
    result := -db(sqrt(result/(n)) * (1/peak2nd));
  end;
  rmslist.destroy;
  peaklist.destroy;
end;

procedure twavereader.execute;
var
  i   : longint;
  tmp : double;
begin
  fpercentage := 0;
  if assigned(fonstart) then
    synchronize(fonstart);
  if assigned(fonprogress) then
    synchronize(fonprogress);

  readfromstream(fstream);
  if fstatus = 0 then
    if ffmt.channels > 0 then
    begin
      tmp := 0;
      for i := 0 to ffmt.channels -1 do
      begin
        tmp := tmp + dr(i);
      end;
      fdravg := round(tmp/ffmt.channels);
    end;

  if assigned(fonstop) then
    synchronize(fonstop);
end;

{ twavereader }

constructor twavereader.create(astream: tstream);
begin
  fblocksize := 0;
  frmssrc    := nil;
  fpeaksrc   := nil;

  fstream    := astream;

  freeonterminate := true;
  inherited create(true);
end;

destructor twavereader.destroy;
begin
  clear;
  inherited destroy;
end;

procedure twavereader.clear;
var
  i: longint;
begin
  fdravg     := 0;
  fstatus    := 0;
  fblocksize := 0;
  for i := low(fdata) to high(fdata) do
  begin
    setlength(fdata[i].channelvalues, 0);
  end;
  setlength(fdata, 0);
end;

procedure twavereader.readfromstream(astream:tstream);
begin
  clear;
  readheaders(astream);
  readsamples(astream);
end;

procedure twavereader.readheaders(astream: tstream);
var
  riff : triffheader;
begin
  if astream.read(riff, sizeof(riff)) <> sizeof(riff) then fstatus := -1;
  if riff.ckid   <> idriff then fstatus := -1;
  if riff.format <> idwave then fstatus := -1;
  riff.cksize := leton(riff.cksize);


  if astream.read(ffmt, sizeof(ffmt)) <> sizeof(ffmt) then fstatus := -1;
  if ffmt.subckid <> idfmt then fstatus := -1;

  ffmt.subcksize     := leton(ffmt.subcksize);
  ffmt.format        := leton(ffmt.format);
  ffmt.channels      := leton(ffmt.channels);
  ffmt.samplerate    := leton(ffmt.samplerate);
  ffmt.byterate      := leton(ffmt.byterate);
  ffmt.blockalign    := leton(ffmt.blockalign);
  ffmt.bitspersample := leton(ffmt.bitspersample);

  if astream.read(fdatachunk, sizeof(fdatachunk)) <> sizeof(fdatachunk) then fstatus := -1;
  if fdatachunk.subck2id <> iddata then fstatus := -1;

  fdatachunk.subck2size := leton(fdatachunk.subck2size);
end;

procedure twavereader.readsamples(astream: tstream);
var
  i   : longint;
  j   : longint;
  s8  : byte;
  s16 : smallint;
begin
  if fstatus = 0 then
  begin
    setlength(fdata, fdatachunk.subck2size div ffmt.blockalign);
    for i := low(fdata) to high(fdata) do
    begin
      setlength(fdata[i].channelvalues, ffmt.channels);
      for j := 0 to ffmt.channels -1 do
      begin

        if ffmt.bitspersample = 8 then
        begin
          s8 := 0;
          astream.read(s8, sizeof(s8));
          fdata[i].channelvalues[j] := s8;
        end else
        if ffmt.bitspersample = 16 then
        begin
          s16 := 0;
          astream.read(s16, sizeof(s16));
          fdata[i].channelvalues[j] := s16;
        end;

      end;
    end;
    setblocksize(3 * ffmt.samplerate);
  end;
end;

procedure twavereader.setblocksize(value: longword);
var
  i: longint;
  j: longint;
begin
  if fstatus = 0 then
  begin
    fblocknum  := 1;
    fblocksize := value;

    j := 0;
    for i := low(fdata) to high(fdata) do
    begin
      if j = fblocksize then
      begin
        inc(fblocknum);
        j := 0;
      end;
      fdata[i].blockid := fblocknum;
      inc(j);
    end;
  end;
end;

function twavereader.getchannels: word;
begin
  result := ffmt.channels;
end;

function twavereader.getbitspersample: word;
begin
  result := ffmt.bitspersample;
end;

function twavereader.getsamplerate: longword;
begin
  result := ffmt.samplerate;
end;

function twavereader.getpercentage: longint;
begin
  result := round(fpercentage);
end;

end.

