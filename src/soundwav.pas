{
  Description: Sound routines.

  Copyright (C) 2022 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
  classes, sysutils, math, fgl;

// WAVE utils

type
  // WAV is formed by the following structures in this order
  // all items are in little endian order, except the char arrays
  // items might be in big endian order if the RIFF identifier is RIFX

  triffheader = packed record
    ckid   : array [0..3] of char; // should be RIFF
    cksize : longword;             // 4 + (8 + subchunk1size) + (8 + subchunk2size).
                                   // the entire file size excluding triffheader.id and .size
    format : array [0..3] of char; // should be WAVE
  end;

  tfmt = packed record
    subckid       : array [0..3] of char; // should be "fmt "
    subcksize     : longword;             // subchunk1size
    format        : word;                 // pcm = 1 (linear quantization), values > 1 indicate a compressed format
    channels      : word;                 // mono = 1, stereo = 2, etc
    samplerate    : longword;             // 8000, 44100, etc
    byterate      : longword;             // = samplerate * numchannels * bitspersample/8
    blockalign    : word;                 // = numchannels * bitspersample/8
    bitspersample : word;                 // examples: 8 bits, 16 bits, etc
  end;

  tdatachunk = packed record
    subck2id   : array [0..3] of char; // should be "data"
    subck2size : longword;             // == numsamples * numchannels * bitspersample/8
  end;
  // and after this header the actual data comes, which is an array of samples

  tsample = packed record
    channelvalues : array of longint;
  end;

  tsamples = array of tsample;

  tdoublelist = specialize tfpglist<double>;

  // ttrackchannel

  ttrackchannel = class
  private
    frms:  tdoublelist;
    fpeak: tdoublelist;
    function getrms (index: longint): double;
    function getpeak(index: longint): double;
    function getcount: longint;
  public
    constructor create;
    destructor destroy; override;
    procedure add(const rmsi, peaki: double);
  public
    property rms [index: longint]: double read getrms;
    property peak[index: longint]: double read getpeak;
    property count: longint read getcount;
  end;

  // ttrack

  ttrack = class
  private
    fname: string;
    falbum: string;
    fnumber: longint;
    fsamplerate: longword;
    fbitspersample: longint;
    fchannels: array of ttrackchannel;
    fbyterate: longint;
    frms: double;
    fpeak: double;
    fdr: double;
    fduration: string;
    procedure setchannelcount(value: longint);
    function getchannel(index: longint): ttrackchannel;
    function getchannelcount: longint;
  public
    constructor create(const filename: string);
    destructor destroy; override;
  public
    property name: string read fname;
    property album: string read falbum;
    property number: longint read fnumber;
    property samplerate: longword read fsamplerate;
    property bitspersample: longint read fbitspersample;
    property byterate: longint read fbyterate;
    property rms: double read frms;
    property peak: double read fpeak;
    property dr: double read fdr;
    property duration: string read fduration;
    property channels[index: longint]: ttrackchannel read getchannel;
    property channelcount: longint read getchannelcount write setchannelcount;
  end;

  { ttrackanalyzer }
  
  ttrackanalyzer = class(tthread)
  private
    ffmt: tfmt;
    fdatachunk: tdatachunk;
    fdata: array of ttrackchannel;
    //---
    ftrack: ttrack;
    fstatus: longint;
    fstream : tstream;
    fpercentage: double;
    fonstart: tthreadmethod;
    fonstop: tthreadmethod;
    fonprogress: tthreadmethod;
    function getpercentage: longint;
    procedure readheader(astream: tstream);
    function readsamples(astream: tstream; ablock: tsamples): longint;
    procedure readfromstream(astream: tstream);
    function getrmsi (block: tsamples; channel: word): double;
    function getpeaki(block: tsamples; channel: word): double;
    function getdr(channel: word): double;
    procedure clear;
  public
    constructor create(atrack: ttrack; astream: tstream);
    destructor destroy; override;
    procedure execute; override;
  public
    property onstart:    tthreadmethod write fonstart;
    property onstop:     tthreadmethod write fonstop;
    property onprogress: tthreadmethod write fonprogress;
    property percentage: longint read getpercentage;
    property status:     longint read fstatus;
  end;

  // ttracklist

  ttracklist = class
  private
    flist: tlist;
    function getcount: longint;
    function gettrack(index: longint): ttrack;
  public
    constructor create;
    destructor destroy; override;
    procedure add(const trackname: string);
    procedure delete(index: longint);
    procedure clear;
    procedure sort;
    procedure savetofile(const filename: string);
  public
    property tracks[index: longint]: ttrack read gettrack;
    property count: longint read getcount;
  end;


  function filesupported(const fileext: string): boolean;


implementation

const
  idriff = 'RIFF';
  idwave = 'WAVE';
  idfmt  = 'fmt ';
  iddata = 'data';
  idlist = 'LIST';

// usefull routines

function filesupported(const fileext: string): boolean;
begin
  result := false;
  if fileext = '.wav'  then result := true;
  if fileext = '.flac' then result := true;
  if fileext = '.mp3'  then result := true;
  if fileext = '.ape'  then result := true;
end;

function comparedouble(const item1, item2: double): longint;
begin
  if item1 < item2 then
    result := +1
  else
    if item1 > item2 then
      result := -1
    else
      result := 0;
end;

function comparetrackname(item1, item2: pointer): longint;
begin
  result := ansicomparefilename(ttrack(item1).name, ttrack(item2).name);
end;

function calculate_db(value: double): double;
begin
  if value > 0 then
    result := 20*log10(value)
  else
    result := 0;
end;

// trackchannel

constructor ttrackchannel.create;
begin
  inherited create;
  frms  := tdoublelist.create;
  fpeak := tdoublelist.create;
end;

destructor ttrackchannel.destroy;
begin
  frms.destroy;
  fpeak.destroy;
  inherited destroy;
end;

procedure ttrackchannel.add(const rmsi, peaki: double);
begin
  frms .add(rmsi );
  fpeak.add(peaki);
end;

function ttrackchannel.getrms(index: longint): double;
begin
  result := frms[index];
end;

function ttrackchannel.getpeak(index: longint): double;
begin
  result := fpeak[index];
end;

function ttrackchannel.getcount: longint;
begin
  result := min(frms.count, fpeak.count);
end;

// ttrack

constructor ttrack.create(const filename: string);
begin
  inherited create;
  fname          := filename;
  falbum         := '';
  fnumber        := -1;
  fsamplerate    :=  0;
  fbitspersample := -1;
  fchannels      := nil;
  fbyterate      := -1;
  frms           := -1;
  fpeak          := -1;
  fdr            := -1;
end;

destructor ttrack.destroy;
var
  i: longint;
begin
  for i := low(fchannels) to high(fchannels) do
  begin
    fchannels[i].destroy;
  end;
  fchannels := nil;
  inherited destroy;
end;

function ttrack.getchannel(index: longint): ttrackchannel;
begin
  result := fchannels[index];
end;

procedure ttrack.setchannelcount(value: longint);
var
  i: longint;
begin
  // de-allocate channels
  for i := low(fchannels) to high(fchannels) do
  begin
    fchannels[i].destroy;
  end;
  fchannels := nil;
  // allocate new channels
  setlength(fchannels, value);
  for i := low(fchannels) to high(fchannels) do
  begin
    fchannels[i] := ttrackchannel.create;
  end;
end;

function ttrack.getchannelcount: longint;
begin
  result := length(fchannels);
end;

// ttrackanalyzer

constructor ttrackanalyzer.create(atrack: ttrack; astream: tstream);
begin
  fdata   := nil;
  ftrack  := atrack;
  fstream := astream;
  clear;

  freeonterminate := true;
  inherited create(true);
end;

destructor ttrackanalyzer.destroy;
begin
  clear;
  inherited destroy;
end;

procedure ttrackanalyzer.clear;
var
  i: longint;
begin
  for i := low(fdata) to high(fdata) do
  begin
    fdata[i].destroy;
  end;
  fdata   := nil;
  fstatus := 0;
end;

function ttrackanalyzer.getrmsi(block: tsamples; channel: word): double;
var
  i: longint;
  smpi2: int64;
begin
  smpi2 := 0;
  for i := low(block) to high(block) do
  begin
    smpi2 := smpi2 + sqr(block[i].channelvalues[channel]);
  end;

  if length(block) > 0 then
    result := sqrt(2*(smpi2/length(block)))
  else
    result := 0;
end;

function ttrackanalyzer.getpeaki(block: tsamples; channel: word): double;
var
  i : longint;
begin
  result := 0;
  for i := low(block) to high(block) do
  begin
    result := max(result, abs(block[i].channelvalues[channel]));
  end;
end;

function ttrackanalyzer.getdr(channel: word): double;
var
  i, n     : longint;
  drj      : double;
  rmsi     : double;
  peaki    : double;
  peak2nd  : double;
begin
  result := 0;
  for i := 0 to fdata[channel].count -1 do
  begin
    rmsi  := fdata[channel].frms [i];
    peaki := fdata[channel].fpeak[i];

    if (rmsi > 1) and (peaki > 1) then
      ftrack.channels[channel].add(calculate_db(rmsi),
                                   calculate_db(peaki))
    else
      ftrack.channels[channel].add(0, 0);
  end;
  fdata[channel].frms .sort(@comparedouble);
  fdata[channel].fpeak.sort(@comparedouble);

  n := round(0.2 * fdata[channel].count);
  if n > 1 then
  begin
    ftrack.frms  := max(ftrack.frms,  calculate_db(fdata[channel].frms [0]));
    ftrack.fpeak := max(ftrack.fpeak, calculate_db(fdata[channel].fpeak[0]));

    drj     := 0;
    peak2nd := fdata[channel].fpeak[1];
    for i := 0 to n -1 do
    begin
      drj := drj + sqr(fdata[channel].frms[i]);
    end;
    result := -calculate_db(sqrt(drj/n)*(1/peak2nd));
  end;
end;

procedure ttrackanalyzer.execute;
var
  i : longint;
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
      ftrack.fdr := 0;
      for i := 0 to ffmt.channels -1 do
      begin
        ftrack.fdr := ftrack.fdr + getdr(i);
      end;
      ftrack.fdr   := ftrack.fdr/ffmt.channels;
    end;

  if assigned(fonstop) then
    synchronize(fonstop);
end;

procedure ttrackanalyzer.readfromstream(astream:tstream);
var
  block: tsamples;
  i: longint;
  samples: longword;
  seconds: longint;
  step: double;
begin
  //read headers
  readheader(astream);
  if status = -1 then exit;
  // update track details
  ftrack.falbum         := '';
  ftrack.fnumber        := 0;
  ftrack.fsamplerate    := ffmt.samplerate;
  ftrack.fbitspersample := ffmt.bitspersample;
  ftrack.channelcount   := ffmt.channels;
  ftrack.fsamplerate    := ffmt.samplerate;
  ftrack.fbyterate      := ffmt.byterate;
  ftrack.frms           := 0;
  ftrack.fpeak          := 0;
  ftrack.fdr            := 0;
  ftrack.fduration      := '00:00';
  if ftrack.fsamplerate > 0 then
  begin
    seconds := (fdatachunk.subck2size div ffmt.blockalign) div ftrack.fsamplerate;
    ftrack.fduration := format('%3.2d:%2.2d', [seconds div (60), seconds mod (60)]);
  end;
  // read samples
  if status = -1 then exit;
  setlength(fdata, ffmt.channels);
  for i := low(fdata) to high(fdata) do
    fdata[i] := ttrackchannel.create;

  samples :=        (fdatachunk.subck2size div ffmt.blockalign);
  step    := 100 / ((fdatachunk.subck2size div ffmt.blockalign) / (3 * ffmt.samplerate));
  while samples > 0 do
  begin
    // allobate new block (of 3 seconds length)
    setlength(block, math.min(3 * ffmt.samplerate, samples));
    for i := low(block) to high(block) do
      setlength(block[i].channelvalues, ffmt.channels);
    // read samples
    dec(samples, readsamples(astream, block));
    //
    for i := 0 to ffmt.channels -1 do
    begin
      fdata[i].add(getrmsi (block, i),
                   getpeaki(block, i));
    end;
    // de-allocate block
    for i := low(block) to high(block) do
      block[i].channelvalues:= nil;
    block := nil;

    fpercentage := fpercentage + step;
    if assigned(fonprogress) then
      synchronize(fonprogress);
  end;
end;

procedure ttrackanalyzer.readheader(astream: tstream);
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

  // read datachunk
  if astream.read(fdatachunk, sizeof(fdatachunk)) <> sizeof(fdatachunk) then fstatus := -1;
  if fdatachunk.subck2id = idlist then
  begin
    astream.seek(fdatachunk.subck2size, socurrent);
    if astream.read(fdatachunk, sizeof(fdatachunk)) <> sizeof(fdatachunk) then fstatus := -1;
  end;

  if fdatachunk.subck2id <> iddata then fstatus := -1;
  fdatachunk.subck2size := leton(fdatachunk.subck2size);
end;

function ttrackanalyzer.readsamples(astream: tstream; ablock: tsamples): longint;
var
  i, j, k: longint;
  dt : array[0..3] of byte;
begin
  result := length(ablock);
  for i := low(ablock) to high(ablock) do
    for j := 0 to ffmt.channels -1 do
    begin
      if ffmt.bitspersample = 8 then
      begin
        if astream.read(dt[0], 1) <> 1 then fstatus := -1;
        ablock[i].channelvalues[j] := pbyte(@dt[0])^;
      end else
      if ffmt.bitspersample = 16 then
      begin
        if astream.read(dt[0], 2) <> 2 then fstatus := -1;
        ablock[i].channelvalues[j] := psmallint(@dt[0])^;
      end else
      if ffmt.bitspersample = 24 then
      begin
        if astream.read(dt[0], 3) <> 3 then fstatus := -1;

        if dt[2] > 127 then
          k := longint($FFFFFFFF)
        else
          k := 0;

        k := (k shl 8) or dt[2];
        k := (k shl 8) or dt[1];
        k := (k shl 8) or dt[0];

        ablock[i].channelvalues[j] := k;
      end else
      if ffmt.bitspersample = 32 then
      begin
        if astream.read(dt[0], 4) <> 4 then fstatus := -1;
        ablock[i].channelvalues[j] := plongint(@dt[0])^;
      end;
   end;
end;

function ttrackanalyzer.getpercentage: longint;
begin
  result := round(fpercentage);
end;

// ttracklist

constructor ttracklist.create;
begin
  inherited create;
  flist := tlist.create;
end;

destructor ttracklist.destroy;
begin
  clear;
  flist.destroy;
  inherited destroy;
end;

procedure ttracklist.add(const trackname: string);
var
  track: ttrack;
begin
  track := ttrack.create(trackname);
  flist.add(track);
end;

procedure ttracklist.delete(index: longint);
begin
  ttrack(flist[index]).destroy;
  flist.delete(index);
end;

procedure ttracklist.clear;
begin
  while flist.count > 0 do
  begin
    ttrack(flist[0]).destroy;
    flist.delete(0);
  end;
end;

procedure ttracklist.sort;
begin
  flist.sort(@comparetrackname);
end;

function ttracklist.getcount: longint;
begin
  result := flist.count;
end;

function ttracklist.gettrack(index: longint): ttrack;
begin
  result := ttrack(flist[index]);
end;

procedure ttracklist.savetofile(const filename: string);
const
  splitter = '--------------------------------------------------------------------------------';
var
  bps: longint;
  ch: longint;
  dr: double;
  i: longint;
  s: tstringlist;
  sr: longint;
  track: ttrack;
begin
  s := tstringlist.create;
  s.add('AudioMeter 0.3 - Dynamic Range Meter');
  s.add(splitter);
  s.add(format('Log date : %s', [datetimetostr(now)]));
  s.add(splitter);
  s.add('');

  ch  := 0;
  sr  := 0;
  bps := 0;
  dr  := 0;
  if count > 0 then
  begin
    track := gettrack(0);
    ch  := track.channelcount;
    sr  := track.samplerate;
    bps := track.bitspersample;

    s.add('DR         Peak         RMS     Duration   Track');
    s.add(splitter);
    for i := 0 to count -1 do
    begin
      track := gettrack(i);
      s.add(format('DR%2.0f     %6.2f dB   %6.2f dB    %-s   %s', [
        track.dr, track.peak, track.rms,
        track.duration, extractfilename(track.name)]));

      if ch  <> track.channelcount  then ch  := -1;
      if sr  <> track.samplerate    then sr  := -1;
      if bps <> track.bitspersample then bps := -1;

      dr := dr + track.dr;
    end;
    dr := round(dr/count);

    s.add(splitter);
    s.add('');
    s.add('Number of tracks:  %d',     [count]);

    if ch  >  0 then s.add('Channels:          %d',     [ch ]);
    if sr  >  0 then s.add('Samplerate:        %d',     [sr ]);
    if bps >  0 then s.add('Bits per sample:   %d',     [bps]);
    if dr  >  0 then s.add('Official DR value: %2.0f',  [dr ]);

    if ch  <= 0 then s.add('Channels:          ---');
    if sr  <= 0 then s.add('Samplerate:        ---');
    if bps <= 0 then s.add('Bits per sample:   ---');
    if dr  <= 0 then s.add('Official DR value: 0'  );

    s.add('');
    s.add(splitter);
  end;
  s.savetofile(filename);
  s.destroy;
end;

end.

