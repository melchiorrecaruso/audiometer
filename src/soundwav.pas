{
  Description: Sound routines.

  Copyright (C) 2020-2023 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
    ckid   : array [0..3] of char; // should be "RIFF"
    cksize : longword;             // 4 + (8 + subchunk1size) + (8 + subchunk2size).
                                   // the entire file size excluding triffheader.id and .size
    waveid : array [0..3] of char; // should be "WAVE"
  end;

  tfmtchunk = packed record
    ckid               : array [0..3] of char;  // should be "fmt "
    cksize             : longword;              // subchunk1size: 16, 18 or 40
    formattag          : word;                  // pcm = 1 (linear quantization), values > 1 indicate a compressed format
    channels           : word;                  // mono = 1, stereo = 2, etc
    samplespersec      : longword;              // 8000, 44100, etc
    bytespersec        : longword;              // = samplerate * numchannels * bitspersample/8
    blockalign         : word;                  // = numchannels * bitspersample/8
    bitspersample      : word;                  // examples: 8 bits, 16 bits, etc
  end;

  tfmtchunkext = packed record
    cbsize             : word;                  // size of the extension (0 or 22)
    validbitspersample : word;                  // number of valid bits
    channelmask        : longword;              // speaker position mask
    subcode            : word;                  // GUID data format code
    subformat          : array [0..13] of byte; // GUID
  end;

  tfactchunk = packed record
    ckid               : array [0..3] of char;  // should be "fact"
    cksize             : longword;              // chunk size: minimum 4
    samplelength       : longword;              // Number of samples (per channel)
  end;

const
  WAVE_FORMAT_PCM        = $0001;
  WAVE_FORMAT_IEEE_FLOAT = $0003;
  WAVE_FORMAT_ALAW       = $0006;
  WAVE_FORMAT_MULAW      = $0007;
  WAVE_FORMAT_EXTENSIBLE = $fffe;

type
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
    frms2: tdoublelist;
    fpeak: tdoublelist;
    function getrms2(index: longint): double;
    function getpeak(index: longint): double;
    function getcount: longint;
  public
    constructor create;
    destructor destroy; override;
    procedure add(const rms2i, peaki: double);
  public
    property rms2[index: longint]: double read getrms2;
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
    ffmt: tfmtchunk;
    ffmtext: tfmtchunkext;
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
    fnorm: longint;
    function getpercentage: longint;
    procedure readheader(astream: tstream);
    function readsamples(astream: tstream; ablock: tsamples): longint;
    procedure readfromstream(astream: tstream);
    function getrms2(block: tsamples; channel: word): double;
    function getpeak(block: tsamples; channel: word): double;
    function getrms(channel: word): double;
    function getpeak(channel: word): double;
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
  function db(const value: double): double;


implementation

const
  idriff = 'RIFF';
  idwave = 'WAVE';
  idfmt  = 'fmt ';
  iffact = 'fact';
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

function compare(const item1, item2: double): longint;
begin
  if item2 > item1 then
    result := +1
  else
    if item2 < item1 then
      result := -1
    else
      result := 0;
end;

function comparetrackname(item1, item2: pointer): longint;
begin
  result := ansicomparefilename(ttrack(item1).name, ttrack(item2).name);
end;

function db(const value: double): double;
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
  frms2 := tdoublelist.create;
  fpeak := tdoublelist.create;
end;

destructor ttrackchannel.destroy;
begin
  frms2.destroy;
  fpeak.destroy;
  inherited destroy;
end;

procedure ttrackchannel.add(const rms2i, peaki: double);
begin
  frms2.add(rms2i);
  fpeak.add(peaki);
end;

function ttrackchannel.getrms2(index: longint): double;
begin
  result := frms2[index];
end;

function ttrackchannel.getpeak(index: longint): double;
begin
  result := fpeak[index];
end;

function ttrackchannel.getcount: longint;
begin
  result := frms2.count;
end;

// ttrack

constructor ttrack.create(const filename: string);
begin
  inherited create;
  fname          := filename;
  falbum         := '';
  fnumber        := -1;
  fsamplerate    := 0;
  fbitspersample := 0;
  fchannels      := nil;
  fbyterate      := 0;
  frms           := 0;
  fpeak          := 0;
  fdr            := 0;
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
    fchannels[i].destroy;
  fchannels := nil;
  // allocate new channels
  setlength(fchannels, value);
  for i := low(fchannels) to high(fchannels) do
    fchannels[i] := ttrackchannel.create;
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

function ttrackanalyzer.getrms2(block: tsamples; channel: word): double;
var
  i: longint;
begin
  result := 0;
  for i := low(block) to high(block) do
  begin
    result := result + sqr(block[i].channelvalues[channel]/fnorm);
  end;
  result := 2*result/length(block);
end;

function ttrackanalyzer.getpeak(block: tsamples; channel: word): double;
var
  i : longint;
begin
  result := 0;
  for i := low(block) to high(block) do
  begin
    result := max(result, abs(block[i].channelvalues[channel])/fnorm);
  end;
end;

function ttrackanalyzer.getdr(channel: word): double;
var
  i, n: longint;
  peak2nd: double;
begin
  result := 0;
  for i := 0 to fdata[channel].count -1 do
  begin
    ftrack.channels[channel].add(
      fdata[channel].frms2[i],
      fdata[channel].fpeak[i]);
  end;
  fdata[channel].frms2.sort(@compare);
  fdata[channel].fpeak.sort(@compare);

  n := trunc(0.2 * fdata[channel].count);
  if n > 1 then
  begin
    peak2nd := fdata[channel].fpeak[1];

    result := 0;
    for i := 0 to n -1 do
    begin
      result := result + fdata[channel].frms2[i];
    end;
    result := db(peak2nd/sqrt(result/n));
  end;
end;

function ttrackanalyzer.getrms(channel: word): double;
var
  i: longint;
begin
  result := 0;
  for i := 0 to ftrack.channels[channel].count -1 do
  begin
    result := result + ftrack.channels[channel].rms2[i];
  end;
  result := sqrt(result / ftrack.channels[channel].count);
end;

function ttrackanalyzer.getpeak(channel: word): double;
var
  i: longint;
begin
  result := 0;
  for i := 0 to ftrack.channels[channel].count -1 do
  begin
    if result < ftrack.channels[channel].peak[i] then
      result := ftrack.channels[channel].peak[i];
  end;
end;

procedure ttrackanalyzer.execute;
var
  i: longint;
  dr: double;
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
        dr := getdr(i);
        ftrack.fdr   := ftrack.fdr   + dr;
        ftrack.frms  := ftrack.frms  + getrms (i);
        ftrack.fpeak := ftrack.fpeak + getpeak(i);
        {$ifopt D+}
        writeln;
        writeln('track.DR  [', i,']      ', dr            :2:1);
        writeln('track.Peak[', i,']      ', db(getpeak(i)):2:2);
        writeln('track.Rms [', i,']      ', db(getrms (i)):2:2);
        {$endif}
      end;
      ftrack.fdr   := ftrack.fdr  /ffmt.channels;
      ftrack.fpeak := ftrack.fpeak/ffmt.channels;
      ftrack.frms  := ftrack.frms /ffmt.channels;
      {$ifopt D+}
      writeln;
      writeln('track.DR:          ', ftrack.fdr      :2:1);
      writeln('track.Peak         ', db(ftrack.fpeak):2:2);
      writeln('track.Rms          ', db(ftrack.frms ):2:2);
      writeln;
      {$endif}
    end;
  if assigned(fonstop) then
    synchronize(fonstop);
end;

procedure ttrackanalyzer.readfromstream(astream:tstream);
var
  block: tsamples;
  blocksize: longword;
  blockcount: longword;
  i, j: longint;
  samples: longword;
  seconds: longword;
begin
  {$ifopt D+}
  writeln('track.name         ', ftrack.fname);
  {$endif}
  //read headers
  readheader(astream);
  if status <> 0 then exit;
  // update track details
  ftrack.falbum         := '';
  ftrack.fnumber        := 0;
  ftrack.fsamplerate    := ffmt.samplespersec;
  ftrack.fbitspersample := ffmt.bitspersample;
  ftrack.channelcount   := ffmt.channels;
  ftrack.fsamplerate    := ffmt.samplespersec;
  ftrack.fbyterate      := ffmt.bytespersec;
  ftrack.frms           := 0;
  ftrack.fpeak          := 0;
  ftrack.fdr            := 0;
  ftrack.fduration      := '00:00';

  fnorm := 1 shl (ftrack.fbitspersample -1);
  if ftrack.fsamplerate > 0 then
  begin
    seconds := (fdatachunk.subck2size div ffmt.blockalign) div ftrack.fsamplerate;
    ftrack.fduration := format('%3.2d:%2.2d', [seconds div (60), seconds mod (60)]);
  end;
  // read samples
  if status <> 0 then exit;
  setlength(fdata, ffmt.channels);
  for i := low(fdata) to high(fdata) do
    fdata[i] := ttrackchannel.create;

  samples    := fdatachunk.subck2size div ffmt.blockalign;
  blocksize  := trunc(132480 / 44100 * ffmt.samplespersec);
  blockcount := 0;
  if blocksize > 0 then
    blockcount := samples div blocksize;

  if blockcount = 0 then fstatus := -3;
  if blocksize  = 0 then fstatus := -3;
  if status <> 0 then exit;
  // allobate new block (of 3 seconds length)
  block := nil;
  setlength(block, blocksize);
  for i := low(block) to high(block) do
    setlength(block[i].channelvalues, ffmt.channels);
  // read samples
  for i := 0 to blockcount -1  do
  begin
    fpercentage := 100*i/blockcount;
    if assigned(fonprogress) then
      Queue(fonprogress);

    readsamples(astream, block);
    // calculate block rms and peak
    for j := 0 to ffmt.channels -1 do
    begin
      fdata[j].add(getrms2(block, j),
                   getpeak(block, j));
    end;
  end;
  fpercentage := 100;
  if assigned(fonprogress) then
    Queue(fonprogress);
  // de-allocate block
  for i := low(block) to high(block) do
    block[i].channelvalues := nil;
  block := nil;
end;

procedure ttrackanalyzer.readheader(astream: tstream);
var
  marker: array[0..3] of char;
  riff: triffheader;
begin
  if astream.read(riff, sizeof(riff)) <> sizeof(riff) then fstatus := -1;
  if riff.ckid   <> idriff then fstatus := -1;
  if riff.waveid <> idwave then fstatus := -1;
  riff.cksize := leton(riff.cksize);
  {$ifopt D+}
  writeln('riff.ckid          ', riff.ckid);
  writeln('riff.cksize        ', riff.cksize);
  writeln('riff.format        ', riff.waveid);
  {$endif}
  if astream.read(ffmt, sizeof(ffmt)) <> sizeof(ffmt) then fstatus := -1;
  if ffmt.ckid <> idfmt then fstatus := -1;

  ffmt.cksize        := leton(ffmt.cksize);
  ffmt.formattag     := leton(ffmt.formattag);
  ffmt.channels      := leton(ffmt.channels);
  ffmt.samplespersec := leton(ffmt.samplespersec);
  ffmt.bytespersec   := leton(ffmt.bytespersec);
  ffmt.blockalign    := leton(ffmt.blockalign);
  ffmt.bitspersample := leton(ffmt.bitspersample);

  if ffmt.channels       = 0  then fstatus := -2;
  if ffmt.bitspersample  = 0  then fstatus := -1;
  if ffmt.bitspersample  = 32 then fstatus := -1;

  {$ifopt D+}
  writeln('ffmt.subckid       ', ffmt.ckid);
  writeln('ffmt.subcksize     ', ffmt.cksize);
  writeln('ffmt.formattag     ', ffmt.formattag);
  writeln('ffmt.channels      ', ffmt.channels);
  writeln('ffmt.samplerate    ', ffmt.samplespersec);
  writeln('ffmt.byterate      ', ffmt.bytespersec);
  writeln('ffmt.blockalign    ', ffmt.blockalign);
  writeln('ffmt.bitspersample ', ffmt.bitspersample);
  {$endif}

  if ffmt.cksize = 40 then
  begin
    if astream.read(ffmtext, sizeof(ffmtext)) <> sizeof(ffmtext) then fstatus := -1;

    ffmtext.cbsize             := leton(ffmtext.cbsize);
    ffmtext.validbitspersample := leton(ffmtext.validbitspersample);
    ffmtext.channelmask        := leton(ffmtext.channelmask);
    {$ifopt D+}
    writeln;
    writeln('ffmtext.cbsize             ', ffmtext.cbsize);
    writeln('ffmtext.validbitspersample ', ffmtext.validbitspersample);
    writeln('ffmtext.channelmask        ', ffmtext.channelmask);
    writeln;
    {$endif}
  end;

  // search data section by scanning
  fillchar(marker, sizeof(marker), ' ');
  while astream.read(marker[3], 1) = 1 do
  begin
    if marker = iddata then
    begin
      fdatachunk.subck2id := iddata;
      astream.read(fdatachunk.subck2size,
        sizeof(fdatachunk.subck2size));
      break;
    end;
    move(marker[1], marker[0], sizeof(marker) -1);
  end;

  if fdatachunk.subck2id <> iddata then fstatus := -1;
  fdatachunk.subck2size := leton(fdatachunk.subck2size);
  {$ifopt D+}
  writeln('data.subck2id      ', fdatachunk.subck2id);
  writeln('data.subck2size    ', fdatachunk.subck2size);
  {$endif}
  if fdatachunk.subck2size = 0 then fstatus := -2;
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
  dr: double;
  i: longint;
  s: tstringlist;
  track: ttrack;
begin
  s := tstringlist.create;
  s.add('AudioMeter 0.4.2 - Dynamic Range Meter');
  s.add(splitter);
  s.add(format('Log date : %s', [datetimetostr(now)]));
  s.add(splitter);
  s.add('');

  dr  := 0;
  if count > 0 then
  begin
    track := gettrack(0);
    s.add('DR      Peak        RMS     bps  chs      SR  Duration  Track');
    s.add(splitter);
    for i := 0 to count -1 do
    begin
      track := gettrack(i);
      s.add(format('DR%2.0f %7.2f dB %7.2f dB %4.0d %4.0d %7.0d %-s     %s', [
        track.dr,
        db(track.peak),
        db(track.rms),
        track.bitspersample,
        track.channelcount,
        track.samplerate,
        track.duration,
        extractfilename(track.name)]));

      dr := dr + track.dr;
    end;
    dr := dr/count;

    s.add(splitter);
    s.add('');
    s.add('Number of tracks:  %d', [count]);
    if dr  >  0 then s.add('Official DR value: %1.0f', [dr ]);
    if dr  <= 0 then s.add('Official DR value: ---');

    s.add('');
    s.add(splitter);
  end;
  s.savetofile(filename);
  s.destroy;
end;

end.

