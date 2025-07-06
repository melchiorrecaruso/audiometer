{
  Description: Sound routines.

  Copyright (C) 2020-2025 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
  classes, common, dynamicrange, sysutils, math, loudness, ufft, utypes;

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
  SPECTRUMWINDOWSIZE     = 1024;

type
  tdatachunk = packed record
    subck2id   : array [0..3] of char; // should be "data"
    subck2size : longword;             // == numsamples * numchannels * bitspersample/8
  end;

  // ttrack

  ttrack = class
  private
    ffilename: string;
    falbum: string;
    fnumber: longint;
    fsamplerate: longword;
    fchannelcount: longint;
    fbitspersample: longint;
    fbyterate: longint;
    fduration: longint;
    fchannels: tchannels;

    fdynamicrange: tblocks;
  public
    constructor create(const afilename: string);
    destructor destroy; override;
    procedure clearchannels;
  public
    property filename: string read ffilename;
    property album: string read falbum;
    property number: longint read fnumber;
    property samplerate: longword read fsamplerate;
    property channelcount: longint read fchannelcount;
    property bitspersample: longint read fbitspersample;
    property byterate: longint read fbyterate;
    property duration: longint read fduration;

    property dynamicrange: tblocks read fdynamicrange;

  end;

  { ttrackanalyzer }
  
  ttrackanalyzer = class(tthread)
  private
    ffmt: tfmtchunk;
    ffmtext: tfmtchunkext;
    fdatachunk: tdatachunk;
    //---
    ftrack: ttrack;
    fblocknum: longint;
    fblocksize: longint;
    fsamplecount: longint;
    //---
    fstatus: longint;
    fstream : tstream;
    fpercentage: double;
    fonstart: tthreadmethod;
    fonstop: tthreadmethod;
    fontick: tthreadmethod;
    fffton: boolean;
    function getpercentage: longint;
    procedure readheader(astream: tstream);
    function readsamples(astream: tstream; achannels: tchannels; achannelsize: longint): longint;
    procedure readfromstream(astream: tstream);
    procedure getspectrum(asamples: pdouble; count: longint; aspectrum: pfloat);
    function getrms2(asamples: arrayofdouble; index, count: longint): double;
    function getpeak(asamples: arrayofdouble; index, count: longint): double;
    function getrms2(channel: word): double;
    function getrms(channel: word): double;
    function getpeak(channel: word): double;
    function getdr(channel: word): double;
    function gettruepeak(channel: word): double;
    procedure normalizesamples;
  public
    constructor create(atrack: ttrack; astream: tstream; aFFTOn: boolean);
    destructor destroy; override;
    procedure execute; override;
  public
    property onstart:    tthreadmethod write fonstart;
    property onstop:     tthreadmethod write fonstop;
    property ontick:     tthreadmethod write fontick;
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
    property count: longint read getcount;
    property tracks[index: longint]: ttrack read gettrack; default;
  end;

  function filesupported(fileext: string): boolean;

var
  audioanalyzer: ttrackanalyzer = nil;


implementation

uses
  dateutils;

const
  idriff = 'RIFF';
  idwave = 'WAVE';
  idfmt  = 'fmt ';
  iffact = 'fact';
  iddata = 'data';
  idlist = 'LIST';

// usefull routines

function filesupported(fileext: string): boolean;
begin
  fileext := lowercase(fileext);
  if fileext = '.wav'  then exit(true);
  if fileext = '.flac' then exit(true);
  if fileext = '.mp3'  then exit(true);
  if fileext = '.ape'  then exit(true);
  if fileext = '.ogg'  then exit(true);
  if fileext = '.m4a'  then exit(true);
  if fileext = '.ac3'  then exit(true);
  result := false;
end;

function comparetrackname(item1, item2: pointer): longint;
begin
  result := ansicomparefilename(ttrack(item1).ffilename, ttrack(item2).ffilename);
end;

// ttrack

constructor ttrack.create(const afilename: string);
begin
  inherited create;
  ffilename      := afilename;
  falbum         := '';
  fnumber        := 0;
  fsamplerate    := 0;
  fbitspersample := 0;
  fbyterate      := 0;
  fchannels      := nil;
end;

destructor ttrack.destroy;
begin
  clearchannels;
  inherited destroy;
end;

procedure ttrack.clearchannels;
begin
  setlength(fchannels, 0);
end;

// ttrackanalyzer

constructor ttrackanalyzer.create(atrack: ttrack; astream: tstream; aFFTOn: boolean);
begin
  fffton := affton;
  ftrack := atrack;
  fstream := astream;
  freeonterminate := true;
  inherited create(true);
end;

destructor ttrackanalyzer.destroy;
begin
  inherited destroy;
end;

procedure ttrackanalyzer.getspectrum(asamples: pdouble; count: longint; aspectrum: pfloat);
var
  i: longint;
  buff: tcompvector = nil;
  freq: tcompvector = nil;
  window: double;
begin
  if count > 0 then
  begin
    setlength(buff, count);
    for i := 0 to count -1 do
    begin
      window := 0.5 - 0.5 * cos(2 * pi * i / (count - 1));
      buff[i].x := window * asamples^;
      buff[i].y := 0;
      inc(asamples);
    end;
    freq := FFT(count, buff);

    // bin 0 (DC)
    aspectrum^ := abs(freq[0].x)/count;
    inc(aspectrum);
    // bin da 1 a N/2 - 1
    for i := 1 to (count div 2) -1 do
    begin
      aspectrum^ := 2*sqrt(sqr(freq[i].x) + sqr(freq[i].y))/count;
      inc(aspectrum);
    end;
    // bin N/2 (Nyquist), if N is even
    if (count mod 2 = 0) then
    begin
      aspectrum^ := abs(freq[count div 2].x)/count;
    //inc(aspectrum);
    end;

    freq := nil;
    buff := nil;
  end;
end;

function ttrackanalyzer.gettruepeak(channel: word): double;
begin
  case ftrack.samplerate of
    44100:  result := truepeak(ftrack.fchannels[channel], 4, 12);
    48000:  result := truepeak(ftrack.fchannels[channel], 4, 12);
    88200:  result := truepeak(ftrack.fchannels[channel], 2, 12);
    96000:  result := truepeak(ftrack.fchannels[channel], 2, 12);
    176400: result := getpeak(channel);
    192000: result := getpeak(channel);
    else    result := getpeak(channel);
  end;
end;

function ttrackanalyzer.getrms2(channel: word): double;
begin
  result := rms2(ftrack.fchannels[channel], 0, length(ftrack.fchannels[channel]));
end;

function ttrackanalyzer.getrms(channel: word): double;
begin
  result := rms(ftrack.fchannels[channel], 0, length(ftrack.fchannels[channel]));
end;

function ttrackanalyzer.getpeak(channel: word): double;
begin
  result := peak(ftrack.fchannels[channel], 0, length(ftrack.fchannels[channel]));
end;

procedure ttrackanalyzer.execute;
var
  i, j: longint;
  dr:       double = 0;
  rms:      double = 0;
  peak:     double = 0;
  truepeak: double = 0;
begin
  fpercentage := 0;
  if assigned(fonstart) then
    synchronize(fonstart);

  readfromstream(fstream);
  if fstatus = 0 then
    if ffmt.channels > 0 then
    begin
      for i := 0 to ffmt.channels -1 do
      begin
        ftrack.fdr      [i] := getdr      (i);
        ftrack.frms     [i] := getrms     (i);
        ftrack.fpeak    [i] := getpeak    (i);
        ftrack.ftruepeak[i] := gettruepeak(i);
        ftrack.flufs    [i] := lufs(ftrack.channels[i].samples, ftrack.samplerate);
        ftrack.flra     [i] := lra (ftrack.channels[i].samples, ftrack.samplerate);
        {$ifopt D+}
        writeln;
        writeln('track.DR  [', i,'] ',         ftrack.fdr      [i] :2:1);
        writeln('track.Rms [', i,'] ', decibel(ftrack.frms     [i]):2:2);
        writeln('track.Peak[', i,'] ', decibel(ftrack.fpeak    [i]):2:2);
        writeln('track.TPL [', i,'] ', decibel(ftrack.ftruepeak[i]):2:2);
        {$endif}
      end;

      for i := 0 to ffmt.channels -1 do
      begin
        dr       := dr   + ftrack.fdr  [i];
        rms      := rms  + ftrack.frms [i];
        peak     := peak + ftrack.fpeak[i];
        truepeak := max(truepeak, ftrack.ftruepeak[i]);
      end;
      dr       := dr  /ffmt.channels;
      rms      := rms /ffmt.channels;
      peak     := peak/ffmt.channels;
      {$ifopt D+}
      writeln;
      writeln('track.DR:          ',        (dr      ):2:1);
      writeln('track.Rms          ', decibel(rms     ):2:2);
      writeln('track.Peak         ', decibel(peak    ):2:2);
      writeln('track.TruePeak     ', decibel(truepeak):2:2);
      writeln;
      {$endif}
    end;

  fpercentage := 100;
  if assigned(fonstop) then
    synchronize(fonstop);
end;

procedure ttrackanalyzer.readfromstream(astream:tstream);
var
  i, j, k: longint;
  step, steps: longint;
  ticktime: tdatetime;
begin
  {$ifopt D+}
  writeln('track.name         ', ftrack.filename);
  {$endif}
  //read headers
  readheader(astream);
  if status <> 0 then exit;
  // update track details
  ftrack.falbum         := '';
  ftrack.fnumber        := 0;
  ftrack.fsamplerate    := ffmt.samplespersec;
  ftrack.fbitspersample := ffmt.bitspersample;
  ftrack.fchannelcount  := ffmt.channels;
  ftrack.fsamplerate    := ffmt.samplespersec;
  ftrack.fbyterate      := ffmt.bytespersec;
  ftrack.fduration      := 0;

  setlength(ftrack.fdr,       ffmt.channels);
  setlength(ftrack.frms,      ffmt.channels);
  setlength(ftrack.fpeak,     ffmt.channels);
  setlength(ftrack.ftruepeak, ffmt.channels);
  setlength(ftrack.flufs,     ffmt.channels);
  setlength(ftrack.flra,      ffmt.channels);

  if ftrack.fsamplerate > 0 then
  begin
    ftrack.fduration := (fdatachunk.subck2size div ffmt.blockalign) div ftrack.fsamplerate;
  end;
  fsamplecount := fdatachunk.subck2size div ffmt.blockalign;

  // read samplecount
  fblocknum  := 0;
  fblocksize := trunc(132480 / 44100 * ffmt.samplespersec);
  if fblocksize > 0 then
    fblocknum := fsamplecount div fblocksize;

  if fblocknum  = 0 then fstatus := -3;
  if fblocksize = 0 then fstatus := -3;
  if status <> 0 then exit;
  // allocate track samples, rms and peak
  setlength(ftrack.fchannels, ftrack.fchannelcount);
  for i := 0 to ftrack.fchannelcount -1 do
  begin
    setlength(ftrack.fchannels[i].samples, fsamplecount);
    setlength(ftrack.fchannels[i].rms2,    fblocknum);
    setlength(ftrack.fchannels[i].peak,    fblocknum);
  end;
  // allocate track samples spectrum
  for i := 0 to ftrack.channelcount -1 do
    setlength(ftrack.fchannels[i].spectrum, fsamplecount div 2);
  // calculate steps
  step  := 1;
  if fffton then
    steps := ftrack.fchannelcount*(fblocknum + fsamplecount div spectrumwindowsize)
  else
    steps := ftrack.fchannelcount*(fblocknum);
  // read samples
  readsamples(astream, ftrack.fchannels, fsamplecount);
  // calculate block rms and peak and db
  for j := 0 to ftrack.fchannelcount -1 do
    for i := 0 to fblocknum -1  do
    begin
      inc(step);
      fpercentage := 100*step/steps;
      if assigned(fontick) then
        synchronize(fontick);

      ftrack.fchannels[j].rms2[i] := getrms2(ftrack.fchannels[j].samples, i * fblocksize, fblocksize);
      ftrack.fchannels[j].peak[i] := getpeak(ftrack.fchannels[j].samples, i * fblocksize, fblocksize);
    end;

  // calculate spectrum (FFT)
  if fffton then
  begin
    ticktime := now;
    for i := 0 to ftrack.fchannelcount -1 do
    begin
      for j := 0 to (fsamplecount div spectrumwindowsize) -1 do
      begin
        inc(step);
        if millisecondsbetween(now, ticktime) > 20 then
        begin
          fpercentage := 100*step/steps;
          if assigned(fontick) then
            synchronize(fontick);
          ticktime := now;
        end;

        k := j * spectrumwindowsize;

        getspectrum(@ftrack.fchannels[i].samples[k], spectrumwindowsize, @ftrack.fchannels[i].spectrum[k div 2]);
      end;
    end;
  end;
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

function ttrackanalyzer.readsamples(astream: tstream; achannels: ttrackchannels; achannelsize: longint): longint;
var
  i, j, k: longint;
  dt : array[0..3] of byte;
begin
  result := 0;
  for i := 0 to achannelsize -1 do
  begin
    for j := 0 to ffmt.channels -1 do
    begin
      if ffmt.bitspersample = 8 then
      begin
        if astream.read(dt[0], 1) <> 1 then fstatus := -1;
        achannels[j].samples[i] := pbyte(@dt[0])^;
      end else
      if ffmt.bitspersample = 16 then
      begin
        if astream.read(dt[0], 2) <> 2 then fstatus := -1;
        achannels[j].samples[i] := psmallint(@dt[0])^;
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

        achannels[j].samples[i] := k;
      end else
      if ffmt.bitspersample = 32 then
      begin
        if astream.read(dt[0], 4) <> 4 then fstatus := -1;
        achannels[j].samples[i] := plongint(@dt[0])^;
      end;
    end;
  end;
  normalizesamples;
end;

procedure ttrackanalyzer.normalizesamples;
var
  i, j: longint;
  minvalue, maxvalue: double;
  meanvalue: double;
  norm: double;
begin
  norm := 1 shl (ftrack.fbitspersample -1);

  for i := low(ftrack.fchannels) to high(ftrack.fchannels) do
  begin
    minvalue :=  maxfloat;
    maxvalue := -maxfloat;
    for j := low(ftrack.fchannels[i].samples) to high(ftrack.fchannels[i].samples) do
    begin
      minvalue := min(minvalue, ftrack.fchannels[i].samples[j]);
      maxvalue := max(maxvalue, ftrack.fchannels[i].samples[j]);
    end;

    meanvalue := (maxvalue + minvalue) / 2;
    for j := low(ftrack.fchannels[i].samples) to high(ftrack.fchannels[i].samples) do
    begin
      ftrack.fchannels[i].samples[j] := (ftrack.fchannels[i].samples[j] - meanvalue) / norm;
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
var
  i: longint;
begin
  for i := 0 to flist.count -1 do
    ttrack(flist[i]).destroy;
  flist.clear;
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
  s.add('AudioMeter 0.5.0 - Dynamic Range Meter');
  s.add(splitter);
  s.add(format('Log date : %s', [datetimetostr(now)]));
  s.add(splitter);
  s.add('');

  dr := 0;
  if count > 0 then
  begin
    track := gettrack(0);
    s.add('DR      Peak        RMS     bps  chs      SR  Duration  Track');
    s.add(splitter);
    for i := 0 to count -1 do
    begin
      track := gettrack(i);
      s.add(format('DR%2.0f %7.2f dB %7.2f dB %4.0d %4.0d %7.0d %-s     %s', [
               (track.dr),
        decibel(track.peak),
        decibel(track.rms),
        track.bitspersample,
        track.channelcount,
        track.samplerate,
        format('%3.2d:%2.2d', [track.fduration div (60), track.fduration mod (60)]),
        extractfilename(track.ffilename)]));

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

