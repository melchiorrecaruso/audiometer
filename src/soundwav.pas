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

unit Soundwav;

{$mode objfpc}

interface

uses
  classes, common, dynamicrange, spectrum, sysutils, math, loudness, ufft, utypes;

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

  // ttrack

  ttrack = class
  private
    ffilename: string;
    falbum: string;
    fnumber: longint;
    fsamplerate: longword;
    fsamplecount: longint;
    fchannels: tchannels;
    fchannelcount: longint;
    fbitspersample: longint;
    fbyterate: longint;
    fduration: longint;
    // dynamicrange
    fdrmeter: TDynamicrangerMeter;
    // spectrum
    fspectrums: TSpectrums;
    // loudness
    floudness: TLoudnessMeter;

    function getsample(channel, index: longint): TSample;
  public
    constructor create(const afilename: string);
    destructor destroy; override;
    procedure clearchannels;
  public
    property filename: string read ffilename;
    property album: string read falbum;
    property number: longint read fnumber;
    property samplecount: longint read fsamplecount;
    property samplerate: longword read fsamplerate;
    property channelcount: longint read fchannelcount;
    property bitspersample: longint read fbitspersample;
    property byterate: longint read fbyterate;
    property duration: longint read fduration;

    property drmeter: TDynamicrangerMeter read fdrmeter;
    property spectrums: tspectrums read fspectrums;
    property loudness: TLoudnessMeter read floudness;

    property samples[channel, index: longint]: TSample read getsample;
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
    fchannels: tchannels;


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

    procedure PrepareSamplesForAnalysis;
    procedure clearsamples;
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

  fDRmeter  := TDynamicrangerMeter.Create;
  fLoudness := TLoudnessMeter.Create;
end;

destructor ttrack.destroy;
begin
  fdrmeter.Destroy;
  floudness.destroy;
  inherited destroy;
end;

function ttrack.getsample(channel, index: longint): TSample;
begin
  result := fchannels[channel][index];
end;

procedure ttrack.clearchannels;
var
  ch: longint;
begin
  for ch := low(fchannels) to high(fchannels) do
    setlength(fchannels[ch], 0);
  setlength(fchannels, 0);
end;

// ttrackanalyzer

constructor ttrackanalyzer.create(atrack: ttrack; astream: tstream; aFFTOn: boolean);
begin
  fffton := affton;
  ftrack := atrack;
  fstream := astream;
  freeonterminate := false;

  inherited create(true);
end;

destructor ttrackanalyzer.destroy;
begin
  inherited destroy;
end;

procedure ttrackanalyzer.execute;
var
  i, j: longint;
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
        (*
        ftrack.fchannels[i].dr       := getdr(i);
        ftrack.fchannels[i].rms      := getrms(i);
        ftrack.fchannels[i].peak     := getpeak(i);
        ftrack.fchannels[i].truepeak := gettruepeak(i);
        ftrack.fchannels[i].lufs     := lufs(ftrack.fchannels[i].samples, ftrack.samplerate);
        ftrack.fchannels[i].lra      := lra (ftrack.fchannels[i].samples, ftrack.samplerate);
        {$ifopt D+}
        writeln;
        writeln('track.DR  [', i,'] ',         ftrack.fchannels[i].dr       :2:1);
        writeln('track.Rms [', i,'] ', decibel(ftrack.fchannels[i].rms     ):2:2);
        writeln('track.Peak[', i,'] ', decibel(ftrack.fchannels[i].peak    ):2:2);
        writeln('track.TPL [', i,'] ', decibel(ftrack.fchannels[i].truepeak):2:2);
        {$endif}
        *)
      end;

      {$ifopt D+}
      writeln;
      writeln('track.DR:          ',        (ftrack.drmeter.DR       ):2:1);
      writeln('track.Rms          ', Decibel(ftrack.drmeter.Rms      ):2:2);
      writeln('track.Peak         ', Decibel(ftrack.drmeter.Peak     ):2:2);
      writeln('track.TruePeak     ', Decibel(ftrack.loudness.truepeak):2:2);
      writeln;
      {$endif}
    end;

  fpercentage := 100;
  if assigned(fonstop) then
    synchronize(fonstop);
end;

procedure ttrackanalyzer.readfromstream(astream:tstream);
var
  ch, i, j, k: longint;
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

  if ftrack.fsamplerate > 0 then
  begin
    ftrack.fduration := (fdatachunk.subck2size div ffmt.blockalign) div ftrack.fsamplerate;
  end;
  fsamplecount := fdatachunk.subck2size div ffmt.blockalign;

  setlength(fchannels, ftrack.fchannelcount);
  for ch := low(fchannels) to high(fchannels) do
  begin
    setlength(fchannels[ch], fsamplecount);
  end;
  readsamples(astream, fchannels, fsamplecount);

  ftrack.fdrmeter .Analyze(fchannels, fsamplecount, ftrack.fsamplerate);
  ftrack.floudness.Analyze(fchannels, fsamplecount, ftrack.fsamplerate);

  // calculate spectrum (FFT)
  if fffton then
  begin



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

function ttrackanalyzer.readsamples(astream: tstream; achannels: tchannels; achannelsize: longint): longint;
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
        achannels[j][i] := pbyte(@dt[0])^;
      end else
      if ffmt.bitspersample = 16 then
      begin
        if astream.read(dt[0], 2) <> 2 then fstatus := -1;
        achannels[j][i] := psmallint(@dt[0])^;
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

        achannels[j][i] := k;
      end else
      if ffmt.bitspersample = 32 then
      begin
        if astream.read(dt[0], 4) <> 4 then fstatus := -1;
        achannels[j][i] := plongint(@dt[0])^;
      end;
    end;
  end;
  PrepareSamplesForAnalysis;
end;

procedure ttrackanalyzer.PrepareSamplesForAnalysis;
var
  i, j: longint;
  minvalue, maxvalue: double;
  meanvalue: double;
  norm: double;
begin
  norm := 1 shl (ftrack.fbitspersample -1);

  for i := low(fchannels) to high(fchannels) do
  begin
    minvalue :=  maxfloat;
    maxvalue := -maxfloat;
    for j := low(fchannels[i]) to high(fchannels[i]) do
    begin
      minvalue := min(minvalue, fchannels[i][j]);
      maxvalue := max(maxvalue, fchannels[i][j]);
    end;

    meanvalue := (maxvalue + minvalue) / 2;
    for j := low(fchannels[i]) to high(fchannels[i]) do
    begin
      fchannels[i][j] := (fchannels[i][j] - meanvalue) / norm;
    end;
  end;
end;

procedure ttrackanalyzer.clearsamples;
var
  ch: longint;
begin
  for ch := low(fchannels) to high(fchannels) do
  begin
    setlength(fchannels[ch], 0);
  end;
  setlength(fchannels, 0);
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
               (track.drmeter.DR),
        decibel(track.drmeter.Peak),
        decibel(track.drmeter.Rms),
        track.bitspersample,
        track.channelcount,
        track.samplerate,
        format('%3.2d:%2.2d', [track.fduration div (60), track.fduration mod (60)]),
        extractfilename(track.ffilename)]));

      dr := dr + track.drmeter.DR;
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

