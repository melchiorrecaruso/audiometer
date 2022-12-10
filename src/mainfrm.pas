{
  Description: Main form.

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

unit mainfrm;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, buttons, stdctrls,
  extctrls, comctrls, tagraph, taseries, tasources, bufstream, soundwav,
  bcradialprogressbar, bclistbox, process, inifiles;

type
  { taudiofrm }

  taudiofrm = class(tform)
    btnfile: timage;
    btnfolder: timage;
    buttons: timagelist;
    Report: TImageList;
    peakseries: tbarseries;
    rmseries: tbarseries;
    progresspanel: tpanel;
    bevel1: tbevel;
    bevel2: tbevel;
    bevel3: tbevel;
    dbchart: tchart;
    khz48: tlabel;
    khz88: tlabel;
    khz96: tlabel;
    khz44: tlabel;
    bit8: tlabel;
    bit16: tlabel;
    bit24: tlabel;
    khz192: tlabel;
    khz176: tlabel;
    audio: tlabel;
    mono: tlabel;
    progressbar: tbcradialprogressbar;
    dirdialog: tselectdirectorydialog;
    drlb: tstatictext;
    drvalue: tstatictext;
    stereo: tlabel;
    peak: tlistchartsource;
    rms: tlistchartsource;
    filedialog: topendialog;
    procedure btnfolderclick(sender: tobject);
    procedure btnfoldermousedown(sender: tobject; button: tmousebutton;
      shift: tshiftstate; x, y: integer);
    procedure btnfoldermouseleave(sender: tobject);
    procedure btnfoldermousemove(sender: tobject;
     shift: tshiftstate; x, y: integer);
    procedure btnfoldermouseup(sender: tobject; button: tmousebutton;
      shift: tshiftstate; x, y: integer);
    procedure formclosequery(sender: tobject; var canclose: boolean);
    procedure formcreate(sender: tobject);
    procedure btnfileclick(sender: tobject);
    procedure btnfilemousedown(sender: tobject; button: tmousebutton;
      shift: tshiftstate; x, y: integer);
    procedure btnfilemouseleave(sender: tobject);
    procedure btnfilemousemove(sender: tobject;
      shift: tshiftstate; x, y: integer);
    procedure btnfilemouseup(sender: tobject; button: tmousebutton;
      shift: tshiftstate; x, y: integer);
    procedure formdestroy(sender: tobject);
    procedure onstart;
    procedure onstop;
    procedure onprogress;
    procedure clear;

    procedure btnloadicon(btn: timage; index: longint; x, y: longint);
    procedure execute;
  private
    buffer:     treadbufstream;
    stream:     tfilestream;
    trackindex: longint;
    trackkill:  boolean;
    tracklist:  ttracklist;
    trackfile:  string;
    tempfile:   string;
    wave:       ttrackanalyzer;
  public
  end;

var
  audiofrm: taudiofrm;

implementation

{$R *.lfm}

{ taudiofrm }

procedure taudiofrm.formcreate(sender: tobject);
begin
  trackindex := 0;
  trackkill  := false;
  tracklist  := ttracklist.create;
  // load openfile button icon
  btnloadicon(btnfile, 1, 0, 0);
  btnfile.onmousemove := @btnfilemousemove;
  // load openfolder button icon
  btnloadicon(btnfolder, 3, 0, 5);
  btnfile.onmousemove := @btnfilemousemove;
  // initialize chart
  dbchart.disableredrawing;
  // initialize progress bar
  progresspanel.visible := false;
  progressbar  .value   := 0;
  // inizialize mail form
  color := clblack;
  // inizialize
  clear;
end;

procedure taudiofrm.formdestroy(sender: tobject);
begin
  rms.clear;
  peak.clear;
  tracklist.destroy;
end;

procedure taudiofrm.formclosequery(sender: tobject; var canclose: boolean);
begin
  trackkill := true;
  canclose  := assigned(wave) = false;
end;

procedure taudiofrm.onstart;
begin
  clear;
  audio.font.color := clwhite;
  audio.caption    := extractfilename(tracklist.tracks[trackindex].name);

  btnfile      .enabled := false;
  btnfolder    .enabled := false;
  drvalue      .visible := false;
  progresspanel.visible := true;
  application.processmessages;
end;

procedure taudiofrm.onstop;
var
  i: longint;
  j: longint;
  mycapture: tbitmap;
  rmsi: double;
  peaki: double;
  track: ttrack;
begin
  freeandnil(buffer);
  freeandnil(stream);
  //if fileexists(tempfile) then
  //begin
    //deletefile(tempfile);
  //end;

  if wave.status <> 0 then
  begin
    audio.font.color := clred;
    audio.caption    := 'File format error!';
    trackindex       := tracklist.count;
  end else
  begin
    rms.clear;
    peak.clear;
    // load rms and peak
    track := tracklist.tracks[trackindex];
    if track.channelcount > 0 then
      for i := 0 to track.channels[0].count -1 do
      begin
        rmsi := 0;
        for j := 0 to track.channelcount -1 do
          rmsi := rmsi + track.channels[j].rms[i];
        rmsi := rmsi/track.channelcount;
        rms.add(i, rmsi);

        peaki := 0;
        for j := 0 to track.channelcount -1 do
          peaki := peaki + track.channels[j].peak[i];
        peaki := peaki/track.channelcount;
        peak.add(i, peaki);
      end;
    dbchart.bottomaxis.range.max := rms.count;
    dbchart.invalidate;

    bit8  .font.color := clgray; if track.bitspersample = 8      then bit8  .font.color := clwhite;
    bit16 .font.color := clgray; if track.bitspersample = 16     then bit16 .font.color := clwhite;
    bit24 .font.color := clgray; if track.bitspersample = 24     then bit24 .font.color := clwhite;

    khz44 .font.color := clgray; if track.samplerate    = 44100  then khz44 .font.color := clwhite;
    khz48 .font.color := clgray; if track.samplerate    = 48000  then khz48 .font.color := clwhite;
    khz88 .font.color := clgray; if track.samplerate    = 88000  then khz88 .font.color := clwhite;
    khz96 .font.color := clgray; if track.samplerate    = 96000  then khz96 .font.color := clwhite;
    khz176.font.color := clgray; if track.samplerate    = 176400 then khz176.font.color := clwhite;
    khz192.font.color := clgray; if track.samplerate    = 192000 then khz192.font.color := clwhite;

    mono  .font.color := clgray; if track.channelcount  = 1      then mono  .font.color := clwhite;
    stereo.font.color := clgray; if track.channelcount  = 2      then stereo.font.color := clwhite;

    drvalue.caption    := '--';
    drvalue.font.color := clwhite;
    if track.dr > 0 then
    begin
      drvalue.caption := inttostr(round(track.dr));
      if round(track.dr) >= 14 then drvalue.font.color := rgbtocolor(  0, 255, 0);
      if round(track.dr) =  13 then drvalue.font.color := rgbtocolor( 72, 255, 0);
      if round(track.dr) =  12 then drvalue.font.color := rgbtocolor(144, 255, 0);
      if round(track.dr) =  11 then drvalue.font.color := rgbtocolor(217, 255, 0);
      if round(track.dr) =  10 then drvalue.font.color := rgbtocolor(255, 217, 0);
      if round(track.dr) =   9 then drvalue.font.color := rgbtocolor(255, 145, 0);
      if round(track.dr) =   8 then drvalue.font.color := rgbtocolor(255,  72, 0);
      if round(track.dr) <=  7 then drvalue.font.color := rgbtocolor(255,   0, 0);
    end;
    btnfile      .enabled := true;
    btnfolder    .enabled := true;
    drvalue      .visible := true;
    progresspanel.visible := false;
    progressbar  .value   := 0;
  end;
  wave := nil;

  mycapture := tbitmap.create;
  mycapture.setsize(446, 146);
  mycapture.canvas.fillrect(0, 0, 446, 146);
  begin
    borderstyle := bsnone;
    paintto(mycapture.canvas, 0, 0);
    borderstyle := bssizeable;
    application.processmessages;
  end;
  report.add(mycapture, nil);
  mycapture.free;

  inc(trackindex);
  if trackindex = tracklist.count then
  begin
    // save text report
    tracklist.savetofile(trackfile);
    // save png report
    mycapture := tbitmap.create;
    mycapture.setsize(446, report.count*146);
    mycapture.canvas.fillrect(0, 0, 446, report.count*146);
    for i := 0 to report.count -1 do
    begin
      report.draw(mycapture.canvas, 0, i*146, i);
    end;
    mycapture.savetofile(changefileext(trackfile, '.png'));
    mycapture.destroy;
    report.clear;
  end;
  sleep(1000);
  execute;
end;

procedure taudiofrm.onprogress;
begin
  progressbar.value := wave.percentage;
  application.processmessages;
end;

procedure taudiofrm.clear;
begin
  bit8  .font.color := clgray;
  bit16 .font.color := clgray;
  bit24 .font.color := clgray;
  khz44 .font.color := clgray;
  khz48 .font.color := clgray;
  khz88 .font.color := clgray;
  khz96 .font.color := clgray;
  khz176.font.color := clgray;
  khz192.font.color := clgray;
  mono  .font.color := clgray;
  stereo.font.color := clgray;

  rms .clear;
  peak.clear;
  audio.caption      := 'Audio';
  audio.font.color   := clwhite;
  drvalue.caption    := '--';
  drvalue.font.color := clwhite;
  dbchart.invalidate;
end;

procedure taudiofrm.execute;
var
  buf: array[0..4095] of byte;
  bit4sample: longint;
  i: longint;
  ini: tinifile;
  mem: tmemorystream;
  process: tprocess;
  track:   ttrack;
begin
  if trackkill then exit;
  if trackindex >= tracklist.count then exit;

  drvalue.visible       := false;
  progresspanel.visible := false;

  track := tracklist.tracks[trackindex];
  try
    if extractfileext(track.name) <> '.wav' then
    begin
      tempfile := includetrailingbackslash(
        gettempdir(false)) + 'audiometer-tmp.wav';

      process := tprocess.create(nil);
      try
        process.parameters.clear;
        process.currentdirectory := extractfiledir(track.name);
        process.executable := 'ffprobe';
        process.parameters.add('-show_streams');
        process.parameters.add('-hide_banner');
        process.parameters.add('-print_format');
        process.parameters.add('ini');
        process.parameters.add(extractfilename(track.name));
        process.options := [ponoconsole, pousepipes];
        process.execute;

        mem := tmemorystream.create;
        while (process.running) or
              (process.output.numbytesavailable > 0) or
              (process.stderr.numbytesavailable > 0) do
        begin
          while process.output.numbytesavailable > 0 do
            mem.write(buf, process.output.read(buf, sizeof(buf)));
          while process.stderr.numbytesavailable > 0 do
            process.stderr.read(buf, sizeof(buf));
          application.processmessages;
        end;
        mem.seek(0, sofrombeginning);

        ini := tinifile.create(mem, [ifostripcomments, ifostripinvalid]);

        i := 0;
        bit4sample := 0;
        while ini.sectionexists('streams.stream.' + inttostr(i)) do
        begin
          if ini.readstring('streams.stream.' + inttostr(i), 'codec_type', '') = 'audio' then
          begin
            bit4sample := ini.readinteger('streams.stream.' + inttostr(i), 'bits_per_raw_sample',  bit4sample);
          end;
          inc(i);
        end;
        ini.destroy;
        mem.destroy;
      except
      end;
      process.destroy;

      process := tprocess.create(nil);
      try
        process.parameters.clear;
        process.currentdirectory := extractfiledir(track.name);
        process.executable := 'ffmpeg';
        process.parameters.add('-y');
        process.parameters.add('-hide_banner');
        process.parameters.add('-i');
        process.parameters.add(extractfilename(track.name));

        if bit4sample = 24 then
        begin
          process.parameters.add('-c:a');
          process.parameters.add('pcm_s24le');
        end;

        if bit4sample = 32 then
        begin
          process.parameters.add('-c:a');
          process.parameters.add('pcm_s24le');
        end;

        process.parameters.add(tempfile);
        process.options := [ponoconsole, powaitonexit];
        process.execute;
      except
      end;
      process.destroy;
    end else
      tempfile := track.name;

    stream := tfilestream.create(tempfile, fmopenread or fmshareexclusive);
  except
    stream := nil;
  end;

  if assigned(stream) then
  begin
    buffer := treadbufstream.create(stream);
    wave   := ttrackanalyzer.create(track, buffer);
    wave.onstart    := @onstart;
    wave.onstop     := @onstop;
    wave.onprogress := @onprogress;
    wave.start;
  end else
  begin
    messagedlg('AudioMeter', format('Error to open file "%s"', [tempfile]), mterror, [mbok], '');
    inc(trackindex);
    execute;
  end;
end;

procedure taudiofrm.btnfileclick(sender: tobject);
begin
  tracklist.clear;
  if filedialog.execute then
  begin
    if filesupported(extractfileext(filedialog.filename)) then
    begin
      tracklist.add(filedialog.filename);
      trackfile  := changefileext(filedialog.filename, '.md');
      trackkill  := false;
      trackindex := 0;
    end else
    begin
      audio.caption    := 'File format error!';
      audio.font.color := clred;
    end;
  end;
  execute;
end;

procedure taudiofrm.btnfolderclick(sender: tobject);
var
  err:  longint;
  path: string;
  sr:   tsearchrec;
begin
  tracklist.clear;
  if dirdialog.execute then
  begin
    path := includetrailingbackslash(dirdialog.filename);
     err := sysutils.findfirst(path + '*.*', faanyfile, sr);
    while err = 0 do
    begin
      if sr.attr and (fadirectory) = 0 then
      begin
        if filesupported(extractfileext(sr.name)) then
          tracklist.add(path + sr.name);
      end;
      err := findnext(sr);
    end;
    sysutils.findclose(sr);
    tracklist.sort;
    trackfile  := path + extractfilename(dirdialog.filename) + '.md';
    trackkill  := false;
    trackindex := 0;
  end;
  execute;
end;

procedure taudiofrm.btnfilemouseup(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  btnfile.onmousemove := @btnfilemousemove;
end;

procedure taudiofrm.btnfilemousedown(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  btnfile.onmousemove := nil;
  btnloadicon(btnfile, 1, 0, 0);
end;

procedure taudiofrm.btnfilemouseleave(sender: tobject);
begin
  btnloadicon(btnfile, 1, 0, 0);
end;

procedure taudiofrm.btnfilemousemove(sender: tobject;
  shift: tshiftstate; x, y: integer);
begin
  btnloadicon(btnfile, 0, 0, 0);
end;

procedure taudiofrm.btnfoldermousedown(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  btnfolder.onmousemove := nil;
  btnloadicon(btnfolder, 3, 0, 5);
end;

procedure taudiofrm.btnfoldermouseleave(sender: tobject);
begin
  btnloadicon(btnfolder, 3, 0, 5);
end;

procedure taudiofrm.btnfoldermousemove(sender: tobject;
  shift: tshiftstate; x, y: integer);
begin
  btnloadicon(btnfolder, 2, 0, 5);
end;

procedure taudiofrm.btnfoldermouseup(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  btnfolder.onmousemove := @btnfoldermousemove;
end;

procedure taudiofrm.btnloadicon(btn: timage; index: longint; x, y: longint);
begin
  btn.center         := false;
  btn.stretch        := false;
  btn.proportional   := true;
  btn.picture.bitmap := nil;
  buttons.draw(btn.canvas, x, y, index);
end;

end.

