{
  Description: Main form.

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

unit mainfrm;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, forms, controls, graphics, dialogs, buttons, stdctrls,
  extctrls, comctrls, tagraph, taseries, tasources, bufstream, soundwav,
  bcradialprogressbar, bclistbox;

type
  { taudiofrm }

  taudiofrm = class(tform)
    btnfile: timage;
    btnfolder: timage;
    buttons: timagelist;
    report: TImageList;
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
    procedure btnfoldermousemove(sender: tobject; shift: tshiftstate; x,
      y: integer);
    procedure btnfoldermouseup(sender: tobject; button: tmousebutton;
      shift: tshiftstate; x, y: integer);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure formcreate(sender: tobject);
    procedure btnfileclick(sender: tobject);
    procedure btnfilemousedown(sender: tobject; button: tmousebutton;
      shift: tshiftstate; x, y: integer);
    procedure btnfilemouseleave(sender: tobject);
    procedure btnfilemousemove(sender: tobject; shift: tshiftstate; x, y: integer);
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
    filereport: string;
    filenames:  tstringlist;
    buffer: treadbufstream;
    stream: tfilestream;
    wave:   twavereader;
  public

  end;

var
  audiofrm: taudiofrm;

implementation

{$R *.lfm}

{ taudiofrm }

procedure taudiofrm.formcreate(sender: tobject);
begin
  filenames := tstringlist.create;
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
  filenames.destroy;
end;

procedure taudiofrm.formclosequery(sender: tobject; var canclose: boolean);
begin
  canclose := assigned(wave) = false;
  while filenames.count > 1 do
  begin
    filenames.delete(1);
  end;
end;

procedure taudiofrm.onstart;
begin
  clear;
  audio.caption    := extractfilename(filenames[0]);
  audio.font.color := clwhite;

  btnfile      .enabled := false;
  btnfolder    .enabled := false;
  drvalue      .visible := false;
  progresspanel.visible := true;
  application.processmessages;
end;

procedure taudiofrm.onstop;
var
  i: longint;
  mycapture: tbitmap;
begin
  dbchart.bottomaxis.range.max := rms.count div wave.channels;
  dbchart.invalidate;

  bit8 .font.color := clgray;
  bit16.font.color := clgray;
  bit24.font.color := clgray;
  if wave.bitspersample = 8  then bit8  .font.color := clwhite;
  if wave.bitspersample = 16 then bit16 .font.color := clwhite;
  if wave.bitspersample = 24 then bit24 .font.color := clwhite;

  khz44 .font.color := clgray;
  khz48 .font.color := clgray;
  khz88 .font.color := clgray;
  khz96 .font.color := clgray;
  khz176.font.color := clgray;
  khz192.font.color := clgray;
  if wave.samplerate = 44100  then khz44  .font.color := clwhite;
  if wave.samplerate = 48000  then khz48  .font.color := clwhite;
  if wave.samplerate = 88000  then khz88  .font.color := clwhite;
  if wave.samplerate = 96000  then khz96  .font.color := clwhite;
  if wave.samplerate = 176400 then khz176 .font.color := clwhite;
  if wave.samplerate = 192000 then khz192 .font.color := clwhite;

  mono  .font.color := clgray;
  stereo.font.color := clgray;
  if wave.channels = 1 then mono  .font.color := clwhite;
  if wave.channels = 2 then stereo.font.color := clwhite;

  drvalue.font.color := clwhite;
  drvalue.caption    := '--';
  if wave.dravg > 0 then
  begin
    drvalue.caption := inttostr(wave.dravg);
    if wave.dravg >= 14 then drvalue.font.color := rgbtocolor(  0, 255, 0);
    if wave.dravg =  13 then drvalue.font.color := rgbtocolor( 72, 255, 0);
    if wave.dravg =  12 then drvalue.font.color := rgbtocolor(144, 255, 0);
    if wave.dravg =  11 then drvalue.font.color := rgbtocolor(217, 255, 0);
    if wave.dravg =  10 then drvalue.font.color := rgbtocolor(255, 217, 0);
    if wave.dravg =   9 then drvalue.font.color := rgbtocolor(255, 145, 0);
    if wave.dravg =   8 then drvalue.font.color := rgbtocolor(255,  72, 0);
    if wave.dravg <=  7 then drvalue.font.color := rgbtocolor(255,   0, 0);
  end;
  buffer.destroy;
  stream.destroy;

  btnfile      .enabled := true;
  btnfolder    .enabled := true;
  drvalue      .visible := true;
  progresspanel.visible := false;
  progressbar  .value   := 0;
  application.processmessages;

  if wave.status <> 0 then
  begin
    audio.caption    := 'File format error!';
    audio.font.color := clred;
  end;

  mycapture := tbitmap.create;
  mycapture.setsize(446, 146);
  mycapture.canvas.fillrect(0, 0, 446, 146);
  paintto(mycapture.canvas, 0, 0);
  report.add(mycapture, nil);
  mycapture.free;

  wave := nil;
  filenames.delete(0);
  if filenames.count > 0 then
  begin
    sleep(1600);
    execute;
  end else
  begin
    mycapture := tbitmap.create;
    mycapture.setsize(446, report.count*146);
    mycapture.canvas.fillrect(0, 0, 446, report.count*146);
    for i := 0 to report.count -1 do
    begin
      report.draw(mycapture.canvas, 0, i*146, i);
    end;
    mycapture.savetofile(filereport);
    mycapture.destroy;
    report.clear;
  end;
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
  audio.caption      := ' Audio';
  audio.font.color   := clwhite;
  drvalue.caption    := '--';
  drvalue.font.color := clwhite;
  dbchart.invalidate;
end;

procedure taudiofrm.execute;
begin
  if filenames.count > 0 then
  begin;
    drvalue.visible       := false;
    progresspanel.visible := false;
    stream := nil;
    try
      stream := tfilestream.create(filenames[0], fmopenread or fmshareexclusive);
    except
      stream := nil;
    end;

    if stream <> nil then
    begin
      buffer  := treadbufstream.create(stream);
      wave := twavereader.create(buffer);
      wave.rmssources  := rms;
      wave.peaksources := peak;
      wave.onstart     := @onstart;
      wave.onstop      := @onstop;
      wave.onprogress  := @onprogress;
      wave.start;
    end else
    begin
      showmessage('Error to open file');
      filenames.delete(0);
      execute;
    end;
  end;
end;

procedure taudiofrm.btnfileclick(sender: tobject);
begin
  if filedialog.execute then
  begin
    filenames.add(filedialog.filename);
    filereport := extractfilepath(filenames[0]) + 'audiometer.png';
  end;
  execute;
end;

procedure taudiofrm.btnfolderclick(sender: tobject);
var
  err:  longint;
  path: string;
  sr:   tsearchrec;
begin
  if dirdialog.execute then
  begin
    path := includetrailingbackslash(dirdialog.filename);
     err := sysutils.findfirst(path + '*.wav',
       fareadonly  or fahidden  or fasysfile or favolumeid or
       fadirectory or faarchive or fasymlink or faanyfile, sr);
    while err = 0 do
    begin
      if sr.attr and (fasysfile or favolumeid) = 0 then
      begin
        if sr.attr and fadirectory = 0 then
          filenames.add(path + sr.name);
      end;
      err := findnext(sr);
    end;
    sysutils.findclose(sr);
    filenames.sort;
    filereport := path + 'audiometer.png';
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

