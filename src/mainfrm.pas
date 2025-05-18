{
  Description: Main form.

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

unit mainfrm;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, uplaysound, forms, controls, graphics, dialogs, buttons,
  stdctrls, extctrls, comctrls, bufstream, soundwav, bcradialprogressbar,
  bclistbox, bcbutton, process, inifiles, bgrabitmap, bgrabitmaptypes, bctypes,
  bgravirtualscreen, drawers;

type
  { taudiofrm }

  taudiofrm = class(tform)
    panelprogressbar: TBCRadialProgressBar;
    screen: TBGRAVirtualScreen;
    blocksbtn: TBCButton;

    btnplay: TImage;
    playsound: Tplaysound;
    timer: TTimer;
    wavebtn: TBCButton;
    spectrumbtn: tbcbutton;
    spectrogrambtn: tbcbutton;
    bevel1: tbevel;
    bevel2: tbevel;
    bevel3: tbevel;
    bit16: tlabel;
    bit24: tlabel;
    bit8: tlabel;
    btnfile: timage;
    btnfolder: timage;
    buttons: timagelist;
    drlb: tstatictext;
    drvalue: tstatictext;
    khz176: tlabel;
    khz192: tlabel;
    khz44: tlabel;
    khz48: tlabel;
    khz88: tlabel;
    khz96: tlabel;
    detailspanel: tpanel;
    bitspanel: tpanel;
    lefthzpanel: tpanel;
    mono: tlabel;
    drpanel: tpanel;
    progressbar: tbcradialprogressbar;
    progresspanel: tpanel;
    righthzpanel: tpanel;
    stereo: tlabel;
    report: timagelist;
    audio: tlabel;
    dirdialog: tselectdirectorydialog;
    filedialog: topendialog;

    procedure formcreate(sender: tobject);
    procedure formclosequery(sender: tobject; var canclose: boolean);
    procedure formresize(sender: tobject);
    procedure formdestroy(sender: tobject);
    // file button
    procedure btnfileclick(sender: tobject);
    procedure btnfilemousedown(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    procedure btnfilemouseleave(sender: tobject);
    procedure btnfilemousemove(sender: tobject; shift: tshiftstate; x, y: integer);
    procedure btnfilemouseup(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    // folder button
    procedure btnfolderclick(sender: tobject);
    procedure btnfoldermousedown(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    procedure btnfoldermouseleave(sender: tobject);
    procedure btnfoldermousemove(sender: tobject; shift: tshiftstate; x, y: integer);
    procedure btnfoldermouseup(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    // play button
    procedure btnplayclick(sender: tobject);
    procedure btnplaymousedown(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    procedure btnplaymouseleave(sender: tobject);
    procedure btnplaymousemove(sender: tobject; shift: tshiftstate; x, y: integer);
    procedure btnplaymouseup(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    // panel buttons
    procedure panelbtnclick(sender: tobject);


    procedure loadbuttonicon(btn: timage; index: longint; x, y: longint);
    procedure screenredraw(Sender: TObject; Bitmap: TBGRABitmap);

    procedure clear;
    procedure execute;

    procedure onstartanalyzer;
    procedure ontickanalyzer;
    procedure onstopanalyzer;

    procedure onstartdrawer;
    procedure ontickdrawer;
    procedure onstopdrawer;

    procedure timerstarttimer(sender: tobject);
    procedure timerstoptimer(sender: tobject);
    procedure timertimer(sender: tobject);

    procedure disablebuttons;
    procedure enablebuttons;
    procedure disablepanel;
    procedure enablepanel;
  private
    screens:     array[0..3] of tbgrabitmap;
    buffer:      treadbufstream;
    stream:      tfilestream;
    trackindex:  longint;
    isneededkillanalyzer:   boolean;
    tracklist:   ttracklist;
    trackfile:   string;
    track:       ttrack;
    tempfile:    string;
    audioanalyzer: ttrackanalyzer;
    applicationisworking: boolean;

    lastheight:  longint;
    lastwidth:   longint;
    pageindex:   longint;
  public
  end;

var
  audiofrm: taudiofrm;

implementation

{$R *.lfm}

uses
  math, fileutil;

function cutoff(const s: string): string;
begin
  result := s;
  setlength(result, max(0, length(result) - 4));
  result := result + '...';
end;

{ taudiofrm }

procedure taudiofrm.formcreate(sender: tobject);
var
  i: longint;
begin
  applicationisworking := false;
  isneededkillanalyzer := false;
  for i := low(screens) to high(screens) do
    screens[i] := tbgrabitmap.create;
  // ---
  track := nil;
  trackindex := 0;
  tracklist  := ttracklist.create;
  // load openfile button icon
  loadbuttonicon(btnfile, 1, 0, 0);
  btnfile.onmousemove := @btnfilemousemove;
  // load openfolder button icon
  loadbuttonicon(btnfolder, 3, 0, 5);
  btnfile.onmousemove := @btnfilemousemove;
  // initialize progress bar
  progresspanel.visible := false;
  progressbar.value := 0;
  // inizialize main form
  color := clblack;
  // initialize buttons
  blocksbtn.statenormal .fontex.shadow := false;
  blocksbtn.statehover  .fontex.shadow := false;
  blocksbtn.stateclicked.fontex.shadow := false;
  spectrogrambtn.statenormal .fontex.shadow := false;
  spectrogrambtn.statehover  .fontex.shadow := false;
  spectrogrambtn.stateclicked.fontex.shadow := false;
  spectrumbtn.statenormal .fontex.shadow := false;
  spectrumbtn.statehover  .fontex.shadow := false;
  spectrumbtn.stateclicked.fontex.shadow := false;
  wavebtn.statenormal .fontex.shadow := false;
  wavebtn.statehover  .fontex.shadow := false;
  wavebtn.stateclicked.fontex.shadow := false;
  // initialize
  clear;
  //
  panelbtnclick(blocksbtn);
end;

procedure taudiofrm.formdestroy(sender: tobject);
var
  i: longint;
begin
  tracklist.destroy;
  for i := low(screens) to high(screens) do
  begin
    screens[i].destroy;
  end;
end;

procedure taudiofrm.formclosequery(sender: tobject; var canclose: boolean);
begin
  isneededkillanalyzer := true;
  canclose := not applicationisworking;
end;

procedure taudiofrm.formresize(sender: tobject);
begin
  while (audio.left + audio.width) > (btnfolder.left + btnfolder.width) do
  begin
    audio.caption := cutoff(audio.caption);
  end;
  timer.enabled := true;
end;

// track analyzer events

procedure taudiofrm.onstartanalyzer;
begin
  writeln('taudiofrm.onstartanalyzer');
  applicationisworking := true;

  clear;
  audio.font.color := clwhite;
  audio.caption    := extractfilename(tracklist.tracks[trackindex].filename);
  while (audio.left + audio.width) > (btnfolder.left + btnfolder.width) do
  begin
    audio.caption := cutoff(audio.caption);
  end;
  disablebuttons;
  //application.processmessages;
end;

procedure taudiofrm.ontickanalyzer;
begin
  //writeln('taudiofrm.ontickanalyzer');

  progressbar.value := audioanalyzer.percentage;
  //application.processmessages;
end;

procedure taudiofrm.onstopanalyzer;
begin
  writeln('taudiofrm.onstopanalyzer.start');
  freeandnil(buffer);
  freeandnil(stream);

  if audioanalyzer.status <> 0 then
  begin
    trackindex := tracklist.count;
    audio.font.color := clred;
    case audioanalyzer.status of
      -1: audio.caption := 'file format error!';
      -2: audio.caption := 'file is empty!';
      -3: audio.caption := 'file is too short!';
    else  audio.caption := 'unknown error!';
    end;
    enablebuttons;
  end else
  begin
    track := tracklist.tracks[trackindex];

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
      drvalue.caption := format('%2.0f', [track.dr]);
      if drvalue.caption = ' 0' then drvalue.font.color := rgbtocolor(255,   0, 0) else
      if drvalue.caption = ' 1' then drvalue.font.color := rgbtocolor(255,   0, 0) else
      if drvalue.caption = ' 2' then drvalue.font.color := rgbtocolor(255,   0, 0) else
      if drvalue.caption = ' 3' then drvalue.font.color := rgbtocolor(255,   0, 0) else
      if drvalue.caption = ' 4' then drvalue.font.color := rgbtocolor(255,   0, 0) else
      if drvalue.caption = ' 5' then drvalue.font.color := rgbtocolor(255,   0, 0) else
      if drvalue.caption = ' 6' then drvalue.font.color := rgbtocolor(255,   0, 0) else
      if drvalue.caption = ' 7' then drvalue.font.color := rgbtocolor(255,   0, 0) else
      if drvalue.caption = ' 8' then drvalue.font.color := rgbtocolor(255,  72, 0) else
      if drvalue.caption = ' 9' then drvalue.font.color := rgbtocolor(255, 145, 0) else
      if drvalue.caption = '10' then drvalue.font.color := rgbtocolor(255, 217, 0) else
      if drvalue.caption = '11' then drvalue.font.color := rgbtocolor(217, 255, 0) else
      if drvalue.caption = '12' then drvalue.font.color := rgbtocolor(144, 255, 0) else
      if drvalue.caption = '13' then drvalue.font.color := rgbtocolor( 72, 255, 0) else
                                     drvalue.font.color := rgbtocolor(  0, 255, 0);
    end;
    enablebuttons;
  end;
  //application.processmessages;
  applicationisworking := false;
  audioanalyzer := nil;

  inc(trackindex);
  if trackindex = tracklist.count then
  begin
    // save text report
    tracklist.savetofile(trackfile);
    // updating screens
    timer.enabled := true;
  end else
  begin
    tracklist.tracks[trackindex-1].clearchannels;
  end;
  execute;
  writeln('taudiofrm.onstopanalyzer.stop');
end;

// drawer events

procedure taudiofrm.onstartdrawer;
begin
  disablepanel;
  applicationisworking := true;
  //application.processmessages;
end;

procedure taudiofrm.ontickdrawer;
begin
  panelprogressbar.value := panelprogressbar.value + 20;
  //application.processmessages;
end;

procedure taudiofrm.onstopdrawer;
begin
  writeln('onstopdrawer.begin');
  applicationisworking := false;
  screen.redrawbitmap;
  enablepanel;

  drawer := nil;
  //application.processmessages;
  writeln('onstopdrawer.end');
end;

//

procedure taudiofrm.execute;
var
  buf: array[0..4095] of byte;
  bit4sample: longint;
  i: longint;
  ini: tinifile;
  mem: tmemorystream;
  process: tprocess;
begin
  writeln('taudiofrm.execute');

  if isneededkillanalyzer then exit;
  if trackindex >= tracklist.count then exit;

  track := tracklist.tracks[trackindex];
  try
    if extractfileext(track.filename) <> '.wav' then
    begin
      tempfile := includetrailingbackslash(
        gettempdir(false)) + 'audiometer-tmp.wav';

      // get file properties
      process := tprocess.create(nil);
      try
        process.parameters.clear;
        process.currentdirectory := extractfiledir(track.filename);
        process.executable := 'ffprobe';
        process.parameters.add('-show_streams');
        process.parameters.add('-hide_banner');
        process.parameters.add('-print_format');
        process.parameters.add('ini');
        process.parameters.add(extractfilename(track.filename));
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

      // decode to .audioanalyzer
      process := tprocess.create(nil);
      try
        process.parameters.clear;
        process.currentdirectory := extractfiledir(track.filename);
        process.executable := 'ffmpeg';
        process.parameters.add('-y');
        process.parameters.add('-hide_banner');
        process.parameters.add('-i');
        process.parameters.add(extractfilename(track.filename));

        if bit4sample = 24 then
        begin
          process.parameters.add('-c:a');
          process.parameters.add('pcm_s24le');
        end;

        if bit4sample = 32 then
        begin
          process.parameters.add('-c:a');
          process.parameters.add('pcm_s32le');
        end;

        process.parameters.add(tempfile);
        process.options := [ponoconsole, powaitonexit];
        process.execute;
      except
      end;
      process.destroy;

    end else
      tempfile := track.filename;

    stream := tfilestream.create(tempfile, fmopenread or fmshareexclusive);
  except
    stream := nil;
  end;

  if assigned(stream) then
  begin
    buffer := treadbufstream.create(stream);
    audioanalyzer := ttrackanalyzer.create(track, buffer);
    audioanalyzer.onstart := @onstartanalyzer;
    audioanalyzer.ontick  := @ontickanalyzer;
    audioanalyzer.onstop  := @onstopanalyzer;
    audioanalyzer.start;
  end else
  begin
    messagedlg('AudioMeter', format('Error to open file "%s"', [tempfile]), mterror, [mbok], '');
    inc(trackindex);
    execute;
  end;
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

  audio.caption      := 'Audio';
  audio.font.color   := clwhite;
  drvalue.caption    := '--';
  drvalue.font.color := clwhite;
end;

// timer events

procedure taudiofrm.timerstarttimer(Sender: TObject);
begin
  writeln('timer.start');
  lastheight := screen.height;
  lastwidth  := screen.width;
end;

procedure taudiofrm.timertimer(Sender: TObject);
begin
  writeln('timer.ontime');
  timer.enabled := (lastheight <> screen.height) or
                   (lastwidth  <> screen.width);

  lastheight := screen.height;
  lastwidth  := screen.width;
end;
                    
procedure taudiofrm.timerstoptimer(sender: tobject);
var
  i: longint;
begin
  writeln('timer.stop');
  if assigned(drawer) then exit;
  if not assigned(track) then exit;

  lastheight := screen.height;
  lastwidth  := screen.width;
  for i := low(screens) to high(screens) do
  begin
    screens[i].setsize(lastwidth, lastheight);
    screens[i].filltransparent;
  end;
  writeln('timer.launcher.start');
  drawer := tdrawer.create(track, screens);
  drawer.onstart := @onstartdrawer;
  drawer.ontick  := @ontickdrawer;
  drawer.onstop  := @onstopdrawer;
  drawer.start;
  writeln('timer.launcher.stop');
end;

procedure taudiofrm.screenredraw(sender: tobject; bitmap: tbgrabitmap);
begin
  bitmap.putimage(0, 0, screens[pageindex], dmset);
end;

// button events

procedure taudiofrm.btnfileclick(sender: tobject);
begin
  tracklist.clear;
  if filedialog.execute then
  begin
    playsound.stopsound;
    btnplay.imageindex := 5;
    if filesupported(extractfileext(filedialog.filename)) then
    begin
      tracklist.add(filedialog.filename);
      trackfile  := changefileext(filedialog.filename, '.md');
      isneededkillanalyzer  := false;
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
    playsound.stopsound;
    btnplay.imageindex := 5;
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
    isneededkillanalyzer  := false;
    trackindex := 0;
  end;
  execute;
end;

procedure taudiofrm.btnplayclick(sender: tobject);
begin
  case btnplay.imageindex of
    6: begin
         playsound.stopsound;
         btnplay.imageindex := 5;
       end;
    7: begin
         playsound.stopsound;
         btnplay.imageindex := 5;
       end;
  else begin
         playsound.stopsound;
         btnplay.imageindex := 5;
         if fileexists(tempfile) then
         begin
           playsound.playstyle := psasync;
           playsound.soundfile := tempfile;
           playsound.execute;
           btnplay.imageindex := 6;
         end;
       end;
  end;
end;

// panel button events

procedure taudiofrm.panelbtnclick(sender: tobject);
var
  btn1: tbcbutton;
  btn2: tbcbutton;
  btn3: tbcbutton;
  btn4: tbcbutton;
begin
  btn1 := sender as tbcbutton;

  if btn1 = blocksbtn      then pageindex := 0 else
  if btn1 = spectrumbtn    then pageindex := 1 else
  if btn1 = spectrogrambtn then pageindex := 2 else
  if btn1 = wavebtn        then pageindex := 3;

  case pageindex of
    0: begin
         btn1 := blocksbtn;
         btn2 := spectrumbtn;
         btn3 := spectrogrambtn;
         btn4 := wavebtn;
       end;
    1: begin
         btn2 := blocksbtn;
         btn1 := spectrumbtn;
         btn3 := spectrogrambtn;
         btn4 := wavebtn;
       end;
    2: begin
         btn2 := blocksbtn;
         btn3 := spectrumbtn;
         btn1 := spectrogrambtn;
         btn4 := wavebtn;
       end;
    3: begin
         btn2 := blocksbtn;
         btn3 := spectrumbtn;
         btn4 := spectrogrambtn;
         btn1 := wavebtn;
       end;
  end;

  btn1.statenormal .background.color  := clwhite;
  btn1.statehover  .background.color  := clwhite;
  btn1.stateclicked.background.color  := clblack;

  btn1.statenormal .fontex    .color  := clblack;
  btn1.statehover  .fontex    .color  := clblack;
  btn1.stateclicked.fontex    .color  := clwhite;

  btn2.statenormal .background.color  := clblack;
  btn2.statehover  .background.color  := clblack;
  btn2.stateclicked.background.color  := clwhite;

  btn2.statenormal .fontex    .color  := clwhite;
  btn2.statehover  .fontex    .color  := clwhite;
  btn2.stateclicked.fontex    .color  := clblack;

  btn3.statenormal .background.color  := clblack;
  btn3.statehover  .background.color  := clblack;
  btn3.stateclicked.background.color  := clwhite;

  btn3.statenormal .fontex    .color  := clwhite;
  btn3.statehover  .fontex    .color  := clwhite;
  btn3.stateclicked.fontex    .color  := clblack;

  btn4.statenormal .background.color  := clblack;
  btn4.statehover  .background.color  := clblack;
  btn4.stateclicked.background.color  := clwhite;

  btn4.statenormal .fontex    .color  := clwhite;
  btn4.statehover  .fontex    .color  := clwhite;
  btn4.stateclicked.fontex    .color  := clblack;

  screen.redrawbitmap;
end;

// mouse events

procedure taudiofrm.btnfilemouseup(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  btnfile.onmousemove := @btnfilemousemove;
end;

procedure taudiofrm.btnfilemousedown(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  btnfile.onmousemove := nil;
  loadbuttonicon(btnfile, 1, 0, 0);
end;

procedure taudiofrm.btnfilemouseleave(sender: tobject);
begin
  loadbuttonicon(btnfile, 1, 0, 0);
end;

procedure taudiofrm.btnfilemousemove(sender: tobject;
  shift: tshiftstate; x, y: integer);
begin
  loadbuttonicon(btnfile, 0, 0, 0);
end;

procedure taudiofrm.btnfoldermousedown(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  btnfolder.onmousemove := nil;
  loadbuttonicon(btnfolder, 3, 0, 5);
end;

procedure taudiofrm.btnfoldermouseleave(sender: tobject);
begin
  loadbuttonicon(btnfolder, 3, 0, 5);
end;

procedure taudiofrm.btnfoldermousemove(sender: tobject;
  shift: tshiftstate; x, y: integer);
begin
  loadbuttonicon(btnfolder, 2, 0, 5);
end;

procedure taudiofrm.btnfoldermouseup(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  btnfolder.onmousemove := @btnfoldermousemove;
end;

procedure taudiofrm.btnplaymousedown(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
 btnplay.onmousemove := nil;
 case btnplay.imageindex of
   4: btnplay.imageindex := 5;
   5: btnplay.imageindex := 4;
   6: btnplay.imageindex := 7;
   7: btnplay.imageindex := 6;
 end;
end;

procedure taudiofrm.btnplaymouseleave(sender: tobject);
begin
 case btnplay.imageindex of
   4: btnplay.imageindex := 5;
   6: btnplay.imageindex := 7;
 end;
end;

procedure taudiofrm.btnplaymousemove(sender: tobject; shift: tshiftstate; x, y: integer);
begin
 case btnplay.imageindex of
   5: btnplay.imageindex := 4;
   7: btnplay.imageindex := 6;
 end;
end;

procedure taudiofrm.btnplaymouseup(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  btnplay.onmousemove := @btnplaymousemove;
  case btnplay.imageindex of
    5: btnplay.imageindex := 4;
    7: btnplay.imageindex := 6;
  end;
end;

// ---

procedure taudiofrm.loadbuttonicon(btn: timage; index: longint; x, y: longint);
begin
  btn.center := false;
  btn.stretch := false;
  btn.proportional := true;
  btn.picture.bitmap := nil;
  buttons.draw(btn.canvas, x, y, index);
end;

procedure taudiofrm.disablebuttons;
begin
  btnplay  .enabled := false;
  btnfile  .enabled := false;
  btnfolder.enabled := false;
  // ---
  drvalue.visible := false;
  // ---
  progressbar.value     := 0;
  progresspanel.visible := true;
end;

procedure taudiofrm.enablebuttons;
begin
  btnplay  .enabled := true;
  btnfile  .enabled := true;
  btnfolder.enabled := true;
  // ---
  drvalue.visible := true;
  // ---
  progresspanel.visible := false;
  progressbar.value     := 0;
end;

procedure taudiofrm.disablepanel;
begin
  blocksbtn      .enabled := false;
  spectrogrambtn .enabled := false;
  spectrumbtn    .enabled := false;
  wavebtn        .enabled := false;
  // ---
  panelprogressbar.value   := 0;
  panelprogressbar.visible := true;
end;

procedure taudiofrm.enablepanel;
begin
  blocksbtn      .enabled := true;
  spectrogrambtn .enabled := true;
  spectrumbtn    .enabled := true;
  wavebtn        .enabled := true;
  // ---
  panelprogressbar.visible := false;
  panelprogressbar.value   := 0;
end;

end.
