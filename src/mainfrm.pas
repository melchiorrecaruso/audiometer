{
  Description: Main form.

  Copyright (C) 2020-2024 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
  bcradialprogressbar, bclistbox, bcbutton,  bgravirtualscreen, process,
  inifiles, bgrabitmap, bgrabitmaptypes, bctypes;

type
  { taudiofrm }

  taudiofrm = class(tform)
    BlocksBtn: TBCButton;
    FrequencyPanel: TPanel;
    frequencyfirstvalue: TLabel;
    frequencysecondvalue: TLabel;
    SpectrumImage: TBGRAVirtualScreen;
    SpectrumPanel: TPanel;
    spectrumsecondvalue: TLabel;
    spectrumfirstvalue: TLabel;
    DurationPanel: TPanel;
    SpectrumBtn: TBCButton;
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    bit16: TLabel;
    bit24: TLabel;
    bit8: TLabel;
    btnfile: timage;
    btnfolder: timage;
    buttons: timagelist;
    dbchart: TChart;
    drlb: TStaticText;
    drvalue: TStaticText;
    khz176: TLabel;
    khz192: TLabel;
    khz44: TLabel;
    khz48: TLabel;
    khz88: TLabel;
    khz96: TLabel;
    DetailsPanel: TPanel;
    BitsPanel: TPanel;
    LeftHzPanel: TPanel;
    mono: TLabel;
    DRPanel: TPanel;
    Notebook: TNotebook;
    Page1: TPage;
    Page2: TPage;
    peakseries: TBarSeries;
    progressbar: TBCRadialProgressBar;
    progresspanel: TPanel;
    RightHzPanel: TPanel;
    rmseries: TBarSeries;
    stereo: TLabel;
    Report: TImageList;
    audio: tlabel;
    dirdialog: tselectdirectorydialog;
    peak: tlistchartsource;
    rms: tlistchartsource;
    filedialog: topendialog;
    procedure blocksbtnclick(sender: tobject);
    procedure btnfolderclick(sender: tobject);
    procedure btnfoldermousedown(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    procedure btnfoldermouseleave(sender: tobject);
    procedure btnfoldermousemove(sender: tobject; shift: tshiftstate; x, y: integer);
    procedure btnfoldermouseup(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    procedure formclosequery(sender: tobject; var canclose: boolean);
    procedure formcreate(sender: tobject);
    procedure btnfileclick(sender: tobject);
    procedure btnfilemousedown(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    procedure btnfilemouseleave(sender: tobject);
    procedure btnfilemousemove(sender: tobject; shift: tshiftstate; x, y: integer);
    procedure btnfilemouseup(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    procedure formdestroy(sender: tobject);
    procedure formresize(sender: tobject);
    procedure onstart;
    procedure onstop;
    procedure onprogress;
    procedure clear;
    procedure execute;
    function drawspectrum(atrack: ttrack): tbgrabitmap;
    procedure btnloadicon(btn: timage; index: longint; x, y: longint);
    procedure SpectrumImageRedraw(Sender: TObject; Bitmap: TBGRABitmap);
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

uses
  math;

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
  // initialize buttons
  blocksbtn  .statenormal .fontex.shadow := false;
  blocksbtn  .statehover  .fontex.shadow := false;
  blocksbtn  .stateclicked.fontex.shadow := false;
  spectrumbtn.statenormal .fontex.shadow := false;
  spectrumbtn.statehover  .fontex.shadow := false;
  spectrumbtn.stateclicked.fontex.shadow := false;
  // initialize notebook
  notebook.pageindex := 0;
  // inizialize
  clear;
end;

function cutoff(const S: string): string;
begin
  result := s;
  setlength(result, max(0, length(result) - 4));
  result := result + '...';
end;

procedure taudiofrm.formdestroy(sender: tobject);
begin
  rms.clear;
  peak.clear;
  tracklist.destroy;
end;

procedure taudiofrm.formresize(Sender: TObject);
begin
  while (audio.Left + audio.Width) > (btnFolder.Left + btnFolder.Width) do
  begin
    audio.Caption := cutoff(audio.Caption);
  end;
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
  while (audio.Left + audio.Width) > (btnFolder.Left + btnFolder.Width) do
  begin
    audio.Caption := cutoff(audio.Caption);
  end;

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
//mycapture: tbitmap;
  norm: longint;
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
    trackindex := tracklist.count;
    audio.font.color := clred;
    case wave.status of
      -1: audio.caption := 'File format error!';
      -2: audio.caption := 'File is empty!';
      -3: audio.caption := 'File is too short!';
    else  audio.caption := 'Unknown error!';
    end;
    btnfile.enabled       := true;
    btnfolder.enabled     := true;
    progresspanel.visible := false;
  end else
  begin
    rms.clear;
    peak.clear;
    // load rms and peak
    track := tracklist.tracks[trackindex];
    if track.channelcount > 0 then
    begin
      norm := 1 shl (track.bitspersample -1);
      for i := 0 to track.channels[0].count -1 do
      begin
        rmsi := 0;
        for j := 0 to track.channelcount -1 do
          rmsi := rmsi + sqrt(track.channels[j].rms2[i]);
          rms.add(i, max(0, db(rmsi/track.channelcount*norm)));

        peaki := 0;
        for j := 0 to track.channelcount -1 do
          peaki := peaki + track.channels[j].peak[i];
        peak.add(i, max(0, db(peaki/track.channelcount*norm)));
      end;
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
    drvalue      .visible := true;
    btnfile      .enabled := true;
    btnfolder    .enabled := true;
    progresspanel.visible := false;
    progressbar  .value   := 0;
    // update spectrum image
    if notebook.pageindex = 1 then
      spectrumimage.redrawbitmap;
  end;
  wave := nil;

  //mycapture := tbitmap.create;
  //mycapture.setsize(446, 146);
  //mycapture.canvas.fillrect(0, 0, 446, 146);
  //begin
  //  borderstyle := bsnone;
  //  paintto(mycapture.canvas, 0, 0);
  //  borderstyle := bssizeable;
  application.processmessages;
  //end;
  //report.add(mycapture, nil);
  //mycapture.free;

  inc(trackindex);
  // release unused spectrum
  if trackindex < tracklist.count then
    tracklist.tracks[trackindex -1].clearspectrum
  else
    if trackindex = tracklist.count then
    begin
      // save text report
      tracklist.savetofile(trackfile);
      // save png report
      (*
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
      *)
    end;
  {$ifopt D-} sleep(500); {$endif}
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

  rms.clear;
  peak.clear;
  audio.caption      := 'Audio';
  audio.font.color   := clwhite;
  drvalue.caption    := '--';
  drvalue.font.color := clwhite;
  dbchart.invalidate;

  blocksbtnclick(nil);
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

  track := tracklist.tracks[trackindex];
  try
    if extractfileext(track.name) <> '.wav' then
    begin
      tempfile := includetrailingbackslash(
        gettempdir(false)) + 'audiometer-tmp.wav';

      // get file properties
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

      // decode to .wave
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
          process.parameters.add('pcm_s32le');
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

function getcolor(factor: double): tbgrapixel;
var
  r1, g1, b1, r2, g2, b2: byte;
  color1: tbgrapixel;
  color2: tbgrapixel;
begin
  if factor < 1/5 then
  begin
    color1 := clblack;
    color2 := clblue;
    factor := (factor -   0) / (1/5);
  end else
  if factor < 2/5 then
  begin
    color1 := clblue;
    color2 := clpurple;
    factor := (factor - 1/5) / (1/5);
  end else
  if factor < 3/5 then
  begin
    color1 := clpurple;
    color2 := clred;
    factor := (factor - 2/5) / (1/5);
  end else
  if factor < 4/5 then
  begin
    color1 := clred;
    color2 := clyellow;
    factor := (factor - 3/5) / (1/5);
  end else
  begin
    color1 := clyellow;
    color2 := clwhite;
    factor := (factor - 4/5) / (1/5);
  end;

  r1 := color1.red;
  g1 := color1.green;
  b1 := color1.blue;

  r2 := color2.red;
  g2 := color2.green;
  b2 := color2.blue;

  result.red   := trunc(r1 + (r2 - r1) * factor);
  result.green := trunc(g1 + (g2 - g1) * factor);
  result.blue  := trunc(b1 + (b2 - b1) * factor);
  result.alpha := 255;
end;

function taudiofrm.drawspectrum(atrack: ttrack): tbgrabitmap;
var
  i, j, k, q: longint;
  x, y, z: double;
  index: longint;
  windowsize: longint;
  windowcount: longint;
  zmax: double;
  zmin: double;
begin
  result := tbgrabitmap.create;
  result.setsize(spectrumpanel.width, spectrumpanel.height);
  result.filltransparent;

  zmax := minfloat;
  zmin := maxfloat;

  if atrack.channelcount > 0 then
  begin
    for i := 0 to atrack.channelcount -1 do
      for j := 0 to length(atrack.channels[i].spectrum) -1 do
      begin
        if zmin > atrack.channels[i].spectrum[j] then
          zmin := atrack.channels[i].spectrum[j];

        if zmax < atrack.channels[i].spectrum[j] then
          zmax := atrack.channels[i].spectrum[j];
      end;

    for i := 0 to result.width -1 do
      for j := 0 to result.height -1 do
      begin
        z := 0;
        for k := 0 to atrack.channelcount -1 do
        begin
          windowsize  := 512;
          windowcount := length(atrack.channels[k].spectrum) div windowsize;

          index := trunc((j+1)/result.height*(windowcount -1))*windowsize + trunc(i/result.width*windowsize);

          z := z + db(atrack.channels[k].spectrum[index] - zmin)/db(zmax - zmin);
        end;
        result.setpixel(i, j, getcolor(z/atrack.channelcount));
      end;
  end;

  spectrumfirstvalue  .caption := '0:00s';
  if atrack.duration = '' then
    spectrumsecondvalue .caption := '0:00s'
  else
    spectrumsecondvalue .caption := atrack.duration + 's';

  frequencyfirstvalue .caption := '0 Hz';
  frequencysecondvalue.caption := (atrack.samplerate div 2).tostring + ' Hz';
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

procedure taudiofrm.blocksbtnclick(sender: tobject);
var
  btn1: tbcbutton;
  btn2: tbcbutton;
begin
 if sender = nil then
 begin
   if notebook.pageindex = 0 then
   begin
     btn1 := blocksbtn;
     btn2 := spectrumbtn;
   end else
   begin
     btn1 := spectrumbtn;
     btn2 := blocksbtn;
   end;
 end else
 begin
   btn1 := sender as tbcbutton;
   if btn1 = blocksbtn then
     btn2 := spectrumbtn
   else
     btn2 := blocksbtn;

   if btn1 = blocksbtn   then notebook.pageindex := 0 else
   if btn1 = spectrumbtn then notebook.pageindex := 1;
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

 // update spectrum image
 if notebook.pageindex = 1 then
   spectrumimage.redrawbitmap;
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

procedure taudiofrm.spectrumimageredraw(sender: tobject; bitmap: tbgrabitmap);
var
  bit: tbgrabitmap;
begin
  if tracklist.count > 0 then
  begin
    if trackindex < tracklist.count then
      bit := drawspectrum(tracklist[trackindex])
    else
      bit := drawspectrum(tracklist[tracklist.count -1]);
  end else
  begin
    bit := tbgrabitmap.create;
    bit.setsize(spectrumimage.width, spectrumimage.height);
    bit.filltransparent;
  end;
  bitmap.putimage(0, 0, bit, dmset, 255);
  bit.destroy;
end;

end.

