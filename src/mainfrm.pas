{
  Description: Main Form.

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

unit MainFrm;

{$mode objfpc}{$H+}

interface

uses
  classes, sysutils, uplaysound, forms, controls, graphics, dialogs, buttons,
  stdctrls, extctrls, comctrls, bufstream, soundwav, bcradialprogressbar,
  bclistbox, bcbutton, process, inifiles, bgrabitmap, bgrabitmaptypes, bctypes,
  bgravirtualscreen, drawers, Common;

type
  { TAudioFrm }

  TAudioFrm = class(tform)
    Bevel4: TBevel;
    Bevel5: TBevel;
    IntegratedLoudnessValue: TLabel;
    CRESTRightValue: TLabel;
    IntegratedLoudnessLabel: TLabel;
    PCM: TLabel;
    Label14: TLabel;
    LoudnessRangeLabel: TLabel;
    LUFSPanel: TPanel;
    MomentaryLoudnessLabel: TLabel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    LoudnessRangeValue: TLabel;
    PLRRightValue: TLabel;
    TopShape: TShape;
    ShortTermLoudnessValue: TLabel;
    RMSRightValue: TLabel;
    ShortTerpLoudnessLabel: TLabel;
    BottomShape: TShape;
    TPLLabel: TLabel;
    PLRLeftValue: TLabel;
    Label13: TLabel;
    PLLabel: TLabel;
    TPLLeftValue: TLabel;
    RMSLabel: TLabel;
    CRESTLabel: TLabel;
    PLRLabel: TLabel;
    RMSLeftValue: TLabel;
    CRESTLeftValue: TLabel;
    MomentaryLoudnessValue: TLabel;
    PLLeftValue: TLabel;
    TPLRightValue: TLabel;
    PLRightValue: TLabel;
    TPMPanel: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ScreenPanel: TPanel;

    BtnPlay: TImage;
    playsound: Tplaysound;
    VirtualScreen: TBGRAVirtualScreen;
    bevel1: tbevel;
    bevel2: tbevel;
    bevel3: tbevel;
    bit16: tlabel;
    bit24: tlabel;
    bit8: tlabel;
    BtnFile: timage;
    BtnFolder: timage;
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

    procedure FormCreate(sender: tobject);
    procedure formclosequery(sender: tobject; var canclose: boolean);
    procedure FormResize(sender: tobject);
    procedure FormDestroy(sender: tobject);
    // file button
    procedure BtnFileClick(sender: tobject);
    procedure btnfilemousedown(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    procedure btnfilemouseleave(sender: tobject);
    procedure btnfilemousemove(sender: tobject; shift: tshiftstate; x, y: integer);
    procedure btnfilemouseup(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    // folder button
    procedure BtnFolderClick(sender: tobject);
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

    procedure loadbuttonicon(btn: timage; index: longint; x, y: longint);


    procedure clear;
    procedure execute;

    procedure OnStartAnalyzer;
    procedure OnTickAnalyzer;
    procedure OnStopAnalyzer;

    procedure OnStartDrawer;
    procedure OnStopDrawer;
    procedure OnWaitDrawer;

    procedure disablebuttons;
    procedure EnableButtons;



    procedure VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);

  private
    VirtualScreens: TVirtualScreens;
    Buffer: TReadBufStream;
    Stream: TFileStream;
    Track: TTrack;
    TrackIndex: longint;
    TrackList: TTrackList;
    TrackFile: string;
    TempFile:  string;

    IsNeededUpdateScreens: boolean;
    IsNeededKillAnalyzer:  boolean;
  public
  end;

var
  AudioFrm: TAudioFrm;

implementation

{$R *.lfm}

uses
  math, fileutil, soundutils;

function cutoff(const s: string): string;
begin
  result := s;
  setlength(result, max(0, length(result) - 4));
  result := result + '...';
end;

{ TAudioFrm }

procedure TAudioFrm.FormCreate(Sender: TObject);
begin
  VirtualScreens[0] := TBitmap.create;
  VirtualScreens[1] := TBitmap.create;
  VirtualScreens[2] := TBitmap.create;
  VirtualScreens[3] := TBitmap.create;
  IsNeededUpdateScreens := False;
  IsNeededKillAnalyzer  := False;
  // ---
  Track := nil;
  TrackIndex := 0;
  TrackList  := TTrackList.create;
  // load openfile button icon
  loadbuttonicon(BtnFile, 1, 0, 0);
  BtnFile.onmousemove := @btnfilemousemove;
  // load openfolder button icon
  loadbuttonicon(BtnFolder, 3, 0, 5);
  BtnFile.onmousemove := @btnfilemousemove;
  // initialize progress bar
  progresspanel.visible := false;
  progressbar.value := 0;
  // inizialize main form
  color := clblack;
  // initialize
  Clear;
end;

procedure TAudioFrm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(VirtualScreens[0]);
  FreeAndNil(VirtualScreens[1]);
  FreeAndNil(VirtualScreens[2]);
  FreeAndNil(VirtualScreens[3]);
  TrackList.destroy;
  Track := nil;
end;

procedure TAudioFrm.formclosequery(sender: tobject; var canclose: boolean);
begin
  IsNeededKillAnalyzer := true;
  canclose := (AudioAnalyzer = nil) and
              (ScreenDrawer  = nil);
end;

procedure TAudioFrm.FormResize(sender: tobject);
begin
  while (audio.left + audio.width) > (BtnFolder.left + BtnFolder.width) do
  begin
    audio.caption := cutoff(audio.caption);
  end;

  IsNeededUpdateScreens := true;
  if AudioAnalyzer <> nil then exit;
  if ScreenDrawer  <> nil then exit;
  ScreenDrawer := TScreenDrawer.Create(Track);
  ScreenDrawer.OnStart := @OnStartDrawer;
  ScreenDrawer.OnStop  := @OnStopDrawer;
  ScreenDrawer.OnWait  := @OnWaitDrawer;
  ScreenDrawer.Start;
end;

// track analyzer events

procedure TAudioFrm.OnStartAnalyzer;
begin
  clear;
  if assigned(Track) then
  begin
    audio.font.color := clwhite;
    audio.caption    := extractfilename(Track.Filename);
    while (audio.left + audio.width) > (BtnFolder.left + BtnFolder.width) do
    begin
      audio.caption := cutoff(audio.caption);
    end;
  end;
  disablebuttons;
  application.processmessages;
end;

procedure TAudioFrm.OnTickAnalyzer;
begin
  progressbar.value := AudioAnalyzer.Percentage;
end;

procedure TAudioFrm.OnStopAnalyzer;
begin
  freeandnil(Buffer);
  freeandnil(Stream);

  if AudioAnalyzer.Status <> 0 then
  begin
    TrackIndex := TrackList.count;
    audio.font.color := clrRed;
    case AudioAnalyzer.Status of
      -1: audio.caption := 'file format error!';
      -2: audio.caption := 'file is empty!';
      -3: audio.caption := 'file is too short!';
    else  audio.caption := 'unknown error!';
    end;
    EnableButtons;
  end else
  begin
    pcm   .Font.Color  := clWhite;
    bit8  .Font.Color  := clGray; if Track.Bitspersample = 8      then bit8  .Font.Color := clWhite;
    bit16 .Font.Color  := clGray; if Track.Bitspersample = 16     then bit16 .Font.Color := clWhite;
    bit24 .Font.Color  := clGray; if Track.Bitspersample = 24     then bit24 .Font.Color := clWhite;

    khz44 .Font.Color  := clGray; if Track.Samplerate    = 44100  then khz44 .Font.Color := clWhite;
    khz48 .Font.Color  := clGray; if Track.Samplerate    = 48000  then khz48 .Font.Color := clWhite;
    khz88 .Font.Color  := clGray; if Track.Samplerate    = 88000  then khz88 .Font.Color := clWhite;
    khz96 .Font.Color  := clGray; if Track.Samplerate    = 96000  then khz96 .Font.Color := clWhite;
    khz176.Font.Color  := clGray; if Track.Samplerate    = 176400 then khz176.Font.Color := clWhite;
    khz192.Font.Color  := clGray; if Track.Samplerate    = 192000 then khz192.Font.Color := clWhite;

    mono  .Font.Color  := clGray; if Track.ChannelCount  = 1      then mono  .Font.Color := clWhite;
    stereo.Font.Color  := clGray; if Track.ChannelCount  = 2      then stereo.Font.Color := clWhite;

    if Track.ChannelCount > 0 then if Decibel(Track.Loudness.TruePeak(0)) <  0.0 then tplleftvalue .Font.Color := clLime;
    if Track.ChannelCount > 0 then if Decibel(Track.Loudness.TruePeak(0)) >= 0.0 then tplleftvalue .Font.Color := clYellow;
    if Track.ChannelCount > 0 then if Decibel(Track.Loudness.TruePeak(0)) >  0.5 then tplleftvalue .Font.Color := clRed;

    if Track.ChannelCount > 1 then if Decibel(Track.Loudness.TruePeak(1)) <  0.0 then tplrightvalue.Font.Color := clLime;
    if Track.ChannelCount > 1 then if Decibel(Track.Loudness.TruePeak(1)) >= 0.0 then tplrightvalue.Font.Color := clYellow;
    if Track.ChannelCount > 1 then if Decibel(Track.Loudness.TruePeak(1)) >  0.5 then tplrightvalue.Font.Color := clRed;

    if Track.ChannelCount > 0 then plleftvalue .Caption := Format('%0.2f', [Track.Loudness.Peak(0)]);
    if Track.ChannelCount > 1 then plrightvalue.Caption := Format('%0.2f', [Track.Loudness.Peak(1)]);

    if Track.ChannelCount > 0 then tplleftvalue   .Caption := Format('%0.2f', [Track.Loudness.TruePeak(0)]);
    if Track.ChannelCount > 1 then tplrightvalue  .Caption := Format('%0.2f', [Track.Loudness.TruePeak(1)]);
    if Track.ChannelCount > 0 then rmsleftvalue   .Caption := Format('%0.2f', [Track.Loudness.Rms(0)]);
    if Track.ChannelCount > 1 then rmsrightvalue  .Caption := Format('%0.2f', [Track.Loudness.Rms(1)]);
    if Track.ChannelCount > 0 then crestleftvalue .Caption := Format('%0.2f', [Track.Loudness.CrestFactor(0)]);
    if Track.ChannelCount > 1 then crestrightvalue.Caption := Format('%0.2f', [Track.Loudness.CrestFactor(1)]);
    if Track.ChannelCount > 0 then plrleftvalue   .Caption := Format('%0.2f', [Track.Loudness.PeakToLoudnessRatio]);

    if Track.ChannelCount > 0 then IntegratedLoudnessValue .Caption := Format('%0.2f', [Track.Loudness.IntegratedLoudness]);
    if Track.ChannelCount > 0 then LoudnessRangeValue      .Caption := Format('%0.2f', [Track.Loudness.LoudnessRange]);


    DRValue.Caption    := '--';
    DRValue.Font.Color := clwhite;
    if (Track.DRMeter.DR) > 0 then
    begin
      DRValue.Caption := format('%2.0f', [Track.DRMeter.DR]);
      if DRValue.Caption = ' 0' then DRValue.Font.Color := rgbtocolor(255,   0, 0) else
      if DRValue.Caption = ' 1' then DRValue.Font.Color := rgbtocolor(255,   0, 0) else
      if DRValue.Caption = ' 2' then DRValue.Font.Color := rgbtocolor(255,   0, 0) else
      if DRValue.Caption = ' 3' then DRValue.Font.Color := rgbtocolor(255,   0, 0) else
      if DRValue.Caption = ' 4' then DRValue.Font.Color := rgbtocolor(255,   0, 0) else
      if DRValue.Caption = ' 5' then DRValue.Font.Color := rgbtocolor(255,   0, 0) else
      if DRValue.Caption = ' 6' then DRValue.Font.Color := rgbtocolor(255,   0, 0) else
      if DRValue.Caption = ' 7' then DRValue.Font.Color := rgbtocolor(255,   0, 0) else
      if DRValue.Caption = ' 8' then DRValue.Font.Color := rgbtocolor(255,  72, 0) else
      if DRValue.Caption = ' 9' then DRValue.Font.Color := rgbtocolor(255, 145, 0) else
      if DRValue.Caption = '10' then DRValue.Font.Color := rgbtocolor(255, 217, 0) else
      if DRValue.Caption = '11' then DRValue.Font.Color := rgbtocolor(217, 255, 0) else
      if DRValue.Caption = '12' then DRValue.Font.Color := rgbtocolor(144, 255, 0) else
      if DRValue.Caption = '13' then DRValue.Font.Color := rgbtocolor( 72, 255, 0) else
                                     DRValue.Font.Color := rgbtocolor(  0, 255, 0);
    end;
  end;
  EnableButtons;
  application.processmessages;

  inc(TrackIndex);
  if TrackIndex = TrackList.count then
  begin
    // save text report
    TrackList.SaveToFile(TrackFile);
    // ---
    AudioAnalyzer := nil;
    ScreenDrawer := TScreenDrawer.Create(Track);
    ScreenDrawer.OnStart := @OnStartDrawer;
    ScreenDrawer.OnStop  := @OnStopDrawer;
    ScreenDrawer.OnWait  := @OnWaitDrawer;
    ScreenDrawer.Start;
  end else
    if Assigned(Track) then
    begin
      Track.ClearChannels;
    end;
  execute;
end;

// chart drawer events

procedure TAudioFrm.OnStartDrawer;
begin
  Application.ProcessMessages;
end;

procedure TAudioFrm.OnStopDrawer;
begin
  if IsNeededUpdateScreens then
  begin
    ScreenDrawer := TScreenDrawer.Create(Track);
    ScreenDrawer.OnStart := @OnStartDrawer;
    ScreenDrawer.OnStop  := @OnStopDrawer;
    ScreenDrawer.OnWait  := @OnWaitDrawer;
    ScreenDrawer.Start;
  end else
  begin
    VirtualScreens[0].SetSize(ScreenDrawer.Screens[0].Width, ScreenDrawer.Screens[0].Height);
    VirtualScreens[1].SetSize(ScreenDrawer.Screens[1].Width, ScreenDrawer.Screens[1].Height);
    VirtualScreens[2].SetSize(ScreenDrawer.Screens[2].Width, ScreenDrawer.Screens[2].Height);
    VirtualScreens[3].SetSize(ScreenDrawer.Screens[3].Width, ScreenDrawer.Screens[3].Height);

    if (ScreenDrawer.ScreenWidth  > 0) and
       (ScreenDrawer.ScreenHeight > 0) then
    begin
      VirtualScreens[0].canvas.draw(0, 0, ScreenDrawer.Screens[0]);
      VirtualScreens[1].canvas.draw(0, 0, ScreenDrawer.Screens[1]);
      VirtualScreens[2].canvas.draw(0, 0, ScreenDrawer.Screens[2]);
      VirtualScreens[3].canvas.draw(0, 0, ScreenDrawer.Screens[3]);
    end;
    ScreenDrawer := nil;
    EnableButtons;

    VirtualScreen.RedrawBitmap;
  end;
  Application.ProcessMessages;
end;

procedure TAudioFrm.OnWaitDrawer;
begin
  if not IsNeededUpdateScreens then
  begin
    ScreenDrawer.ScreenWidth  := ScreenPanel.Width;
    ScreenDrawer.ScreenHeight := ScreenPanel.Height;
  end;
  IsNeededUpdateScreens := false;
end;

//

procedure TAudioFrm.execute;
var
  buf: array[0..4095] of byte;
  bit4sample: longint;
  i: longint;
  ini: tinifile;
  mem: tmemorystream;
  process: tprocess;
begin
  if (TrackIndex >= TrackList.count) then exit;
  if IsNeededKillAnalyzer then TrackIndex := TrackList.count -1;

  Track := TrackList.Tracks[TrackIndex];
  try
    if extractfileext(Track.Filename) <> '.wav' then
    begin
      TempFile := includetrailingbackslash(
        gettempdir(false)) + 'audiometer-tmp.wav';

      // get file properties
      process := tprocess.create(nil);
      try
        process.parameters.clear;
        process.currentdirectory := extractfiledir(Track.Filename);
        process.executable := 'ffprobe';
        process.parameters.add('-show_streams');
        process.parameters.add('-hide_banner');
        process.parameters.add('-print_format');
        process.parameters.add('ini');
        process.parameters.add(extractfilename(Track.Filename));
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

      // decode to .AudioAnalyzer
      process := tprocess.create(nil);
      try
        process.parameters.clear;
        process.currentdirectory := extractfiledir(Track.Filename);
        process.executable := 'ffmpeg';
        process.parameters.add('-y');
        process.parameters.add('-hide_banner');
        process.parameters.add('-i');
        process.parameters.add(extractfilename(Track.Filename));

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

        process.parameters.add(TempFile);
        process.options := [ponoconsole, powaitonexit];
        process.execute;
      except
      end;
      process.destroy;

    end else
      TempFile := Track.Filename;

    Stream := tfilestream.create(TempFile, fmopenread or fmshareexclusive);
  except
    Stream := nil;
  end;

  if assigned(Stream) then
  begin
    Buffer := TReadBufStream.create(Stream);
    AudioAnalyzer := TTrackAnalyzer.create(Track, Buffer, TrackIndex = TrackList.count -1);
    AudioAnalyzer.OnStart := @OnStartAnalyzer;
    AudioAnalyzer.OnTick  := @OnTickAnalyzer;
    AudioAnalyzer.OnStop  := @OnStopAnalyzer;
    AudioAnalyzer.Start;
  end else
  begin
    messagedlg('AudioMeter', format('Error to open file "%s"', [TempFile]), mterror, [mbok], '');
    inc(TrackIndex);
    execute;
  end;
end;

procedure TAudioFrm.clear;
begin
  pcm   .font.color := clGray;
  bit8  .font.color := clGray;
  bit16 .font.color := clGray;
  bit24 .font.color := clGray;
  khz44 .font.color := clGray;
  khz48 .font.color := clGray;
  khz88 .font.color := clGray;
  khz96 .font.color := clGray;
  khz176.font.color := clGray;
  khz192.font.color := clGray;
  mono  .font.color := clGray;
  stereo.font.color := clGray;

  tplleftvalue   .font.color := clGray;
  tplrightvalue  .font.color := clGray;
  rmsleftvalue   .font.color := clGray;
  rmsrightvalue  .font.color := clGray;
  crestleftvalue .font.color := clGray;
  crestrightvalue.font.color := clGray;

  audio.caption      := 'Audio';
  audio.font.color   := clwhite;
  drvalue.caption    := '--';
  drvalue.font.color := clwhite;
end;

// Button Events

procedure TAudioFrm.BtnFileClick(sender: tobject);
begin
  TrackList.clear;
  if filedialog.execute then
  begin
    playsound.stopsound;
    BtnPlay.imageindex := 5;
    if IsFileSupported(extractfileext(filedialog.filename)) then
    begin
      TrackList.Add(filedialog.filename);
      TrackFile  := changefileext(filedialog.filename, '.md');
      IsNeededKillAnalyzer  := false;
      TrackIndex := 0;
    end else
    begin
      audio.caption    := 'File format error!';
      audio.font.color := clrRed;
    end;
  end;
  execute;
end;

procedure TAudioFrm.BtnFolderClick(sender: tobject);
var
  err:  longint;
  path: string;
  sr:   tsearchrec;
begin
  TrackList.clear;
  if dirdialog.execute then
  begin
    playsound.stopsound;
    BtnPlay.imageindex := 5;
    path := includetrailingbackslash(dirdialog.filename);
     err := sysutils.findfirst(path + '*.*', faanyfile, sr);
    while err = 0 do
    begin
      if sr.attr and (fadirectory) = 0 then
      begin
        if IsFileSupported(ExtractFileExt(sr.name)) then
          TrackList.Add(path + sr.name);
      end;
      err := findnext(sr);
    end;
    sysutils.findclose(sr);
    TrackList.Sort;
    TrackFile  := path + extractfilename(dirdialog.filename) + '.md';
    IsNeededKillAnalyzer  := false;
    TrackIndex := 0;
  end;
  execute;
end;

procedure TAudioFrm.btnplayclick(sender: tobject);
begin
  case BtnPlay.imageindex of
    6: begin
         playsound.stopsound;
         BtnPlay.imageindex := 5;
       end;
    7: begin
         playsound.stopsound;
         BtnPlay.imageindex := 5;
       end;
  else begin
         playsound.stopsound;
         BtnPlay.imageindex := 5;
         if fileexists(TempFile) then
         begin
           playsound.playstyle := psasync;
           playsound.soundfile := TempFile;
           playsound.execute;
           BtnPlay.imageindex := 6;
         end;
       end;
  end;
end;

// mouse events

procedure TAudioFrm.btnfilemouseup(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  BtnFile.onmousemove := @btnfilemousemove;
end;

procedure TAudioFrm.btnfilemousedown(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  BtnFile.onmousemove := nil;
  loadbuttonicon(BtnFile, 1, 0, 0);
end;

procedure TAudioFrm.btnfilemouseleave(sender: tobject);
begin
  loadbuttonicon(BtnFile, 1, 0, 0);
end;

procedure TAudioFrm.btnfilemousemove(sender: tobject;
  shift: tshiftstate; x, y: integer);
begin
  loadbuttonicon(BtnFile, 0, 0, 0);
end;

procedure TAudioFrm.btnfoldermousedown(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  BtnFolder.onmousemove := nil;
  loadbuttonicon(BtnFolder, 3, 0, 5);
end;

procedure TAudioFrm.btnfoldermouseleave(sender: tobject);
begin
  loadbuttonicon(BtnFolder, 3, 0, 5);
end;

procedure TAudioFrm.btnfoldermousemove(sender: tobject;
  shift: tshiftstate; x, y: integer);
begin
  loadbuttonicon(BtnFolder, 2, 0, 5);
end;

procedure TAudioFrm.btnfoldermouseup(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  BtnFolder.onmousemove := @btnfoldermousemove;
end;

procedure TAudioFrm.btnplaymousedown(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
 BtnPlay.onmousemove := nil;
 case BtnPlay.imageindex of
   4: BtnPlay.imageindex := 5;
   5: BtnPlay.imageindex := 4;
   6: BtnPlay.imageindex := 7;
   7: BtnPlay.imageindex := 6;
 end;
end;

procedure TAudioFrm.btnplaymouseleave(sender: tobject);
begin
 case BtnPlay.imageindex of
   4: BtnPlay.imageindex := 5;
   6: BtnPlay.imageindex := 7;
 end;
end;

procedure TAudioFrm.btnplaymousemove(sender: tobject; shift: tshiftstate; x, y: integer);
begin
 case BtnPlay.imageindex of
   5: BtnPlay.imageindex := 4;
   7: BtnPlay.imageindex := 6;
 end;
end;

procedure TAudioFrm.btnplaymouseup(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  BtnPlay.onmousemove := @btnplaymousemove;
  case BtnPlay.imageindex of
    5: BtnPlay.imageindex := 4;
    7: BtnPlay.imageindex := 6;
  end;
end;

// ---

procedure TAudioFrm.loadbuttonicon(btn: timage; index: longint; x, y: longint);
begin
  btn.center := false;
  btn.stretch := false;
  btn.proportional := true;
  btn.picture.bitmap := nil;
  buttons.draw(btn.canvas, x, y, index);
end;

procedure TAudioFrm.disablebuttons;
begin
  BtnPlay  .Enabled := False;
  BtnFile  .Enabled := False;
  BtnFolder.Enabled := False;

  drvalue      .visible := False;
  progressbar  .value   := 0;
  progresspanel.visible := true;
end;

procedure TAudioFrm.EnableButtons;
begin
  BtnPlay       .Enabled := true;
  BtnFile       .Enabled := true;
  BtnFolder     .Enabled := true;

  progressbar.value     := 0;
  progresspanel.visible := False;
  drvalue.visible       := true;
end;

procedure TAudioFrm.VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  i: longint;
  OffSet: longint;
begin
  if (AudioAnalyzer <> nil) then Exit;
  if (ScreenDrawer  <> nil) then Exit;
  if IsNeededUpdateScreens  then Exit;

  OffSet := 0;
  Bitmap.FillTransparent;
  for i := Low(VirtualScreens) to High(VirtualScreens) do
  begin
    if (VirtualScreens[i].Width  > 0) and
       (VirtualScreens[i].Height > 0) then
    begin
      Bitmap.PutImage(0, OffSet , VirtualScreens[i], dmSet);
      Inc(OffSet, VirtualScreens[i].Height);
    end;
  end;
end;

end.
