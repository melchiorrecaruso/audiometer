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
  Classes, sysutils, uPlaySound, forms, controls, graphics, dialogs, Buttons,
  stdctrls, extctrls, comctrls, IniPropStorage, bufstream, soundwav,
  bclistbox, process, inifiles, bgrabitmap,
  bgrabitmaptypes, bgravirtualscreen, BCFluentProgressRing, drawers, Common;

type
  { TAudioFrm }

  TAudioFrm = class(TForm)
    ProgressPanel: TPanel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    IniPropStorage: TIniPropStorage;
    IntegratedLoudnessValue: TLabel;
    CRESTRightValue: TLabel;
    IntegratedLoudnessLabel: TLabel;
    PCM: TLabel;
    LoudnessFSLabel: TLabel;
    PeakToLoudnessRatioLabel: TLabel;
    PeakToLoudnessRatioValue: TLabel;
    ProgressRing: TBCFluentProgressRing;
    RangeLoudnessLabel: TLabel;
    LUFSPanel: TPanel;
    MomentaryLoudnessLabel: TLabel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    RangeLoudnessValue: TLabel;
    ScreenTimer: TIdleTimer;
    TopShape: TShape;
    ShortTermLoudnessValue: TLabel;
    RMSRightValue: TLabel;
    ShortTermLoudnessLabel: TLabel;
    BottomShape: TShape;
    TPLLabel: TLabel;
    TruePeakLabel: TLabel;
    PLLabel: TLabel;
    TPLLeftValue: TLabel;
    RMSLabel: TLabel;
    CRESTLabel: TLabel;
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
    PlaySound: Tplaysound;
    VirtualScreen: TBGRAVirtualScreen;
    bevel1: tbevel;
    bevel2: tbevel;
    Bevel3: tbevel;
    bit16: tlabel;
    bit24: tlabel;
    bit8: tlabel;
    BtnFile: timage;
    BtnFolder: timage;
    Buttons: timagelist;
    DRLabel: TStaticText;
    DRValue: TStaticText;
    khz176: tlabel;
    khz192: tlabel;
    khz44: tlabel;
    khz48: tlabel;
    khz88: tlabel;
    khz96: tlabel;
    DetailsPanel: tpanel;
    bitspanel: tpanel;
    lefthzpanel: tpanel;
    Mono: tlabel;
    DRPanel: tpanel;
    righthzpanel: tpanel;
    Stereo: tlabel;
    TrackFileName: tlabel;
    DirDialog: tselectdirectorydialog;
    FileDialog: TOpenDialog;

    procedure FormCreate(sender: tobject);
    procedure formclosequery(sender: tobject; var canclose: boolean);
    procedure FormResize(sender: tobject);
    procedure FormDestroy(sender: tobject);
    // file button
    procedure BtnFileClick(sender: tobject);
    procedure BtnFileMouseDown(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    procedure BtnFileMouseLeave(sender: tobject);
    procedure BtnFileMouseMove(sender: tobject; shift: tshiftstate; x, y: integer);
    procedure BtnFileMouseUp(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    // folder button
    procedure BtnFolderClick(sender: tobject);
    procedure BtnFolderMouseDown(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    procedure btnfoldermouseleave(sender: tobject);
    procedure btnfoldermousemove(sender: tobject; shift: tshiftstate; x, y: integer);
    procedure btnfoldermouseup(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    // play button
    procedure BtnPlayClick(sender: tobject);
    procedure BtnPlayMouseDown(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);
    procedure BtnPlayMouseLeave(sender: tobject);
    procedure BtnPlayMouseMove(sender: tobject; shift: tshiftstate; x, y: integer);
    procedure BtnPlayMouseUp(sender: tobject; button: tmousebutton; shift: tshiftstate; x, y: integer);

    procedure LoadButtonIcon(btn: timage; index: longint; x, y: longint);

    procedure Execute;
    procedure ClearTrackList;
    procedure Clear;

    procedure OnStartAnalyzer;
    procedure OnTickAnalyzer;
    procedure OnStopAnalyzer;

    procedure OnStartDrawer;
    procedure OnStopDrawer;

    procedure DisableButtons;
    procedure EnableButtons;
    procedure ScreenTimerTimer(Sender: TObject);

    procedure VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure MainBoardRedraw(ATrack: TTrack);

  private
    VirtualScreens: TVirtualScreens;
    Buffer: TReadBufStream;
    Stream: TFileStream;

    TrackIndex: longint;
    TrackList: TTrackList;
    TrackFile: string;
    TempFile:  string;

    LastIndex: longint;
    LastWidth: longint;
    LastHeight: longint;

    IsNeededUpdateScreens: boolean;
    IsNeededKillAnalyzer:  boolean;
  public
  end;

var
  AudioFrm: TAudioFrm;

implementation

{$R *.lfm}

uses
  Math, FileUtil, SoundUtils;

function CutOff(const S: string): string;
begin
  result := S;
  setlength(result, Max(0, Length(result) - 4));
  result := result + '...';
end;

{ TAudioFrm }

procedure TAudioFrm.FormCreate(Sender: TObject);
begin
  IniPropStorage.IniFileName := GetAppFile('audiometer.ini');
  IniPropStorage.Active := True;
  // ---
  VirtualScreens[0] := TBGRABitmap.Create;
  VirtualScreens[1] := TBGRABitmap.Create;
  VirtualScreens[2] := TBGRABitmap.Create;
  VirtualScreens[3] := TBGRABitmap.Create;
  IsNeededUpdateScreens := False;
  IsNeededKillAnalyzer  := False;
  // ---
  LastIndex  := -1;
  TrackIndex := -1;
  TrackList  := TTrackList.create;
  // Load openfile button icon
  LoadButtonIcon(BtnFile, 1, 0, 0);
  BtnFile.OnMouseMove := @BtnFileMouseMove;
  // Load openfolder button icon
  LoadButtonIcon(BtnFolder, 3, 0, 5);
  BtnFile.OnMouseMove := @BtnFileMouseMove;
  // Initialize progress bar
  ProgressRing.Value := 0;
  ProgressRing.Visible := True;
  // inizialize main form
  Color := clBlack;
  // Initialize
  Clear;
end;

procedure TAudioFrm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(VirtualScreens[0]);
  FreeAndNil(VirtualScreens[1]);
  FreeAndNil(VirtualScreens[2]);
  FreeAndNil(VirtualScreens[3]);
  TrackList.Destroy;
end;

procedure TAudioFrm.FormClosequery(Sender: TObject; var CanClose: boolean);
begin
  IsNeededKillAnalyzer := True;
  CanClose := (AudioAnalyzer = nil) and (ScreenDrawer = nil);
end;

procedure TAudioFrm.FormResize(Sender: TObject);
begin
  while (TrackFileName.Left + TrackFileName.Width) > (BtnFolder.Left + BtnFolder.Width) do
  begin
    TrackFileName.Caption := CutOff(TrackFileName.Caption);
  end;
end;

// Track analyzer events

procedure TAudioFrm.OnStartAnalyzer;
begin
  DisableButtons;
end;

procedure TAudioFrm.OnTickAnalyzer;
begin
  ProgressRing.Value := AudioAnalyzer.Percentage;
end;

procedure TAudioFrm.OnStopAnalyzer;
var
  Track: TTrack;
begin
  FreeAndNil(Buffer);
  FreeAndNil(Stream);

  Track := TrackList[TrackIndex];
  if AudioAnalyzer.Status <> 0 then
  begin
    TrackIndex := TrackList.Count;
    TrackFileName.Font.Color := clrRed;
    case AudioAnalyzer.Status of
     -1:  TrackFileName.Caption := Format('File format error "%s".',  [Track.Filename]);
     -2:  TrackFileName.Caption := Format('File "%s" is empty.',      [Track.Filename]);
     -3:  TrackFileName.Caption := Format('File "%s" is too short.',  [Track.Filename]);
    else  TrackFileName.Caption := Format('Unknown error with "%s".', [Track.Filename]);
    end;
  end;
  AudioAnalyzer := nil;

  Inc(TrackIndex);
  if TrackIndex = TrackList.Count then
  begin
    TrackList.SaveToFile(TrackFile);
  end;
  EnableButtons;
  Execute;
end;

// chart drawer events

procedure TAudioFrm.OnStartDrawer;
begin
  // nothing to do
end;

procedure TAudioFrm.OnStopDrawer;
begin
  if IsNeededUpdateScreens = False then
  begin
    VirtualScreens[0].SetSize(ScreenDrawer.Screens[0].Width, ScreenDrawer.Screens[0].Height);
    VirtualScreens[1].SetSize(ScreenDrawer.Screens[1].Width, ScreenDrawer.Screens[1].Height);
    VirtualScreens[2].SetSize(ScreenDrawer.Screens[2].Width, ScreenDrawer.Screens[2].Height);
    VirtualScreens[3].SetSize(ScreenDrawer.Screens[3].Width, ScreenDrawer.Screens[3].Height);

    VirtualScreens[0].PutImage(0, 0, ScreenDrawer.Screens[0], dmSet);
    VirtualScreens[1].PutImage(0, 0, ScreenDrawer.Screens[1], dmSet);
    VirtualScreens[2].PutImage(0, 0, ScreenDrawer.Screens[2], dmSet);
    VirtualScreens[3].PutImage(0, 0, ScreenDrawer.Screens[3], dmSet);

    MainBoardRedraw(ScreenDrawer.Track);
  end;
  VirtualScreen.RedrawBitmap;
  ScreenDrawer := nil;
end;

//

procedure TAudioFrm.Execute;
var
  Buff: array[0..4095] of byte;
  bit4sample: longint;
  i: longint;
  Ini: TIniFile;
  Mem: TMemoryStream;
  Process: TProcess;
  Track: TTRack;
begin
  if IsNeededKillAnalyzer then Exit;
  if TrackIndex >= TrackList.Count then Exit;

  Track := TrackList[TrackIndex];
  try
    if ExtractFileExt(Track.Filename) <> '.wav' then
    begin
      TempFile := IncludeTrailingBackSlash(GetTempDir(False)) + 'audiometer-tmp.wav';

      // get file properties
      Process := TProcess.Create(nil);
      try
        Process.Parameters.Clear;
        Process.CurrentDirectory := ExtractFileDir(Track.Filename);
        Process.Executable := 'ffprobe';
        Process.Parameters.Add('-show_streams');
        Process.Parameters.Add('-hide_banner');
        Process.Parameters.Add('-print_format');
        Process.Parameters.Add('ini');
        Process.Parameters.Add(ExtractFileName(Track.Filename));
        Process.Options := [poNoConsole, poUsePipes];
        Process.Execute;

        Mem := TMemoryStream.Create;
        while (Process.Running) or
              (Process.Output.NumBytesAvailable > 0) or
              (Process.stderr.NumBytesAvailable > 0) do
        begin
          while Process.Output.NumBytesAvailable > 0 do
            Mem.write(Buff, Process.Output.read(Buff, sizeof(Buff)));
          while Process.stderr.NumBytesAvailable > 0 do
            Process.stderr.read(Buff, sizeof(Buff));
        end;
        Mem.Seek(0, sofrombeginning);

        Ini := TIniFile.Create(Mem, [ifostripcomments, ifostripinvalid]);

        i := 0;
        bit4sample := 0;
        while Ini.SectionExists('streams.stream.' + inttostr(i)) do
        begin
          if Ini.ReadString('streams.stream.' + inttostr(i), 'codec_type', '') = 'audio' then
          begin
            bit4sample := Ini.ReadInteger('streams.stream.' + inttostr(i), 'bits_per_raw_sample',  bit4sample);
          end;
          inc(i);
        end;
        Ini.Destroy;
        Mem.Destroy;
      except
      end;
      Process.Destroy;

      // decode to .AudioAnalyzer
      Process := TProcess.Create(nil);
      try
        Process.Parameters.Clear;
        Process.CurrentDirectory := ExtractFileDir(Track.Filename);
        Process.Executable := 'ffmpeg';
        Process.Parameters.Add('-y');
        Process.Parameters.Add('-hide_banner');
        Process.Parameters.Add('-i');
        Process.Parameters.Add(ExtractFileName(Track.Filename));

        if bit4sample = 24 then
        begin
          Process.Parameters.Add('-c:a');
          Process.Parameters.Add('pcm_s24le');
        end;

        if bit4sample = 32 then
        begin
          Process.Parameters.Add('-c:a');
          Process.Parameters.Add('pcm_s32le');
        end;

        Process.Parameters.Add(TempFile);
        Process.Options := [poNoConsole, poWaitOnExit];
        Process.Execute;
      except
      end;
      Process.Destroy;

    end else
      TempFile := Track.Filename;

    Stream := TFileStream.Create(TempFile, fmOpenRead or fmShareExclusive);
  except
    Stream := nil;
  end;

  if Assigned(Stream) then
  begin
    Buffer := TReadBufStream.Create(Stream);
    AudioAnalyzer := TTrackAnalyzer.Create(Track, Buffer, True);
    AudioAnalyzer.OnStart := @OnStartAnalyzer;
    AudioAnalyzer.OnTick  := @OnTickAnalyzer;
    AudioAnalyzer.OnStop  := @OnStopAnalyzer;
    AudioAnalyzer.Start;
  end else
  begin
    MessageDlg('AudioMeter', Format('Error to open file "%s"', [TempFile]), mtError, [mbOk], '');
    Track := nil;
  end;
end;

procedure TAudioFrm.Clear;
begin
  pcm   .Font.Color := clGray;
  bit8  .Font.Color := clGray;
  bit16 .Font.Color := clGray;
  bit24 .Font.Color := clGray;
  khz44 .Font.Color := clGray;
  khz48 .Font.Color := clGray;
  khz88 .Font.Color := clGray;
  khz96 .Font.Color := clGray;
  khz176.Font.Color := clGray;
  khz192.Font.Color := clGray;
  Mono  .Font.Color := clGray;
  Stereo.Font.Color := clGray;

  TruePeakLabel  .Font.Color := clGray;
  TPLLeftValue   .Font.Color := clGray;
  TPLRightValue  .Font.Color := clGray;
  PLLeftValue    .Font.Color := clGray;
  PLRightValue   .Font.Color := clGray;

  RMSLeftValue   .Font.Color := clGray;
  RMSRightValue  .Font.Color := clGray;
  CRestLeftValue .Font.Color := clGray;
  CRestRightValue.Font.Color := clGray;

  LoudnessFSLabel         .Font.Color := clGray;
  MomentaryLoudnessValue  .Font.Color := clGray;
  ShortTermLoudnessValue  .Font.Color := clGray;
  IntegratedLoudnessValue .Font.Color := clGray;
  RangeLoudnessValue      .Font.Color := clGray;
  PeakToLoudnessRatioValue.Font.Color := clGray;
  DRValue                 .Font.Color := clGray;
  DRLabel                 .Font.Color := clGray;

  TPLLeftValue   .Caption := '-';
  TPLRightValue  .Caption := '-';
  PLLeftValue    .Caption := '-';
  PLRightValue   .Caption := '-';

  RMSLeftValue   .Caption := '-';
  RMSRightValue  .Caption := '-';
  CRestLeftValue .Caption := '-';
  CRestRightValue.Caption := '-';

  MomentaryLoudnessValue  .Caption := '-';
  ShortTermLoudnessValue  .Caption := '-';
  IntegratedLoudnessValue .Caption := '-';
  RangeLoudnessValue      .Caption := '-';
  PeakToLoudnessRatioValue.Caption := '-';
  DRValue                 .Caption := '--';

  TrackFileName.Font.Color := clwhite;
  TrackFileName.Caption := 'Audio';

  IsNeededUpdateScreens := True;
end;

procedure TAudioFrm.ClearTrackList;
begin
  LastIndex  := -1;
  TrackIndex := -1;
  TrackList.Clear;
end;

// Button Events

procedure TAudioFrm.BtnFileClick(sender: tobject);
begin
  FileDialog.Filter:= OpenDialogFileFilter;
  if FileDialog.Execute then
  begin
    PlaySound.StopSound;
    BtnPlay.ImageIndex := 5;

    ClearTrackList;
    if IsFileSupported(ExtractFileExt(FileDialog.FileName)) then
    begin
      TrackList.Add(FileDialog.FileName);
      TrackFile  := ChangeFileExt(FileDialog.FileName, '.md');
      TrackIndex := 0;

      IsNeededKillAnalyzer := False;
    end else
    begin
      TrackFileName.Caption    := 'File format error!';
      TrackFileName.Font.Color := clrRed;
    end;
  end;
  Execute;
end;

procedure TAudioFrm.BtnFolderClick(Sender: TObject);
var
  Err:  longint;
  Path: string;
  SR:   TSearchRec;
begin
  if DirDialog.Execute then
  begin
    PlaySound.StopSound;
    BtnPlay.ImageIndex := 5;

    ClearTrackList;
    Path := IncludeTrailingBackslash(DirDialog.FileName);
     Err := SysUtils.FindFirst(Path + '*.*', faAnyfile, SR);
    while Err = 0 do
    begin
      if SR.Attr and (faDirectory) = 0 then
      begin
        if IsFileSupported(ExtractFileExt(SR.Name)) then
          TrackList.Add(Path + SR.Name);
      end;
      Err := FindNext(SR);
    end;
    SysUtils.FindClose(SR);
    TrackList.Sort;
    TrackFile  := Path + ExtractFileName(DirDialog.FileName) + '.md';
    TrackIndex := 0;

    IsNeededKillAnalyzer  := False;
  end;
  Execute;
end;

procedure TAudioFrm.BtnPlayClick(Sender: TObject);
begin
  case BtnPlay.ImageIndex of
    6: begin
         PlaySound.StopSound;
         BtnPlay.ImageIndex := 5;
       end;
    7: begin
         PlaySound.StopSound;
         BtnPlay.ImageIndex := 5;
       end;
  else begin
         PlaySound.StopSound;
         BtnPlay.ImageIndex := 5;
         if FileExists(TempFile) then
         begin
           PlaySound.PlayStyle := psaSync;
           PlaySound.SoundFile := TempFile;
           PlaySound.Execute;
           BtnPlay.ImageIndex := 6;
         end;
       end;
  end;
end;

// mouse events

procedure TAudioFrm.BtnFileMouseUp(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  BtnFile.onmousemove := @btnfilemousemove;
end;

procedure TAudioFrm.BtnFileMouseDown(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  BtnFile.onmousemove := nil;
  LoadButtonIcon(BtnFile, 1, 0, 0);
end;

procedure TAudioFrm.BtnFileMouseLeave(sender: tobject);
begin
  LoadButtonIcon(BtnFile, 1, 0, 0);
end;

procedure TAudioFrm.BtnFileMouseMove(sender: tobject;
  shift: tshiftstate; x, y: integer);
begin
  LoadButtonIcon(BtnFile, 0, 0, 0);
end;

procedure TAudioFrm.BtnFolderMouseDown(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  BtnFolder.onmousemove := nil;
  LoadButtonIcon(BtnFolder, 3, 0, 5);
end;

procedure TAudioFrm.btnfoldermouseleave(sender: tobject);
begin
  LoadButtonIcon(BtnFolder, 3, 0, 5);
end;

procedure TAudioFrm.btnfoldermousemove(sender: tobject;
  shift: tshiftstate; x, y: integer);
begin
  LoadButtonIcon(BtnFolder, 2, 0, 5);
end;

procedure TAudioFrm.btnfoldermouseup(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  BtnFolder.onmousemove := @btnfoldermousemove;
end;

procedure TAudioFrm.BtnPlayMouseDown(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
 BtnPlay.onmousemove := nil;
 case BtnPlay.ImageIndex of
   4: BtnPlay.ImageIndex := 5;
   5: BtnPlay.ImageIndex := 4;
   6: BtnPlay.ImageIndex := 7;
   7: BtnPlay.ImageIndex := 6;
 end;
end;

procedure TAudioFrm.BtnPlayMouseLeave(sender: tobject);
begin
 case BtnPlay.ImageIndex of
   4: BtnPlay.ImageIndex := 5;
   6: BtnPlay.ImageIndex := 7;
 end;
end;

procedure TAudioFrm.BtnPlayMouseMove(sender: tobject; shift: tshiftstate; x, y: integer);
begin
 case BtnPlay.ImageIndex of
   5: BtnPlay.ImageIndex := 4;
   7: BtnPlay.ImageIndex := 6;
 end;
end;

procedure TAudioFrm.BtnPlayMouseUp(sender: tobject; button: tmousebutton;
  shift: tshiftstate; x, y: integer);
begin
  BtnPlay.onmousemove := @btnplaymousemove;
  case BtnPlay.ImageIndex of
    5: BtnPlay.ImageIndex := 4;
    7: BtnPlay.ImageIndex := 6;
  end;
end;

// ---

procedure TAudioFrm.LoadButtonIcon(Btn: TImage; Index: longint; X, Y: longint);
begin
  Btn.Center := False;
  Btn.Stretch := False;
  Btn.Proportional := True;
  Btn.Picture.Bitmap := nil;
  Buttons.Draw(Btn.Canvas, X, Y, Index);
end;

procedure TAudioFrm.DisableButtons;
begin
  BtnPlay  .Enabled := False;
  BtnFile  .Enabled := False;
  BtnFolder.Enabled := False;

  DRValue.Visible := True;
  ProgressRing.Value := 0;
end;

procedure TAudioFrm.EnableButtons;
begin
  BtnPlay  .Enabled := True;
  BtnFile  .Enabled := True;
  BtnFolder.Enabled := True;

  DRValue.Visible := True;
  ProgressRing.Value := 0;
end;

procedure TAudioFrm.ScreenTimerTimer(Sender: TObject);
var
  Index: longint;
  Track: TTrack;
begin
  if LastWidth <> Width  then
  begin
    LastWidth := Width;
    IsNeededUpdateScreens := True;
    Exit;
  end;

  if LastHeight <> Height then
  begin
    LastHeight := Height;
    IsNeededUpdateScreens := True;
    Exit;
  end;

  Index := TrackIndex -1;
  if (Index >= 0) and (Index < TrackList.Count) then
  begin
    if Index <> LastIndex then
    begin
      LastIndex := Index;
      IsNeededUpdateScreens := True;
      Exit;
    end;
  end;

  if ScreenDrawer <> nil then Exit;

  if IsNeededUpdateScreens then
  begin
    Track := nil;
    if (LastIndex > -1) and (LastIndex < TrackList.Count) then
    begin
      Track := TrackList[LastIndex]
    end;

    ScreenDrawer := TScreenDrawer.Create(Track, VirtualScreen.Width, VirtualScreen.Height);
    ScreenDrawer.OnStart := @OnStartDrawer;
    ScreenDrawer.OnStop  := @OnStopDrawer;
    ScreenDrawer.Start;
    IsNeededUpdateScreens := False;
  end;
end;

procedure TAudioFrm.MainBoardRedraw(ATrack: TTrack);
begin
  if Assigned(ATrack) then
  begin
    TrackFileName.Font.Color := clWhite;
    TrackFileName.Caption    := ExtractFileName(ATrack.FileName);
    while (TrackFileName.Left + TrackFileName.Width) > (BtnFolder.left + BtnFolder.Width) do
    begin
      TrackFileName.Caption := CutOff(TrackFileName.Caption);
    end;

    pcm   .Font.Color := clWhite;
    bit8  .Font.Color := clGray; if ATrack.Bitspersample = 8      then bit8  .Font.Color := clWhite;
    bit16 .Font.Color := clGray; if ATrack.Bitspersample = 16     then bit16 .Font.Color := clWhite;
    bit24 .Font.Color := clGray; if ATrack.Bitspersample = 24     then bit24 .Font.Color := clWhite;

    khz44 .Font.Color := clGray; if ATrack.Samplerate    = 44100  then khz44 .Font.Color := clWhite;
    khz48 .Font.Color := clGray; if ATrack.Samplerate    = 48000  then khz48 .Font.Color := clWhite;
    khz88 .Font.Color := clGray; if ATrack.Samplerate    = 88000  then khz88 .Font.Color := clWhite;
    khz96 .Font.Color := clGray; if ATrack.Samplerate    = 96000  then khz96 .Font.Color := clWhite;
    khz176.Font.Color := clGray; if ATrack.Samplerate    = 176400 then khz176.Font.Color := clWhite;
    khz192.Font.Color := clGray; if ATrack.Samplerate    = 192000 then khz192.Font.Color := clWhite;

    Mono  .Font.Color := clGray; if ATrack.ChannelCount  = 1      then Mono  .Font.Color := clWhite;
    Stereo.Font.Color := clGray; if ATrack.ChannelCount  = 2      then Stereo.Font.Color := clWhite;

    TruePeakLabel.Font.Color := clWhite;
    if ATrack.ChannelCount > 0 then if Decibel(ATrack.Loudness.TruePeak(0)) <  0.0 then tplleftvalue .Font.Color := clLime;
    if ATrack.ChannelCount > 0 then if Decibel(ATrack.Loudness.TruePeak(0)) >= 0.0 then tplleftvalue .Font.Color := clYellow;
    if ATrack.ChannelCount > 0 then if Decibel(ATrack.Loudness.TruePeak(0)) >  0.5 then tplleftvalue .Font.Color := clRed;

    if ATrack.ChannelCount > 1 then if Decibel(ATrack.Loudness.TruePeak(1)) <  0.0 then tplrightvalue.Font.Color := clLime;
    if ATrack.ChannelCount > 1 then if Decibel(ATrack.Loudness.TruePeak(1)) >= 0.0 then tplrightvalue.Font.Color := clYellow;
    if ATrack.ChannelCount > 1 then if Decibel(ATrack.Loudness.TruePeak(1)) >  0.5 then tplrightvalue.Font.Color := clRed;

    if ATrack.ChannelCount > 0 then PLleftvalue    .Font.Color := clWhite;
    if ATrack.ChannelCount > 1 then PLRightvalue   .Font.Color := clWhite;
    if ATrack.ChannelCount > 0 then Rmsleftvalue   .Font.Color := clWhite;
    if ATrack.ChannelCount > 1 then RmsRightvalue  .Font.Color := clWhite;
    if ATrack.ChannelCount > 0 then CRestleftvalue .Font.Color := clWhite;
    if ATrack.ChannelCount > 1 then CRestRightvalue.Font.Color := clWhite;

    LoudnessFSLabel.Font.Color := clWhite;
    if ATrack.ChannelCount > 0 then IntegratedLoudnessValue .Font.Color := clWhite;
    if ATrack.ChannelCount > 1 then RangeLoudnessValue      .Font.Color := clWhite;
    if ATrack.ChannelCount > 0 then PeakToLoudnessRatioValue.Font.Color := clWhite;

    if ATrack.ChannelCount > 0 then plleftvalue .Caption := Format('%0.2f', [ATrack.Loudness.Peak(0)]);
    if ATrack.ChannelCount > 1 then plrightvalue.Caption := Format('%0.2f', [ATrack.Loudness.Peak(1)]);

    if ATrack.ChannelCount > 0 then tplleftvalue   .Caption := Format('%0.2f', [ATrack.Loudness.TruePeak(0)]);
    if ATrack.ChannelCount > 1 then tplrightvalue  .Caption := Format('%0.2f', [ATrack.Loudness.TruePeak(1)]);
    if ATrack.ChannelCount > 0 then rmsleftvalue   .Caption := Format('%0.2f', [ATrack.Loudness.Rms(0)]);
    if ATrack.ChannelCount > 1 then rmsrightvalue  .Caption := Format('%0.2f', [ATrack.Loudness.Rms(1)]);
    if ATrack.ChannelCount > 0 then crestleftvalue .Caption := Format('%0.2f', [ATrack.Loudness.CrestFactor(0)]);
    if ATrack.ChannelCount > 1 then crestrightvalue.Caption := Format('%0.2f', [ATrack.Loudness.CrestFactor(1)]);
    if ATrack.ChannelCount > 0 then PeakToLoudnessRatioValue   .Caption := Format('%0.2f', [ATrack.Loudness.PeakToLoudnessRatio]);

    if ATrack.ChannelCount > 0 then IntegratedLoudnessValue .Caption := Format('%0.2f', [ATrack.Loudness.IntegratedLoudness]);
    if ATrack.ChannelCount > 0 then RangeLoudnessValue      .Caption := Format('%0.2f', [ATrack.Loudness.LoudnessRange]);
    if ATrack.ChannelCount > 0 then PeakToLoudnessRatioValue.Caption := Format('%0.2f', [ATrack.Loudness.PeakToLoudnessRatio]);

    DRValue.Caption := '--';
    DRValue.Font.Color := clWhite;
    DRLabel.Font.Color := clWhite;
    if (ATrack.DRMeter.DR) > 0 then
    begin
      DRValue.Caption := format('%2.0f', [ATrack.DRMeter.DR]);
      if DRValue.Caption = ' 0' then DRValue.Font.Color := RGBToColor(255,   0, 0) else
      if DRValue.Caption = ' 1' then DRValue.Font.Color := RGBToColor(255,   0, 0) else
      if DRValue.Caption = ' 2' then DRValue.Font.Color := RGBToColor(255,   0, 0) else
      if DRValue.Caption = ' 3' then DRValue.Font.Color := RGBToColor(255,   0, 0) else
      if DRValue.Caption = ' 4' then DRValue.Font.Color := RGBToColor(255,   0, 0) else
      if DRValue.Caption = ' 5' then DRValue.Font.Color := RGBToColor(255,   0, 0) else
      if DRValue.Caption = ' 6' then DRValue.Font.Color := RGBToColor(255,   0, 0) else
      if DRValue.Caption = ' 7' then DRValue.Font.Color := RGBToColor(255,   0, 0) else
      if DRValue.Caption = ' 8' then DRValue.Font.Color := RGBToColor(255,  72, 0) else
      if DRValue.Caption = ' 9' then DRValue.Font.Color := RGBToColor(255, 145, 0) else
      if DRValue.Caption = '10' then DRValue.Font.Color := RGBToColor(255, 217, 0) else
      if DRValue.Caption = '11' then DRValue.Font.Color := RGBToColor(217, 255, 0) else
      if DRValue.Caption = '12' then DRValue.Font.Color := RGBToColor(144, 255, 0) else
      if DRValue.Caption = '13' then DRValue.Font.Color := RGBToColor( 72, 255, 0) else
                                     DRValue.Font.Color := RGBToColor(  0, 255, 0);
    end;
  end;
end;

procedure TAudioFrm.VirtualScreenRedraw(Sender: TObject; Bitmap: TBGRABitmap);
var
  i: longint;
  OffSet: longint;
begin
  OffSet := 0;
  Bitmap.FillTransparent;
  for i := Low(VirtualScreens) to High(VirtualScreens) do
  begin
    Bitmap.PutImage(0, OffSet , VirtualScreens[i], dmSet);
    Inc(OffSet, VirtualScreens[i].Height);
  end;
end;

end.
