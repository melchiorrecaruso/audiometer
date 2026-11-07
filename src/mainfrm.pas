{
  Description: Main Form.

  Copyright (C) 2020-2026 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
  stdctrls, extctrls, comctrls, Menus, bufstream, AudioTrack, TrackAnalyzer, Decoder, bclistbox,
  bgrabitmap, bgrabitmaptypes, bgravirtualscreen, BCFluentProgressRing,
  drawers, Common, LCLType, Interfaces, BaseGraphics, BaseFrm;

type
  { TAudioFrm }

  TAudioFrm = class(TBaseForm)
    bit16: TLabel;
    bit24: TLabel;
    bit32: TLabel;
    bit8: TLabel;
    DynamicRangeItem: TMenuItem;
    khz176: TLabel;
    khz192: TLabel;
    khz44: TLabel;
    khz48: TLabel;
    khz88: TLabel;
    khz96: TLabel;
    LeftBitPanel: TPanel;
    LeftHzPanel: TPanel;
    Mono: TLabel;
    LeftChannelsPanel: TPanel;
    RigthChannelsPanel: TPanel;
    PlayTimer: TIdleTimer;
    LoudnessItem: TMenuItem;
    MenuItem1: TMenuItem;
    RightBitPanel: TPanel;
    RightHzPanel: TPanel;
    SeparatorItem: TMenuItem;
    ShowAlltem: TMenuItem;
    Stereo: TLabel;
    WaveFormItem: TMenuItem;
    FreqSpectrumItem: TMenuItem;
    SpectrogramItem: TMenuItem;
    Popup: TPopupMenu;
    ProgressPanel: TPanel;
    Bevel4: TBevel;
    Bevel5: TBevel;
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
    PlayBtn: TSpeedButton;
    OpenFileBtn: TSpeedButton;
    OpenFolderBtn: TSpeedButton;
    ReportBtn: TSpeedButton;
    BtnBevel: TShape;
    StopBtn: TSpeedButton;
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

    PlaySound: TPlaySound;
    VirtualScreen: TBGRAVirtualScreen;
    bevel1: tbevel;
    bevel2: tbevel;
    Bevel3: tbevel;
    DRLabel: TStaticText;
    DRValue: TStaticText;
    DetailsPanel: tpanel;
    DRPanel: tpanel;
    TrackFileName: tlabel;
    DirDialog: tselectdirectorydialog;
    FileDialog: TOpenDialog;

    procedure FormCreate(sender: tobject);
    procedure FormCloseQuery(sender: tobject; var canclose: boolean);
    procedure FormResize(sender: tobject);
    procedure FormDestroy(sender: tobject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItemClick(Sender: TObject);
    // buttons
    procedure OpenFileBtnClick(sender: tobject);
    procedure OpenFolderBtnClick(sender: tobject);
    procedure PlayBtnClick(Sender: TObject);
    procedure PlayTimerStartTimer(Sender: TObject);
    procedure PlayTimerStopTimer(Sender: TObject);
    procedure PlayTimerTimer(Sender: TObject);
    procedure ReportBtnClick(Sender: TObject);
    procedure ScreenMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScreenMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure ScreenMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ScreenTimerTimer(Sender: TObject);
    procedure ShowAlltemClick(Sender: TObject);
    procedure StopBtnClick(Sender: TObject);
    procedure RedrawVirtualScreen(Sender: TObject; Bitmap: TBGRABitmap);
  private
    Buffer: TReadBufStream;
    Stream: TFileStream;
    Screen: TBGRABitmap;

    // A batch analyzes TrackList in order, one track at a time: non-WAV files
    // are first decoded to SessionTempFile by WaveDecoder, then AudioAnalyzer
    // reads TempFile. The batch is running while one of the two is assigned.
    TrackList: TTrackList;
    TrackFile: string;
    TrackIndex: longint;      // track being decoded or analyzed
    CompletedIndex: longint;  // last track analyzed successfully
    ShownIndex: longint;      // track shown by the labels and the charts
    BatchError: string;       // why the last batch stopped, '' if it ended
    TempFile: string;         // WAV read by the analyzer
    PlayFile: string;         // WAV of CompletedIndex, '' if not playable
    SessionTempFile: string;
    FFmpegExecutable: string;
    FFprobeExecutable: string;
    WaveDecoder: TWaveDecoder;
    // Cancelling: the workers are being stopped, their OnTerminate must not
    // continue the batch. Closing: the form is closing, nothing restarts.
    Cancelling: boolean;
    Closing: boolean;

    LastWidth: longint;
    LastHeight: longint;
    LastMode: TScreenDrawerModes;
    IsNeededUpdateScreens: boolean;

    PanX: longint;
    PanY: longint;
    PanStartX: longint;
    PanStartY: longint;
    PanStartOffsetX: longint;
    PanStartOffsetY: longint;
    IsPanning: boolean;
    MaxRenderWidth: longint;
    MaxRenderHeight: longint;

    PlayStart: QWord;
    FullTrackTitle: string;

    function BatchActive: boolean;
    function WorkersActive: boolean;
    procedure StopWorkers;
    procedure StopPlayback;
    procedure UpdateControls;

    procedure ClearTrackList;
    procedure StartBatch;
    procedure StartTrack;
    procedure StartAnalyzer(ATrack: TTrack);
    procedure FinishBatch(const AError: string);
    procedure ShowBatchError(Data: PtrInt);
    procedure AbortBatch(E: Exception);
    procedure ShowAbortedBatch(Data: PtrInt);

    procedure OnStartAnalyzer;
    procedure OnTickAnalyzer;
    procedure OnStopAnalyzer(Sender: TObject);
    procedure OnStopDecoder(Sender: TObject);
    procedure OnStartDrawer;
    procedure OnStopDrawer(Sender: TObject);

    procedure ClearData;
    procedure ShowTrackInfo(ATrack: TTrack);
    procedure UpdateTitle;
    procedure SetTrackTitle(const ATitle: string);
    procedure FitTrackTitle;

    procedure CalculateBackingSize(out AWidth, AHeight: longint);
    procedure ClampPan;
    procedure UpdatePanCursor;
    procedure ClearTemporaryWave;
    procedure InitializeTemporaryWave;
    procedure LocateFFmpegTools;
    procedure ReadSetting; overload;
    procedure WriteSetting;  overload;
  public
  end;

var
  AudioFrm: TAudioFrm;

implementation

{$R *.lfm}

uses
  Math, FileUtil, LazFileUtils, LazUTF8, ReportFrm, TrackReport;

const
  MAX_RENDER_WIDTH_KEY = 'MaxRenderWidth';
  MAX_RENDER_HEIGHT_KEY = 'MaxRenderHeight';

// Workers

function TAudioFrm.BatchActive: boolean;
begin
  Result := Assigned(AudioAnalyzer) or Assigned(WaveDecoder);
end;

function TAudioFrm.WorkersActive: boolean;
begin
  Result := BatchActive or Assigned(ScreenDrawer);
end;

procedure TAudioFrm.StopWorkers;
begin
  Cancelling := True;
  try
    if Assigned(WaveDecoder) then WaveDecoder.Terminate;
    if Assigned(AudioAnalyzer) then AudioAnalyzer.Terminate;
    if Assigned(ScreenDrawer) then ScreenDrawer.Terminate;
    // OnTerminate is synchronized even when Terminate precedes Execute, and
    // it always clears the worker reference. Run these callbacks here before
    // releasing tracks or their streams.
    while WorkersActive do
      CheckSynchronize(10);
  finally
    Cancelling := False;
  end;
end;

procedure TAudioFrm.StopPlayback;
begin
  PlayTimer.Enabled := False;
  PlaySound.StopSound;
end;

procedure TAudioFrm.UpdateControls;
var
  Busy: boolean;
begin
  Busy := BatchActive or Closing;
  // Opening a file or a folder cancels the running batch.
  OpenFileBtn.Enabled := not Closing;
  OpenFolderBtn.Enabled := not Closing;
  ReportBtn.Enabled := not Busy and Assigned(ReportForm) and
    (ReportForm.Memo.Lines.Count > 0);
  PlayBtn.Enabled := not Busy and (PlayFile <> '') and
    (ShownIndex = CompletedIndex) and FileExistsUTF8(PlayFile);
  StopBtn.Enabled := PlayTimer.Enabled;
end;

{ TAudioFrm }

procedure TAudioFrm.FormCreate(Sender: TObject);
begin
  Caption := APPTITLE;
  DefaultFontName := 'DejaVu Sans';
  DefaultFontFileName := ExtractFilePath(ParamStr(0)) + 'fonts/DejaVuSans/DejaVuSans.ttf';
  DoDirSeparators(DefaultFontFileName);
  InitializeChartFont;
  // ---
  Screen := TBGRABitmap.Create;
  IsNeededUpdateScreens := False;
  PanX := 0;
  PanY := 0;
  IsPanning := False;
  MaxRenderWidth := 0;
  MaxRenderHeight := 0;
  LastMode := [];
  // ---
  TrackList := TTrackList.Create;
  WaveDecoder := nil;
  Cancelling := False;
  Closing := False;
  InitializeTemporaryWave;
  ProgressRing.Value := 0;
  ProgressRing.Visible := True;
  Color := clBlack;
  // ---
  ReadSetting;
  ClearTrackList;
  LocateFFmpegTools;
end;

procedure TAudioFrm.FormDestroy(Sender: TObject);
begin
  ScreenTimer.Enabled := False;
  Application.RemoveAsyncCalls(Self);
  StopPlayback;
  ClearTemporaryWave;
  FreeAndNil(Screen);
  FreeAndNil(TrackList);
  WriteSetting;
end;

procedure TAudioFrm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  Closing := True;
  ScreenTimer.Enabled := False;
  StopPlayback;
  StopWorkers;
  CanClose := True;
end;

procedure TAudioFrm.FormResize(Sender: TObject);
var
  BackingWidth, BackingHeight: longint;
begin
  CalculateBackingSize(BackingWidth, BackingHeight);
  if Assigned(Screen) and
     ((BackingWidth > Screen.Width) or
      (BackingHeight > Screen.Height)) then
    IsNeededUpdateScreens := True;
  ClampPan;
  UpdatePanCursor;
  if Assigned(VirtualScreen) then
    VirtualScreen.RedrawBitmap;
  FitTrackTitle;
end;

procedure TAudioFrm.ReadSetting;
begin
  DynamicRangeItem.Checked := ReadSetting(DynamicRangeItem.Name, True);
  FreqSpectrumItem.Checked := ReadSetting(FreqSpectrumItem.Name, True);
  LoudnessItem    .Checked := ReadSetting(LoudnessItem    .Name, False);
  SpectrogramItem .Checked := ReadSetting(SpectrogramItem .Name, True);
  WaveformItem    .Checked := ReadSetting(WaveformItem    .Name, True);
  MaxRenderWidth  := Max(0, ReadSetting(MAX_RENDER_WIDTH_KEY, 0));
  MaxRenderHeight := Max(0, ReadSetting(MAX_RENDER_HEIGHT_KEY, 0));
end;

procedure TAudioFrm.WriteSetting;
begin
  WriteSetting(DynamicRangeItem.Name, DynamicRangeItem.Checked);
  WriteSetting(FreqSpectrumItem.Name, FreqSpectrumItem.Checked);
  WriteSetting(LoudnessItem    .Name, LoudnessItem    .Checked);
  WriteSetting(SpectrogramItem .Name, SpectrogramItem .Checked);
  WriteSetting(WaveformItem    .Name, WaveformItem    .Checked);
  WriteSetting(MAX_RENDER_WIDTH_KEY, MaxRenderWidth);
  WriteSetting(MAX_RENDER_HEIGHT_KEY, MaxRenderHeight);
end;

// Batch

procedure TAudioFrm.ClearTrackList;
begin
  StopWorkers;
  if Assigned(ReportForm) and Assigned(ReportForm.Memo) then
    ReportForm.Memo.Clear;
  TrackList.Clear;
  TrackIndex := -1;
  CompletedIndex := -1;
  ShownIndex := -1;
  BatchError := '';
  TempFile := '';
  PlayFile := '';
  PanX := 0;
  PanY := 0;
  ProgressRing.Value := 0;
  ShowTrackInfo(nil);
  // Blank the previous charts at once, the new ones follow the analysis.
  if (Screen.Width > 0) and (Screen.Height > 0) then
    Screen.Fill(BGRA(0, 0, 0, 255));
  VirtualScreen.RedrawBitmap;
  IsNeededUpdateScreens := True;
  UpdateControls;
end;

procedure TAudioFrm.StartBatch;
begin
  if TrackList.Count = 0 then
  begin
    FinishBatch('No supported audio files found.');
    Exit;
  end;
  BatchError := '';
  TrackIndex := 0;
  StartTrack;
end;

procedure TAudioFrm.StartTrack;
var
  Track: TTrack;
begin
  Track := TrackList[TrackIndex];
  ProgressRing.Value := 0;
  if SameText(ExtractFileExt(Track.Filename), '.wav') then
  begin
    TempFile := Track.Filename;
    StartAnalyzer(Track);
  end else
  begin
    // The decoder overwrites the session WAV: it no longer holds PlayFile.
    if PlayFile = SessionTempFile then PlayFile := '';
    TempFile := SessionTempFile;
    try
      WaveDecoder := TWaveDecoder.Create(FFmpegExecutable, FFprobeExecutable,
        Track.Filename, TempFile);
      WaveDecoder.OnTerminate := @OnStopDecoder;
      WaveDecoder.Start;
    except
      on E: Exception do
      begin
        FreeAndNil(WaveDecoder);
        FinishBatch(Format('Unable to start the decoder for "%s": %s',
          [Track.Filename, E.Message]));
      end;
    end;
  end;
  UpdateControls;
end;

procedure TAudioFrm.StartAnalyzer(ATrack: TTrack);
begin
  try
    Stream := TFileStream.Create(TempFile, fmOpenRead or fmShareDenyWrite);
    Buffer := TReadBufStream.Create(Stream);
    AudioAnalyzer := TTrackAnalyzer.Create(ATrack, Buffer, True);
    AudioAnalyzer.OnStart := @OnStartAnalyzer;
    AudioAnalyzer.OnTick  := @OnTickAnalyzer;
    AudioAnalyzer.OnTerminate := @OnStopAnalyzer;
    AudioAnalyzer.Start;
  except
    on E: Exception do
    begin
      FreeAndNil(AudioAnalyzer);
      FreeAndNil(Buffer);
      FreeAndNil(Stream);
      FinishBatch(Format('Unable to analyze "%s": %s', [ATrack.Filename, E.Message]));
    end;
  end;
end;

procedure TAudioFrm.FinishBatch(const AError: string);
begin
  TrackIndex := -1;
  BatchError := AError;
  ProgressRing.Value := 0;
  // The report lists the analyzed tracks only: no report without tracks.
  if Assigned(ReportForm) then
  begin
    ReportForm.Memo.Clear;
    if CompletedIndex >= 0 then
    begin
      WriteTrackReport(TrackList, ReportForm.Memo.Lines, CompletedIndex + 1);
      if AError <> '' then
        ReportForm.Memo.Lines.Add('Analysis stopped: ' + AError);
    end;
  end;
  UpdateTitle;
  UpdateControls;
  // Shown after the synchronized OnTerminate has returned to its thread.
  if AError <> '' then
    Application.QueueAsyncCall(@ShowBatchError, 0);
end;

procedure TAudioFrm.ShowBatchError(Data: PtrInt);
begin
  if (BatchError <> '') and not Closing then
    MessageDlg('AudioMeter', BatchError, mtError, [mbOk], '');
end;

// FPC runs OnTerminate outside the thread's exception handler: an exception
// leaving it would kill the process. Stop the batch with the bare minimum
// here and update the form later, outside the synchronized call.
procedure TAudioFrm.AbortBatch(E: Exception);
begin
  // Cancellation/close owns the reset; do not queue a stale UI update.
  if Cancelling or Closing then Exit;
  TrackIndex := -1;
  // This is the last-resort handler of a synchronized OnTerminate. Even
  // formatting the message or allocating the async call may fail (e.g. OOM).
  try
    BatchError := Format('%s: %s', [E.ClassName, E.Message]);
    Application.QueueAsyncCall(@ShowAbortedBatch, 0);
  except
    // Never propagate an exception back into the terminating worker.
  end;
end;

procedure TAudioFrm.ShowAbortedBatch(Data: PtrInt);
begin
  if Closing then Exit;
  ProgressRing.Value := 0;
  UpdateTitle;
  UpdateControls;
  ShowBatchError(Data);
end;

// Track analyzer events

procedure TAudioFrm.OnStartAnalyzer;
begin
  UpdateControls;
end;

procedure TAudioFrm.OnTickAnalyzer;
begin
  if Assigned(AudioAnalyzer) and not Cancelling then
    ProgressRing.Value := AudioAnalyzer.Percentage;
end;

procedure TAudioFrm.OnStopAnalyzer(Sender: TObject);
var
  Status: longint;
  ErrorText: string;
begin
  // Clear the reference even if reading results or releasing streams fails.
  AudioAnalyzer := nil;
  try
    try
      Status := TTrackAnalyzer(Sender).Status;
    finally
      try
        FreeAndNil(Buffer);
      finally
        FreeAndNil(Stream);
      end;
    end;
    if Cancelling then Exit;
    if Status <> 0 then
    begin
      case Status of
        -1: ErrorText := 'Invalid WAV format';
        -2: ErrorText := 'Empty audio file';
        -3: ErrorText := 'Audio file too short';
        else ErrorText := 'Audio analysis failed';
      end;
      FinishBatch(Format('%s: "%s"', [ErrorText, TrackList[TrackIndex].Filename]));
      Exit;
    end;

    CompletedIndex := TrackIndex;
    PlayFile := TempFile;
    Inc(TrackIndex);
    if TrackIndex < TrackList.Count then
      StartTrack
    else
      FinishBatch('');
  except
    on E: Exception do
      AbortBatch(E);
  end;
end;

// Wave decoder events

procedure TAudioFrm.OnStopDecoder(Sender: TObject);
var
  Successful: boolean;
  ErrorMessage, InputFile: string;
begin
  WaveDecoder := nil;
  try
    if Cancelling then Exit;
    Successful   := TWaveDecoder(Sender).Successful;
    ErrorMessage := TWaveDecoder(Sender).ErrorMessage;
    InputFile    := TWaveDecoder(Sender).InputFile;
    if Successful then
      StartAnalyzer(TrackList[TrackIndex])
    else
      FinishBatch(Format('Unable to decode "%s": %s', [InputFile, ErrorMessage]));
  except
    on E: Exception do
      AbortBatch(E);
  end;
end;

// Chart drawer events

procedure TAudioFrm.OnStartDrawer;
var
  BackingWidth, BackingHeight: longint;
begin
  // Keep the largest chart area reached by the form. Smaller windows display
  // the same backing bitmap through the pan viewport.
  CalculateBackingSize(BackingWidth, BackingHeight);
  Screen.SetSize(BackingWidth, BackingHeight);
end;

procedure TAudioFrm.OnStopDrawer(Sender: TObject);
var
  Successful: boolean;
  ErrorMessage: string;
begin
  ScreenDrawer := nil;
  // Same reason as AbortBatch: nothing may leave the synchronized call.
  try
    if Cancelling or Closing then Exit;
    Successful   := TScreenDrawer(Sender).Successful;
    ErrorMessage := TScreenDrawer(Sender).ErrorMessage;
    if not Successful and (ErrorMessage = '') then
    begin
      // Cancelled: do not show the partial picture, draw it again.
      IsNeededUpdateScreens := True;
      Exit;
    end;
    VirtualScreen.Hint := ErrorMessage;
    VirtualScreen.ShowHint := ErrorMessage <> '';
    ClampPan;
    UpdatePanCursor;
    VirtualScreen.RedrawBitmap;
  except
    on E: Exception do
    begin
      try
        VirtualScreen.Hint := Format('%s: %s', [E.ClassName, E.Message]);
        VirtualScreen.ShowHint := True;
      except
        // Reporting a drawing error must not terminate the process either.
      end;
    end;
  end;
end;

// Temporary wave

procedure TAudioFrm.ClearTemporaryWave;
begin
  if (SessionTempFile <> '') and FileExistsUTF8(SessionTempFile) then
    DeleteFileUTF8(SessionTempFile);
  TempFile := '';
  PlayFile := '';
end;

procedure TAudioFrm.InitializeTemporaryWave;
var
  SessionGuid: TGUID;
begin
  // An empty name makes the decoder refuse the conversion.
  SessionTempFile := '';
  if CreateGUID(SessionGuid) <> 0 then
  begin
    MessageDlg('AudioMeter',
      'Warning: unable to generate the temporary WAV name. Files that require conversion cannot be analyzed.',
      mtWarning, [mbOk], '');
    Exit;
  end;
  SessionTempFile := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'audiometer_' + Copy(GUIDToString(SessionGuid), 2, 36) + '.wav';
end;

procedure TAudioFrm.LocateFFmpegTools;
var
  Missing: string;
begin
  Missing := Decoder.LocateFFmpegTools(FFmpegExecutable, FFprobeExecutable);
  if Missing <> '' then
  begin
    {$IFDEF MSWINDOWS}
    MessageDlg('AudioMeter', Format(
      'Warning: required audio tools not found next to the AudioMeter executable: %s.' + LineEnding +
      'Files that require conversion cannot be analyzed until the missing tools are installed.',
      [Missing]), mtWarning, [mbOk], '');
    {$ELSE}
    MessageDlg('AudioMeter', Format(
      'Warning: required audio tools not found in the system PATH: %s.' + LineEnding +
      'Files that require conversion cannot be analyzed until the missing tools are installed.',
      [Missing]), mtWarning, [mbOk], '');
    {$ENDIF}
  end;
end;

// Button Events

procedure TAudioFrm.OpenFileBtnClick(sender: tobject);
begin
  if Closing then Exit;
  FileDialog.Filter := OpenDialogFileFilter;
  if not FileDialog.Execute then Exit;

  StopPlayback;
  ClearTrackList;
  if IsFileSupported(ExtractFileExt(FileDialog.FileName)) then
  begin
    TrackList.Add(FileDialog.FileName);
    TrackFile := ChangeFileExt(FileDialog.FileName, '.md');
    StartBatch;
  end else
    FinishBatch(Format('Unsupported file format: "%s"', [FileDialog.FileName]));
end;

procedure TAudioFrm.OpenFolderBtnClick(Sender: TObject);
var
  Err:  longint;
  Path: string;
  SR:   TSearchRec;
begin
  if Closing then Exit;
  if not DirDialog.Execute then Exit;

  StopPlayback;
  ClearTrackList;
  Path := IncludeTrailingPathDelimiter(DirDialog.FileName);
  Err := SysUtils.FindFirst(Path + '*', faAnyfile, SR);
  while Err = 0 do
  begin
    if (SR.Attr and faDirectory = 0) and IsFileSupported(ExtractFileExt(SR.Name)) then
      TrackList.Add(Path + SR.Name);
    Err := FindNext(SR);
  end;
  SysUtils.FindClose(SR);
  TrackList.Sort;
  TrackFile := Path + ExtractFileName(ExcludeTrailingPathDelimiter(DirDialog.FileName)) + '.md';
  StartBatch;
end;

procedure TAudioFrm.ReportBtnClick(Sender: TObject);
begin
  if not ReportBtn.Enabled then Exit;
  ReportForm.SaveDialog.InitialDir := ExtractFileDir (TrackFile);
  ReportForm.SaveDialog.FileName   := ExtractFileName(TrackFile);
  ReportForm.ShowModal;
end;

procedure TAudioFrm.PlayBtnClick(Sender: TObject);
begin
  if not PlayBtn.Enabled then Exit;
  StopPlayback;
  PlaySound.PlayStyle := psAsync;
  PlaySound.SoundFile := PlayFile;
  try
    PlaySound.Execute;
  except
    on E: Exception do
    begin
      MessageDlg('AudioMeter', 'Unable to play the track: ' + E.Message,
        mtError, [mbOk], '');
      Exit;
    end;
  end;
  PlayStart := GetTickCount64;
  PlayTimer.Enabled := True;
end;

procedure TAudioFrm.PlayTimerStartTimer(Sender: TObject);
begin
  ShortTermLoudnessValue.Font.Color := clSkyBlue;
  MomentaryLoudnessValue.Font.Color := clLime;
  UpdateControls;
end;

procedure TAudioFrm.PlayTimerStopTimer(Sender: TObject);
begin
  ShortTermLoudnessValue.Font.Color := clGray;
  MomentaryLoudnessValue.Font.Color := clGray;

  ShortTermLoudnessValue.Caption := '-';
  MomentaryLoudnessValue.Caption := '-';
  UpdateControls;
end;

procedure TAudioFrm.PlayTimerTimer(Sender: TObject);
var
  Track: TTrack;
  PlayTime: QWord;
begin
  if (ShownIndex < 0) or (ShownIndex >= TrackList.Count) then
  begin
    PlayTimer.Enabled := False;
    Exit;
  end;
  Track := TrackList[ShownIndex];
  PlayTime := GetTickCount64 - PlayStart;
  if (PlayTime > High(longint)) or (PlayTime >= Track.DurationSeconds * 1000) then
  begin
    PlayTimer.Enabled := False;
    Exit;
  end;
  ShortTermLoudnessValue.Caption := Format('%0.2f', [Track.Loudness.ShortTermLoudness(PlayTime)]);
  MomentaryLoudnessValue.Caption := Format('%0.2f', [Track.Loudness.MomentaryLoudness(PlayTime)]);
end;

procedure TAudioFrm.StopBtnClick(Sender: TObject);
begin
  StopPlayback;
end;

procedure TAudioFrm.MenuItemClick(Sender: TObject);
begin
  TMenuItem(Sender).Checked := not TMenuItem(Sender).Checked;
end;

procedure TAudioFrm.MenuItem1Click(Sender: TObject);
begin
  DynamicRangeItem.Checked := False;
  LoudnessItem    .Checked := False;
  WaveFormItem    .Checked := False;
  FreqSpectrumItem.Checked := False;
  SpectrogramItem .Checked := False;
end;

procedure TAudioFrm.ShowAlltemClick(Sender: TObject);
begin
  DynamicRangeItem.Checked := True;
  LoudnessItem    .Checked := False;
  WaveFormItem    .Checked := True;
  FreqSpectrumItem.Checked := True;
  SpectrogramItem .Checked := True;
end;

procedure TAudioFrm.CalculateBackingSize(out AWidth, AHeight: longint);
begin
  if Assigned(VirtualScreen) then
  begin
    MaxRenderWidth := Max(MaxRenderWidth, VirtualScreen.Width);
    MaxRenderHeight := Max(MaxRenderHeight, VirtualScreen.Height);
  end;
  // Bound persistent backing storage (64 MiB for the main RGBA bitmap).
  MaxRenderWidth := EnsureRange(MaxRenderWidth, 1, 4096);
  MaxRenderHeight := EnsureRange(MaxRenderHeight, 1, 4096);
  AWidth := MaxRenderWidth;
  AHeight := MaxRenderHeight;
end;

procedure TAudioFrm.ClampPan;
begin
  if not Assigned(Screen) or not Assigned(VirtualScreen) then Exit;
  PanX := EnsureRange(PanX, 0, Max(0, Screen.Width - VirtualScreen.Width));
  PanY := EnsureRange(PanY, 0, Max(0, Screen.Height - VirtualScreen.Height));
end;

procedure TAudioFrm.UpdatePanCursor;
begin
  if not Assigned(Screen) or not Assigned(VirtualScreen) then Exit;
  if (Screen.Width > VirtualScreen.Width) or
     (Screen.Height > VirtualScreen.Height) then
    VirtualScreen.Cursor := crSizeAll
  else
    VirtualScreen.Cursor := crDefault;
end;

procedure TAudioFrm.ScreenMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (Button <> mbLeft) or not Assigned(Screen) then Exit;
  if (Screen.Width <= VirtualScreen.Width) and
     (Screen.Height <= VirtualScreen.Height) then Exit;

  IsPanning := True;
  PanStartX := X;
  PanStartY := Y;
  PanStartOffsetX := PanX;
  PanStartOffsetY := PanY;
  SetCaptureControl(VirtualScreen);
end;

procedure TAudioFrm.ScreenMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if not IsPanning then Exit;

  // Move the image as if it were grabbed directly with the mouse.
  PanX := PanStartOffsetX - (X - PanStartX);
  PanY := PanStartOffsetY - (Y - PanStartY);
  ClampPan;
  VirtualScreen.RedrawBitmap;
end;

procedure TAudioFrm.ScreenMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then Exit;
  IsPanning := False;
  SetCaptureControl(nil);
end;

procedure TAudioFrm.ScreenTimerTimer(Sender: TObject);
var
  i: longint;
  BackingWidth, BackingHeight: longint;
  Track: TTrack;
  Mode: TScreenDrawerModes;
begin
  if Closing then Exit;

  if (LastWidth <> VirtualScreen.Width) or
     (LastHeight <> VirtualScreen.Height) then
  begin
    // Wait for the next tick: the window may still be resizing.
    LastWidth  := VirtualScreen.Width;
    LastHeight := VirtualScreen.Height;
    CalculateBackingSize(BackingWidth, BackingHeight);
    if (BackingWidth > Screen.Width) or
       (BackingHeight > Screen.Height) then
      IsNeededUpdateScreens := True;
    ClampPan;
    UpdatePanCursor;
    VirtualScreen.RedrawBitmap;
    Exit;
  end;

  Mode := [];
  if DynamicRangeItem.Checked then Include(Mode, smDynamicRange);
  if LoudnessItem    .Checked then Include(Mode, smLoudness    );
  if WaveFormItem    .Checked then Include(Mode, smWaveForm    );
  if FreqSpectrumItem.Checked then Include(Mode, smFreqSpectrum);
  if SpectrogramItem .Checked then Include(Mode, smSpectrogram );
  if LastMode <> Mode then
  begin
    LastMode := Mode;
    IsNeededUpdateScreens := True;
  end;
  if ShownIndex <> CompletedIndex then
    IsNeededUpdateScreens := True;

  if Assigned(ScreenDrawer) then
  begin
    // The picture being drawn is already out of date: drop it.
    if IsNeededUpdateScreens then ScreenDrawer.Terminate;
    Exit;
  end;

  if ShownIndex <> CompletedIndex then
  begin
    ShownIndex := CompletedIndex;
    // Only the shown track needs its samples for the charts.
    for i := ShownIndex - 1 downto 0 do
      TrackList[i].ClearChannels;
    ShowTrackInfo(TrackList[ShownIndex]);
    UpdateControls;
  end;

  if IsNeededUpdateScreens then
  begin
    IsNeededUpdateScreens := False;
    Track := nil;
    if (ShownIndex >= 0) and (ShownIndex < TrackList.Count) then
      Track := TrackList[ShownIndex];
    try
      // TrackList owns the track: ClearTrackList stops the drawer first.
      ScreenDrawer := TScreenDrawer.Create(Track, Screen);
      ScreenDrawer.OnStart := @OnStartDrawer;
      ScreenDrawer.OnTerminate := @OnStopDrawer;
      ScreenDrawer.Mode    := Mode;
      ScreenDrawer.Start;
    except
      on E: Exception do
      begin
        FreeAndNil(ScreenDrawer);
        VirtualScreen.Hint := 'Unable to render: ' + E.Message;
        VirtualScreen.ShowHint := True;
      end;
    end;
  end;
end;

procedure TAudioFrm.ClearData;
begin
  pcm   .Font.Color := clGray;
  bit8  .Font.Color := clGray;
  bit16 .Font.Color := clGray;
  bit24 .Font.Color := clGray;
  bit32 .Font.Color := clGray;
  kHz44 .Font.Color := clGray;
  kHz48 .Font.Color := clGray;
  kHz88 .Font.Color := clGray;
  kHz96 .Font.Color := clGray;
  kHz176.Font.Color := clGray;
  kHz192.Font.Color := clGray;
  Mono  .Font.Color := clGray;
  Stereo.Font.Color := clGray;
  Stereo.Caption := 'Stereo';

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
end;

procedure TAudioFrm.ShowTrackInfo(ATrack: TTrack);
begin
  ClearData;
  UpdateTitle;
  if Assigned(ATrack) then
  begin
    pcm   .Font.Color := clWhite;
    bit8  .Font.Color := clGray; if ATrack.Bitspersample = 8      then bit8  .Font.Color := clWhite;
    bit16 .Font.Color := clGray; if ATrack.Bitspersample = 16     then bit16 .Font.Color := clWhite;
    bit24 .Font.Color := clGray; if ATrack.Bitspersample = 24     then bit24 .Font.Color := clWhite;
    bit32 .Font.Color := clGray; if ATrack.Bitspersample = 32     then bit32 .Font.Color := clWhite;

    kHz44 .Font.Color := clGray; if ATrack.Samplerate    = 44100  then kHz44 .Font.Color := clWhite;
    kHz48 .Font.Color := clGray; if ATrack.Samplerate    = 48000  then kHz48 .Font.Color := clWhite;
    kHz88 .Font.Color := clGray; if ATrack.Samplerate    = 88200  then kHz88 .Font.Color := clWhite;
    kHz96 .Font.Color := clGray; if ATrack.Samplerate    = 96000  then kHz96 .Font.Color := clWhite;
    kHz176.Font.Color := clGray; if ATrack.Samplerate    = 176400 then kHz176.Font.Color := clWhite;
    kHz192.Font.Color := clGray; if ATrack.Samplerate    = 192000 then kHz192.Font.Color := clWhite;

    Mono  .Font.Color := clGray; if ATrack.ChannelCount  = 1 then Mono  .Font.Color := clWhite;
    Stereo.Font.Color := clGray; if ATrack.ChannelCount  = 2 then Stereo.Font.Color := clWhite;

    Stereo.Caption := 'Stereo';
    if ATrack.ChannelCount > 2 then
    begin
      Stereo.Font.Color := clWhite;
      Stereo.Caption    := ChannelLayoutName(ATrack.ChannelCount, ATrack.ChannelMask);
    end;

    TruePeakLabel.Font.Color := clWhite;
    if ATrack.ChannelCount > 0 then if ATrack.Loudness.TruePeak(0) <= 0.0 then TPLLeftValue .Font.Color := clLime;
    if ATrack.ChannelCount > 0 then if ATrack.Loudness.TruePeak(0) >  0.0 then TPLLeftValue .Font.Color := clYellow;
    if ATrack.ChannelCount > 0 then if ATrack.Loudness.TruePeak(0) >  0.5 then TPLLeftValue .Font.Color := clRed;

    if ATrack.ChannelCount > 1 then if ATrack.Loudness.TruePeak(1) <= 0.0 then TPLRightValue.Font.Color := clLime;
    if ATrack.ChannelCount > 1 then if ATrack.Loudness.TruePeak(1) >  0.0 then TPLRightValue.Font.Color := clYellow;
    if ATrack.ChannelCount > 1 then if ATrack.Loudness.TruePeak(1) >  0.5 then TPLRightValue.Font.Color := clRed;

    if ATrack.ChannelCount > 0 then PLleftvalue    .Font.Color := clWhite;
    if ATrack.ChannelCount > 1 then PLRightvalue   .Font.Color := clWhite;
    if ATrack.ChannelCount > 0 then Rmsleftvalue   .Font.Color := clWhite;
    if ATrack.ChannelCount > 1 then RmsRightvalue  .Font.Color := clWhite;
    if ATrack.ChannelCount > 0 then CRestleftvalue .Font.Color := clWhite;
    if ATrack.ChannelCount > 1 then CRestRightvalue.Font.Color := clWhite;

    LoudnessFSLabel.Font.Color := clWhite;
    if ATrack.ChannelCount > 0 then IntegratedLoudnessValue .Font.Color := clWhite;
    if ATrack.ChannelCount > 0 then RangeLoudnessValue      .Font.Color := clWhite;
    if ATrack.ChannelCount > 0 then PeakToLoudnessRatioValue.Font.Color := clWhite;

    if ATrack.ChannelCount > 0 then plleftvalue .Caption := Format('%0.2f', [ATrack.Loudness.Peak(0)]);
    if ATrack.ChannelCount > 1 then plrightvalue.Caption := Format('%0.2f', [ATrack.Loudness.Peak(1)]);

    if ATrack.ChannelCount > 0 then TPLLeftValue   .Caption := Format('%0.2f', [ATrack.Loudness.TruePeak(0)]);
    if ATrack.ChannelCount > 1 then TPLRightValue  .Caption := Format('%0.2f', [ATrack.Loudness.TruePeak(1)]);
    if ATrack.ChannelCount > 0 then rmsleftvalue   .Caption := Format('%0.2f', [ATrack.Loudness.Rms(0)]);
    if ATrack.ChannelCount > 1 then rmsrightvalue  .Caption := Format('%0.2f', [ATrack.Loudness.Rms(1)]);
    if ATrack.ChannelCount > 0 then crestleftvalue .Caption := Format('%0.2f', [ATrack.Loudness.CrestFactor(0)]);
    if ATrack.ChannelCount > 1 then crestrightvalue.Caption := Format('%0.2f', [ATrack.Loudness.CrestFactor(1)]);

    if ATrack.ChannelCount > 0 then IntegratedLoudnessValue .Caption := Format('%0.2f', [ATrack.Loudness.IntegratedLoudness]);
    if ATrack.ChannelCount > 0 then RangeLoudnessValue      .Caption := Format('%0.2f', [ATrack.Loudness.LoudnessRange]);
    if ATrack.ChannelCount > 0 then PeakToLoudnessRatioValue.Caption := Format('%0.2f', [ATrack.Loudness.PeakToLoudnessRatio]);

    DRValue.Caption := '--';
    DRValue.Font.Color := clWhite;
    DRLabel.Font.Color := clWhite;
    if not IsInfinite(ATrack.DRMeter.DR) and not IsNan(ATrack.DRMeter.DR)
      and (ATrack.DRMeter.DR >= 0) then
    begin
      DRValue.Caption := Format('%2.0f', [ATrack.DRMeter.DR]);
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

procedure TAudioFrm.UpdateTitle;
var
  Title: string;
  N: SizeInt;
begin
  if BatchError <> '' then
  begin
    // The title holds one line, the hint and the dialog the whole message.
    Title := BatchError;
    N := Pos(#10, Title);
    if N > 0 then Title := TrimRight(Copy(Title, 1, N - 1));
    TrackFileName.Font.Color := clrRed;
    TrackFileName.Hint := BatchError;
  end else
  if (ShownIndex >= 0) and (ShownIndex < TrackList.Count) then
  begin
    Title := ExtractFileName(TrackList[ShownIndex].Filename);
    TrackFileName.Font.Color := clWhite;
    TrackFileName.Hint := TrackList[ShownIndex].Filename;
  end else
  begin
    Title := 'Audio';
    TrackFileName.Font.Color := clWhite;
    TrackFileName.Hint := '';
  end;
  TrackFileName.ShowHint := TrackFileName.Hint <> '';
  SetTrackTitle(Title);
end;

procedure TAudioFrm.SetTrackTitle(const ATitle: string);
begin
  FullTrackTitle := ATitle;
  FitTrackTitle;
end;

procedure TAudioFrm.FitTrackTitle;
var
  DisplayTitle: string;
  N, Available: integer;
begin
  // Room left of the progress ring, which keeps its spacing on both sides.
  Available := Max(0, PlayBtn.Left - TrackFileName.Left -
    2 * ProgressPanel.BorderSpacing.Left - ProgressPanel.Width);
  Canvas.Font.Assign(TrackFileName.Font);
  DisplayTitle := FullTrackTitle;
  N := UTF8Length(DisplayTitle);
  while (N > 0) and (Canvas.TextWidth(DisplayTitle) > Available) do
  begin
    Dec(N);
    DisplayTitle := UTF8Copy(FullTrackTitle, 1, N) + '...';
  end;
  if Canvas.TextWidth(DisplayTitle) > Available then DisplayTitle := '';
  TrackFileName.Caption := DisplayTitle;
end;

procedure TAudioFrm.RedrawVirtualScreen(Sender: TObject; Bitmap: TBGRABitmap);
begin
  if (ScreenDrawer = nil) and Assigned(Screen) and
     (Screen.Width > 0) and (Screen.Height > 0) and
     (Bitmap.Width > 0) and (Bitmap.Height > 0) then
  begin
    ClampPan;
    Bitmap.Fill(BGRA(0, 0, 0, 255));
    // Negative coordinates select the visible 1:1 viewport.  PutImage clips
    // the rest, so no scaling or text deformation takes place.
    Bitmap.PutImage(-PanX, -PanY, Screen, dmSet);
  end;
end;

end.
