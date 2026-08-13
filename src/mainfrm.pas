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
  stdctrls, extctrls, comctrls, Menus, bufstream, soundwav, bclistbox, process,
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

    procedure Execute;
    procedure ClearTrackList;
    procedure ClearData;
    procedure Clear;

    procedure OnStartAnalyzer;
    procedure OnTickAnalyzer;
    procedure OnStopAnalyzer;

    procedure OnStartDrawer;
    procedure OnStopDrawer;

    procedure DisableButtons;
    procedure EnableButtons;

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

    procedure RedrawScreen(ATrack: TTrack);
    procedure RedrawVirtualScreen(Sender: TObject; Bitmap: TBGRABitmap);
  private
    Buffer: TReadBufStream;
    Stream: TFileStream;
    Screen: TBGRABitmap;

    TrackIndex: longint;
    TrackList: TTrackList;
    TrackFile: string;
    TempFile:  string;
    SessionTempFile: string;
    TempWaveError: string;
    TempWaveReady: boolean;
    FFmpegExecutable: string;
    FFprobeExecutable: string;

    LastIndex: longint;
    LastWidth: longint;
    LastHeight: longint;
    LastMode: TScreenDrawerModes;

    PanX: longint;
    PanY: longint;
    PanStartX: longint;
    PanStartY: longint;
    PanStartOffsetX: longint;
    PanStartOffsetY: longint;
    IsPanning: boolean;
    MaxRenderWidth: longint;
    MaxRenderHeight: longint;

    PlayStart: TDateTime;

    IsNeededUpdateScreens: boolean;
    IsNeededKillAnalyzer:  boolean;

    procedure CalculateBackingSize(out AWidth, AHeight: longint);
    procedure ClampPan;
    procedure ClearTemporaryWave;
    procedure InitializeTemporaryWave;
    procedure LocateFFmpegTools;
    function ExtractWave(const AInputFile: string): boolean;
    procedure UpdatePanCursor;
    procedure ReadSetting; overload;
    procedure WriteSetting;  overload;
  public
  end;

var
  AudioFrm: TAudioFrm;

implementation

{$R *.lfm}

uses
  DateUtils, Math, FileUtil, LazFileUtils, ReportFrm, SoundUtils;

const
  MAX_RENDER_WIDTH_KEY = 'MaxRenderWidth';
  MAX_RENDER_HEIGHT_KEY = 'MaxRenderHeight';

function CutOff(const S: string): string;
begin
  Result := S;
  SetLength(Result, Max(0, Length(Result) - 4));
  Result := Result + '...';
end;

function StreamToString(AStream: TStream): string;
begin
  Result := '';
  SetLength(Result, AStream.Size);
  AStream.Position := 0;
  if AStream.Size > 0 then
    AStream.ReadBuffer(Result[1], AStream.Size);
end;

function RunProcess(const AExecutable: string; AParameters: TStrings;
  out AOutput, AError: string): boolean;
var
  Buff: array[0..4095] of byte;
  Count: longint;
  OutputStream, ErrorStream: TMemoryStream;
  Process: TProcess;
begin
  Result := False;
  AOutput := '';
  AError := '';
  Process := TProcess.Create(nil);
  OutputStream := TMemoryStream.Create;
  ErrorStream := TMemoryStream.Create;
  try
    Process.Executable := AExecutable;
    Process.Parameters.Assign(AParameters);
    Process.Options := [poNoConsole, poUsePipes];
    try
      Process.Execute;
      while Process.Running or (Process.Output.NumBytesAvailable > 0) or
            (Process.Stderr.NumBytesAvailable > 0) do
      begin
        while Process.Output.NumBytesAvailable > 0 do
        begin
          Count := Process.Output.Read(Buff, SizeOf(Buff));
          if Count > 0 then OutputStream.WriteBuffer(Buff, Count);
        end;
        while Process.Stderr.NumBytesAvailable > 0 do
        begin
          Count := Process.Stderr.Read(Buff, SizeOf(Buff));
          if Count > 0 then ErrorStream.WriteBuffer(Buff, Count);
        end;
        if Process.Running and (Process.Output.NumBytesAvailable = 0) and
           (Process.Stderr.NumBytesAvailable = 0) then
          Sleep(1);
      end;
      Result := Process.ExitStatus = 0;
    except
      on E: Exception do AError := E.Message;
    end;
    AOutput := StreamToString(OutputStream);
    if AError = '' then AError := Trim(StreamToString(ErrorStream));
  finally
    ErrorStream.Free;
    OutputStream.Free;
    Process.Free;
  end;
end;

function ProbeAudio(const AExecutable, AFilename: string;
  out ASampleFormat: string; out ABitsPerSample, ASampleRate, AChannels: longint;
  out AError: string): boolean;
var
  Output: string;
  Parameters, Values: TStringList;
begin
  ASampleFormat := '';
  ABitsPerSample := 0;
  ASampleRate := 0;
  AChannels := 0;
  Parameters := TStringList.Create;
  Values := TStringList.Create;
  try
    Parameters.Add('-v');
    Parameters.Add('error');
    Parameters.Add('-select_streams');
    Parameters.Add('a:0');
    Parameters.Add('-show_entries');
    Parameters.Add('stream=sample_fmt,bits_per_sample,bits_per_raw_sample,sample_rate,channels');
    Parameters.Add('-of');
    Parameters.Add('default=noprint_wrappers=1:nokey=0');
    Parameters.Add(AFilename);
    Result := RunProcess(AExecutable, Parameters, Output, AError);
    if not Result then
    begin
      if AError = '' then AError := 'ffprobe failed without an error message.';
      Exit;
    end;

    Values.Text := Output;
    ASampleFormat := LowerCase(Trim(Values.Values['sample_fmt']));
    ABitsPerSample := StrToIntDef(Values.Values['bits_per_raw_sample'], 0);
    if ABitsPerSample <= 0 then
      ABitsPerSample := StrToIntDef(Values.Values['bits_per_sample'], 0);
    ASampleRate := StrToIntDef(Values.Values['sample_rate'], 0);
    AChannels := StrToIntDef(Values.Values['channels'], 0);
    Result := (ASampleFormat <> '') and (ASampleRate > 0) and (AChannels > 0);
    if not Result then
      AError := 'ffprobe did not return complete properties for the first audio stream.';
  finally
    Values.Free;
    Parameters.Free;
  end;
end;

{ TAudioFrm }

procedure TAudioFrm.FormCreate(Sender: TObject);
begin
  DefaultFontName := 'DejaVu Sans';
  DefaultFontFileName := ExtractFilePath(ParamStr(0)) + 'fonts/DejaVuSans/DejaVuSans.ttf';
  DoDirSeparators(DefaultFontFileName);
  InitializeChartFont;
  // ---
  Screen := TBGRABitmap.Create;
  IsNeededUpdateScreens := False;
  IsNeededKillAnalyzer  := False;
  PanX := 0;
  PanY := 0;
  IsPanning := False;
  MaxRenderWidth := 0;
  MaxRenderHeight := 0;
  // ---
  LastMode   := [];
  LastIndex  := -1;
  TrackIndex := -1;
  TrackList  := TTrackList.create;
  TempFile := '';
  InitializeTemporaryWave;
  // Initialize progress bar
  ProgressRing.Value := 0;
  ProgressRing.Visible := True;
  // inizialize main form
  Color := clBlack;
  // Initialize
  ReadSetting;
  Clear;
  LocateFFmpegTools;
end;

procedure TAudioFrm.FormDestroy(Sender: TObject);
begin
  WriteSetting;
  PlayTimer.Enabled := False;
  PlaySound.StopSound;
  ClearTemporaryWave;
  FreeAndNil(Screen);
  TrackList.Destroy;
end;

procedure TAudioFrm.FormClosequery(Sender: TObject; var CanClose: boolean);
begin
  IsNeededKillAnalyzer := True;
  CanClose := (AudioAnalyzer = nil) and (ScreenDrawer = nil);
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

  while (ProgressPanel.Left + ProgressPanel.Width) > (PlayBtn.Left) do
  begin
    TrackFileName.Caption := CutOff(TrackFileName.Caption);
  end;
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
    TempWaveReady := False;
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
    TrackList.Save(ReportForm.Memo.Lines);
  end;

  EnableButtons;
  Execute;
end;

// chart drawer events

procedure TAudioFrm.OnStartDrawer;
var
  BackingWidth, BackingHeight: longint;
begin
  // Keep the largest chart area reached by the form. Smaller windows display
  // the same backing bitmap through the pan viewport.
  CalculateBackingSize(BackingWidth, BackingHeight);
  Screen.SetSize(BackingWidth, BackingHeight);
end;

procedure TAudioFrm.OnStopDrawer;
begin
  if Assigned(ScreenDrawer) then
  begin
    VirtualScreen.Hint := ScreenDrawer.ErrorMessage;
    VirtualScreen.ShowHint := ScreenDrawer.ErrorMessage <> '';
    if ScreenDrawer.Successful and (IsNeededUpdateScreens = False) then
      RedrawScreen(ScreenDrawer.Track);
  end;
  ScreenDrawer := nil;
  ClampPan;
  UpdatePanCursor;
  VirtualScreen.RedrawBitmap;
  if (AudioAnalyzer = nil) and not IsNeededUpdateScreens and
     not IsNeededKillAnalyzer then
    EnableButtons;
end;

//

procedure TAudioFrm.ClearTemporaryWave;
begin
  if (SessionTempFile <> '') and FileExistsUTF8(SessionTempFile) then
    DeleteFileUTF8(SessionTempFile);
  TempFile := '';
  TempWaveError := '';
  TempWaveReady := False;
end;

procedure TAudioFrm.InitializeTemporaryWave;
var
  SessionGuid: TGUID;
  SessionName: string;
begin
  SessionTempFile := '';
  TempWaveError := '';
  TempWaveReady := False;
  if CreateGUID(SessionGuid) <> 0 then
  begin
    TempWaveError := 'Unable to generate the temporary WAV name.';
    MessageDlg('AudioMeter',
      'Warning: unable to generate the temporary WAV name. Files that require conversion cannot be analyzed.',
      mtWarning, [mbOk], '');
    Exit;
  end;
  SessionName := IncludeTrailingPathDelimiter(GetTempDir(False)) +
    'audiometer_' + Copy(GUIDToString(SessionGuid), 2, 36);
  SessionTempFile := SessionName + '.wav';
end;

procedure TAudioFrm.LocateFFmpegTools;
var
  Missing: string;
begin
  {$IFDEF MSWINDOWS}
  FFmpegExecutable := ExtractFilePath(ParamStr(0)) + 'ffmpeg.exe';
  FFprobeExecutable := ExtractFilePath(ParamStr(0)) + 'ffprobe.exe';
  if not FileExistsUTF8(FFmpegExecutable) then FFmpegExecutable := '';
  if not FileExistsUTF8(FFprobeExecutable) then FFprobeExecutable := '';
  {$ELSE}
  FFmpegExecutable := FindDefaultExecutablePath('ffmpeg');
  FFprobeExecutable := FindDefaultExecutablePath('ffprobe');
  {$ENDIF}

  Missing := '';
  if FFmpegExecutable = '' then Missing := 'ffmpeg';
  if FFprobeExecutable = '' then
  begin
    if Missing <> '' then Missing := Missing + ' and ';
    Missing := Missing + 'ffprobe';
  end;
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

function TAudioFrm.ExtractWave(const AInputFile: string): boolean;
var
  Codec, ErrorOutput, ExpectedFormat, Output: string;
  InputBits, InputChannels, InputRate: longint;
  OutputBits, OutputChannels, OutputRate: longint;
  InputFormat, OutputFormat: string;
  Parameters: TStringList;
begin
  Result := False;
  TempWaveReady := False;
  TempWaveError := '';
  TempFile := SessionTempFile;
  PlayBtn.Enabled := False;
  StopBtn.Enabled := False;
  if (FFmpegExecutable = '') or not FileExistsUTF8(FFmpegExecutable) then
  begin
    TempWaveError := 'ffmpeg is not available. The audio file was not converted.';
    Exit;
  end;
  if (FFprobeExecutable = '') or not FileExistsUTF8(FFprobeExecutable) then
  begin
    TempWaveError := 'ffprobe is not available. The audio file was not converted.';
    Exit;
  end;
  if SessionTempFile = '' then
  begin
    TempWaveError := 'The temporary WAV name is not available for this application session.';
    Exit;
  end;

  if not ProbeAudio(FFprobeExecutable, AInputFile, InputFormat, InputBits,
    InputRate, InputChannels, TempWaveError) then Exit;
  if (Length(InputFormat) > 0) and
     (InputFormat[Length(InputFormat)] = 'p') then
    Delete(InputFormat, Length(InputFormat), 1);

  if InputFormat = 'u8' then
    Codec := 'pcm_u8'
  else if InputFormat = 's16' then
    Codec := 'pcm_s16le'
  else if InputFormat = 's32' then
  begin
    if InputBits = 24 then Codec := 'pcm_s24le'
                      else Codec := 'pcm_s32le';
  end
  else if InputFormat = 'flt' then
    Codec := 'pcm_f32le'
  else if InputFormat = 'dbl' then
    Codec := 'pcm_f64le'
  else
  begin
    TempWaveError := Format(
      'The decoded sample format "%s" is not supported without a possible loss of precision.',
      [InputFormat]);
    Exit;
  end;

  // The GUID and WAV path are created once for the application session.
  // ffmpeg overwrites this file; TempWaveReady is the authority that prevents
  // an incomplete output from being analyzed or played after an error.

  Parameters := TStringList.Create;
  try
    Parameters.Add('-nostdin');
    Parameters.Add('-v');
    Parameters.Add('error');
    Parameters.Add('-xerror');
    Parameters.Add('-y');
    // Apply bit-exact mode to the decoder as well as to the WAV encoder/muxer.
    Parameters.Add('-bitexact');
    Parameters.Add('-i');
    Parameters.Add(AInputFile);
    Parameters.Add('-map');
    Parameters.Add('0:a:0');
    Parameters.Add('-c:a');
    Parameters.Add(Codec);
    Parameters.Add('-bitexact');
    Parameters.Add('-f');
    Parameters.Add('wav');
    Parameters.Add(TempFile);
    if not RunProcess(FFmpegExecutable, Parameters, Output, ErrorOutput) then
    begin
      if ErrorOutput = '' then ErrorOutput := 'ffmpeg failed without an error message.';
      TempWaveError := 'Unable to create the analysis WAV:' + LineEnding + ErrorOutput;
      Exit;
    end;
  finally
    Parameters.Free;
  end;

  if not ProbeAudio(FFprobeExecutable, TempFile, OutputFormat, OutputBits,
    OutputRate, OutputChannels, TempWaveError) then
  begin
    TempWaveError := 'The WAV created by ffmpeg is invalid:' + LineEnding + TempWaveError;
    Exit;
  end;
  if (Length(OutputFormat) > 0) and
     (OutputFormat[Length(OutputFormat)] = 'p') then
    Delete(OutputFormat, Length(OutputFormat), 1);
  if Codec = 'pcm_u8' then ExpectedFormat := 'u8'
  else if Codec = 'pcm_s16le' then ExpectedFormat := 's16'
  else if (Codec = 'pcm_s24le') or (Codec = 'pcm_s32le') then ExpectedFormat := 's32'
  else if Codec = 'pcm_f32le' then ExpectedFormat := 'flt'
  else ExpectedFormat := 'dbl';
  if (OutputFormat <> ExpectedFormat) or
     ((Codec = 'pcm_s24le') and (OutputBits > 0) and (OutputBits <> 24)) then
  begin
    TempWaveError := Format(
      'The WAV created by ffmpeg has an unexpected sample format (%s, %d bit).',
      [OutputFormat, OutputBits]);
    Exit;
  end;
  if (OutputRate <> InputRate) or (OutputChannels <> InputChannels) then
  begin
    TempWaveError := Format(
      'The WAV created by ffmpeg changed the audio layout (%d Hz/%d channels to %d Hz/%d channels).',
      [InputRate, InputChannels, OutputRate, OutputChannels]);
    Exit;
  end;
  TempWaveReady := True;
  Result := True;
end;

procedure TAudioFrm.Execute;
var
  Track: TTrack;
begin
  if IsNeededKillAnalyzer then Exit;
  if TrackIndex >= TrackList.Count then Exit;
  if TrackIndex < 0 then Exit;

  Track := TrackList[TrackIndex];
  TempWaveReady := False;
  TempWaveError := '';
  PlayBtn.Enabled := False;
  StopBtn.Enabled := False;
  try
    if not SameText(ExtractFileExt(Track.Filename), '.wav') then
    begin
      if not ExtractWave(Track.Filename) then
      begin
        TrackFileName.Font.Color := clrRed;
        TrackFileName.Caption := Format('Unable to decode "%s".', [Track.Filename]);
        MessageDlg('AudioMeter', TempWaveError, mtError, [mbOk], '');
        Exit;
      end;
    end else
    begin
      TempFile := Track.Filename;
      TempWaveReady := True;
    end;

    Stream := TFileStream.Create(TempFile, fmOpenRead or fmShareExclusive);
  except
    Stream := nil;
    TempWaveReady := False;
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
  TrackFileName.Font.Color := clwhite;
  TrackFileName.Caption := 'Audio';

  ClearData;

  IsNeededUpdateScreens := True;
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
  Stereo .Font.Color := clGray;

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

procedure TAudioFrm.ClearTrackList;
begin
  // Do not expose the report from the previous selection while the newly
  // opened file/folder is being decoded and analyzed. Button state is left
  // unchanged; TrackList.Save will populate the memo when analysis completes.
  if Assigned(ReportForm) and Assigned(ReportForm.Memo) then
    ReportForm.Memo.Clear;
  TempFile := '';
  TempWaveError := '';
  TempWaveReady := False;
  PlayBtn.Enabled := False;
  StopBtn.Enabled := False;
  LastIndex  := -1;
  TrackIndex := -1;
  TrackList.Clear;
end;

// Button Events

procedure TAudioFrm.OpenFileBtnClick(sender: tobject);
begin
  FileDialog.Filter:= OpenDialogFileFilter;
  if FileDialog.Execute then
  begin
    PlaySound.StopSound;
    PlayTimer.Enabled := False;

    ClearTrackList;
    if IsFileSupported(ExtractFileExt(FileDialog.FileName)) then
    begin
      TrackList.Add(FileDialog.FileName);
      TrackFile  := ChangeFileExt(FileDialog.FileName, '.md');
      TrackIndex := 0;

      IsNeededKillAnalyzer := False;
      Execute;
    end else
    begin
      TrackFileName.Caption    := 'File format error!';
      TrackFileName.Font.Color := clrRed;
    end;
  end;
end;

procedure TAudioFrm.OpenFolderBtnClick(Sender: TObject);
var
  Err:  longint;
  Path: string;
  SR:   TSearchRec;
begin
  if DirDialog.Execute then
  begin
    PlaySound.StopSound;
    PlayTimer.Enabled := False;

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
    Execute;
  end;
end;

procedure TAudioFrm.ReportBtnClick(Sender: TObject);
begin
  ReportForm.SaveDialog.InitialDir := ExtractFileDir (TrackFile);
  ReportForm.SaveDialog.FileName   := ExtractFileName(TrackFile);
  ReportForm.ShowModal;
end;

procedure TAudioFrm.PlayBtnClick(Sender: TObject);
begin
  PlaySound.StopSound;
  PlayTimer.Enabled := False;
  if TempWaveReady and FileExistsUTF8(TempFile) then
  begin
    PlaySound.PlayStyle := psaSync;
    PlaySound.SoundFile := TempFile;
    PlaySound.Execute;

    PlayStart := Now;
    PlayTimer.Enabled := True;
  end;
end;

procedure TAudioFrm.PlayTimerStartTimer(Sender: TObject);
begin
  ShortTermLoudnessValue.Font.Color := clSkyBlue;
  MomentaryLoudnessValue.Font.Color := clLime;
end;

procedure TAudioFrm.PlayTimerStopTimer(Sender: TObject);
begin
  ShortTermLoudnessValue.Font.Color := clGray;
  MomentaryLoudnessValue.Font.Color := clGray;

  ShortTermLoudnessValue.Caption := '-';
  MomentaryLoudnessValue.Caption := '-';
end;

procedure TAudioFrm.PlayTimerTimer(Sender: TObject);
var
  Track: TTrack;
  PlayTime: longint;
begin
  Track := nil;
  if (LastIndex > -1) and (LastIndex < TrackList.Count) then
  begin
    Track := TrackList[LastIndex];

    PlayTime := MilliSecondsBetween(Now, PlayStart);
    ShortTermLoudnessValue.Caption := Format('%0.2f', [Track.Loudness.ShortTermLoudness(PlayTime)]);
    MomentaryLoudnessValue.Caption := Format('%0.2f', [Track.Loudness.MomentaryLoudness(PlayTime)]);

    if PlayTime > (Track.Duration * 1000) then
    begin
      PlayTimer.Enabled := False;
    end;
  end;
end;

procedure TAudioFrm.StopBtnClick(Sender: TObject);
begin
  if not TempWaveReady then Exit;
  PlayTimer.Enabled := False;
  PlaySound.StopSound;
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

procedure TAudioFrm.DisableButtons;
begin
  PlayBtn      .Enabled := False;
  StopBtn      .Enabled := False;
  OpenFileBtn  .Enabled := False;
  OpenFolderBtn.Enabled := False;
  ReportBtn    .Enabled := False;

  DRValue.Visible := True;
  ProgressRing.Value := 0;
  Popup.AutoPopup := False;
end;

procedure TAudioFrm.EnableButtons;
begin
  PlayBtn      .Enabled := TempWaveReady and FileExistsUTF8(TempFile);
  StopBtn      .Enabled := TempWaveReady and FileExistsUTF8(TempFile);
  OpenFileBtn  .Enabled := True;
  OpenFolderBtn.Enabled := True;
  ReportBtn    .Enabled := True;

  DRValue.Visible := True;
  ProgressRing.Value := 0;
  Popup.AutoPopup := True;
end;

procedure TAudioFrm.CalculateBackingSize(out AWidth, AHeight: longint);
begin
  if Assigned(VirtualScreen) then
  begin
    MaxRenderWidth := Max(MaxRenderWidth, VirtualScreen.Width);
    MaxRenderHeight := Max(MaxRenderHeight, VirtualScreen.Height);
  end;
  AWidth := Max(1, MaxRenderWidth);
  AHeight := Max(1, MaxRenderHeight);
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
  i, Index: longint;
  BackingWidth, BackingHeight: longint;
  Track: TTrack;
  Mode: TScreenDrawerModes;
begin
  if ScreenDrawer <> nil then Exit;

  if (LastWidth <> VirtualScreen.Width) or
     (LastHeight <> VirtualScreen.Height) then
  begin
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
    Exit;
  end;

  Index := TrackIndex -1;
  if (Index >= 0) and (Index < TrackList.Count) then
  begin
    if LastIndex <> Index then
    begin
      LastIndex := Index;
      for i := Index -1 downto 0 do
        TrackList [i].ClearChannels;
      IsNeededUpdateScreens := True;
      Exit;
    end;
  end;

  if IsNeededUpdateScreens then
  begin
    IsNeededUpdateScreens := False;

    Track := nil;
    if (LastIndex > -1) and (LastIndex < TrackList.Count) then
    begin
      Track := TrackList[LastIndex]
    end;

    ScreenDrawer := TScreenDrawer.Create(Track, Screen);
    ScreenDrawer.OnStart := @OnStartDrawer;
    ScreenDrawer.OnStop  := @OnStopDrawer;
    ScreenDrawer.Mode    := Mode;
    // TrackList owns the track used by the renderer. Prevent commands that
    // can clear the list until the background renderer releases that pointer.
    DisableButtons;
    ScreenDrawer.Start;
  end;
end;

procedure TAudioFrm.RedrawScreen(ATrack: TTrack);
begin
  ClearData;
  if Assigned(ATrack) then
  begin
    TrackFileName.Font.Color := clWhite;
    TrackFileName.Caption    := ExtractFileName(ATrack.FileName);
    while (TrackFileName.Left + TrackFileName.Width) > (PlayBtn.Left + PlayBtn.Width) do
    begin
      TrackFileName.Caption := CutOff(TrackFileName.Caption);
    end;

    pcm   .Font.Color := clWhite;
    bit8  .Font.Color := clGray; if ATrack.Bitspersample = 8      then bit8  .Font.Color := clWhite;
    bit16 .Font.Color := clGray; if ATrack.Bitspersample = 16     then bit16 .Font.Color := clWhite;
    bit24 .Font.Color := clGray; if ATrack.Bitspersample = 24     then bit24 .Font.Color := clWhite;
    bit32 .Font.Color := clGray; if ATrack.Bitspersample = 32     then bit32 .Font.Color := clWhite;

    kHz44 .Font.Color := clGray; if ATrack.Samplerate    = 44100  then kHz44 .Font.Color := clWhite;
    kHz48 .Font.Color := clGray; if ATrack.Samplerate    = 48000  then kHz48 .Font.Color := clWhite;
    kHz88 .Font.Color := clGray; if ATrack.Samplerate    = 88000  then kHz88 .Font.Color := clWhite;
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
    if ATrack.ChannelCount > 0 then if Decibel(ATrack.Loudness.TruePeak(0)) <= 0.0 then TPLLeftValue .Font.Color := clLime;
    if ATrack.ChannelCount > 0 then if Decibel(ATrack.Loudness.TruePeak(0)) >  0.0 then TPLLeftValue .Font.Color := clYellow;
    if ATrack.ChannelCount > 0 then if Decibel(ATrack.Loudness.TruePeak(0)) >  0.5 then TPLLeftValue .Font.Color := clRed;

    if ATrack.ChannelCount > 1 then if Decibel(ATrack.Loudness.TruePeak(1)) <= 0.0 then TPLRightValue.Font.Color := clLime;
    if ATrack.ChannelCount > 1 then if Decibel(ATrack.Loudness.TruePeak(1)) >  0.0 then TPLRightValue.Font.Color := clYellow;
    if ATrack.ChannelCount > 1 then if Decibel(ATrack.Loudness.TruePeak(1)) >  0.5 then TPLRightValue.Font.Color := clRed;

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
    if ATrack.ChannelCount > 0 then PeakToLoudnessRatioValue   .Caption := Format('%0.2f', [ATrack.Loudness.PeakToLoudnessRatio]);

    if ATrack.ChannelCount > 0 then IntegratedLoudnessValue .Caption := Format('%0.2f', [ATrack.Loudness.IntegratedLoudness]);
    if ATrack.ChannelCount > 0 then RangeLoudnessValue      .Caption := Format('%0.2f', [ATrack.Loudness.LoudnessRange]);
    if ATrack.ChannelCount > 0 then PeakToLoudnessRatioValue.Caption := Format('%0.2f', [ATrack.Loudness.PeakToLoudnessRatio]);

    DRValue.Caption := '--';
    DRValue.Font.Color := clWhite;
    DRLabel.Font.Color := clWhite;
    if (ATrack.DRMeter.DR) > 0 then
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
