{
  Description: Regression runner.

  Builds and runs, one process each, the programs registered in
  tests/regression/tests.txt. Portable: Linux, Windows and macOS.
  From the repository root:

    instantfpc .github/workflows/regression.pas [options]

  Exit code 0 only when every selected program passed.

  Copyright (C) 2026 Melchiorre Caruso <melchiorrecaruso@gmail.com>
  GPL v2 or later.
}

program Regression;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, StrUtils, Process;

const
  ExeExt = {$IFDEF WINDOWS}'.exe'{$ELSE}''{$ENDIF};
  BuildTimeout = 600;
  // The application ships as a 64-bit program: build the tests the same way.
  DefaultCPU = {$IFDEF WINDOWS}'x86_64'{$ELSE}''{$ENDIF};
  DefaultOS = {$IFDEF WINDOWS}'win64'{$ELSE}''{$ENDIF};
  // Widgetsets the application is validated on.
  DefaultWidgetsets =
    {$IF DEFINED(WINDOWS)}'win32'
    {$ELSEIF DEFINED(DARWIN)}'cocoa'
    {$ELSE}'gtk2,gtk3,qt5,qt6'{$ENDIF};
  // X11 desktops other than macOS need a display: by default a private Xvfb.
  NeedsDisplay = {$IF DEFINED(UNIX) and not DEFINED(DARWIN)}True{$ELSE}False{$ENDIF};

type
  // Described in the header of tests/regression/tests.txt.
  TMode = (mdConsole, mdNoGUI, mdWidgetset, mdFFmpeg, mdEBU);

const
  ModeNames: array[TMode] of string = ('console', 'nogui', 'widgetset', 'ffmpeg', 'ebu');

var
  Root, ManifestFile, BuildDir, LazBuild, PCP, OnlyPrefix: string;
  EBUFolder, FFmpegDir, TargetCPU, TargetOS, Display: string;
  Widgetsets: TStringList;
  RunTimeout: longint = 120;
  Passed, Failed, Skipped, Selected: longint;

procedure Usage;
begin
  Writeln('Usage: instantfpc .github/workflows/regression.pas [options]');
  Writeln('  --only=PREFIX       run the tests whose source starts with PREFIX');
  Writeln('  --ebu[=FOLDER]      run the EBU test (default folder: ',
    '../audiometer-references/ebu-loudness-test-setv05)');
  Writeln('  --lazbuild=FILE     lazbuild executable (default: lazbuild)');
  Writeln('  --pcp=FOLDER        Lazarus configuration folder');
  Writeln('  --cpu=CPU --os=OS   lazbuild target (default: ',
    {$IFDEF WINDOWS}'x86_64 and win64'{$ELSE}'the compiler''s own'{$ENDIF},
    '; an empty value keeps the compiler''s own)');
  Writeln('  --ws=LIST           comma-separated widgetsets for the widgetset tests ',
    '(default: ', DefaultWidgetsets, ')');
  if NeedsDisplay then
    Writeln('  --display=xvfb|current  run widgetset tests on a private Xvfb ',
      '(default) or on $DISPLAY');
  Writeln('  --ffmpeg-dir=FOLDER folder holding ffmpeg and ffprobe (default: PATH)');
  Writeln('  --timeout=SECONDS   run timeout per test (default: 120)');
  Writeln('  --root=FOLDER --manifest=FILE --build-dir=FOLDER');
end;

procedure ParseOptions;
var
  i, p: longint;
  Arg, Key, Value: string;
begin
  Root := GetCurrentDir;
  ManifestFile := '';
  BuildDir := '';
  LazBuild := 'lazbuild';
  PCP := '';
  OnlyPrefix := '';
  EBUFolder := '';
  FFmpegDir := '';
  TargetCPU := DefaultCPU;
  TargetOS := DefaultOS;
  Display := 'xvfb';
  Widgetsets := TStringList.Create;
  Widgetsets.StrictDelimiter := True;
  Widgetsets.CommaText := DefaultWidgetsets;
  for i := 1 to ParamCount do
  begin
    Arg := ParamStr(i);
    if (Arg = '-h') or (Arg = '--help') then
    begin
      Usage;
      Halt(0);
    end;
    if Arg = '--ebu' then
    begin
      EBUFolder := '*';
      Continue;
    end;
    p := Pos('=', Arg);
    if not AnsiStartsStr('--', Arg) or (p = 0) then
      raise Exception.Create('Invalid option: ' + Arg);
    Key := Copy(Arg, 3, p - 3);
    Value := Copy(Arg, p + 1, MaxInt);
    if Key = 'root' then Root := Value
    else if Key = 'manifest' then ManifestFile := Value
    else if Key = 'build-dir' then BuildDir := Value
    else if Key = 'lazbuild' then LazBuild := Value
    else if Key = 'pcp' then PCP := ExpandFileName(Value)
    else if Key = 'cpu' then TargetCPU := Value
    else if Key = 'os' then TargetOS := Value
    else if Key = 'ws' then Widgetsets.CommaText := Value
    else if Key = 'display' then
    begin
      if (Value <> 'xvfb') and (Value <> 'current') then
        raise Exception.Create('Invalid display: ' + Value);
      Display := Value;
    end
    else if Key = 'only' then OnlyPrefix := Value
    else if Key = 'ebu' then EBUFolder := Value
    else if Key = 'ffmpeg-dir' then FFmpegDir := ExpandFileName(Value)
    else if Key = 'timeout' then
    begin
      RunTimeout := StrToIntDef(Value, 0);
      if RunTimeout <= 0 then raise Exception.Create('Invalid timeout: ' + Value);
    end
    else raise Exception.Create('Unknown option: ' + Arg);
  end;
  Root := ExcludeTrailingPathDelimiter(ExpandFileName(Root));
  if ManifestFile = '' then
    ManifestFile := Root + '/tests/regression/tests.txt';
  ManifestFile := ExpandFileName(ManifestFile);
  if BuildDir = '' then BuildDir := Root + '/.regression-build';
  BuildDir := ExpandFileName(BuildDir);
  if EBUFolder = '*' then
    EBUFolder := Root + '/../audiometer-references/ebu-loudness-test-setv05';
  if EBUFolder <> '' then EBUFolder := ExpandFileName(EBUFolder);
  for i := Widgetsets.Count - 1 downto 0 do
  begin
    Widgetsets[i] := Trim(Widgetsets[i]);
    if Widgetsets[i] = '' then Widgetsets.Delete(i);
  end;
end;

// Runs AExe in ADir and saves its command line, output and exit status to
// ALog. The pipe is read while the child runs, so a verbose child cannot
// block on a full pipe. Launch errors, nonzero exit codes and timeouts fail.
// AEnv entries (NAME=VALUE) are added to the inherited environment.
function RunLogged(const AExe: string; const AArgs, AEnv: array of string;
  const ADir, ALog: string; ATimeout: longint): boolean;
var
  Child: TProcess;
  Log: TStringStream;
  Buffer: array[0..4095] of byte;
  Count: longint;
  Start: QWord;
  TimedOut: boolean;
  Arg, Status: string;
  i: longint;

  procedure Drain;
  begin
    while Child.Output.NumBytesAvailable > 0 do
    begin
      Count := Child.Output.Read(Buffer, SizeOf(Buffer));
      if Count <= 0 then Break;
      Log.WriteBuffer(Buffer, Count);
    end;
  end;

begin
  Result := False;
  TimedOut := False;
  Log := TStringStream.Create('');
  Child := TProcess.Create(nil);
  try
    for Arg in AEnv do Log.WriteString(Arg + ' ');
    Log.WriteString('$ ' + AExe);
    for Arg in AArgs do Log.WriteString(' ' + Arg);
    Log.WriteString(LineEnding + LineEnding);
    Child.Executable := AExe;
    for Arg in AArgs do Child.Parameters.Add(Arg);
    Child.CurrentDirectory := ADir;
    if Length(AEnv) > 0 then
    begin
      for i := 1 to GetEnvironmentVariableCount do
        Child.Environment.Add(GetEnvironmentString(i));
      for Arg in AEnv do
        Child.Environment.Values[Copy(Arg, 1, Pos('=', Arg) - 1)] :=
          Copy(Arg, Pos('=', Arg) + 1, MaxInt);
    end;
    Child.Options := [poUsePipes, poStderrToOutPut];
    try
      Child.Execute;
      Start := GetTickCount64;
      while Child.Running do
      begin
        Drain;
        if GetTickCount64 - Start >= QWord(ATimeout) * 1000 then
        begin
          TimedOut := True;
          Child.Terminate(124);
          Child.WaitOnExit(5000);
          Break;
        end;
        Sleep(10);
      end;
      Drain;
      if TimedOut then
        Status := Format('timed out after %d seconds', [ATimeout])
      else
      begin
        Result := Child.ExitStatus = 0;
        Status := Format('exit code %d', [Child.ExitStatus]);
      end;
    except
      on E: Exception do
        Status := 'cannot start: ' + E.ClassName + ': ' + E.Message;
    end;
    Log.WriteString(LineEnding + Status + LineEnding);
    Log.SaveToFile(ALog);
    if not Result then
    begin
      Writeln(Log.DataString);
      Writeln('FAIL: ', Status);
    end;
  finally
    Child.Free;
    Log.Free;
  end;
end;

procedure CopyTool(const AName, ADestDir: string);
var
  Source: string;
  Src, Dst: TFileStream;
begin
  if FFmpegDir <> '' then
    Source := IncludeTrailingPathDelimiter(FFmpegDir) + AName + ExeExt
  else
    Source := ExeSearch(AName + ExeExt, GetEnvironmentVariable('PATH'));
  if (Source = '') or not FileExists(Source) then
    raise Exception.Create(AName + ' not found: install it or pass --ffmpeg-dir');
  Src := TFileStream.Create(Source, fmOpenRead or fmShareDenyWrite);
  try
    Dst := TFileStream.Create(IncludeTrailingPathDelimiter(ADestDir) + AName + ExeExt, fmCreate);
    try
      Dst.CopyFrom(Src, 0);
    finally
      Dst.Free;
    end;
  finally
    Src.Free;
  end;
end;

procedure Skip(const ASource, AReason: string);
begin
  Writeln('SKIP: ', ASource, ' (', AReason, ')');
  Inc(Skipped);
end;

// Builds and runs one test; AWidgetset is the lazbuild --ws value, empty for
// the compiler's default.
procedure RunOne(const ASource: string; AMode: TMode; const AWidgetset: string);
var
  Source, Project, Exe, Name, OutDir, Title, RunExe: string;
  Args, Env: array of string;
  Ok: boolean;
begin
  Source := ExpandFileName(Root + '/tests/regression/' + ASource);
  Project := ChangeFileExt(Source, '.lpi');
  Inc(Selected);
  Title := ASource;
  if AWidgetset <> '' then Title := ASource + ' [' + AWidgetset + ']';

  // The project builds its program beside itself, named after the source.
  Name := ChangeFileExt(ExtractFileName(Source), '');
  Exe := ChangeFileExt(Source, ExeExt);
  OutDir := BuildDir + PathDelim + ModeNames[AMode];
  if AMode = mdWidgetset then OutDir := OutDir + PathDelim + AWidgetset;
  OutDir := OutDir + PathDelim + Name;
  if not ForceDirectories(OutDir) then raise Exception.Create('Cannot create ' + OutDir);
  if FileExists(Exe) and not DeleteFile(Exe) then
    raise Exception.Create('Cannot remove the previous program ' + Exe);

  Writeln('BUILD [', ModeNames[AMode], ']: ', Title);
  Args := ['--no-write-project'];
  if AWidgetset <> '' then Args := Concat(Args, ['--ws=' + AWidgetset]);
  if TargetCPU <> '' then Args := Concat(Args, ['--cpu=' + TargetCPU]);
  if TargetOS <> '' then Args := Concat(Args, ['--os=' + TargetOS]);
  if PCP <> '' then Args := Concat(Args, ['--pcp=' + PCP]);
  Args := Concat(Args, [Project]);
  Ok := RunLogged(LazBuild, Args, [], Root, OutDir + PathDelim + 'build.log', BuildTimeout);
  if Ok and not FileExists(Exe) then
  begin
    Writeln('FAIL: lazbuild did not produce ', Exe);
    Ok := False;
  end;

  if Ok then
  begin
    {$IFDEF WINDOWS}
    // On Windows the decoder looks for the tools beside the executable.
    if AMode = mdFFmpeg then
    begin
      CopyTool('ffmpeg', ExtractFileDir(Exe));
      CopyTool('ffprobe', ExtractFileDir(Exe));
    end;
    {$ENDIF}
    case AMode of
      mdNoGUI, mdWidgetset: Args := [OutDir];
      mdEBU:   Args := [EBUFolder];
      else     Args := [];
    end;
    Env := [];
    RunExe := Exe;
    if AMode = mdWidgetset then
    begin
      // Settings and temporary files stay in the log folder.
      Env := ['XDG_CONFIG_HOME=' + OutDir, 'TMPDIR=' + OutDir];
      if NeedsDisplay and (Display = 'xvfb') then
      begin
        // GTK and Qt would otherwise prefer a Wayland session over Xvfb.
        Env := Concat(Env, ['GDK_BACKEND=x11', 'QT_QPA_PLATFORM=xcb',
          'WAYLAND_DISPLAY=']);
        Args := Concat(['-a', '-s', '-screen 0 1280x1024x24', Exe], Args);
        RunExe := ExeSearch('xvfb-run', GetEnvironmentVariable('PATH'));
        if RunExe = '' then
          raise Exception.Create('xvfb-run not found: install Xvfb or pass --display=current');
      end;
    end;
    Writeln('RUN: ', Title);
    Ok := RunLogged(RunExe, Args, Env, OutDir, OutDir + PathDelim + 'run.log', RunTimeout);
  end;

  if Ok then
  begin
    Writeln('PASS: ', Title);
    Inc(Passed);
  end else
  begin
    Writeln('FAIL: ', Title, ' (logs: ', OutDir, ')');
    Inc(Failed);
  end;
end;

procedure RunTest(const ASource: string; AMode: TMode);
var
  Source, Widgetset: string;
begin
  Source := ExpandFileName(Root + '/tests/regression/' + ASource);
  if not FileExists(Source) then raise Exception.Create('Missing test source: ' + Source);
  if not FileExists(ChangeFileExt(Source, '.lpi')) then
    raise Exception.Create('Missing test project: ' + ChangeFileExt(Source, '.lpi'));
  case AMode of
    mdEBU:
      if EBUFolder = '' then
      begin
        Inc(Selected);
        Skip(ASource, 'requires --ebu with the EBU reference WAVs');
      end else
        RunOne(ASource, AMode, '');
    mdNoGUI: RunOne(ASource, AMode, 'nogui');
    mdWidgetset:
      if Widgetsets.Count = 0 then
      begin
        Inc(Selected);
        Skip(ASource, 'no widgetset selected with --ws');
      end else
        for Widgetset in Widgetsets do RunOne(ASource, AMode, Widgetset);
    else RunOne(ASource, AMode, '');
  end;
end;

function ParseMode(const AName: string): TMode;
begin
  for Result := Low(TMode) to High(TMode) do
    if ModeNames[Result] = AName then Exit;
  raise Exception.Create('Unknown regression mode: ' + AName);
end;

var
  Manifest: TStringList;
  Line, Source: string;
  Bar, i: longint;
begin
  try
    ParseOptions;
    Manifest := TStringList.Create;
    try
      Manifest.LoadFromFile(ManifestFile);
      for i := 0 to Manifest.Count - 1 do
      begin
        Line := Trim(Manifest[i]);
        if (Line = '') or (Line[1] = '#') then Continue;
        Bar := Pos('|', Line);
        if (Bar = 0) or (PosEx('|', Line, Bar + 1) > 0) then
          raise Exception.Create('Invalid manifest line: ' + Line);
        Source := Trim(Copy(Line, 1, Bar - 1));
        if (OnlyPrefix <> '') and not AnsiStartsStr(OnlyPrefix, Source) then Continue;
        RunTest(Source, ParseMode(Trim(Copy(Line, Bar + 1, MaxInt))));
      end;
    finally
      Manifest.Free;
      Widgetsets.Free;
    end;
    if Selected = 0 then raise Exception.Create('No regression test selected');
    Writeln('Regression results: ', Passed, ' passed, ', Failed, ' failed, ',
      Skipped, ' skipped');
    if Failed <> 0 then Halt(1);
  except
    on E: Exception do
    begin
      Writeln(StdErr, 'FAIL: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
