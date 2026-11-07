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

type
  // Described in the header of tests/regression/tests.txt.
  TMode = (mdConsole, mdNoGUI, mdFFmpeg, mdEBU);

const
  ModeNames: array[TMode] of string = ('console', 'nogui', 'ffmpeg', 'ebu');

var
  Root, ManifestFile, BuildDir, LazBuild, PCP, OnlyPrefix: string;
  EBUFolder, FFmpegDir, TargetCPU, TargetOS: string;
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
end;

// Runs AExe in ADir and saves its command line, output and exit status to
// ALog. The pipe is read while the child runs, so a verbose child cannot
// block on a full pipe. Launch errors, nonzero exit codes and timeouts fail.
function RunLogged(const AExe: string; const AArgs: array of string;
  const ADir, ALog: string; ATimeout: longint): boolean;
var
  Child: TProcess;
  Log: TStringStream;
  Buffer: array[0..4095] of byte;
  Count: longint;
  Start: QWord;
  TimedOut: boolean;
  Arg, Status: string;

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
    Log.WriteString('$ ' + AExe);
    for Arg in AArgs do Log.WriteString(' ' + Arg);
    Log.WriteString(LineEnding + LineEnding);
    Child.Executable := AExe;
    for Arg in AArgs do Child.Parameters.Add(Arg);
    Child.CurrentDirectory := ADir;
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

procedure RunTest(const ASource: string; AMode: TMode);
var
  Source, Project, Exe, Name, OutDir: string;
  Args: array of string;
  Ok: boolean;
begin
  Source := ExpandFileName(Root + '/tests/regression/' + ASource);
  Project := ChangeFileExt(Source, '.lpi');
  if not FileExists(Source) then raise Exception.Create('Missing test source: ' + Source);
  if not FileExists(Project) then raise Exception.Create('Missing test project: ' + Project);
  Inc(Selected);
  if (AMode = mdEBU) and (EBUFolder = '') then
  begin
    Skip(ASource, 'requires --ebu with the EBU reference WAVs');
    Exit;
  end;

  // The project builds its program beside itself, named after the source.
  Name := ChangeFileExt(ExtractFileName(Source), '');
  Exe := ChangeFileExt(Source, ExeExt);
  OutDir := BuildDir + PathDelim + ModeNames[AMode] + PathDelim + Name;
  if not ForceDirectories(OutDir) then raise Exception.Create('Cannot create ' + OutDir);
  if FileExists(Exe) and not DeleteFile(Exe) then
    raise Exception.Create('Cannot remove the previous program ' + Exe);

  Writeln('BUILD [', ModeNames[AMode], ']: ', ASource);
  Args := ['--no-write-project'];
  if AMode = mdNoGUI then Args := Concat(Args, ['--ws=nogui']);
  if TargetCPU <> '' then Args := Concat(Args, ['--cpu=' + TargetCPU]);
  if TargetOS <> '' then Args := Concat(Args, ['--os=' + TargetOS]);
  if PCP <> '' then Args := Concat(Args, ['--pcp=' + PCP]);
  Args := Concat(Args, [Project]);
  Ok := RunLogged(LazBuild, Args, Root, OutDir + PathDelim + 'build.log', BuildTimeout);
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
      mdNoGUI: Args := [OutDir];
      mdEBU:   Args := [EBUFolder];
      else     Args := [];
    end;
    Writeln('RUN: ', ASource);
    Ok := RunLogged(Exe, Args, OutDir, OutDir + PathDelim + 'run.log', RunTimeout);
  end;

  if Ok then
  begin
    Writeln('PASS: ', ASource);
    Inc(Passed);
  end else
  begin
    Writeln('FAIL: ', ASource, ' (logs: ', OutDir, ')');
    Inc(Failed);
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
