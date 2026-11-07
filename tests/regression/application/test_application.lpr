program Test_application;
{$mode objfpc}{$H+}
// Creates the application's forms as audiometer.lpr does, shows them on the
// widgetset the program was built for, analyzes two synthetic WAVs through
// the main form's batch, resizes and closes the main form.
// Argument: results folder, where the WAVs are written.
uses
  CMem, {$ifdef unix}cthreads,{$endif}
  Interfaces, InterfaceBase, Forms, Controls, Classes, SysUtils, Math,
  Common, AudioTrack, Decoder, MainFrm, ReportFrm;

procedure Require(B: Boolean; const Msg: string);
begin
  if not B then raise Exception.Create(Msg);
end;

procedure Pump(AMilliseconds: QWord);
var
  Deadline: QWord;
begin
  Deadline := GetTickCount64 + AMilliseconds;
  while not Application.Terminated and (GetTickCount64 < Deadline) do
  begin
    Application.ProcessMessages;
    Sleep(10);
  end;
end;

// 5 s stereo 1 kHz sine, 48 kHz 16 bit, with the given peak. EBU Tech 3341
// cases 1 and 2: a -23 (-33) dBFS stereo sine measures -23 (-33) LUFS.
procedure WriteSine(const AFileName: string; APeakDB: double);
const
  Rate = 48000;
  Count = 5 * Rate;
var
  F: TFileStream;
  Samples: array of SmallInt;
  i: longint;
  A: double;

  procedure Put32(V: longword); begin F.WriteBuffer(V, 4); end;
  procedure Put16(V: word); begin F.WriteBuffer(V, 2); end;

begin
  A := Power(10, APeakDB / 20) * 32767;
  SetLength(Samples, 2 * Count);
  for i := 0 to Count - 1 do
  begin
    Samples[2 * i] := Round(A * Sin(2 * Pi * 1000 * i / Rate));
    Samples[2 * i + 1] := Samples[2 * i];
  end;
  F := TFileStream.Create(AFileName, fmCreate);
  try
    F.WriteBuffer('RIFF', 4); Put32(36 + 4 * Count); F.WriteBuffer('WAVE', 4);
    F.WriteBuffer('fmt ', 4); Put32(16); Put16(1); Put16(2);
    Put32(Rate); Put32(4 * Rate); Put16(4); Put16(16);
    F.WriteBuffer('data', 4); Put32(4 * Count);
    F.WriteBuffer(Samples[0], 4 * Count);
  finally
    F.Free;
  end;
end;

// Opens AFiles in the form as the folder button does and waits for the
// analysis and for the charts of the shown track.
procedure Analyze(const AFiles: array of string; const AReportFile: string);
var
  Deadline: QWord;
begin
  AudioFrm.AnalyzeFiles(AFiles, AReportFile);
  Deadline := GetTickCount64 + 60000;
  repeat
    Pump(20);
    Require(GetTickCount64 < Deadline, 'analysis timed out');
  until not AudioFrm.WorkersActive;
  Require(AudioFrm.LastBatchError = '', 'batch error: ' + AudioFrm.LastBatchError);
end;

procedure CheckTrack(ATrack: TTrack; const AFileName: string; ALoudness: double);
begin
  Require(ATrack.Filename = AFileName, 'track order: ' + ATrack.Filename);
  Require((ATrack.SampleRate = 48000) and (ATrack.ChannelCount = 2), 'format');
  Require(Abs(ATrack.Loudness.IntegratedLoudness - ALoudness) <= 0.1,
    Format('%s: integrated loudness %.2f LUFS, expected %.1f',
      [ExtractFileName(AFileName), ATrack.Loudness.IntegratedLoudness, ALoudness]));
  Require(Abs(Decibel(ATrack.DRMeter.Peak) - ALoudness) <= 0.05,
    Format('%s: peak %.2f dBFS, expected %.1f',
      [ExtractFileName(AFileName), Decibel(ATrack.DRMeter.Peak), ALoudness]));
end;

var
  Deadline: QWord;
  Folder, Loud, Quiet, FFmpeg, FFprobe, Missing: string;
begin
  if ParamCount <> 1 then
  begin
    Writeln(StdErr, 'Usage: test_application results-folder');
    Halt(2);
  end;
  // Fail non-interactively before form creation can show a missing-tools dialog.
  Missing := LocateFFmpegTools(FFmpeg, FFprobe);
  if Missing <> '' then
  begin
    Writeln(StdErr, 'Missing required audio tools: ', Missing);
    Halt(1);
  end;
  Folder := IncludeTrailingPathDelimiter(ExpandFileName(ParamStr(1)));
  Loud := Folder + 'sine-23.wav';
  Quiet := Folder + 'sine-33.wav';
  WriteSine(Loud, -23);
  WriteSine(Quiet, -33);

  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Title := 'AudioMeter';
  Application.Initialize;
  Application.CreateForm(TAudioFrm, AudioFrm);
  Application.CreateForm(TReportForm, ReportForm);
  Require(Application.MainForm = AudioFrm, 'main form');

  AudioFrm.Show;
  ReportForm.Show;
  Pump(500);
  Require(AudioFrm.HandleAllocated and AudioFrm.Visible, 'main form shown');
  Require(ReportForm.HandleAllocated and ReportForm.Visible, 'report form shown');
  Require(AudioFrm.ClientWidth > 0, 'main form has a client area');

  // A two-track batch, then a single file replacing it.
  Analyze([Loud, Quiet], Folder + 'batch.md');
  Require(AudioFrm.Tracks.Count = 2, 'two tracks analyzed');
  CheckTrack(AudioFrm.Tracks[0], Loud, -23);
  CheckTrack(AudioFrm.Tracks[1], Quiet, -33);
  Require(Pos('sine-33.wav', ReportForm.Memo.Text) > 0, 'report lists the batch');
  Analyze([Quiet], ChangeFileExt(Quiet, '.md'));
  Require(AudioFrm.Tracks.Count = 1, 'the batch is replaced');
  CheckTrack(AudioFrm.Tracks[0], Quiet, -33);

  // Resizing redraws the charts of the analyzed track.
  AudioFrm.SetBounds(AudioFrm.Left, AudioFrm.Top, 640, 480);
  Pump(300);
  AudioFrm.SetBounds(AudioFrm.Left, AudioFrm.Top, 1024, 700);
  Pump(300);
  ReportForm.Hide;
  Pump(100);

  // Closing the main form terminates the application.
  AudioFrm.Close;
  Deadline := GetTickCount64 + 5000;
  while not Application.Terminated do
  begin
    Application.ProcessMessages;
    Require(GetTickCount64 < Deadline, 'closing the main form terminates the application');
    Sleep(10);
  end;
  Writeln('Application forms shown, two WAVs analyzed, resized and closed on ',
    WidgetSet.ClassName);
end.
