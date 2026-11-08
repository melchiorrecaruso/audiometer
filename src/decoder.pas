{
  Description: Audio decoding to WAV through ffmpeg/ffprobe.

  Copyright (C) 2025-2026 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>.
}

unit Decoder;

{$mode objfpc}{$h+}

interface

uses
  Classes, SysUtils;

type
  TCancelQuery = function: boolean of object;

  // Decodes one file to a WAV in a background thread. OnTerminate is synchronized
  // with the main thread; Terminate stops ffmpeg/ffprobe.
  TWaveDecoder = class(TThread)
  private
    FFFmpeg: string;
    FFFprobe: string;
    FInputFile: string;
    FOutputFile: string;
    FSuccessful: boolean;
    FErrorMessage: string;
    function IsCancelled: boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(const AFFmpeg, AFFprobe, AInputFile, AOutputFile: string);
    property InputFile: string read FInputFile;
    property OutputFile: string read FOutputFile;
    property Successful: boolean read FSuccessful;
    property ErrorMessage: string read FErrorMessage;
  end;

function IsFileSupported(AFileExtension: string): boolean;
function OpenDialogFileFilter: string;

// Finds ffmpeg and ffprobe (next to the executable on Windows, in the PATH
// elsewhere). Returns the names of the missing tools, '' when both exist.
function LocateFFmpegTools(out AFFmpeg, AFFprobe: string): string;

// Decodes the first audio stream of AInputFile to AOutputFile as a WAV with
// the same sample format, rate and channels, then checks the result with
// ffprobe. ACancelled, when given, is polled while the tools run.
function DecodeToWave(const AFFmpeg, AFFprobe, AInputFile, AOutputFile: string;
  out AError: string; ACancelled: TCancelQuery = nil): boolean;


implementation

uses
  FileUtil, LazFileUtils, Process, Pipes, Math;

function IsFileSupported(AFileExtension: string): boolean;
begin
  AFileExtension := LowerCase(AFileExtension);
  if AFileExtension = '.wav'  then exit(True);
  if AFileExtension = '.flac' then exit(True);
  if AFileExtension = '.mp3'  then exit(True);
  if AFileExtension = '.ape'  then exit(True);
  if AFileExtension = '.ogg'  then exit(True);
  if AFileExtension = '.m4a'  then exit(True);
  if AFileExtension = '.ac3'  then exit(True);
  if AFileExtension = '.opus' then exit(True);
  if AFileExtension = '.aac'  then exit(True);
  if AFileExtension = '.aiff' then exit(True);
  if AFileExtension = '.alac' then exit(True);
  if AFileExtension = '.wma'  then exit(True);
  Result := False;
end;

function OpenDialogFileFilter: string;
begin
  Result := 'Supported files|*.wav;*.flac;*.mp3;*.ape;*.ogg;*.m4a;*.ac3;*.opus;*.aac;*.aiff;*.alac;*.wma;|All files|*.*;';
end;

function LocateFFmpegTools(out AFFmpeg, AFFprobe: string): string;
begin
  {$IFDEF MSWINDOWS}
  AFFmpeg := ExtractFilePath(ParamStr(0)) + 'ffmpeg.exe';
  AFFprobe := ExtractFilePath(ParamStr(0)) + 'ffprobe.exe';
  if not FileExistsUTF8(AFFmpeg) then AFFmpeg := '';
  if not FileExistsUTF8(AFFprobe) then AFFprobe := '';
  {$ELSE}
  AFFmpeg := FindDefaultExecutablePath('ffmpeg');
  AFFprobe := FindDefaultExecutablePath('ffprobe');
  {$ENDIF}

  Result := '';
  if AFFmpeg = '' then Result := 'ffmpeg';
  if AFFprobe = '' then
  begin
    if Result <> '' then Result := Result + ' and ';
    Result := Result + 'ffprobe';
  end;
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
  out AOutput, AError: string; ACancelled: TCancelQuery): boolean;
var
  Buff: array[0..4095] of byte;
  Count: longint;
  OutputStream, ErrorStream: TMemoryStream;
  Process: TProcess;
  Cancelled: boolean;

  procedure DrainOnce(AStream: TInputPipeStream; ADestination: TMemoryStream);
  const MaxDiagnosticBytes = 65536;
  var Keep: longint;
  begin
    if AStream.NumBytesAvailable = 0 then Exit;
    Count := AStream.Read(Buff, SizeOf(Buff));
    if Count <= 0 then Exit;
    Keep := Min(Count, MaxDiagnosticBytes - ADestination.Size);
    if Keep > 0 then ADestination.WriteBuffer(Buff, Keep);
  end;
begin
  Result := False;
  Cancelled := False;
  AOutput := '';
  AError := '';
  if Assigned(ACancelled) and ACancelled() then
  begin
    AError := 'Cancelled.';
    Exit;
  end;
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
        if Assigned(ACancelled) and ACancelled() then
        begin
          if Process.Running then
          begin
            Process.Terminate(255);
            Process.WaitOnExit;
          end;
          Cancelled := True;
          Break;
        end;
        // Bounded reads keep both pipes and cancellation responsive, even
        // when a child continuously writes. Drain excess diagnostics too.
        DrainOnce(Process.Output, OutputStream);
        DrainOnce(Process.Stderr, ErrorStream);
        if Process.Running and (Process.Output.NumBytesAvailable = 0) and
           (Process.Stderr.NumBytesAvailable = 0) then
          Sleep(1);
      end;
      Result := (not Cancelled) and (Process.ExitStatus = 0);
    except
      on E: Exception do AError := E.Message;
    end;
    AOutput := StreamToString(OutputStream);
    if Cancelled then
      AError := 'Cancelled.'
    else if AError = '' then
      AError := Trim(StreamToString(ErrorStream));
  finally
    try
      if Process.Running then
      begin
        Process.Terminate(255);
        Process.WaitOnExit;
      end;
    finally
      ErrorStream.Free;
      OutputStream.Free;
      Process.Free;
    end;
  end;
end;

function ProbeAudio(const AExecutable, AFilename: string;
  out ASampleFormat, ALayout: string; out ABitsPerSample, ASampleRate, AChannels: longint;
  out AError: string; ACancelled: TCancelQuery): boolean;
var
  Output: string;
  Parameters, Values: TStringList;
begin
  ASampleFormat := '';
  ALayout := '';
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
    Parameters.Add('stream=sample_fmt,bits_per_sample,bits_per_raw_sample,sample_rate,channels,channel_layout');
    Parameters.Add('-of');
    Parameters.Add('default=noprint_wrappers=1:nokey=0');
    Parameters.Add(AFilename);
    Result := RunProcess(AExecutable, Parameters, Output, AError, ACancelled);
    if not Result then
    begin
      if AError = '' then AError := 'ffprobe failed without an error message.';
      Exit;
    end;

    Values.Text := Output;
    ASampleFormat := LowerCase(Trim(Values.Values['sample_fmt']));
    ALayout := LowerCase(Trim(Values.Values['channel_layout']));
    if ALayout = 'unknown' then ALayout := '';
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

function DecodeToWave(const AFFmpeg, AFFprobe, AInputFile, AOutputFile: string;
  out AError: string; ACancelled: TCancelQuery): boolean;
var
  Codec, ErrorOutput, ExpectedFormat, Output: string;
  InputBits, InputChannels, InputRate: longint;
  OutputBits, OutputChannels, OutputRate: longint;
  InputFormat, OutputFormat, InputLayout, OutputLayout: string;
  WaveStream: TFileStream;
  Header: array[0..3] of char;
  Parameters: TStringList;
begin
  Result := False;
  AError := '';
  if (AFFmpeg = '') or not FileExistsUTF8(AFFmpeg) then
  begin
    AError := 'ffmpeg is not available. The audio file was not converted.';
    Exit;
  end;
  if (AFFprobe = '') or not FileExistsUTF8(AFFprobe) then
  begin
    AError := 'ffprobe is not available. The audio file was not converted.';
    Exit;
  end;
  if AOutputFile = '' then
  begin
    AError := 'The temporary WAV name is not available for this application session.';
    Exit;
  end;

  if ExpandFileName(AInputFile) = ExpandFileName(AOutputFile) then
  begin
    AError := 'Input and output files must be different.';
    Exit;
  end;

  if not ProbeAudio(AFFprobe, AInputFile, InputFormat, InputLayout, InputBits,
    InputRate, InputChannels, AError, ACancelled) then Exit;
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
    AError := Format(
      'The decoded sample format "%s" is not supported without a possible loss of precision.',
      [InputFormat]);
    Exit;
  end;

  // The caller reuses the same WAV path for every file of the session.
  // ffmpeg overwrites this file; the result of this function is the authority
  // that prevents an incomplete output from being analyzed or played.

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
    // Avoid silently overflowing RIFF sizes. RF64 is detected below and
    // rejected explicitly because the application reader only accepts RIFF.
    Parameters.Add('-rf64');
    Parameters.Add('auto');
    Parameters.Add('-f');
    Parameters.Add('wav');
    Parameters.Add(AOutputFile);
    if not RunProcess(AFFmpeg, Parameters, Output, ErrorOutput, ACancelled) then
    begin
      if ErrorOutput = '' then ErrorOutput := 'ffmpeg failed without an error message.';
      AError := 'Unable to create the analysis WAV:' + LineEnding + ErrorOutput;
      Exit;
    end;
  finally
    Parameters.Free;
  end;

  try
    WaveStream := TFileStream.Create(AOutputFile, fmOpenRead or fmShareDenyWrite);
    try
      WaveStream.ReadBuffer(Header, SizeOf(Header));
      if Header = 'RF64' then
      begin
        AError := 'The decoded WAV exceeds the RIFF size limit. RF64 is not supported by the audio reader.';
        Exit;
      end;
      if Header <> 'RIFF' then
      begin
        AError := 'The decoded file is not a RIFF WAV.';
        Exit;
      end;
    finally
      WaveStream.Free;
    end;
  except
    on E: Exception do
    begin
      AError := 'Unable to validate the analysis WAV: ' + E.Message;
      Exit;
    end;
  end;

  if not ProbeAudio(AFFprobe, AOutputFile, OutputFormat, OutputLayout, OutputBits,
    OutputRate, OutputChannels, AError, ACancelled) then
  begin
    AError := 'The WAV created by ffmpeg is invalid:' + LineEnding + AError;
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
    AError := Format(
      'The WAV created by ffmpeg has an unexpected sample format (%s, %d bit).',
      [OutputFormat, OutputBits]);
    Exit;
  end;
  if (OutputRate <> InputRate) or (OutputChannels <> InputChannels) then
  begin
    AError := Format(
      'The WAV created by ffmpeg changed the audio layout (%d Hz/%d channels to %d Hz/%d channels).',
      [InputRate, InputChannels, OutputRate, OutputChannels]);
    Exit;
  end;
  // A plain PCM WAV (mono, stereo) has no channel mask, so ffprobe reports
  // its layout as unknown: compare the layouts only when both are known.
  if (InputLayout <> '') and (OutputLayout <> '') and
     (OutputLayout <> InputLayout) then
  begin
    AError := Format('The WAV changed the channel layout (%s to %s).',
      [InputLayout, OutputLayout]);
    Exit;
  end;
  if Assigned(ACancelled) and ACancelled() then
  begin
    AError := 'Cancelled.';
    Exit;
  end;
  Result := True;
end;

// TWaveDecoder

constructor TWaveDecoder.Create(const AFFmpeg, AFFprobe, AInputFile, AOutputFile: string);
begin
  FFFmpeg := AFFmpeg;
  FFFprobe := AFFprobe;
  FInputFile := AInputFile;
  FOutputFile := AOutputFile;
  FSuccessful := False;
  FErrorMessage := '';
  FreeOnTerminate := True;
  inherited Create(True);
end;

function TWaveDecoder.IsCancelled: boolean;
begin
  Result := Terminated;
end;

procedure TWaveDecoder.Execute;
begin
  try
    FSuccessful := DecodeToWave(FFFmpeg, FFFprobe, FInputFile, FOutputFile,
      FErrorMessage, @IsCancelled);
  except
    on E: Exception do
    begin
      FSuccessful := False;
      FErrorMessage := E.Message;
    end;
  end;
end;

end.
