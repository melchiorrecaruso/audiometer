{
  Description: Track analyzer thread.

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

unit TrackAnalyzer;

{$mode objfpc}

interface

uses
  Classes, Common, AudioTrack, SysUtils;

type
  { TTrackAnalyzer }

  // Reads a WAV stream into a track and runs every meter on it, reporting
  // the progress. Status: 0 = done, -1 = unreadable or unsupported file,
  // -2 = no channels or no audio, -4 = the analysis raised an exception,
  // -5 = cancelled by Terminate (checked at every progress tick).
  TTrackAnalyzer = class(TThread)
  private
    FTrack: TTrack;
    //---
    FStatus: longint;
    FStream: tstream;
    FPercentage: longint;
    FOnStart: tthreadmethod;
    FOnStop: tthreadmethod;
    FOnTick: tthreadmethod;
    FTick, FTickCount: longint;
    FFFTOn: boolean;
    procedure DoTick;
    procedure ReadStream(AStream: TStream);
  public
    constructor Create(ATrack: TTrack; AStream: TStream; AFFTOn: boolean);
    destructor Destroy; override;
    procedure Execute; override;
  public
    property OnStart: tthreadmethod write FOnStart;
    property OnStop: tthreadmethod write FOnStop;
    property OnTick: tthreadmethod write FOnTick;
    property Percentage: longint read FPercentage;
    property Status: longint read FStatus;
  end;

var
  AudioAnalyzer: TTrackAnalyzer = nil;


implementation

uses
  Spectrum, WavReader;

// TTrackAnalyzer

constructor TTrackAnalyzer.Create(ATrack: TTrack; AStream: TStream; AFFTOn: boolean);
begin
  FFFTOn := AFFTOn;
  FTrack := ATrack;
  FStream := AStream;
  FreeOnTerminate := True;
  inherited Create(True);
end;

destructor TTrackAnalyzer.Destroy;
begin
  inherited Destroy;
end;

procedure TTrackAnalyzer.Execute;
{$ifopt D+}
var
  ch: longint;
{$endif}
begin
  FPercentage := 0;
  if Assigned(FOnStart) then
    Synchronize(FOnStart);
  try
    try
      ReadStream(FStream);
      {$ifopt D+}
      writeln;
      writeln('LOUDNESS:');
      writeln('Track.Rms         ', FTrack.Loudness.Rms                :2:2);
      writeln('Track.Peak        ', FTrack.Loudness.Peak               :2:2);
      writeln('Track.TruePeak    ', FTrack.Loudness.TruePeak           :2:2);
      writeln('Track.Lk          ', FTrack.Loudness.IntegratedLoudness :2:2);
      writeln('Track.LRA         ', FTrack.Loudness.LoudnessRange      :2:2);
      writeln('Track.PLR         ', FTrack.Loudness.PeakToLoudnessRatio:2:2);

      writeln;
      writeln('DYNAMIC RANGE:');
      writeln('Track.DR:         ', FTrack.DRMeter.DR                  :2:2);
      for ch := 0 to FTrack.DRMeter.ChannelCount -1 do
      begin
        writeln(ChannelName(ch, FTrack.ChannelCount),'.Rms         ', Decibel(FTrack.DRMeter.Rms (ch)):2:2);
        writeln(ChannelName(ch, FTrack.ChannelCount),'.Peak        ', Decibel(FTrack.DRMeter.Peak(ch)):2:2);
        writeln(ChannelName(ch, FTrack.ChannelCount),'.DR          ',         FTrack.DRMeter.DR  (ch) :2:2);
      end;
      writeln;
      {$endif}
    except
      on EAbort do
        FStatus := -5;
      // Out of memory, arithmetic errors, ...: report a failed analysis instead
      // of letting OnStop present partially computed results as valid.
      on E: Exception do
      begin
        {$ifopt D+}
        writeln('Track.Exception    ', E.ClassName, ': ', E.Message);
        {$endif}
        FStatus := -4;
      end;
    end;
  finally
    FPercentage := 100;
    if Assigned(FOnStop) then
      Synchronize(FOnStop);
  end;
end;

procedure TTrackAnalyzer.DoTick;
begin
  if Terminated then Abort;
  Inc(FTick);
  FPercentage := Trunc(100 * FTick / FTickCount);
  if Assigned(FOnTick) then
  begin
    Queue(FOnTick);
  end;
end;

procedure TTrackAnalyzer.ReadStream(AStream: TStream);
var
  Format: TWaveFormat;
  FrameCount: longword;
begin
  {$ifopt D+}
  writeln('Track.Filename     ', FTrack.Filename);
  {$endif}
  //Read headers
  FStatus := ReadWaveHeader(AStream, Format);
  if Status <> 0 then Exit;

  // Sample positions are longint throughout the meters: reject data chunks
  // holding more frames than that (only reachable with tiny block alignments).
  FrameCount := Format.DataSize div Format.BlockAlign;
  if FrameCount > High(longint) then
  begin
    FTrack.SetFormat(Format, 0);
    FStatus := -1;
    Exit;
  end;
  // Update track details
  FTrack.SetFormat(Format, FrameCount);

  FStatus := ReadWaveSamples(AStream, Format, FTrack.Channels, FTrack.SampleCount);
  if Status <> 0 then Exit;
  if Terminated then Abort;

  // Init
  FTrack.DRMeter  .Init(@DoTick);
  FTrack.Loudness .Init(@DoTick);
  FTrack.Spectrums.Init(DEFAULTWINDOWSIZE, DEFAULTHOPSIZE, @DoTick);

  // Estimate TickCount
  FTick := 0;
  FTickCount := 0;
  Inc(FTickCount, FTrack.DRMeter.EstimatedTicks(FTrack.ChannelCount, FTrack.SampleCount, FTrack.SampleRate));
  Inc(FTickCount, FTrack.Loudness.EstimatedTicks(FTrack.ChannelCount, FTrack.SampleCount, FTrack.SampleRate));
  if FFFTOn then
  begin
    Inc(FTickCount, FTrack.Spectrums.EstimatedTicks(FTrack.ChannelCount, FTrack.SampleCount));
  end;

  // Process
  FTrack.DRMeter.Process(FTrack.Channels, FTrack.SampleCount, FTrack.SampleRate);
  FTrack.Loudness.Process(FTrack.Channels, FTrack.SampleCount, FTrack.SampleRate);
  if FFFTOn then
  begin
    FTrack.Spectrums.Process(FTrack.Channels, FTrack.SampleCount);
  end;
end;

end.
