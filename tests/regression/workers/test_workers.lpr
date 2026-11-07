program Test_workers;
{$mode objfpc}{$H+}
uses
  {$ifdef unix}cthreads,{$endif}
  Interfaces, Forms, Classes, SysUtils, TrackAnalyzer, Decoder, Drawers,
  AudioTrack, BGRABitmap;

var
  Destroyed: LongInt;

type
  TObservedAnalyzer = class(TTrackAnalyzer)
    destructor Destroy; override;
  end;
  TObservedDecoder = class(TWaveDecoder)
    destructor Destroy; override;
  end;
  TObservedDrawer = class(TScreenDrawer)
    destructor Destroy; override;
  end;

  TObserver = class
    Active: TThread;
    Calls: Integer;
    OnMainThread: Boolean;
    Status: LongInt;
    Successful: Boolean;
    procedure Finished(Sender: TObject);
    procedure CancelOnStart;
  end;

destructor TObservedAnalyzer.Destroy;
begin
  inherited Destroy;
  InterlockedIncrement(Destroyed);
end;

destructor TObservedDecoder.Destroy;
begin
  inherited Destroy;
  InterlockedIncrement(Destroyed);
end;

destructor TObservedDrawer.Destroy;
begin
  inherited Destroy;
  InterlockedIncrement(Destroyed);
end;

procedure TObserver.CancelOnStart;
begin
  Active.Terminate;
end;

procedure TObserver.Finished(Sender: TObject);
begin
  if Sender <> Active then
    raise Exception.Create('Unexpected termination sender');
  if Sender is TTrackAnalyzer then
    Status := TTrackAnalyzer(Sender).Status
  else if Sender is TWaveDecoder then
    Successful := TWaveDecoder(Sender).Successful
  else if Sender is TScreenDrawer then
    Successful := TScreenDrawer(Sender).Successful;
  Inc(Calls);
  OnMainThread := GetCurrentThreadID = MainThreadID;
  Active := nil;
end;

// Cancellation: 0 = before Start, 1 = immediately after Start,
// 2 = let Execute finish (invalid input exercises the error path).
procedure Run(Kind, Cancellation: Integer; AutoFree: Boolean);
var
  Worker: TThread;
  Observer: TObserver;
  Deadline: QWord;
  DestroyedBefore: LongInt;
begin
  DestroyedBefore := InterlockedCompareExchange(Destroyed, 0, 0);
  // Invalid inputs are intentional: even if Execute wins the race, it must
  // terminate safely without needing audio fixtures, a display or ffmpeg.
  case Kind of
    0: Worker := TObservedAnalyzer.Create(nil, nil, False);
    1: Worker := TObservedDecoder.Create('', '', '', '');
    else Worker := TObservedDrawer.Create(nil, nil);
  end;
  Observer := TObserver.Create;
  Worker.FreeOnTerminate := AutoFree;
  try
    Observer.Active := Worker;
    Worker.OnTerminate := @Observer.Finished;
    if Cancellation = 0 then Worker.Terminate;
    Worker.Start;
    // An auto-free worker might already have destroyed itself, so cancel
    // through Active before pumping its synchronized termination callback.
    if Cancellation = 1 then Observer.Active.Terminate;
    // Same reference-based wait as MainFrm.StopWorkers, with a test timeout.
    Deadline := GetTickCount64 + 5000;
    while Assigned(Observer.Active) do
    begin
      CheckSynchronize(10);
      if GetTickCount64 > Deadline then
        raise Exception.Create('Termination callback missing');
    end;
    if not AutoFree then Worker.WaitFor;
    if (Observer.Calls <> 1) or not Observer.OnMainThread then
      raise Exception.Create('OnTerminate must run once on the main thread');
    if (Kind = 0) and (Cancellation = 0) and (Observer.Status <> -5) then
      raise Exception.Create('Unstarted analysis must be cancelled, not successful');
    if (Kind = 0) and (Cancellation = 2) and (Observer.Status = 0) then
      raise Exception.Create('Invalid analysis must not succeed');
    if (Kind <> 0) and Observer.Successful then
      raise Exception.Create('Invalid or cancelled worker must not succeed');
  finally
    if not AutoFree then Worker.Free;
    // Do not dereference Worker after OnTerminate in the auto-free case.
    // The callback is delivered before the destructor has completed.
    Deadline := GetTickCount64 + 5000;
    while InterlockedCompareExchange(Destroyed, 0, 0) = DestroyedBefore do
    begin
      CheckSynchronize(10);
      if GetTickCount64 > Deadline then
        raise Exception.Create('Worker was not destroyed');
    end;
    Observer.Free;
    if InterlockedCompareExchange(Destroyed, 0, 0) <> DestroyedBefore + 1 then
      raise Exception.Create('Worker must be destroyed exactly once');
  end;
end;

// Real input, owned by the caller just as in MainFrm. Release it as soon
// as OnTerminate clears Active, before the auto-free worker is destroyed.
procedure RunRealInput(CancelDuringExecute: Boolean);
const
  Header: array[0..43] of Byte = (
    $52,$49,$46,$46, $24,$77,$01,$00, $57,$41,$56,$45,
    $66,$6D,$74,$20, $10,$00,$00,$00, $01,$00,$01,$00,
    $80,$BB,$00,$00, $00,$77,$01,$00, $02,$00,$10,$00,
    $64,$61,$74,$61, $00,$77,$01,$00);
var
  Input: TMemoryStream;
  Track: TTrack;
  Bitmap: TBGRABitmap;
  Observer: TObserver;
  Analyzer: TObservedAnalyzer;
  Drawer: TObservedDrawer;
  Deadline: QWord;
  Before: LongInt;

  procedure AwaitCallback;
  begin
    Deadline := GetTickCount64 + 10000;
    while Assigned(Observer.Active) do
    begin
      CheckSynchronize(10);
      if GetTickCount64 > Deadline then
        raise Exception.Create('Real-input worker did not finish');
    end;
    if not Observer.OnMainThread then
      raise Exception.Create('Real-input callback not on main thread');
  end;

begin
  Before := InterlockedCompareExchange(Destroyed, 0, 0);
  Input := TMemoryStream.Create;
  Track := TTrack.Create('synthetic.wav');
  Bitmap := TBGRABitmap.Create(16, 16);
  Observer := TObserver.Create;
  try
    Input.Size := 44 + 96000;
    FillChar(Input.Memory^, Input.Size, 0);
    Input.WriteBuffer(Header, SizeOf(Header));
    Input.Position := 0;
    Analyzer := TObservedAnalyzer.Create(Track, Input, False);
    Observer.Active := Analyzer;
    Analyzer.OnTerminate := @Observer.Finished;
    if CancelDuringExecute then Analyzer.OnStart := @Observer.CancelOnStart;
    Analyzer.Start;
    AwaitCallback;
    FreeAndNil(Input);
    if CancelDuringExecute then
    begin
      if Observer.Status <> -5 then
        raise Exception.Create('Analysis cancelled in Execute must return -5');
    end
    else if Observer.Status <> 0 then
      raise Exception.CreateFmt('Valid WAV failed: %d', [Observer.Status]);

    // Start the next worker immediately from the completion state. No wait
    // for the preceding auto-free thread is required to reuse the track.
    Drawer := TObservedDrawer.Create(Track, Bitmap);
    Drawer.Mode := []; // exercise lifecycle without font/rendering fixtures
    Observer.Active := Drawer;
    Drawer.OnTerminate := @Observer.Finished;
    Drawer.Start;
    AwaitCallback;
    if not Observer.Successful or (Observer.Calls <> 2) then
      raise Exception.Create('Analyzer/drawer chain did not complete');
    FreeAndNil(Track);
    FreeAndNil(Bitmap);
    Deadline := GetTickCount64 + 5000;
    while InterlockedCompareExchange(Destroyed, 0, 0) <> Before + 2 do
    begin
      CheckSynchronize(10);
      if GetTickCount64 > Deadline then
        raise Exception.Create('Real-input workers were not destroyed');
    end;
  finally
    Observer.Free;
    Bitmap.Free;
    Track.Free;
    Input.Free;
  end;
end;

var I, Kind, Cancellation: Integer;
begin
  Application.Initialize;
  for I := 1 to 100 do
    for Kind := 0 to 2 do
      for Cancellation := 0 to 2 do
      begin
        Run(Kind, Cancellation, False);
        Run(Kind, Cancellation, True);
      end;
  for I := 1 to 10 do
  begin
    RunRealInput(False);
    RunRealInput(True);
  end;
  Writeln('1800 worker lifecycle checks and 20 real-input chains passed');
end.
