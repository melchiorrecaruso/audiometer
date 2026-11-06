program Test_decoder;
{$mode objfpc}{$H+}
uses {$ifdef unix}BaseUnix,{$endif} Classes, SysUtils, Process, Decoder;
type
  TCancel = class
    Calls, CancelAfter: integer;
    function Query: boolean;
  end;
function TCancel.Query: boolean;
begin Inc(Calls); Result:=Calls>CancelAfter; end;
{$ifdef unix}
procedure Script(const Path, Body: string);
var Lines: TStringList;
begin
  Lines:=TStringList.Create;
  try
    Lines.Text:='#!/bin/sh'+LineEnding+Body+LineEnding;
    Lines.SaveToFile(Path);
    if fpChmod(Path, &700)<>0 then raise Exception.Create('chmod failed');
  finally Lines.Free; end;
end;
{$endif}
procedure Require(B: boolean; const Msg: string);
begin if not B then raise Exception.Create(Msg); Writeln('PASS: ',Msg); end;
var FF,FP,Err,Folder,Source,Dest: string; P:TProcess; C:TCancel;
  Codec, Fake: string; I:integer;
const Codecs: array[0..5] of string = ('pcm_u8','pcm_s16le','pcm_s24le','pcm_s32le','pcm_f32le','pcm_f64le');
begin
  Require(LocateFFmpegTools(FF,FP)='', 'FFmpeg tools available');
  Folder:=GetTempFileName;
  DeleteFile(Folder); CreateDir(Folder);
  Source:=Folder+'/source.wav'; Dest:=Folder+'/output.wav';
  Fake:=Folder+'/fake-tool';
  C:=TCancel.Create;
  try
    Require(not IsFileSupported('.pcm'),'raw PCM not advertised');
    Require(IsFileSupported('.FLAC'),'case insensitive extension');
    Require(not DecodeToWave('',FP,Source,Dest,Err),'missing executable rejected');
    Require(not DecodeToWave(FF,FP,Source,Dest,Err),'missing input rejected');
    for I:=0 to High(Codecs) do begin
      Codec:=Codecs[I];
      P:=TProcess.Create(nil);
      try
        P.Executable:=FF;
        P.Parameters.AddStrings(['-v','error','-y','-f','lavfi','-i',
          'sine=frequency=997:sample_rate=48000:duration=0.05','-c:a',Codec,Source]);
        P.Options:=[poWaitOnExit]; P.Execute;
        Require(P.ExitStatus=0,'generate '+Codec);
      finally P.Free; end;
      Require(DecodeToWave(FF,FP,Source,Dest,Err),'decode '+Codec+': '+Err);
    end;
    P:=TProcess.Create(nil);
    try
      P.Executable:=FF;
      P.Parameters.AddStrings(['-v','error','-y','-f','lavfi','-i',
        'anullsrc=r=48000:cl=5.1','-t','0.05','-c:a','pcm_s24le',Source]);
      P.Options:=[poWaitOnExit]; P.Execute;
      Require(P.ExitStatus=0,'generate 5.1 layout');
    finally P.Free; end;
    Require(DecodeToWave(FF,FP,Source,Dest,Err),'preserve 5.1 layout: '+Err);
    // Stereo FLAC reports "stereo", the plain PCM WAV written for it "unknown".
    P:=TProcess.Create(nil);
    try
      P.Executable:=FF;
      P.Parameters.AddStrings(['-v','error','-y','-f','lavfi','-i',
        'anullsrc=r=44100:cl=stereo','-t','0.05','-c:a','flac',Folder+'/source.flac']);
      P.Options:=[poWaitOnExit]; P.Execute;
      Require(P.ExitStatus=0,'generate stereo FLAC');
    finally P.Free; end;
    Require(DecodeToWave(FF,FP,Folder+'/source.flac',Dest,Err),'decode stereo FLAC: '+Err);
    DeleteFile(Folder+'/source.flac');
    Require(not DecodeToWave(FF,FP,Source,Source,Err),'same path rejected');
    DeleteFile(Dest);
    Require(not DecodeToWave(FF,FP,Source,Dest,Err,@C.Query),'pre-cancel rejected');
    Require((Err='Cancelled.') and not FileExists(Dest),'pre-cancel creates no output');
    {$ifdef unix}
    // A real child that continuously fills both pipes must remain cancellable.
    Script(Fake,'while :; do printf "stdout data\\n"; printf "stderr data\\n" >&2; done');
    C.Calls:=0; C.CancelAfter:=100;
    Require(not DecodeToWave(FF,Fake,Source,Dest,Err,@C.Query),'flooding process cancelled');
    Require(Err='Cancelled.','cancellation diagnostic');
    // Large diagnostics must still be drained, but retained memory is bounded.
    Script(Fake,'i=0; while [ "$i" -lt 20000 ]; do echo "diagnostic data" >&2; i=$((i+1)); done; exit 1');
    Require(not DecodeToWave(FF,Fake,Source,Dest,Err),'verbose process failure');
    Require((Length(Err)>0) and (Length(Err)<=65536),'bounded diagnostic');
    // Fake the container signature without allocating a >4 GB fixture.
    Script(Fake,'for arg do :; done; printf RF64 > "$arg"');
    Require(not DecodeToWave(Fake,FP,Source,Dest,Err),'RF64 rejected');
    Require(Pos('RF64',Err)>0,'explicit RF64 diagnostic');
    {$endif}
  finally
    C.Free; DeleteFile(Source); DeleteFile(Dest); DeleteFile(Fake); RemoveDir(Folder);
  end;
end.
