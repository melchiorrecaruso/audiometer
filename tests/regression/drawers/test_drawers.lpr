program Test_drawers;
{$mode objfpc}{$H+}
uses HeapTrc, {$ifdef unix}cthreads,{$endif} Interfaces, Forms, Classes, SysUtils, Math,
 BGRABitmap, BGRABitmapTypes, BaseGraphics, Drawers, AudioTrack, WavReader;
type TDrawerClass = class of TCustomDrawer;
const Workers: array[0..4] of TDrawerClass =
 (TSpectrogramDrawer, TSpectrumDrawer, TWaveDrawer, TLoudnessDrawer, TBlockDrawer);
var Checks: integer;
procedure Require(B: boolean; const Msg: string);
begin
 Inc(Checks);
 if not B then raise Exception.Create(Msg);
end;
procedure Run(Count,W,H,Kind:integer; Silence,Fault:boolean);
var T:TTrack; F:TWaveFormat; B:TBGRABitmap; D:TCustomDrawer;
 i,x,y,RedColumns,Colored:integer; P:TBGRAPixel; Raised,HasRed:boolean;
begin
 T:=TTrack.Create('synthetic'); B:=nil;
 try
  if not Fault then B:=TBGRABitmap.Create(W,H);
  F:=Default(TWaveFormat); F.SampleRate:=48000; F.BitsPerSample:=16;
  F.Channels:=1; F.BlockAlign:=2; F.DataSize:=Count*2;
  T.SetFormat(F,Count);
  Require(Abs(T.DurationSeconds-Count/48000)<1e-12,'fractional duration');
  for i:=0 to Count-1 do
    if not Silence then T.Channels[0][i]:=0.5*Sin(2*Pi*1000*i/48000);
  T.Spectrums.Init; T.Spectrums.Process(T.Channels,Count);
  T.Loudness.Init; T.Loudness.Process(T.Channels,Count,48000);
  T.DRMeter.Init; T.DRMeter.Process(T.Channels,Count,48000);
  D:=Workers[Kind].Create(T,B);
  try
   D.Start; D.WaitFor; Raised:=False;
   try D.RaiseIfFailed;
   except on E:Exception do begin
    if not Fault then raise;
    Raised:=True;
   end; end;
   Require(Raised=Fault,'worker failure propagation');
  finally D.Free; end;
  if (Kind=0) and (Count=24000) and (W=800) and not Silence and not Fault then begin
   Colored:=0;
   for y:=0 to H-1 do for x:=0 to W-1 do begin
    P:=B.GetPixel(x,y);
    if (P.alpha>0) and ((P.red<>P.green) or (P.green<>P.blue)) then Inc(Colored);
   end;
   Require(Colored>2000,'subsecond spectrogram must contain data, not just axes');
  end;
  if (Kind=2) and (Count=24000) and (W=800) and not Silence and not Fault then begin
   RedColumns:=0;
   for x:=0 to W-1 do begin
    HasRed:=False;
    for y:=0 to H-1 do begin
     P:=B.GetPixel(x,y);
     if (P.red>150) and (P.green<100) and (P.blue<100) then HasRed:=True;
    end;
    if HasRed then Inc(RedColumns);
   end;
   Require(RedColumns>100,'subsecond waveform must not collapse onto one column');
  end;
 finally B.Free; T.Free; end;
end;
var K:integer;
begin
 // A leak sets exit code 203 at the end of the program.
 HaltOnNotReleased:=True;
 Application.Initialize;
 // Resolve the bundled font relative to the executable, not the cwd.
 DefaultFontName:='DejaVu Sans';
 DefaultFontFileName:=ExpandFileName(ExtractFilePath(ParamStr(0))+
   '../../../fonts/DejaVuSans/DejaVuSans.ttf');
 Require(FileExists(DefaultFontFileName),'bundled font available');
 InitializeChartFont;
 for K:=0 to High(Workers) do begin
  Run(96000,800,300,K,False,False);
  Run(24000,800,300,K,False,False);
  Run(60000,800,300,K,False,False);
  Run(1000,800,300,K,False,False);
  Run(0,800,300,K,False,False);
  Run(192000,800,300,K,True,False);
  Run(192000,800,300,K,False,False);
  Run(96000,20,10,K,False,False);
  Run(96000,0,0,K,False,False);
  // Force an exception after allocating chart resources; heaptrc verifies cleanup.
  Run(96000,800,300,K,False,True);
 end;
 Writeln(Checks,' checks passed');
end.
