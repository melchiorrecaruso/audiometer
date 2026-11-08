program Test_drawers;
{$mode objfpc}{$H+}{$modeswitch nestedprocvars}
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
// Data painted through the Draw callback must stay below both axes.
procedure CheckAxesOverData;
var C:TChart; B:TBGRABitmap; x,y,Axis,Data:integer; P:TBGRAPixel;
 procedure Flood(AChart:TChart; ACanvas:TBGRABitmap);
 begin ACanvas.Fill(BGRA(0,0,255)); end;
begin
 C:=TChart.Create; B:=TBGRABitmap.Create(400,200);
 try
  C.XAxisLineColor:=BGRA(255,0,0); C.YAxisLineColor:=BGRA(255,0,0);
  C.XMinF:=0; C.XMaxF:=10; C.YMinF:=0; C.YMaxF:=10;
  C.AddPixel(10,10,BGRA(0,0,0));
  C.Draw(B,B.Width,B.Height,True,@Flood);
  Axis:=0; Data:=0;
  for y:=0 to B.Height-1 do for x:=0 to B.Width-1 do begin
   P:=B.GetPixel(x,y);
   if (P.red>200) and (P.blue<50) then Inc(Axis);
   if (P.blue>200) and (P.red<50) then Inc(Data);
  end;
  Require(Data>1000,'callback paints the data');
  Require(Axis>300,'axes are drawn over the data');
 finally B.Free; C.Free; end;
end;
// Steps are 1, 2, 2.5 or 5 x 10^n; the axes end where the data ends.
procedure CheckAxisRange;
 procedure Step(R:double; N:integer; I:boolean; Expected:double);
 var S:double;
 begin
  S:=GetNiceStep(R,N,I);
  Require(Abs(S-Expected)<=1e-9*Max(1,Expected),
   Format('nice step for %g / %d: got %g, expected %g',[R,N,S,Expected]));
 end;
var C:TChart; B:TBGRABitmap;
begin
 Step(8,8,False,1);
 Step(24000,5,False,5000);
 Step(96,4,False,25);
 Step(96,5,False,20);
 Step(75,5,False,20);
 Step(10,5,False,2);
 Step(100,4,False,25);
 Step(3.35,8,False,0.5);
 Step(3.35,8,True,1);
 Step(0.5,8,True,1);
 Step(0.012,6,False,0.002);
 Step(0,8,False,0);
 Step(8,0,False,0);
 C:=TChart.Create; B:=TBGRABitmap.Create(400,200);
 try
  C.AddPixel(0,0,BGRA(0,0,0));
  C.AddPixel(8.2,3.3,BGRA(0,0,0));
  C.Draw(B,B.Width,B.Height);
  Require(Abs(C.XMaxF-8.2)<1e-4,'X axis ends at the data');
  Require(Abs(C.YMaxF-3.3)<1e-4,'Y axis ends at the data');
 finally B.Free; C.Free; end;
end;
// With fixed limits, data beyond them is cut at the plot border: nothing is
// painted outside, and a line leaving the plot keeps its slope instead of
// running along the border.
procedure CheckDataClipped;
 procedure Flood(AChart:TChart; ACanvas:TBGRABitmap);
 begin ACanvas.FillRect(0,0,ACanvas.Width,ACanvas.Height,BGRA(0,0,255)); end;
var C:TChart; B:TBGRABitmap; R:TRect; x,y,BlueOut,RedMaxX,ExitX:integer;
 P:TBGRAPixel; Line:ArrayOfTPointF;
begin
 C:=TChart.Create; B:=TBGRABitmap.Create(400,200);
 try
  C.XMinF:=0; C.XMaxF:=10; C.YMinF:=0; C.YMaxF:=10;
  C.PenColor:=BGRA(255,0,0); C.PenWidth:=1;
  SetLength(Line,2);
  Line[0].x:=0; Line[0].y:=5; Line[1].x:=10; Line[1].y:=30;
  C.AddPolyLine(Line,False,'');
  // The callback paints after the items: first the line alone, then the flood.
  C.Draw(B,B.Width,B.Height);
  RedMaxX:=-1;
  for y:=0 to B.Height-1 do for x:=0 to B.Width-1 do begin
   P:=B.GetPixel(x,y);
   if (P.red>150) and (P.blue<150) and (x>RedMaxX) then RedMaxX:=x;
  end;
  C.Draw(B,B.Width,B.Height,True,@Flood);
  R:=C.GetDrawingRect; // one pixel inside the axes
  BlueOut:=0;
  for y:=0 to B.Height-1 do for x:=0 to B.Width-1 do begin
   P:=B.GetPixel(x,y);
   if (P.blue>200) and (P.red<50) and
      ((x<R.Left-1) or (x>R.Right+1) or (y<R.Top-1) or (y>R.Bottom+1)) then Inc(BlueOut);
  end;
  Require(BlueOut=0,'data is clipped to the plot area');
  // The line reaches the top limit (y = 10) at x = 2.
  ExitX:=Round(C.DataToCanvasX(2));
  Require((RedMaxX>=0) and (RedMaxX<=ExitX+3),
   Format('clipped line keeps its slope (last pixel at %d, exits at %d)',[RedMaxX,ExitX]));
 finally B.Free; C.Free; end;
end;

// Samples beyond full scale must not draw the waveform below the X axis.
procedure CheckWaveClipped;
var T:TTrack; F:TWaveFormat; B:TBGRABitmap; D:TCustomDrawer;
 i,x,y,Run,AxisRow,Below,Count:integer; P:TBGRAPixel;
begin
 Count:=48000; T:=TTrack.Create('overs'); B:=TBGRABitmap.Create(800,300);
 try
  F:=Default(TWaveFormat); F.SampleRate:=48000; F.BitsPerSample:=16;
  F.Channels:=1; F.BlockAlign:=2; F.DataSize:=Count*2;
  T.SetFormat(F,Count);
  for i:=0 to Count-1 do T.Channels[0][i]:=1.5*Sin(2*Pi*200*i/48000);
  D:=TWaveDrawer.Create(T,B);
  try D.Start; D.WaitFor; D.RaiseIfFailed; finally D.Free; end;
  // The X axis is the lowest row with a long yellow run.
  AxisRow:=-1;
  for y:=0 to B.Height-1 do begin
   Run:=0;
   for x:=0 to B.Width-1 do begin
    P:=B.GetPixel(x,y);
    if (P.red>200) and (P.green>200) and (P.blue<100) then Inc(Run);
   end;
   if Run>400 then AxisRow:=y;
  end;
  Require(AxisRow>0,'waveform X axis found');
  Below:=0;
  for y:=AxisRow+1 to B.Height-1 do for x:=0 to B.Width-1 do begin
   P:=B.GetPixel(x,y);
   if (P.red>150) and (P.green<100) and (P.blue<100) then Inc(Below);
  end;
  Require(Below=0,'waveform stays above the X axis');
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
 CheckAxesOverData;
 CheckAxisRange;
 CheckDataClipped;
 CheckWaveClipped;
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
