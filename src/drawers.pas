{
  Description: Charts drawer routines.

  Copyright (C) 2025 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit Drawers;

{$mode objfpc}

interface

uses
  BaseGraphics, Classes, Common, FPImage, Graphics, SoundWav, Spectrum, SysUtils, Types;

type
  TVirtualScreens = array[0..3] of TBitmap;

  TScreenDrawer = class(TThread)
  private
    FOnStart: TThreadMethod;
    FOnStop: TThreadMethod;
    FOnWait: TThreadMethod;
    FScreens: TVirtualScreens;
    FScreenHeight: longint;
    FScreenWidth: longint;
    FTrack: TTrack;
    function GetScreen(AIndex: longint):TBitmap;
  public
    constructor Create(ATrack: TTrack);
    destructor Destroy; override;
    procedure Execute; override;
  public
    property OnStart: TThreadMethod read FOnStart write FOnStart;
    property OnStop: TThreadMethod read FOnStop write FOnStop;
    property OnWait: TThreadMethod read FOnWait write FOnWait;
    property Screens[AIndex: longint]: TBitmap read GetScreen;
    property ScreenWidth: longint read FScreenWidth write FScreenWidth;
    property ScreenHeight: longint read FScreenHeight write FScreenHeight;
  end;

  TCustomDrawer = class(TThread)
  private
    FTrack: TTrack;
    FScreen: TBitmap;
    FWorking: boolean;
    procedure DrawDefault; virtual; abstract;
    procedure Draw; virtual; abstract;
  public
    constructor Create(ATrack: TTrack; AScreen: TBitmap);
    destructor Destroy; override;
    procedure Execute; override;

    property Working: boolean read FWorking;
  end;

  TBlockDrawer = class(TCustomDrawer)
  private
    procedure DrawDefault; override;
    procedure Draw; override;
  end;

  TSpectrumDrawer = class(TCustomDrawer)
  private
    procedure DrawDefault; override;
    procedure Draw; override;
  end;

  TSpectrogramDrawer = class(TCustomDrawer)
  private
    procedure DrawDefault; override;
    procedure Draw; override;
  end;

  TWaveDrawer = class(TCustomDrawer)
  private
    procedure DrawDefault; override;
    procedure Draw; override;
  end;


var
  ScreenDrawer: TScreenDrawer = nil;

implementation

uses
  Math, DateUtils, SoundUtils;


function GetColor(AFactor: double): TColor;
const
  BaseColors: array[0..5] of TColor = (clBlack, clNavy, clPurple, clRed, clYellow, clWhite);
var
  FPBase: array[0..5] of TFPColor;
  Seg: longint;
  Fraction: Double;
  R1, G1, B1, R2, G2, B2: Word;
  Color3: TFPColor;
begin
  if AFactor < 0 then AFactor := 0;
  if AFactor > 1 then AFactor := 1;

  // Gamma correction per migliorare visibilità nelle basse ampiezze
  AFactor := Power(AFactor, 0.95);

  // Prepara i colori base (una sola volta sarebbe ancora meglio in una variabile globale)
  FPBase[0] := TColorToFPColor(BaseColors[0]);
  FPBase[1] := TColorToFPColor(BaseColors[1]);
  FPBase[2] := TColorToFPColor(BaseColors[2]);
  FPBase[3] := TColorToFPColor(BaseColors[3]);
  FPBase[4] := TColorToFPColor(BaseColors[4]);
  FPBase[5] := TColorToFPColor(BaseColors[5]);

  // Determina in quale segmento di colore ci troviamo
  Seg  := Floor(AFactor * 5);
  if Seg > 4 then Seg := 4;

  Fraction := Frac(AFactor * 5); // posizione fra i due colori

  // Interpolazione RGB lineare
  R1 := FPBase[Seg].Red;   R2 := FPBase[Seg + 1].Red;
  G1 := FPBase[Seg].Green; G2 := FPBase[Seg + 1].Green;
  B1 := FPBase[Seg].Blue;  B2 := FPBase[Seg + 1].Blue;

  Color3.Red   := Round(R1 + (R2 - R1) * Fraction);
  Color3.Green := Round(G1 + (G2 - G1) * Fraction);
  Color3.Blue  := Round(B1 + (B2 - B1) * Fraction);
  Color3.Alpha := $FFFF;

  result := FPColorToTColor(Color3);
end;


// TScreenDrawer

constructor TScreenDrawer.Create(ATrack: TTrack);
var
  i: longint;
begin
  FOnStart := nil;
  FOnStop  := nil;
  FOnWait  := nil;
  FTrack   := ATrack;

  FScreenWidth  := 0;
  FScreenHeight := 0;
  for i := Low(FScreens) to High(FScreens) do
  begin
    FScreens[i] := TBitmap.create;
  end;
  FreeOnTerminate := True;
  inherited create(true);
end;

destructor TScreenDrawer.Destroy;
var
  i: longint;
begin
  for i := Low(FScreens) to High(FScreens) do
    FreeAndNil(FScreens[i]);
  inherited Destroy;
end;

procedure TScreenDrawer.Execute;
var
  StartTime: TDateTime;
  BlockDrawer: TBlockDrawer;
  SpectrumDrawer: TSpectrumDrawer;
  SpectrogramDrawer: TSpectrogramDrawer;
  WaveDrawer: TWaveDrawer;
begin
  if assigned(FOnStart) then
    synchronize(FOnStart);

  StartTime := now;
  if Assigned(FOnWait) then
    while (FScreenWidth = 0) or (FScreenHeight = 0) do
    begin
      if MilliSecondsBetween(Now, StartTime) > 250 then
      begin
        Queue(FOnWait);
        StartTime := Now;
      end;
    end;

  if (FScreenWidth > 0) and (FScreenHeight > 0) then
  begin
    FScreens[0].SetSize(FScreenWidth, FScreenHeight);
    FScreens[1].SetSize(FScreenWidth, FScreenHeight);
    FScreens[2].SetSize(FScreenWidth, FScreenHeight);
    FScreens[3].SetSize(FScreenWidth, FScreenHeight);

    BlockDrawer := TBlockDrawer.Create(FTrack, FScreens[0]);
    while BlockDrawer.Working do Sleep(10);
    BlockDrawer.Destroy;

  //SpectrumDrawer := TSpectrumDrawer.Create(FTrack, FScreens[1]);
  //while SpectrumDrawer.Working do Sleep(10);
  //SpectrumDrawer.Destroy;

    SpectrogramDrawer := TSpectrogramDrawer.Create(FTrack, FScreens[2]);
    while SpectrogramDrawer.Working do Sleep(10);
    SpectrogramDrawer.Destroy;

    WaveDrawer := TWaveDrawer.Create(FTrack, FScreens[3]);
    while WaveDrawer.Working do Sleep(10);
    WaveDrawer.Destroy;
  end;

  if Assigned(FOnStop) then
    Synchronize(FOnStop);
end;

function TScreenDrawer.GetScreen(AIndex: longint):TBitmap;
begin
  result := FScreens[AIndex];
end;

// TDrawer

constructor TCustomDrawer.Create(ATrack: TTrack; AScreen: TBitmap);
begin
  FTrack   := ATrack;
  FScreen  := AScreen;
  FreeOnTerminate := True;
  inherited Create(False);
end;

destructor TCustomDrawer.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomDrawer.Execute;
begin
  FWorking := True;
  case Assigned(FTrack) of
    True : Draw;
    False: DrawDefault;
  end;
  FWorking := False;
end;

// TBlockDrawer

procedure TBlockDrawer.DrawDefault;
var
  Points: array of TPointf = nil;
  Chart: tchart;
begin
  // create and configure the chart
  Chart := tchart.create;
  Chart.legendenabled := false;
  Chart.title := '';
  Chart.xaxislabel := 'blocknum';
  Chart.yaxislabel := 'audio [dB]';
  Chart.xgridlinewidth := 0;
  Chart.ygridlinewidth := 0;
  Chart.scale := 1.0;
  Chart.backgroundcolor := clblack;
  Chart.titlefontcolor := clwhite;
  Chart.xaxisfontcolor := clwhite;
  Chart.yaxisfontcolor := clwhite;
  Chart.xaxislinecolor := clwhite;
  Chart.yaxislinecolor := clwhite;
  Chart.textureheight := 1;
  Chart.texturewidth := 1;
  Chart.texturebackgroundcolor := clblack;
  Chart.pencolor := clblack;

  setlength(Points, 2);
  Points[0].x := 0;
  Points[0].y := 0;
  Points[1].x := 30;
  Points[1].y := 96;
  Chart.addpolygon(Points, '');
  setlength(Points, 0);
  // draw Chart on screen
  Chart.draw(FScreen.Canvas, FScreen.Width, FScreen.Height, True);
  Chart.Destroy;
end;

procedure TBlockDrawer.Draw;
var
  i, j: longint;
  Rms2, Peak: TDouble;
  Points: array of TPointF = nil;
  Chart: TChart;
  OffSet: TDouble;
begin
  // create and configure the chart
  Chart := TChart.Create;
  Chart.LegendEnabled := false;
  Chart.Title := '';
  Chart.XAxisLabel := 'blocknum';
  Chart.YAxisLabel := 'audio [dB]';
  Chart.XGridLineWidth := 0;
  Chart.YGridLineWidth := 0;
  Chart.Scale := 1.0;
  Chart.BackgroundColor := clBlack;
  Chart.TitleFontColor := clWhite;
  Chart.XAxisFontColor := clWhite;
  Chart.YAxisFontColor := clWhite;
  Chart.XAxisLineColor := clWhite;
  Chart.YAxisLineColor := clWhite;
  Chart.TextureHeight := 1;
  Chart.TextureWidth := 1;
  Chart.TextureBackgroundColor := clBlack;
  Chart.PenColor := clBlack;
  Chart.XMinF := 0;
  Chart.YMinF := 0;
  Chart.YCount := 8;
  Chart.YDeltaF := 0.75*FTrack.Bitspersample;

  OffSet := 6 * FTrack.BitsPerSample;

  // loop through each block
  SetLength(Points, 4);
  for i := 0 to FTrack.DRMeter.BlockCount -1 do
  begin
    Rms2 := 0;
    // calculate average rms across channels
    for j := 0 to FTrack.ChannelCount -1 do
    begin
      Rms2 := Rms2 + FTrack.DRMeter.Rms2(j, i);
    end;
    Rms2 := Rms2 / FTrack.ChannelCount;

    // draw yellow block for rms level
    Points[0].x := (i + 1) - 0.35;
    Points[0].y := 0;
    Points[1].x := (i + 1) - 0.35;
    Points[1].y := OffSet + Decibel(Sqrt(Rms2));
    Points[2].x := (i + 1) + 0.35;
    Points[2].y := OffSet + Decibel(Sqrt(Rms2));
    Points[3].x := (i + 1) + 0.35;
    Points[3].y := 0;

    Chart.PenColor := clBlack;
    Chart.TextureColor := clYellow;
    Chart.AddPolygon(Points, '');

    Peak := 0;
    // calculate average Peak across channels
    for j := 0 to FTrack.ChannelCount - 1 do
    begin
      Peak := Peak + FTrack.DRMeter.Peak(j, i);
    end;
    Peak := Peak / FTrack.ChannelCount;

    // draw red block from rms to Peak
    Points[0].x := (i + 1) - 0.35;
    Points[0].y := OffSet + Decibel(Sqrt(Rms2));
    Points[1].x := (i + 1) - 0.35;
    Points[1].y := OffSet + Decibel(Peak);
    Points[2].x := (i + 1) + 0.35;
    Points[2].y := OffSet + Decibel(Peak);
    Points[3].x := (i + 1) + 0.35;
    Points[3].y := OffSet + Decibel(Sqrt(Rms2));

    Chart.PenColor := clBlack;
    Chart.TextureColor := clRed;
    Chart.AddPolygon(Points, '');
  end;
  SetLength(Points, 0);
  // draw Chart on screen
  Chart.Draw(FScreen.Canvas, FScreen.Width, FScreen.Height, True);
  Chart.Destroy;
end;

// TSpectrumDrawer

procedure TSpectrumDrawer.DrawDefault;
var
  Points: array of TPointF = nil;
  Chart: tchart;
begin
  // create and configure the chart
  Chart := tchart.create;
  Chart.legendenabled := false;
  Chart.title := '';
  Chart.xaxislabel := 'freq [Hz]';
  Chart.yaxislabel := 'audio [dB]';
  Chart.xgridlinewidth := 0;
  Chart.ygridlinewidth := 0;
  Chart.scale := 1.0;
  Chart.backgroundcolor := clblack;
  Chart.titlefontcolor := clwhite;
  Chart.xaxisfontcolor := clwhite;
  Chart.yaxisfontcolor := clwhite;
  Chart.xaxislinecolor := clwhite;
  Chart.yaxislinecolor := clwhite;
  Chart.textureheight := 1;
  Chart.texturewidth := 1;
  Chart.texturebackgroundcolor := clblack;
  Chart.pencolor := clblack;

  setlength(Points, 2);
  Points[0].x := 0;
  Points[0].y := 0;
  Points[1].x := 44000;
  Points[1].y := 96;
  Chart.addpolygon(Points, '');
  setlength(Points, 0);
  // draw Chart on screen
  Chart.draw(FScreen.canvas, FScreen.width, FScreen.height, true);
  Chart.free;
end;

procedure TSpectrumDrawer.Draw;
var
  Chart: TChart;
  i, j, k: longint;
  WindowSize: longint;
  WindowCount: longint;
  Points: array of TPointF = nil;
  index: longint;
  x, y: single;
  Factor: single;
  OffSet: TDouble;
begin
  // create and configure the chart
  Chart := TChart.Create;
  Chart.LegendEnabled := False;
  Chart.Title := '';
  Chart.XAxisLabel := 'freq [Hz]';
  Chart.YAxisLabel := 'audio [dB]';
  Chart.XGridlineWidth := 0;
  Chart.YGridlineWidth := 0;
  Chart.Scale := 1.0;
  Chart.BackgroundColor := clBlack;
  Chart.XAxisFontColor := clWhite;
  Chart.YAxisFontColor := clWhite;
  Chart.XAxisLineColor := clWhite;
  Chart.YAxisLineColor := clWhite;
  Chart.XMinF := 0;
  Chart.TextureHeight := 1;
  Chart.TextureWidth := 1;
  Chart.TextureBackgroundColor := clBlack;
  Chart.TextureColor := clYellow;
  Chart.PenColor := clYellow;

  // initialize frequency bin array (half of fft size)
  WindowSize  := DEFAULTWINDOWSIZE div 2;
  WindowCount := FTrack.Spectrums.WindowCount;
  Factor      := 0.5 * ftrack.Samplerate / (WindowSize -1);

  OffSet := 6 * FTrack.BitsPerSample;

  setlength(Points, 4);
  for i := 1 to WindowSize -1 do
  begin
    x := i * Factor;
    y := 0;

    for j := 0 to WindowCount -1 do
    begin
      index := j * WindowSize + i;
      for k := 0 to ftrack.Channelcount -1 do
      begin
        y := max(y, OffSet + Decibel(FTrack.Spectrums.Channels[k, index]));
      end;
    end;

    Points[0].x := x -0.25 * Factor;
    Points[0].y := 0;
    Points[1].x := x -0.25 * Factor;
    Points[1].y := y;
    Points[2].x := x +0.25 * Factor;
    Points[2].y := y;
    Points[3].x := x +0.25 * Factor;
    Points[3].y := 0;
    Chart.addpolygon(Points, '');
  end;
  setlength(Points, 0);
  // Draw Chart on screen
  Chart.Draw(FScreen.canvas, FScreen.Width, FScreen.Height, True);
  Chart.Destroy;
end;

// TSpectrogramDrawer

procedure TSpectrogramDrawer.DrawDefault;
var
  Points: array of TPointF = nil;
  Chart: TChart;
begin
  // create and configure the Chart
  Chart := TChart.create;
  Chart.legendenabled := false;
  Chart.title := '';
  Chart.xaxislabel := 'freq [Hz]';
  Chart.yaxislabel := 'time [s]';
  Chart.xgridlinewidth := 0;
  Chart.ygridlinewidth := 0;
  Chart.scale := 1.0;
  Chart.backgroundcolor := clblack;
  Chart.titlefontcolor := clwhite;
  Chart.xaxisfontcolor := clwhite;
  Chart.yaxisfontcolor := clwhite;
  Chart.xaxislinecolor := clwhite;
  Chart.yaxislinecolor := clwhite;
  Chart.textureheight := 1;
  Chart.texturewidth := 1;
  Chart.texturebackgroundcolor := clblack;
  Chart.pencolor := clblack;

  setlength(Points, 2);
  Points[0].x := 0;
  Points[0].y := 0;
  Points[1].x := 22050;
  Points[1].y := 96;
  Chart.addpolygon(Points, '');
  setlength(Points, 0);
  // draw Chart on screen
  Chart.draw(FScreen.Canvas, FScreen.Width, FScreen.Height, True);
  Chart.Destroy;
end;

procedure TSpectrogramDrawer.Draw;
var
  Chart: TChart;
  TimeIndex, FreqIndex: longint;
  X, Y, ch: longint;
  Amp: double;
  WindowCount: longint;
  OutBins: LongInt;
  Bit: TBitmap;
  maxDB, XFactor, YFactor: TDouble;
begin
  // create chart
  Chart := TChart.Create;
  Chart.LegendEnabled := False;
  Chart.Title := '';
  Chart.XAxisLabel := 'freq [Hz]';
  Chart.YAxisLabel := 'time [s]';
  Chart.XGridLineWidth := 0;
  Chart.YGridLineWidth := 0;
  Chart.Scale := 1.0;
  Chart.BackgroundColor := clBlack;
  Chart.XAxisFontColor := clWhite;
  Chart.YAxisFontColor := clWhite;
  Chart.XAxisLineColor := clWhite;
  Chart.YAxisLineColor := clWhite;
  Chart.XMinF   := 0;
  Chart.YMinF   := 0;
  Chart.AddPixel(FTrack.Samplerate div 2, FTrack.Duration, clblack);
  Chart.Draw(FScreen.Canvas, FScreen.Width, FScreen.Height, True);

  Bit := TBitmap.create;
  Bit.SetSize(
    Trunc(Chart.GetDrawingRect.Width *((FTrack.Samplerate div 2) / (Chart.XMaxF - Chart.XMinF))),
    Trunc(Chart.GetDrawingRect.Height*((FTrack.Duration        ) / (Chart.YMaxf - Chart.YMinF))));

  Bit.Canvas.Brush.Color := clNavy;
  Bit.Canvas.FillRect(0, 0, Bit.Width -1, Bit.Height -1);

  // set fft analysis window size (half of total window size)
  WindowCount := FTrack.Spectrums.WindowCount;
  OutBins     := FTrack.Spectrums.OutBins;

  XFactor := (OutBins      - 1) / (Bit.Width  - 1);
  YFactor := (WindowCount  - 1) / (Bit.Height - 1);

  maxDB := 6 * FTrack.BitsPerSample;

  // loop over output bitmap pixels
  for Y := 0 to Bit.Height -1 do
  begin
    TimeIndex  := Trunc(Y * YFactor);

    for X := 0 to Bit.Width -1 do
    begin
      FreqIndex  :=  Trunc(X * XFactor);

      // compute fft bin index for this pixel
      Amp := 0;
      for ch := 0 to FTrack.ChannelCount -1 do
      begin
        Amp := Max(Amp, FTrack.Spectrums.Channels[ch, TimeIndex * OutBins + FreqIndex]);
      end;
      // map amplitude to color and set pixel
      Bit.Canvas.Pixels[X, Bit.Height - 1 - Y] := GetColor((Decibel(Amp) + maxDB) / maxDB);
    end;
  end;
  FScreen.Canvas.Draw(
    Chart.GetDrawingRect.Left,
    Chart.GetDrawingRect.Top + (Chart.GetDrawingRect.Height - Bit.Height), Bit);
  Bit.Destroy;
  Chart.Destroy;
end;

// TWaveDrawer

procedure TWaveDrawer.DrawDefault;
var
  Points: array of TPointF = nil;
  Chart: TChart;
begin
  // create and configure the chart
  Chart := TChart.Create;
  Chart.legendenabled := false;
  Chart.title := '';
  Chart.xaxislabel := 'time [s]';
  Chart.yaxislabel := '';
  Chart.xgridlinewidth := 0;
  Chart.ygridlinewidth := 0;
  Chart.scale := 1.0;
  Chart.backgroundcolor := clblack;
  Chart.titlefontcolor := clwhite;
  Chart.xaxisfontcolor := clwhite;
  Chart.yaxisfontcolor := clwhite;
  Chart.xaxislinecolor := clwhite;
  Chart.yaxislinecolor := clwhite;
  Chart.textureheight := 1;
  Chart.texturewidth := 1;
  Chart.texturebackgroundcolor := clblack;
  Chart.pencolor := clblack;

  Chart.ymaxf := +1.0;
  Chart.yminf := -1.0;
  Chart.xminf := 0;
  Chart.xmaxf := 100;
  Chart.ycount  := 8;
  Chart.ydeltaf := 0.25;

  SetLength(Points, 2);
  Points[0].x := 0;
  Points[0].y := -1;
  Points[1].x := 100;
  Points[1].y := 1;
  Chart.addpolygon(Points, '');
  SetLength(Points, 0);
  // draw Chart on screen
  Chart.draw(FScreen.Canvas, FScreen.Width, FScreen.Height, True);
  Chart.Destroy;
end;

procedure TWaveDrawer.Draw;
var
  ch, i, x, SampleIndex: longint;
  WindowxSize, WindowySize: longint;
  WindowxCount, WindowyCount: longint;
  zMax, zMin: double;
  P1, P2: tpointf;
  Bit: array of TBitmap = nil;
  Chart: TChart;
begin
  // create a bitmap for each audio channel
  SetLength(Bit, FTrack.ChannelCount);
  for ch := Low(Bit) to High(Bit) do
    Bit[ch] := TBitmap.create;

  WindowxCount := FScreen.Width;       // horizontal resolution (pixels)
  WindowyCount := FTrack.ChannelCount; // one row per channel

  // loop through each channel
  for ch := Low(Bit) to High(Bit) do
  begin
    Chart := TChart.Create;
    Chart.LegendEnabled := False;
    Chart.Title := '';
    Chart.XAxisLabel := 'time [s]';
    Chart.YAxisLabel := '1';
    Chart.Scale := 1.0;
    Chart.BackgroundColor := clblack;
    Chart.XAxisFontColor := clwhite;
    Chart.YAxisFontColor := clwhite;
    Chart.XAxisLineColor := clwhite;
    Chart.YAxisLineColor := clwhite;
    Chart.XGridLineWidth := 0;
    Chart.YGridLineWidth := 0;
    Chart.YCount  := 8;
    Chart.YDeltaF := 0.25;
    Chart.PenWidth := 1;
    Chart.PenColor := clred;
    Chart.TextureColor := clred;

    // calculate number of samples per horizontal pixel
    WindowxSize := FTrack.Samplecount div WindowxCount;
    // calculate vertical size per channel section
    WindowySize := FScreen.Height div WindowyCount;

    // prepare per-channel bitmap
    Bit[ch].setsize(FScreen.width, WindowySize);
    // loop through horizontal pixels (time segments)
    for x := 0 to WindowxCount - 1 do
    begin
      zMin :=  infinity;
      zMax := -infinity;

      // find min and max sample values in this time segment
      for i := 0 to WindowxSize -1 do
      begin
        SampleIndex := x * WindowxSize + i;

        zMin := min(zMin, FTrack.Channels[ch, SampleIndex]);
        zMax := max(zMax, FTrack.Channels[ch, SampleIndex]);
      end;

      // create a vertical line for waveform range at this segment
      P1.x := x / (WindowxCount -1) * FTrack.Duration;
      P1.y := zMin;
      P2.x := x / (WindowxCount -1) * FTrack.Duration;
      P2.y := zMax;

      Chart.AddPolyline([P1, P2], False, '');
      // dotick;
    end;
    // set visible bounds for Chart
    Chart.YMaxF := +1.0;
    Chart.YMinF := -1.0;
    Chart.XMinF := 0;
    Chart.XMaxF := Max(1, FTrack.Duration);
    // draw Chart on bitmap
    Chart.Draw(Bit[ch].Canvas, Bit[ch].Width, Bit[ch].Height);
    Chart.Destroy;
  end;

  // composite each channel's bitmap into the final output
  for ch := Low(Bit) to High(Bit) do
  begin
    FScreen.Canvas.draw(0, trunc(ch * (FScreen.Height / FTrack.ChannelCount)), Bit[ch]);
    Bit[ch].Destroy;
  end;
  SetLength(Bit, 0);
end;

end.

