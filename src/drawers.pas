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
  BGRABitmap, BaseGraphics, BGRABitmapTypes, Classes, Common, FPImage, Graphics, SoundWav, Spectrum, SysUtils, Types;

type
  TVirtualScreens = array[0..3] of TBGRABitmap;

  TScreenDrawer = class(TThread)
  private
    FOnStart: TThreadMethod;
    FOnStop: TThreadMethod;
    FScreens: TVirtualScreens;
    FScreenWidth: longint;
    FScreenHeight: longint;
    FTrack: TTrack;
    function GetScreen(AIndex: longint):TBGRABitmap;
  public
    constructor Create(ATrack: TTrack; AWidth, AHeight: longint);
    destructor Destroy; override;
    procedure Execute; override;
  public
    property OnStart: TThreadMethod read FOnStart write FOnStart;
    property OnStop: TThreadMethod read FOnStop write FOnStop;
    property Screens[AIndex: longint]: TBGRABitmap read GetScreen;
    property Track: TTrack read FTrack;
  end;

  TCustomDrawer = class(TThread)
  private
    FTrack: TTrack;
    FScreen: TBGRABitmap;
    function NewDefaultChart: TChart;
    procedure Draw; virtual; abstract;
  public
    constructor Create(ATrack: TTrack; AScreen: TBGRABitmap);
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TBlockDrawer = class(TCustomDrawer)
  private
    procedure Draw; override;
  end;

  TSpectrumDrawer = class(TCustomDrawer)
  private
    procedure Draw; override;
  end;

  TSpectrogramDrawer = class(TCustomDrawer)
  private
    procedure Draw; override;
  end;

  TWaveDrawer = class(TCustomDrawer)
  private
    procedure Draw; override;
  end;

  procedure DrawDefaultBlockChart      (var ABitmap: TBGRABitmap);
  procedure DrawDefaultSpectrumChart   (var ABitmap: TBGRABitmap);
  procedure DrawDefaultSpectrogramChart(var ABitmap: TBGRABitmap);
  procedure DrawDefaultWaveChart       (var ABitmap: TBGRABitmap);

const
  clrBlack  : TBGRAPixel = (red: $00; green: $00; blue: $00; alpha: 255);
  clrBlue   : TBGRAPixel = (red: $20; green: $4A; blue: $87; alpha: 255);
  clrPurple : TBGRAPixel = (red: $75; green: $50; blue: $7B; alpha: 255);
  clrRed    : TBGRAPixel = (red: $EF; green: $29; blue: $29; alpha: 255);
  clrYellow : TBGRAPixel = (red: $FC; green: $E9; blue: $4F; alpha: 255);
  clrWhite  : TBGRAPixel = (red: $FF; green: $FF; blue: $FF; alpha: 255);
  clrGreen  : TBGRAPixel = (red: $8A; green: $E2; blue: $34; alpha: 255);

var
  ScreenDrawer: TScreenDrawer = nil;


implementation

uses
  Math, DateUtils, SoundUtils;

function GetColor(AFactor: double): TBGRAPixel;
const
  BaseColors: array[0..5] of TBGRAPixel =
    ((red: $00; green: $00; blue: $00; alpha: 255),  // $000000
     (red: $20; green: $4A; blue: $87; alpha: 255),  // $874A20
     (red: $75; green: $50; blue: $7B; alpha: 255),  // $7B5075
     (red: $EF; green: $29; blue: $29; alpha: 255),  // $2929EF
     (red: $FC; green: $E9; blue: $4F; alpha: 255),  // $4FE9FC
     (red: $FF; green: $FF; blue: $FF; alpha: 255)); // $FFFFFF
var
  Seg: Integer;
  Fraction: Double;
  C1, C2: TBGRAPixel;
begin
  if AFactor < 0 then AFactor := 0;
  if AFactor > 1 then AFactor := 1;

  AFactor := Power(AFactor, 0.95);

  Seg := Floor(AFactor * 5);
  if Seg > 4 then Seg := 4;

  Fraction := Frac(AFactor * 5);

  C1 := BaseColors[Seg];
  C2 := BaseColors[Seg + 1];

  Result.red   := C1.red   + Round((C2.red   - C1.red)   * Fraction);
  Result.green := C1.green + Round((C2.green - C1.green) * Fraction);
  Result.blue  := C1.blue  + Round((C2.blue  - C1.blue)  * Fraction);
  Result.alpha := 255;
end;

function NewDefaultChart: TChart;
begin
  result := TChart.Create;
  result.LegendEnabled := False;

  result.TitleFontHeight := 12;
  result.XAxisFontHeight := 12;
  result.YAxisFontHeight := 12;

  result.XGridLineWidth := 0;
  result.YGridLineWidth := 0;
  result.XAxisLineWidth := 1;
  result.YAxisLineWidth := 1;

  result.Scale := 1.0;

  result.BackgroundColor := clBlack;
  result.TitleFontColor  := clGray;
  result.XAxisFontColor  := clGray;
  result.YAxisFontColor  := clGray;
  result.XAxisLineColor  := clYellow;
  result.YAxisLineColor  := clYellow;

  result.TextureHeight := 1;
  result.TextureWidth  := 1;
  result.TextureBackgroundColor := clBlack;

  result.PenColor := clBlack;

  result.XMinF  := 0;
  result.YMinF  := 0;
  result.XCount := 6;
  result.YCount := 4;

  result.XAxisLabelLen := 0;
  result.YAxisLabelLen := result.GetXAxisLabelSize('Amplitude [dB]').Width;
end;

// Draw default chart

procedure DrawDefaultBlockChart(var ABitmap: TBGRABitmap);
var
  Points: array of TPointF = nil;
  Chart: TChart;
begin
  // create and configure the chart
  Chart := NewDefaultChart;
  Chart.Title      := 'Energy & peaks (1s blocks)';
  Chart.XAxisLabel := 'Block num';
  Chart.YAxisLabel := 'Amplitude [dB]';

  SetLength(Points, 2);
  Points[0].x := 0;
  Points[0].y := 0;
  Points[1].x := 30;
  Points[1].y := 96;
  Chart.AddPolygon(Points, '');
  // draw chart on screen
  Chart.Draw(ABitmap, ABitmap.Width, ABitmap.Height, True);
  Chart.Destroy;
end;

procedure DrawDefaultSpectrumChart(var ABitmap: TBGRABitmap);
var
  Points: array of TPointF = nil;
  Chart: TChart;
begin
  // create and configure the chart
  Chart := NewDefaultChart;
  Chart.Title      := 'Frequency spectrum';
  Chart.XAxisLabel := 'Freq [Hz]';
  Chart.YAxisLabel := 'Amplitude [dB]';

  SetLength(Points, 2);
  Points[0].x := 0;
  Points[0].y := 0;
  Points[1].x := 44100 div 2;
  Points[1].y := 96;
  Chart.AddPolygon(Points, '');
  // draw Chart on screen
  Chart.Draw(ABitmap, ABitmap.Width, ABitmap.Height, True);
  Chart.Destroy;
end;

procedure DrawDefaultSpectrogramChart(var ABitmap: TBGRABitmap);
var
  Points: array of TPointF = nil;
  Chart: TChart;
begin
  // create and configure the chart
  Chart := NewDefaultChart;
  Chart.Title := 'Spectrogram';
  Chart.XAxisLabel := 'Freq [Hz]';
  Chart.YAxisLabel := 'Time [s]';

  SetLength(Points, 2);
  Points[0].x := 0;
  Points[0].y := 0;
  Points[1].x := 22050;
  Points[1].y := 96;
  Chart.AddPolygon(Points, '');
  // draw chart on screen
  Chart.Draw(ABitmap, ABitmap.Width, ABitmap.Height, True);
  Chart.Destroy;
end;

procedure DrawDefaultWaveChart(var ABitmap: TBGRABitmap);
var
  Points: array of TPointF = nil;
  Chart: TChart;
begin
  // create and configure the chart
  Chart := NewDefaultChart;
  Chart.LegendEnabled := True;
  Chart.Title := 'Waveform (Mono)';
  Chart.XAxisLabel := 'Time [s]';
  Chart.YAxisLabel := 'Amplitude';

  Chart.YMaxF   := +1.0;
  Chart.YMinF   := -1.0;
  Chart.XMinF   := 0;
  Chart.XMaxF   := 100;
  Chart.YCount  := 4;
  Chart.YDeltaF := 0.5;

  SetLength(Points, 2);
  Points[0].x := 0;
  Points[0].y := -1;
  Points[1].x := 100;
  Points[1].y := 1;
  Chart.AddPolygon(Points, '');
  SetLength(Points, 0);
  // draw Chart on screen
  Chart.Draw(ABitmap, ABitmap.Width, ABitmap.Height, True);
  Chart.Destroy;
end;

// TScreenDrawer

constructor TScreenDrawer.Create(ATrack: TTrack; AWidth, AHeight: longint);
var
  i: longint;
begin
  FOnStart := nil;
  FOnStop  := nil;
  FTrack   := ATrack;

  FScreenWidth  := AWidth;
  FScreenHeight := AHeight;
  for i := Low(FScreens) to High(FScreens) do
    FScreens[i] := TBGRABitmap.Create;
  FreeOnTerminate := True;
  inherited Create(True);
end;

destructor TScreenDrawer.Destroy;
var
  i: longint;
begin
  for i := Low(FScreens) to High(FScreens) do
  begin
    FreeAndNil(FScreens[i]);
  end;
  inherited Destroy;
end;

procedure TScreenDrawer.Execute;
var
  BlockDrawer: TBlockDrawer;
  SpectrumDrawer: TSpectrumDrawer;
  SpectrogramDrawer: TSpectrogramDrawer;
  WaveDrawer: TWaveDrawer;
  ChartCount: longint;
  WavesCount: longint;
  BaseHeight: longint;
begin
  if Assigned(FOnStart) then
    Synchronize(FOnStart);

  if (FScreenWidth  > 0) and (FScreenHeight > 0) then
  begin
    if Assigned(FTrack) then
    begin
      ChartCount := 3;
      WavesCount := 1 + Max(0, FTrack.ChannelCount - 1);
      BaseHeight := FScreenHeight div (ChartCount + WavesCount);

      FScreens[0].SetSize(FScreenWidth, BaseHeight);
      FScreens[1].SetSize(FScreenWidth, BaseHeight * WavesCount);
      FScreens[2].SetSize(FScreenWidth, BaseHeight);
      FScreens[3].SetSize(FScreenWidth, BaseHeight);

      BlockDrawer := TBlockDrawer.Create(FTrack, FScreens[0]);
      BlockDrawer.Start;
      BlockDrawer.WaitFor;
      BlockDrawer.Destroy;

      WaveDrawer := TWaveDrawer.Create(FTrack, FScreens[1]);
      WaveDrawer.Start;
      WaveDrawer.WaitFor;
      WaveDrawer.Destroy;

      SpectrumDrawer := TSpectrumDrawer.Create(FTrack, FScreens[2]);
      SpectrumDrawer.Start;
      SpectrumDrawer.WaitFor;
      SpectrumDrawer.Destroy;

      SpectrogramDrawer := TSpectrogramDrawer.Create(FTrack, FScreens[3]);
      SpectrogramDrawer.Start;
      SpectrogramDrawer.WaitFor;
      SpectrogramDrawer.Destroy;
    end else
    begin
      FScreens[0].SetSize(FScreenWidth, FScreenHeight div 4);
      FScreens[1].SetSize(FScreenWidth, FScreenHeight div 4);
      FScreens[2].SetSize(FScreenWidth, FScreenHeight div 4);
      FScreens[3].SetSize(FScreenWidth, FScreenHeight div 4);

      DrawDefaultBlockChart      (FScreens[0]);
      DrawDefaultWaveChart       (FScreens[1]);
      DrawDefaultSpectrumChart   (FScreens[2]);
      DrawDefaultSpectrogramChart(FScreens[3]);
    end;
  end;

  if Assigned(FOnStop) then
    Synchronize(FOnStop);
end;

function TScreenDrawer.GetScreen(AIndex: longint):TBGRABitmap;
begin
  result := FScreens[AIndex];
end;

// TDrawer

constructor TCustomDrawer.Create(ATrack: TTrack; AScreen: TBGRABitmap);
begin
  FTrack  := ATrack;
  FScreen := AScreen;
  FreeOnTerminate := False;
  inherited Create(False);
end;

destructor TCustomDrawer.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomDrawer.Execute;
begin
  if Assigned(FTrack) then Draw;
end;

function TCustomDrawer.NewDefaultChart: TChart;
begin
  result := TChart.Create;
  result.LegendEnabled := False;

  result.TitleFontHeight := 12;
  result.XAxisFontHeight := 12;
  result.YAxisFontHeight := 12;

  result.XGridLineWidth := 0;
  result.YGridLineWidth := 0;
  result.XAxisLineWidth := 1;
  result.YAxisLineWidth := 1;

  result.Scale := 1.0;

  result.BackgroundColor := clBlack;
  result.TitleFontColor  := clGray;
  result.XAxisFontColor  := clGray;
  result.YAxisFontColor  := clGray;
  result.XAxisLineColor  := clYellow;
  result.YAxisLineColor  := clYellow;

  result.TextureHeight := 1;
  result.TextureWidth  := 1;
  result.TextureBackgroundColor := clBlack;

  result.PenColor := clBlack;

  result.XMinF  := 0;
  result.YMinF  := 0;
  result.XCount := 6;
  result.YCount := 4;

  result.XAxisLabelLen := 0;
  result.YAxisLabelLen := result.GetXAxisLabelSize('Amplitude [dB]').Width;
end;

// TBlockDrawer

procedure TBlockDrawer.Draw;
var
  i, j: longint;
  Rms2, Peak: TDouble;
  Points: array of TPointF = nil;
  Chart: TChart;
  OffSet: TDouble;
begin
  if (FTrack.ChannelCount = 0) then Exit;
  if (FTrack.Samplecount  = 0) then Exit;
  // create and configure the chart
  Chart := NewDefaultChart;
  Chart.Title      := 'Energy & peaks (1s blocks)';
  Chart.XAxisLabel := 'Block num';
  Chart.YAxisLabel := 'Amplitude [dB]';
  Chart.TitleFontColor := clrwhite;
  Chart.XAxisFontColor := clrWhite;
  Chart.YAxisFontColor := clrWhite;

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
    Chart.TextureColor := clrYellow;
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
    Chart.TextureColor := clrRed;
    Chart.AddPolygon(Points, '');
  end;
  // draw Chart on screen
  Chart.Draw(FScreen, FScreen.Width, FScreen.Height, True);
  Chart.Destroy;
end;

// TSpectrumDrawer

procedure TSpectrumDrawer.Draw;
var
  Chart: TChart;
  ch, i, j: longint;
  WindowCount: longint;
  OutBins: longint;
  Points: array of TPointF = nil;
  index: longint;
  FreqIndex, Amp, Peak: TDouble;
  Factor: single;
  MaxDB: TDouble;
begin
  if (FTrack.ChannelCount = 0) then Exit;
  if (FTrack.Samplecount  = 0) then Exit;
  // create and configure the chart
  Chart := NewDefaultChart;
  Chart.Title      := 'Frequency spectrum';
  Chart.XAxisLabel := 'Freq [Hz]';
  Chart.YAxisLabel := 'Amplitude [dB]';
  Chart.TitleFontColor := clrwhite;
  Chart.XAxisFontColor := clrWhite;
  Chart.YAxisFontColor := clrWhite;

  WindowCount := FTrack.Spectrums.WindowCount;
  OutBins     := FTrack.Spectrums.OutBins;
  Factor      := (0.5 * FTrack.Samplerate) / (OutBins - 1);
  MaxDB       := 6 * FTrack.BitsPerSample;

  SetLength(Points, 4);
  for i := 1 to OutBins - 1 do
  begin
    FreqIndex := i * Factor;

    Amp  := 0;
    Peak := 0;
    for j := 0 to WindowCount - 1 do
    begin
      Index := j * OutBins + i;
      for ch := 0 to FTrack.ChannelCount - 1 do
      begin
        Amp  := Amp + Sqr(FTrack.Spectrums.Channels[ch, Index]);
        Peak := Max(Peak, FTrack.Spectrums.Channels[ch, Index]);
      end;
    end;
    Amp := Sqrt(Amp / (WindowCount * FTrack.ChannelCount));

    Amp := (Decibel(Amp) + MaxDB);
    if Amp < 0 then Amp := 0;

    Peak := (Decibel(Peak) + MaxDB);
    if Peak < 0 then Peak := 0;

    Chart.PenColor     := clrYellow;
    Chart.TextureColor := clrYellow;
    Points[0].X := FreqIndex -0.25 * Factor;
    Points[0].Y := 0;
    Points[1].X := FreqIndex -0.25 * Factor;
    Points[1].Y := Amp;
    Points[2].X := FreqIndex +0.25 * Factor;
    Points[2].Y := Amp;
    Points[3].X := FreqIndex +0.25 * Factor;
    Points[3].Y := 0;
    Chart.AddPolygon(Points, '');

    Chart.PenColor     := clrRed;
    Chart.TextureColor := clrRed;
    Points[0].X := FreqIndex -0.25 * Factor;
    Points[0].Y := Max(Peak - 0.5, 0);
    Points[1].X := FreqIndex -0.25 * Factor;
    Points[1].Y := Peak;
    Points[2].X := FreqIndex +0.25 * Factor;
    Points[2].Y := Peak;
    Points[3].X := FreqIndex +0.25 * Factor;
    Points[3].Y := Max(Peak - 0.5, 0);
    Chart.AddPolygon(Points, '');
  end;
  // draw chart on screen
  Chart.Draw(FScreen, FScreen.Width, FScreen.Height, True);
  Chart.Destroy;
end;

// TSpectrogramDrawer

procedure TSpectrogramDrawer.Draw;
var
  Chart: TChart;
  TimeIndex, FreqIndex: longint;
  X, Y, ch: longint;
  Amp: double;
  WindowCount: longint;
  OutBins: LongInt;
  Bit: TBGRABitmap;
  MaxDB, XFactor, YFactor: TDouble;
begin
  if (FTrack.ChannelCount = 0) then Exit;
  if (FTrack.Samplecount  = 0) then Exit;
  // create and configure the chart
  Chart := NewDefaultChart;
  Chart.Title := 'Spectrogram';
  Chart.XAxisLabel := 'Freq [Hz]';
  Chart.YAxisLabel := 'Time [s]';
  Chart.TitleFontColor := clrwhite;
  Chart.XAxisFontColor := clrWhite;
  Chart.YAxisFontColor := clrWhite;

  Chart.AddPixel(FTrack.Samplerate div 2, FTrack.Duration, clblack);
  Chart.Draw(FScreen, FScreen.Width, FScreen.Height, True);

  Bit := TBGRABitmap.create;
  Bit.SetSize(
    Trunc(Chart.GetDrawingRect.Width *((FTrack.Samplerate div 2) / (Chart.XMaxF - Chart.XMinF))),
    Trunc(Chart.GetDrawingRect.Height*((FTrack.Duration        ) / (Chart.YMaxf - Chart.YMinF))));

  // set fft analysis window size (half of total window size)
  WindowCount := FTrack.Spectrums.WindowCount;
  OutBins     := FTrack.Spectrums.OutBins;

  XFactor := (OutBins      - 1) / (Bit.Width  - 1);
  YFactor := (WindowCount  - 1) / (Bit.Height - 1);

  MaxDB := 6 * FTrack.BitsPerSample;

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
      Bit.SetPixel(X, Bit.Height - 1 - Y, GetColor((Decibel(Amp) + MaxDB) / MaxDB));
    end;
  end;
  FScreen.PutImage(
    Chart.GetDrawingRect.Left,
    Chart.GetDrawingRect.Top + (Chart.GetDrawingRect.Height - Bit.Height), Bit, dmSet);
  Bit.Destroy;
  Chart.Destroy;
end;

// TWaveDrawer

procedure TWaveDrawer.Draw;
var
  ch, i, x, SampleIndex: longint;
  WindowxSize, WindowySize: longint;
  WindowxCount, WindowyCount: longint;
  zMax, zMin: double;
  P1, P2: tpointf;
  Bit: array of TBGRABitmap = nil;
  Chart: TChart;
  OffSet: longint;
begin
  if (FTrack.ChannelCount = 0) then Exit;
  if (FTrack.Samplecount  = 0) then Exit;
  // create a bitmap for each audio channel
  SetLength(Bit, FTrack.ChannelCount);
  for ch := Low(Bit) to High(Bit) do
    Bit[ch] := TBGRABitmap.create;

  WindowxCount := FScreen.Width;       // horizontal resolution (pixels)
  WindowyCount := FTrack.ChannelCount; // one row per channel

  // loop through each channel
  for ch := Low(Bit) to High(Bit) do
  begin
    Chart := NewDefaultChart;
    Chart.LegendEnabled := False;
    Chart.Title := Format('Waveform (%s)', [ChannelName(ch, FTrack.ChannelCount)]);
    Chart.XAxisLabel := 'Time [s]';
    Chart.YAxisLabel := 'Amplitude';
    Chart.TitleFontColor := clrwhite;
    Chart.XAxisFontColor := clrWhite;
    Chart.YAxisFontColor := clrWhite;

    Chart.YCount  := 4;
    Chart.YDeltaF := 0.5;

    Chart.PenWidth     := 1;
    Chart.PenColor     := clrRed;
    Chart.TextureColor := clrBlack;

    // calculate number of samples per horizontal pixel
    WindowxSize := FTrack.SampleCount div WindowxCount;
    // calculate vertical size per channel section
    WindowySize := FScreen.Height div WindowyCount;

    // prepare per-channel bitmap
    Bit[ch].SetSize(FScreen.width, WindowySize);
    // loop through horizontal pixels (time segments)
    for x := 0 to WindowxCount - 1 do
    begin
      zMin :=  infinity;
      zMax := -infinity;

      // find min and max sample values in this time segment
      for i := 0 to WindowxSize -1 do
      begin
        SampleIndex := x * WindowxSize + i;

        zMin := Min(zMin, FTrack.Channels[ch, SampleIndex]);
        zMax := Max(zMax, FTrack.Channels[ch, SampleIndex]);
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
    Chart.Draw(Bit[ch], Bit[ch].Width, Bit[ch].Height);
    Chart.Destroy;
  end;

  // composite each channel's bitmap into the final output
  OffSet := 0;
  for ch := Low(Bit) to High(Bit) do
  begin
    FScreen.PutImage(0, OffSet, Bit[ch], dmSet);
    Inc(OffSet, FScreen.Height div FTrack.ChannelCount);

    Bit[ch].Destroy;
  end;
end;

end.

