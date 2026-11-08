{
  Description: Charts drawer routines.

  Copyright (C) 2025-2026 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
{$modeswitch nestedprocvars}

interface

uses
  BGRABitmap, BaseGraphics, BGRABitmapTypes, Classes, Common, FPImage, Graphics, AudioTrack, Spectrum, SysUtils, Types;

type
  TScreenDrawerMode  = (smDynamicRange, smWaveForm, smFreqSpectrum, smSpectrogram, smLoudness);
  TScreenDrawerModes = set of TScreenDrawerMode;
  TCustomDrawer = class;

  TScreenDrawer = class(TThread)
  private
    FModes: TScreenDrawerModes;
    FOnStart: TThreadMethod;
    FScreen: TBGRABitmap;
    FTrack: TTrack;
    FErrorMessage: string;
    FSuccessful: boolean;
  public
    constructor Create(ATrack: TTrack; AScreen: TBGRABitmap);
    destructor Destroy; override;
    procedure Execute; override;
    property Mode: TScreenDrawerModes read FModes write FModes;
    property OnStart: TThreadMethod read FOnStart write FOnStart;
    property Track: TTrack read FTrack;
    property ErrorMessage: string read FErrorMessage;
    property Successful: boolean read FSuccessful;
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
    procedure RaiseIfFailed;
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

  TLoudnessDrawer = class(TCustomDrawer)
  private
    procedure Draw; override;
  end;

const
  {$IFDEF LCLgtk}  {$DEFINE RGBAPIXEL} {$ENDIF}
  {$IFDEF LCLgtk2} {$DEFINE RGBAPIXEL} {$ENDIF}
  {$IFDEF LCLgtk3} {$UNDEF  RGBAPIXEL} {$ENDIF}
  {$IFDEF LCLQt}   {$UNDEF  RGBAPIXEL} {$ENDIF}
  {$IFDEF LCLQt5}  {$UNDEF  RGBAPIXEL} {$ENDIF}
  {$IFDEF LCLQt6}  {$UNDEF  RGBAPIXEL} {$ENDIF}

  {$IFDEF RGBAPIXEL}
  {** Channels if ordered RGBA ordered }
  clrBlack  : TBGRAPixel = (red: $00; green: $00; blue: $00; alpha: 255);
  clrBlue   : TBGRAPixel = (red: $20; green: $4A; blue: $87; alpha: 255);
  clrPurple : TBGRAPixel = (red: $75; green: $50; blue: $7B; alpha: 255);
  clrRed    : TBGRAPixel = (red: $EF; green: $29; blue: $29; alpha: 255);
  clrYellow : TBGRAPixel = (red: $FC; green: $E9; blue: $4F; alpha: 255);
  clrWhite  : TBGRAPixel = (red: $FF; green: $FF; blue: $FF; alpha: 255);
  clrGreen  : TBGRAPixel = (red: $8A; green: $E2; blue: $34; alpha: 255);
  clrSkyBlu : TBGRAPixel = (red: $72; green: $9F; blue: $CF; alpha: 255);
  {$ELSE}
  {** Channels if ordered BGRA ordered }
  clrBlack  : TBGRAPixel = (blue: $00; green: $00; red: $00; alpha: 255);
  clrBlue   : TBGRAPixel = (blue: $87; green: $4A; red: $20; alpha: 255);
  clrPurple : TBGRAPixel = (blue: $7B; green: $50; red: $75; alpha: 255);
  clrRed    : TBGRAPixel = (blue: $29; green: $29; red: $EF; alpha: 255);
  clrYellow : TBGRAPixel = (blue: $4F; green: $E9; red: $FC; alpha: 255);
  clrWhite  : TBGRAPixel = (blue: $FF; green: $FF; red: $FF; alpha: 255);
  clrGreen  : TBGRAPixel = (blue: $34; green: $E2; red: $8A; alpha: 255);
  clrSkyBlu : TBGRAPixel = (blue: $CF; green: $9F; red: $72; alpha: 255);
  {$ENDIF}

var
  ScreenDrawer: TScreenDrawer = nil;


implementation

uses
  Math;

function GetColor(AFactor: double): TBGRAPixel;
const
  // Black, dark blue, purple, red, orange, yellow, white, interpolated
  // linearly between evenly spaced points.
  {$IFDEF RGBAPIXEL}
  {** Channels if ordered RGBA ordered }
  BaseColors: array[0..6] of TBGRAPixel =
    ((red: $00; green: $00; blue: $00; alpha: 255),  // #000000
     (red: $00; green: $00; blue: $70; alpha: 255),  // #000070
     (red: $70; green: $00; blue: $90; alpha: 255),  // #700090
     (red: $D0; green: $00; blue: $30; alpha: 255),  // #D00030
     (red: $FF; green: $80; blue: $00; alpha: 255),  // #FF8000
     (red: $FF; green: $FF; blue: $00; alpha: 255),  // #FFFF00
     (red: $FF; green: $FF; blue: $FF; alpha: 255)); // #FFFFFF
  {$ELSE}
  {** Channels if ordered BGRA ordered }
  BaseColors: array[0..6] of TBGRAPixel =
    ((blue: $00; green: $00; red: $00; alpha: 255),  // #000000
     (blue: $70; green: $00; red: $00; alpha: 255),  // #000070
     (blue: $90; green: $00; red: $70; alpha: 255),  // #700090
     (blue: $30; green: $00; red: $D0; alpha: 255),  // #D00030
     (blue: $00; green: $80; red: $FF; alpha: 255),  // #FF8000
     (blue: $00; green: $FF; red: $FF; alpha: 255),  // #FFFF00
     (blue: $FF; green: $FF; red: $FF; alpha: 255)); // #FFFFFF
  {$ENDIF}
var
  Seg: Integer;
  Fraction: Double;
  C1, C2: TBGRAPixel;
begin
  if AFactor < 0 then AFactor := 0;
  if AFactor > 1 then AFactor := 1;

  Seg := Floor(AFactor * High(BaseColors));
  if Seg > High(BaseColors) - 1 then Seg := High(BaseColors) - 1;

  Fraction := AFactor * High(BaseColors) - Seg;

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
  result.TitleFontColor  := clLtGray;
  result.XAxisLabelColor := clGray;
  result.YAxisLabelColor := clGray;
  result.XAxisFontColor  := clLtGray;
  result.YAxisFontColor  := clLtGray;
  result.XAxisLineColor  := clYellow;
  result.YAxisLineColor  := clYellow;

  result.TextureHeight := 1;
  result.TextureWidth  := 1;
  result.TextureBackgroundColor := clBlack;

  result.PenColor := clBlack;

  result.XMinF  := 0;
  result.YMinF  := 0;
  result.XCount := 8;
  result.YCount := 5;

  result.YAxisLabelLength := result.GetXAxisLabelSize('Amplitude').Width;
end;

// Draw default chart

procedure DrawDefaultBlockChart(var ABitmap: TBGRABitmap);
var
  Points: array of TPointF = nil;
  Chart: TChart;
begin
  // create and configure the chart
  Chart := NewDefaultChart;
  try
  Chart.Title      := 'Energy & peaks (1s blocks)';
  Chart.XAxisLabel := 'Block';
  Chart.XIntegerTicks := True;
  Chart.YAxisLabel := 'dBFS';

  // Same dBFS scale as TBlockDrawer for 16-bit audio: 0 dBFS at the top.
  Chart.YMinF   := -96;
  Chart.YMaxF   := 0;
  Chart.YCount  := 4;
  Chart.YDeltaF := 24;

  SetLength(Points, 2);
  Points[0].x := 0;
  Points[0].y := -96;
  Points[1].x := 30;
  Points[1].y := 0;
  Chart.AddPolygon(Points, '');

  // draw chart on screen
  Chart.Draw(ABitmap, ABitmap.Width, ABitmap.Height, True);
  finally
    Chart.Free;
  end;
end;

procedure DrawDefaultSpectrumChart(var ABitmap: TBGRABitmap);
var
  Points: array of TPointF = nil;
  Chart: TChart;
begin
  // create and configure the chart
  Chart := NewDefaultChart;
  try
  Chart.Title      := 'Frequency spectrum';
  Chart.XAxisLabel := 'Hz';
  Chart.YAxisLabel := 'dBFS';

  // Same dBFS scale as TSpectrumDrawer for 16-bit audio: 0 dBFS at the top.
  Chart.YMinF   := -96;
  Chart.YMaxF   := 0;
  Chart.YCount  := 4;
  Chart.YDeltaF := 24;

  SetLength(Points, 2);
  Points[0].x := 0;
  Points[0].y := -96;
  Points[1].x := 44100 div 2;
  Points[1].y := 0;
  Chart.AddPolygon(Points, '');
  // draw Chart on screen
  Chart.Draw(ABitmap, ABitmap.Width, ABitmap.Height, True);
  finally
    Chart.Free;
  end;
end;

procedure DrawDefaultSpectrogramChart(var ABitmap: TBGRABitmap);
var
  Points: array of TPointF = nil;
  Chart: TChart;
begin
  // create and configure the chart
  Chart := NewDefaultChart;
  try
  Chart.Title := 'Spectrogram';
  Chart.XAxisLabel := 's';
  Chart.YAxisLabel := 'Hz';

  SetLength(Points, 2);
  Points[0].x := 0;
  Points[0].y := 0;
  Points[1].x := 96;
  Points[1].y := 22050;
  Chart.AddPolygon(Points, '');
  // draw chart on screen
  Chart.Draw(ABitmap, ABitmap.Width, ABitmap.Height, True);
  finally
    Chart.Free;
  end;
end;

procedure DrawDefaultWaveChart(var ABitmap: TBGRABitmap);
var
  Points: array of TPointF = nil;
  Chart: TChart;
begin
  // create and configure the chart
  Chart := NewDefaultChart;
  try
  Chart.LegendEnabled := True;
  Chart.Title := 'Waveform (Mono)';
  Chart.XAxisLabel := 's';
  Chart.YAxisLabel := '';

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
  finally
    Chart.Free;
  end;
end;

procedure DrawDefaultLoudnessChart(var ABitmap: TBGRABitmap);
var
  Points: array of TPointF = nil;
  Chart: TChart;
begin
  // create and configure the chart
  Chart := NewDefaultChart;
  try
  Chart.LegendEnabled := True;
  Chart.Title := 'ShortTerm & Momentary Loudness';
  Chart.XAxisLabel := 's';
  Chart.YAxisLabel := 'LUFS';

  Chart.YMaxF   := 5;
  Chart.YMinF   := -70;
  Chart.XMinF   := 0;
  Chart.XMaxF   := 100;
  Chart.YCount  := 5;
  Chart.YDeltaF := 15;

  SetLength(Points, 2);
  Points[0].x := 0;
  Points[0].y := -70;
  Points[1].x := 100;
  Points[1].y := 5;
  Chart.AddPolygon(Points, '');
  SetLength(Points, 0);
  // draw Chart on screen
  Chart.Draw(ABitmap, ABitmap.Width, ABitmap.Height, True);
  finally
    Chart.Free;
  end;
end;

// TScreenDrawer

constructor TScreenDrawer.Create(ATrack: TTrack; AScreen: TBGRABitmap);
begin
  FModes   := [smDynamicRange, smWaveForm, smFreqSpectrum, smSpectrogram];
  FOnStart := nil;
  FScreen  := AScreen;
  FTrack   := ATrack;

  FreeOnTerminate := True;
  inherited Create(True);
end;

destructor TScreenDrawer.Destroy;
begin
  inherited Destroy;
end;

procedure TScreenDrawer.Execute;
var
  BlockDrawer: TBlockDrawer;
  LoudnessDrawer: TLoudnessDrawer;
  SpectrumDrawer: TSpectrumDrawer;
  SpectrogramDrawer: TSpectrogramDrawer;
  WaveDrawer: TWaveDrawer;
  ChartCount: longint;
  BaseHeight: longint;
  Bit: TBGRABitmap;
  OffSet: longint;
begin
  BlockDrawer := nil;
  LoudnessDrawer := nil;
  SpectrumDrawer := nil;
  SpectrogramDrawer := nil;
  WaveDrawer := nil;
  Bit := nil;
  FSuccessful := False;
  FErrorMessage := '';
  try
    try
      if Assigned(FOnStart) then
        Synchronize(FOnStart);

      FScreen.FillTransparent;
      if (FScreen.Width > 0) and (FScreen.Height > 0) and (FModes <> []) then
      begin

    if Assigned(FTrack) and (FTrack.ChannelCount > 0) then
    begin
      OffSet     := 0;
      ChartCount := 0;
      if smDynamicRange in FModes then Inc(ChartCount);
      if smLoudness     in FModes then Inc(ChartCount);
      if smWaveForm     in FModes then Inc(ChartCount, FTrack.ChannelCount);
      if smFreqSpectrum in FModes then Inc(ChartCount);
      if smSpectrogram  in FModes then Inc(ChartCount);

      BaseHeight := FScreen.Height div ChartCount;
      if smDynamicRange in FModes then
      begin
        Bit := TBGRABitmap.Create(FScreen.Width, BaseHeight);
        BlockDrawer := TBlockDrawer.Create(FTrack, Bit);
        BlockDrawer.Start;
        BlockDrawer.WaitFor;
        BlockDrawer.RaiseIfFailed;
        FScreen.PutImage(0, OffSet, Bit, dmSet);
        Inc(OffSet, Bit.Height);
        FreeAndNil(BlockDrawer);
        FreeAndNil(Bit);
        if Terminated then Exit;
      end;

      if smLoudness in FModes then
      begin
        Bit := TBGRABitmap.Create(FScreen.Width, BaseHeight);
        LoudnessDrawer := TLoudnessDrawer.Create(FTrack, Bit);
        LoudnessDrawer.Start;
        LoudnessDrawer.WaitFor;
        LoudnessDrawer.RaiseIfFailed;
        FScreen.PutImage(0, OffSet, Bit, dmSet);
        Inc(OffSet, Bit.Height);
        FreeAndNil(LoudnessDrawer);
        FreeAndNil(Bit);
        if Terminated then Exit;
      end;

      if smWaveForm in FModes then
      begin
        Bit := TBGRABitmap.Create(FScreen.Width, BaseHeight * FTrack.ChannelCount);
        WaveDrawer := TWaveDrawer.Create(FTrack, Bit);
        WaveDrawer.Start;
        WaveDrawer.WaitFor;
        WaveDrawer.RaiseIfFailed;
        FScreen.PutImage(0, OffSet, Bit, dmSet);
        Inc(OffSet, Bit.Height);
        FreeAndNil(WaveDrawer);
        FreeAndNil(Bit);
        if Terminated then Exit;
      end;

      if smFreqSpectrum in FModes then
      begin
        Bit := TBGRABitmap.Create(FScreen.Width, BaseHeight);
        SpectrumDrawer := TSpectrumDrawer.Create(FTrack, Bit);
        SpectrumDrawer.Start;
        SpectrumDrawer.WaitFor;
        SpectrumDrawer.RaiseIfFailed;
        FScreen.PutImage(0, OffSet, Bit, dmSet);
        Inc(OffSet, Bit.Height);
        FreeAndNil(SpectrumDrawer);
        FreeAndNil(Bit);
        if Terminated then Exit;
      end;

      if smSpectrogram in FModes then
      begin
       Bit := TBGRABitmap.Create(FScreen.Width, BaseHeight);
       SpectrogramDrawer := TSpectrogramDrawer.Create(FTrack, Bit);
       SpectrogramDrawer.Start;
       SpectrogramDrawer.WaitFor;
       SpectrogramDrawer.RaiseIfFailed;
       FScreen.PutImage(0, OffSet, Bit, dmSet);
       Inc(OffSet, Bit.Height);
       FreeAndNil(SpectrogramDrawer);
       FreeAndNil(Bit);
       if Terminated then Exit;
     end;

    end else
    begin
      OffSet     := 0;
      ChartCount := 0;
      if smDynamicRange in FModes then Inc(ChartCount);
      if smLoudness     in FModes then Inc(ChartCount);
      if smWaveForm     in FModes then Inc(ChartCount);
      if smFreqSpectrum in FModes then Inc(ChartCount);
      if smSpectrogram  in FModes then Inc(ChartCount);

      BaseHeight := FScreen.Height div ChartCount;

      Bit := TBGRABitmap.Create(FScreen.Width, BaseHeight);
      if smDynamicRange in FModes then
      begin
        DrawDefaultBlockChart(Bit);
        FScreen.PutImage(0, OffSet, Bit, dmSet);
        Inc(OffSet, Bit.Height);
      end;

      if smLoudness in FModes then
      begin
        DrawDefaultLoudnessChart(Bit);
        FScreen.PutImage(0, OffSet, Bit, dmSet);
        Inc(OffSet, Bit.Height);
      end;

      if smWaveForm in FModes then
      begin
        DrawDefaultWaveChart(Bit);
        FScreen.PutImage(0, OffSet, Bit, dmSet);
        Inc(OffSet, Bit.Height);
      end;

      if smFreqSpectrum in FModes then
      begin
        DrawDefaultSpectrumChart(Bit);
        FScreen.PutImage(0, OffSet, Bit, dmSet);
        Inc(OffSet, Bit.Height);
      end;

      if smSpectrogram  in FModes then
      begin
        DrawDefaultSpectrogramChart(Bit);
        FScreen.PutImage(0, OffSet, Bit, dmSet);
        Inc(OffSet, Bit.Height);
      end;
      FreeAndNil(Bit);
    end;
      end;

      FSuccessful := True;
    except
      on E: Exception do
        FErrorMessage := Format('%s: %s', [E.ClassName, E.Message]);
    end;
  finally
    FreeAndNil(BlockDrawer);
    FreeAndNil(LoudnessDrawer);
    FreeAndNil(SpectrumDrawer);
    FreeAndNil(SpectrogramDrawer);
    FreeAndNil(WaveDrawer);
    FreeAndNil(Bit);
  end;
end;

// TDrawer

constructor TCustomDrawer.Create(ATrack: TTrack; AScreen: TBGRABitmap);
begin
  FTrack  := ATrack;
  FScreen := AScreen;
  FreeOnTerminate := False;
  // TScreenDrawer explicitly starts each worker and then waits for it.  Keep
  // the worker suspended here so it is started exactly once.
  inherited Create(True);
end;

destructor TCustomDrawer.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomDrawer.Execute;
begin
  if Assigned(FTrack) then Draw;
end;

procedure TCustomDrawer.RaiseIfFailed;
begin
  if not Assigned(FatalException) then Exit;
  if FatalException is Exception then
    raise Exception.CreateFmt('%s: %s',
      [FatalException.ClassName, Exception(FatalException).Message])
  else
    raise Exception.Create(FatalException.ClassName);
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
  result.TitleFontColor  := clLtGray;
  result.XAxisFontColor  := clLtGray;
  result.YAxisFontColor  := clLtGray;
  result.XAxisLabelColor := clGray;
  result.YAxisLabelColor := clGray;
  result.XAxisLineColor  := clYellow;
  result.YAxisLineColor  := clYellow;

  result.TextureHeight := 1;
  result.TextureWidth  := 1;
  result.TextureBackgroundColor := clBlack;

  result.PenColor := clBlack;

  result.XMinF  := 0;
  result.YMinF  := 0;
  result.XCount := 8;
  result.YCount := 5;

  result.YAxisLabelLength := result.GetXAxisLabelSize('Amplitude').Width;
end;

// TBlockDrawer

procedure TBlockDrawer.Draw;
var
  i, j: longint;
  Rms2, Peak: TDouble;
  RmsValues: TDoubleVector = nil;
  PeakValues: TDoubleVector = nil;
  Chart: TChart;
  MaxDB, MinDB: double;

  // Domain-specific bars are rasterized here so TChart remains generic.
  procedure PaintBars(AChart: TChart; ACanvas: TBGRABitmap);
  var
    i, X0, X1, YBase, YRms, YPeak: longint;
  begin
    YBase := Round(AChart.DataToCanvasY(MinDB));
    for i := 0 to High(RmsValues) do
    begin
      X0 := Round(AChart.DataToCanvasX((i + 1) - 0.35));
      X1 := Round(AChart.DataToCanvasX((i + 1) + 0.35));
      if X1 <= X0 then X1 := X0 + 1;
      YRms := Round(AChart.DataToCanvasY(RmsValues[i]));
      ACanvas.FillRect(X0, Min(YRms, YBase), X1, Max(YRms, YBase) + 1,
        clrYellow);
      YPeak := Round(AChart.DataToCanvasY(PeakValues[i]));
      ACanvas.FillRect(X0, Min(YPeak, YRms), X1, Max(YPeak, YRms) + 1,
        clrRed);
    end;
  end;

begin
  if (FTrack.ChannelCount = 0) then Exit;
  if (FTrack.SampleCount  = 0) then Exit;
  // create and configure the chart
  Chart := NewDefaultChart;
  try
  Chart.Title      := 'Energy & peaks (1s blocks)';
  Chart.XAxisLabel := 'Block';
  Chart.XIntegerTicks := True;
  Chart.YAxisLabel := 'dBFS';
  Chart.TitleFontColor := clrwhite;
  Chart.XAxisFontColor := clrWhite;
  Chart.YAxisFontColor := clrWhite;

  MinDB := -6 * FTrack.BitsPerSample;
  MaxDB :=  0;

  Chart.YMinF   := MinDB;
  Chart.YMaxF   := MaxDB;
  Chart.YCount  := 4;
  Chart.YDeltaF := (6 * FTrack.BitsPerSample) div Chart.YCount;

  // loop through each block
  SetLength(RmsValues, FTrack.DRMeter.BlockCount);
  SetLength(PeakValues, FTrack.DRMeter.BlockCount);
  for i := 0 to FTrack.DRMeter.BlockCount -1 do
  begin
    Rms2 := 0;
    // calculate average rms across channels
    for j := 0 to FTrack.ChannelCount -1 do
    begin
      Rms2 := Rms2 + FTrack.DRMeter.Rms2(j, i);
    end;
    Rms2 := Rms2 / FTrack.ChannelCount;

    RmsValues[i] := Max(Decibel(Sqrt(Rms2)), MinDB);

    Peak := 0;
    // calculate average Peak across channels
    for j := 0 to FTrack.ChannelCount - 1 do
    begin
      Peak := Peak + FTrack.DRMeter.Peak(j, i);
    end;
    Peak := Peak / FTrack.ChannelCount;

    PeakValues[i] := Max(Decibel(Peak), MinDB);
  end;
  if Length(RmsValues) > 0 then
  begin
    // NewDefaultChart already fixes XMinF at zero. The original polygons
    // determined only the upper horizontal bound.
    Chart.XMaxF := Length(RmsValues) + 0.35;
    Chart.AddPixel(0, MinDB, clBlack);
  end;
  // draw Chart on screen, with the bars below the axes
  Chart.Draw(FScreen, FScreen.Width, FScreen.Height, True, @PaintBars);
  finally
    Chart.Free;
  end;
end;

// TSpectrumDrawer

procedure TSpectrumDrawer.Draw;
var
  Chart: TChart;
  i: longint;
  OutBins: longint;
  Amplitudes: TDoubleVector = nil;
  Peaks: TDoubleVector = nil;
  Amp, Peak, FreqIndex: TDouble;
  Factor: single;
  MaxDB, MinDB: TDouble;

  // Domain-specific spectrum bars are kept out of the generic chart unit.
  procedure PaintBars(AChart: TChart; ACanvas: TBGRABitmap);
  var
    i, X0, X1, YAmp, YBase, YPeak0, YPeak1: longint;
    FreqIndex: TDouble;
  begin
    YBase := Round(AChart.DataToCanvasY(MinDB));
    for i := 0 to High(Amplitudes) do
    begin
      FreqIndex := (i + 1) * Factor;
      X0 := Round(AChart.DataToCanvasX(FreqIndex - 0.25 * Factor));
      X1 := Round(AChart.DataToCanvasX(FreqIndex + 0.25 * Factor));
      if X1 <= X0 then X1 := X0 + 1;
      YAmp := Round(AChart.DataToCanvasY(Amplitudes[i]));
      ACanvas.FillRect(X0, Min(YAmp, YBase), X1, Max(YAmp, YBase) + 1,
        clrYellow);
      YPeak0 := Round(AChart.DataToCanvasY(Peaks[i]));
      YPeak1 := Round(AChart.DataToCanvasY(Min(Peaks[i] + 0.5, 0)));
      ACanvas.FillRect(X0, Min(YPeak0, YPeak1), X1,
        Max(YPeak0, YPeak1) + 1, clrRed);
    end;
  end;

begin
  if (FTrack.ChannelCount = 0) then Exit;
  if (FTrack.Samplecount  = 0) then Exit;
  // create and configure the chart
  Chart := NewDefaultChart;
  try
  Chart.Title      := 'Frequency spectrum';
  Chart.XAxisLabel := 'Hz';
  Chart.YAxisLabel := 'dBFS';
  Chart.TitleFontColor := clrwhite;
  Chart.XAxisFontColor := clrWhite;
  Chart.YAxisFontColor := clrWhite;

  MinDB := -6 * FTrack.BitsPerSample;
  MaxDB :=  0;

  Chart.YMinF   := MinDB;
  Chart.YMaxF   := MaxDB;
  Chart.YCount  := 4;
  Chart.YDeltaF := (6 * FTrack.BitsPerSample) div Chart.YCount;

  OutBins     := FTrack.Spectrums.OutBins;
  if OutBins > 1 then
    Factor := (0.5 * FTrack.Samplerate) / (OutBins - 1)
  else
    Factor := 1;

  SetLength(Amplitudes, Max(0, OutBins - 1));
  SetLength(Peaks, Max(0, OutBins - 1));
  for i := 1 to OutBins - 1 do
  begin
    Amp  := FTrack.Spectrums.SpectrumRms(i);
    Peak := FTrack.Spectrums.SpectrumPeak(i);

    Amplitudes[i - 1] := Max(Decibel(Amp), MinDB);
    Peaks[i - 1] := Max(Decibel(Peak), MinDB);
  end;
  if Length(Amplitudes) > 0 then
  begin
    // XMinF remains the zero fixed by NewDefaultChart, as in the polygon
    // implementation. Only the last bar supplied the automatic upper bound.
    FreqIndex := Length(Amplitudes) * Factor;
    Chart.XMaxF := FreqIndex + 0.25 * Factor;
    Chart.AddPixel(0, MinDB, clBlack);
  end;
  // draw chart on screen, with the bars below the axes
  Chart.Draw(FScreen, FScreen.Width, FScreen.Height, True, @PaintBars);
  finally
    Chart.Free;
  end;
end;

// TSpectrogramDrawer

procedure TSpectrogramDrawer.Draw;
var
  Chart: TChart;

  // Paints the spectrogram picture inside the drawing area, below the axes.
  procedure PaintImage(AChart: TChart; ACanvas: TBGRABitmap);
  var
    Row, Bin, Bin0, Bin1, Base: longint;
    X, Y, ch: longint;
    Amp: double;
    RowCount: longint;
    OutBins: LongInt;
    Row0: array of longint = nil;
    Row1: array of longint = nil;
    Bit: TBGRABitmap;
    MaxDB: TDouble;
    Area: TRect;
  begin
    Area := AChart.GetDrawingRect;
    if (Area.Width <= 0) or (Area.Height <= 0) or
       (AChart.XMaxF <= AChart.XMinF) or (AChart.YMaxF <= AChart.YMinF) then Exit;
    Bit := TBGRABitmap.create;
    try
    Bit.SetSize(
      Trunc(Area.Width * (FTrack.DurationSeconds / (AChart.XMaxF - AChart.XMinF))),
      Trunc(Area.Height * (FTrack.Samplerate * 0.5 / (AChart.YMaxF - AChart.YMinF))));

    RowCount := FTrack.Spectrums.RowCount;
    OutBins  := FTrack.Spectrums.OutBins;

    if (Bit.Width  > 2) and
       (Bit.Height > 2) and
       (RowCount   > 0) and
       (OutBins    > 0) then
    begin
      // Every pixel shows the maximum of the rows (time) and bins (frequency)
      // it covers, so nothing is skipped when the picture is smaller than the
      // data; when it is larger, neighbouring pixels share the same cell.
      SetLength(Row0, Bit.Width);
      SetLength(Row1, Bit.Width);
      for X := 0 to Bit.Width -1 do
      begin
        Row0[X] := int64(X) * RowCount div Bit.Width;
        Row1[X] := Max(Row0[X], int64(X + 1) * RowCount div Bit.Width - 1);
      end;

      MaxDB := Max(1, 6 * FTrack.BitsPerSample);
      // loop over output bitmap pixels, low frequencies at the bottom
      for Y := 0 to Bit.Height -1 do
      begin
        Bin0 := int64(Y) * OutBins div Bit.Height;
        Bin1 := Max(Bin0, int64(Y + 1) * OutBins div Bit.Height - 1);
        for X := 0 to Bit.Width -1 do
        begin
          Amp := 0;
          for ch := 0 to FTrack.ChannelCount -1 do
            for Row := Row0[X] to Row1[X] do
            begin
              Base := Row * OutBins;
              for Bin := Bin0 to Bin1 do
                Amp := Max(Amp, FTrack.Spectrums.Channels[ch, Base + Bin]);
            end;
          // map amplitude to color and set pixel
          Bit.SetPixel(X, Bit.Height - 1 - Y, GetColor((Decibel(Amp) + MaxDB) / MaxDB));
        end;
      end;
    end;
    ACanvas.PutImage(Area.Left, Area.Top + (Area.Height - Bit.Height), Bit, dmSet);
    finally
      Bit.Free;
    end;
  end;

begin
  if (FTrack.ChannelCount = 0) then Exit;
  if (FTrack.Samplecount  = 0) then Exit;
  // create and configure the chart
  Chart := NewDefaultChart;
  try
  Chart.Title := 'Spectrogram';
  Chart.XAxisLabel := 's';
  Chart.YAxisLabel := 'Hz';
  Chart.TitleFontColor := clrWhite;
  Chart.XAxisFontColor := clrWhite;
  Chart.YAxisFontColor := clrWhite;

  Chart.AddPixel(FTrack.DurationSeconds, FTrack.Samplerate * 0.5, clblack);
  // draw chart on screen, with the spectrogram below the axes
  Chart.Draw(FScreen, FScreen.Width, FScreen.Height, True, @PaintImage);
  finally
    Chart.Free;
  end;
end;

// TWaveDrawer

procedure TWaveDrawer.Draw;
var
  ch, Channel: longint;
  WindowySize: longint;
  WindowxCount, WindowyCount: longint;
  Bit: array of TBGRABitmap = nil;
  Chart: TChart;
  OffSet: longint;

  // Draws the cached envelope of Channel below the axes.  The chart is used
  // only for axes and labels; no per-column chart items are allocated or
  // traversed by TChart.DrawItems.
  procedure PaintEnvelope(AChart: TChart; ACanvas: TBGRABitmap);
  var
    x: longint;
    zMax, zMin: double;
    DataX, PixelX, PixelYMin, PixelYMax: double;
  begin
    for x := 0 to WindowxCount - 1 do
    begin
      // Samples beyond full scale (float or lossy sources) fall outside the
      // plot area, where TChart.Draw cuts them.
      zMin := FTrack.WaveformMin(Channel, x);
      zMax := FTrack.WaveformMax(Channel, x);
      DataX := x / Max(1, WindowxCount - 1) * FTrack.DurationSeconds;

      PixelX := AChart.DataToCanvasX(DataX);
      PixelYMin := AChart.DataToCanvasY(zMin);
      PixelYMax := AChart.DataToCanvasY(zMax);

      // The envelope is one vertical segment per output column.  Integer,
      // pixel-aligned fills avoid the much more expensive antialias pipeline
      // without changing the effective one-pixel geometry.
      ACanvas.FillRect(
        Round(PixelX),
        Round(Min(PixelYMin, PixelYMax)),
        Round(PixelX) + 1,
        Round(Max(PixelYMin, PixelYMax)) + 1,
        clrRed);
    end;
  end;

begin
  if (FTrack.ChannelCount = 0) then Exit;
  if (FTrack.Samplecount  = 0) then Exit;
  // create a bitmap for each audio channel
  SetLength(Bit, FTrack.ChannelCount);
  try
  for ch := Low(Bit) to High(Bit) do
    Bit[ch] := TBGRABitmap.create;

  WindowxCount := FScreen.Width;       // horizontal resolution (pixels)
  WindowyCount := FTrack.ChannelCount; // one row per channel
  FTrack.PrepareWaveformCache(WindowxCount);

  // loop through each channel
  for ch := Low(Bit) to High(Bit) do
  begin
    Chart := NewDefaultChart;
    try
    Chart.LegendEnabled := False;
    Chart.Title := Format('Waveform (%s)', [ChannelName(ch, FTrack.ChannelCount)]);
    Chart.XAxisLabel := 's';
    Chart.YAxisLabel := '';
    Chart.TitleFontColor := clrwhite;
    Chart.XAxisFontColor := clrWhite;
    Chart.YAxisFontColor := clrWhite;

    Chart.YCount  := 4;
    Chart.YDeltaF := 0.5;

    Chart.PenWidth     := 1;
    Chart.PenColor     := clrRed;
    Chart.TextureColor := clrBlack;

    // calculate vertical size per channel section
    WindowySize := FScreen.Height div WindowyCount;

    // prepare per-channel bitmap
    Bit[ch].SetSize(FScreen.width, WindowySize);
    // set visible bounds for Chart
    Chart.YMaxF := +1.0;
    Chart.YMinF := -1.0;
    Chart.XMinF := 0;
    Chart.XMaxF := Max(1.0, FTrack.DurationSeconds);
    // Keep CalculateDataArea active while the actual waveform is rendered
    // by PaintEnvelope.
    Chart.AddPixel(0, -1, clrBlack);
    Chart.AddPixel(Max(1.0, FTrack.DurationSeconds), 1, clrBlack);
    // draw Chart on bitmap, with the envelope below the axes
    Channel := ch;
    Chart.Draw(Bit[ch], Bit[ch].Width, Bit[ch].Height, True, @PaintEnvelope);
    finally
      Chart.Free;
    end;
  end;

  // composite each channel's bitmap into the final output
  OffSet := 0;
  for ch := Low(Bit) to High(Bit) do
  begin
    FScreen.PutImage(0, OffSet, Bit[ch], dmSet);
    Inc(OffSet, FScreen.Height div FTrack.ChannelCount);
  end;
  finally
    for ch := Low(Bit) to High(Bit) do Bit[ch].Free;
  end;
end;

// TLoudnessDrawer

procedure TLoudnessDrawer.Draw;
const
  MinLUFS = -70;
  MaxLUFS = 5;
var
  Chart: TChart;
  TickCount: longint;

  procedure AddCurve(AShortTerm: boolean; AColor: TBGRAPixel);
  var
    Points: ArrayOfTPointF;
    i, Used: longint;
    Level: double;

    procedure Flush;
    begin
      if Used > 0 then
      begin
        SetLength(Points, Used);
        if Used = 1 then Chart.AddPixel(Points[0].x, Points[0].y, AColor)
        else Chart.AddPolyline(Points, False, '');
        SetLength(Points, TickCount);
        Used := 0;
      end;
    end;

  begin
    Chart.PenColor := AColor;
    Chart.TextureColor := AColor;
    SetLength(Points, TickCount);
    Used := 0;
    // Meter entry zero ends at 100 ms. Include the last complete interval.
    for i := 1 to TickCount do
    begin
      if AShortTerm then Level := FTrack.Loudness.ShortTermLoudness(i * 100)
      else Level := FTrack.Loudness.MomentaryLoudness(i * 100);
      // Do not pass infinities to chart geometry, or connect across gaps.
      if IsNan(Level) or IsInfinite(Level) then Flush
      else
      begin
        Points[Used].x := i * 0.1;
        // Levels outside [MinLUFS, MaxLUFS] keep their true slope and are cut
        // at the plot border by TChart.Draw.
        Points[Used].y := Level;
        Inc(Used);
      end;
    end;
    Flush;
  end;

begin
  if (FTrack.ChannelCount = 0) or (FTrack.SampleCount = 0) or
     (FTrack.SampleRate = 0) then Exit;
  Chart := NewDefaultChart;
  try
    Chart.LegendEnabled := False;
    Chart.Title := 'ShortTerm & Momentary Loudness';
    Chart.XAxisLabel := 's';
    Chart.YAxisLabel := 'LUFS';
    Chart.TitleFontColor := clrWhite;
    Chart.XAxisFontColor := clrWhite;
    Chart.YAxisFontColor := clrWhite;
    Chart.XMinF := 0;
    Chart.XMaxF := Max(1.0, FTrack.DurationSeconds);
    Chart.YMinF := MinLUFS;
    Chart.YMaxF := MaxLUFS;
    Chart.PenWidth := 1.5;
    // Keep valid bounds even for silence and tracks shorter than 100 ms.
    Chart.AddPixel(0, MinLUFS, clrBlack);
    // Meter time arguments are signed millisecond counts. Clamp before
    // narrowing the sample-based calculation to LongInt.
    TickCount := Min(int64(FTrack.SampleCount) * 10 div FTrack.SampleRate,
      int64(High(longint) div 100));
    AddCurve(True, clrSkyBlu);
    AddCurve(False, clrGreen);
    Chart.Draw(FScreen, FScreen.Width, FScreen.Height);
  finally
    Chart.Free;
  end;
end;

end.

