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

interface

uses
  BGRABitmap, BaseGraphics, BGRABitmapTypes, Classes, Common, FPImage, Graphics, SoundWav, Spectrum, SysUtils, Types;

type
  TScreenDrawerMode  = (smDynamicRange, smWaveForm, smFreqSpectrum, smSpectrogram, smLoudness);
  TScreenDrawerModes = set of TScreenDrawerMode;
  TCustomDrawer = class;

  TScreenDrawer = class(TThread)
  private
    FModes: TScreenDrawerModes;
    FOnStart: TThreadMethod;
    FOnStop: TThreadMethod;
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
    property OnStop: TThreadMethod read FOnStop write FOnStop;
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
  Math, SoundUtils;

function GetColor(AFactor: double): TBGRAPixel;
const
  {$IFDEF RGBAPIXEL}
  {** Channels if ordered RGBA ordered }
  BaseColors: array[0..5] of TBGRAPixel =
    ((red: $00; green: $00; blue: $00; alpha: 255),  // $000000
     (red: $20; green: $4A; blue: $87; alpha: 255),  // $874A20
     (red: $75; green: $50; blue: $7B; alpha: 255),  // $7B5075
     (red: $EF; green: $29; blue: $29; alpha: 255),  // $2929EF
     (red: $FC; green: $E9; blue: $4F; alpha: 255),  // $4FE9FC
     (red: $FF; green: $FF; blue: $FF; alpha: 255)); // $FFFFFF
  {$ELSE}
  {** Channels if ordered BGRA ordered }
  BaseColors: array[0..5] of TBGRAPixel =
    ((blue: $00; green: $00; red: $00; alpha: 255),  // $000000
     (blue: $87; green: $4A; red: $20; alpha: 255),  // $874A20
     (blue: $7B; green: $50; red: $75; alpha: 255),  // $7B5075
     (blue: $29; green: $29; red: $EF; alpha: 255),  // $2929EF
     (blue: $4F; green: $E9; red: $FC; alpha: 255),  // $4FE9FC
     (blue: $FF; green: $FF; red: $FF; alpha: 255)); // $FFFFFF
  {$ENDIF}
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
  result.XCount := 6;
  result.YCount := 4;

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
  Chart.Title      := 'Energy & peaks (1s blocks)';
  Chart.XAxisLabel := 'Block';
  Chart.YAxisLabel := 'dBFS';

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
  Chart.XAxisLabel := 'Hz';
  Chart.YAxisLabel := 'dBFS';

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
  Chart.XAxisLabel := 'Hz';
  Chart.YAxisLabel := 's';

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
  Chart.Destroy;
end;

procedure DrawDefaultLoudnessChart(var ABitmap: TBGRABitmap);
var
  Points: array of TPointF = nil;
  Chart: TChart;
begin
  // create and configure the chart
  Chart := NewDefaultChart;
  Chart.LegendEnabled := True;
  Chart.Title := 'ShortTerm & Momentary Loudness';
  Chart.XAxisLabel := 's';
  Chart.YAxisLabel := 'dBFS';

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

constructor TScreenDrawer.Create(ATrack: TTrack; AScreen: TBGRABitmap);
begin
  FModes   := [smDynamicRange, smWaveForm, smFreqSpectrum, smSpectrogram];
  FOnStart := nil;
  FOnStop  := nil;
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
    if Assigned(FOnStop) then
      Synchronize(FOnStop);
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
  result.XCount := 6;
  result.YCount := 4;

  result.YAxisLabelLength := result.GetXAxisLabelSize('Amplitude').Width;
end;

// TBlockDrawer

procedure TBlockDrawer.Draw;
var
  i, j, X0, X1, YBase, YRms, YPeak: longint;
  Rms2, Peak: TDouble;
  RmsValues: TDoubleVector = nil;
  PeakValues: TDoubleVector = nil;
  Chart: TChart;
  MaxDB, MinDB: double;
begin
  if (FTrack.ChannelCount = 0) then Exit;
  if (FTrack.SampleCount  = 0) then Exit;
  // create and configure the chart
  Chart := NewDefaultChart;
  Chart.Title      := 'Energy & peaks (1s blocks)';
  Chart.XAxisLabel := 'Block';
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
  // draw Chart on screen
  Chart.Draw(FScreen, FScreen.Width, FScreen.Height, True);

  // Domain-specific bars are rasterized here so TChart remains generic.
  YBase := Round(Chart.DataToCanvasY(MinDB));
  for i := 0 to High(RmsValues) do
  begin
    X0 := Round(Chart.DataToCanvasX((i + 1) - 0.35));
    X1 := Round(Chart.DataToCanvasX((i + 1) + 0.35));
    if X1 <= X0 then X1 := X0 + 1;
    YRms := Round(Chart.DataToCanvasY(RmsValues[i]));
    FScreen.FillRect(X0, Min(YRms, YBase), X1, Max(YRms, YBase) + 1,
      clrYellow);
    YPeak := Round(Chart.DataToCanvasY(PeakValues[i]));
    FScreen.FillRect(X0, Min(YPEak, YRms), X1, Max(YPEak, YRms) + 1,
      clrRed);
  end;
  Chart.Destroy;
end;

// TSpectrumDrawer

procedure TSpectrumDrawer.Draw;
var
  Chart: TChart;
  i, X0, X1, YAmp, YBase, YPeak0, YPeak1: longint;
  OutBins: longint;
  Amplitudes: TDoubleVector = nil;
  Peaks: TDoubleVector = nil;
  Amp, Peak, FreqIndex: TDouble;
  Factor: single;
  MaxDB, MinDB: TDouble;
begin
  if (FTrack.ChannelCount = 0) then Exit;
  if (FTrack.Samplecount  = 0) then Exit;
  // create and configure the chart
  Chart := NewDefaultChart;
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
  // draw chart on screen
  Chart.Draw(FScreen, FScreen.Width, FScreen.Height, True);

  // Domain-specific spectrum bars are kept out of the generic chart unit.
  YBase := Round(Chart.DataToCanvasY(MinDB));
  for i := 0 to High(Amplitudes) do
  begin
    FreqIndex := (i + 1) * Factor;
    X0 := Round(Chart.DataToCanvasX(FreqIndex - 0.25 * Factor));
    X1 := Round(Chart.DataToCanvasX(FreqIndex + 0.25 * Factor));
    if X1 <= X0 then X1 := X0 + 1;
    YAmp := Round(Chart.DataToCanvasY(Amplitudes[i]));
    FScreen.FillRect(X0, Min(YAmp, YBase), X1, Max(YAmp, YBase) + 1,
      clrYellow);
    YPeak0 := Round(Chart.DataToCanvasY(Peaks[i]));
    YPeak1 := Round(Chart.DataToCanvasY(Min(Peaks[i] + 0.5, 0)));
    FScreen.FillRect(X0, Min(YPEak0, YPeak1), X1,
      Max(YPEak0, YPeak1) + 1, clrRed);
  end;
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
  Chart.XAxisLabel := 'Hz';
  Chart.YAxisLabel := 's';
  Chart.TitleFontColor := clrWhite;
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

  if (Bit.Width  > 2) and
     (Bit.Height > 2) then
  begin
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
  ch, x: longint;
  WindowySize: longint;
  WindowxCount, WindowyCount: longint;
  zMax, zMin: double;
  DataX, PixelX, PixelYMin, PixelYMax: double;
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
  FTrack.PrepareWaveformCache(WindowxCount);

  // loop through each channel
  for ch := Low(Bit) to High(Bit) do
  begin
    Chart := NewDefaultChart;
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
    Chart.XMaxF := Max(1, FTrack.Duration);
    // Keep CalculateDataArea active while the actual waveform is rendered
    // directly after Chart.Draw.
    Chart.AddPixel(0, -1, clrBlack);
    Chart.AddPixel(Max(1, FTrack.Duration), 1, clrBlack);
    // draw Chart on bitmap
    Chart.Draw(Bit[ch], Bit[ch].Width, Bit[ch].Height);

    // Draw the cached envelope directly on the chart bitmap.  The chart is
    // used only for axes and labels; no per-column chart items are allocated
    // or traversed by TChart.DrawItems.
    for x := 0 to WindowxCount - 1 do
    begin
      zMin := FTrack.WaveformMin(ch, x);
      zMax := FTrack.WaveformMax(ch, x);
      DataX := x / Max(1, WindowxCount - 1) * FTrack.Duration;

      PixelX := Chart.DataToCanvasX(DataX);
      PixelYMin := Chart.DataToCanvasY(zMin);
      PixelYMax := Chart.DataToCanvasY(zMax);

      // The envelope is one vertical segment per output column.  Integer,
      // pixel-aligned fills avoid the much more expensive antialias pipeline
      // without changing the effective one-pixel geometry.
      Bit[ch].FillRect(
        Round(PixelX),
        Round(Min(PixelYMin, PixelYMax)),
        Round(PixelX) + 1,
        Round(Max(PixelYMin, PixelYMax)) + 1,
        clrRed);
    end;
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

// TLoudnessDrawer

procedure TLoudnessDrawer.Draw;
var
  ch, i, TimeMs, SampleIndex, MaxDB: longint;
  WindowxSize, WindowxCount: longint;
  zMax, zMin: double;
  P1, P2: ArrayOfTPointF;
  Chart: TChart;
  OffSet: longint;
begin
  if (FTrack.ChannelCount = 0) then Exit;
  if (FTrack.Samplecount  = 0) then Exit;

  Chart := NewDefaultChart;
  Chart.LegendEnabled := False;
  Chart.Title := 'ShortTerm & Momentary Loudness';
  Chart.XAxisLabel := 's';
  Chart.YAxisLabel := 'dBFS';
  Chart.TitleFontColor := clrwhite;
  Chart.XAxisFontColor := clrWhite;
  Chart.YAxisFontColor := clrWhite;

//Chart.YCount  := 4;
//Chart.YDeltaF := 0.5;

  MaxDB := 6 * FTrack.BitsPerSample;

  Chart.PenWidth     := 1.5;
  Chart.PenColor     := clrSkyBlu;
  Chart.TextureColor := clrSkyBlu;

  TimeMs := 0;
  SetLength(P1, FTrack.Duration * 10);
  for i := Low(P1) to High(P1) do
  begin
    P1[i].x := TimeMs / 1000;
    P1[i].y := MaxDB + FTrack.Loudness.ShortTermLoudness(TimeMs);

    Inc(TimeMs, 100);
  end;
  Chart.AddPolyline(P1, False, '');

  Chart.PenWidth     := 1.5;
  Chart.PenColor     := clrGreen;
  Chart.TextureColor := clrGreen;

  TimeMs := 0;
  SetLength(P2, FTrack.Duration * 10);
  for i := Low(P2) to High(P2) do
  begin
    P2[i].x := TimeMs / 1000;
    P2[i].y := MaxDB + FTrack.Loudness.MomentaryLoudness(TimeMs);

    Inc(TimeMs, 100);
  end;
  Chart.AddPolyline(P2, False, '');

  // set visible bounds for Chart
  //Chart.YMaxF := +1.0;
  Chart.YMinF := 0;
  Chart.XMinF := 0;
  //Chart.XMaxF := Max(1, FTrack.Duration);
  // draw Chart on bitmap
  Chart.Draw(FScreen, FScreen.Width, FScreen.Height);
  Chart.Destroy;
end;

end.

