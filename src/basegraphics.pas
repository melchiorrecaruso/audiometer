{
  Helical Compression Spring Designer

  Copyright (C) 2022-2025 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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
  to the Free Software Foundation, Inc., 51 Franklin Street - Fifth Floor,
  Boston, MA 02110-1335, USA.
}

unit BaseGraphics;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses
  BGRABitmap, BGRABitmapTypes, BGRAFreeType, BGRATextFX, BGRACanvas2D,
  Classes, DateUtils, Graphics, SysUtils, EasyLazFreeType, LazFreeTypeFontCollection;

type
  TDrawingArea = record
    Left, Right, Top, Bottom: integer;
    function Width: integer;
    function Height: integer;
    procedure Clear;
  end;

  TArea = record
    Left, Right, Top, Bottom: single;
    function Width: single;
    function Height: single;
    procedure Clear;
  end;

  TChartItem = class
  private
    FCaption: string;
    FFontName: string;
    FFontHeight: single;
    FFontColor: TBGRAPixel;
    FFontStyle: TFontStyles;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TChartPixelItem = class(TChartItem)
  private
    FX: single;
    FY: single;
    FPenColor: TBGRAPixel;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TChartLabelItem = class(TChartItem)
  private
    FX: single;
    FY: single;
    FShiftX: longint;
    FShiftY: longint;
    FAlign: TAlignment;
    FVertAlign: TVerticalAlignment;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TChartPolyLineItem = class(TChartItem)
  private
    FExtend: boolean;
    FPenColor: TBGRAPixel;
    FPenStyle: TPenStyle;
    FPenWidth: single;
    FPoints: ArrayOfTPointF;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TChartPolygonItem = class(TChartPolyLineItem)
  private
    FTextureColor: TBGRAPixel;
    FTextureBackgroundColor: TBGRAPixel;
    FTextureWidth: longint;
    FTextureHeight: longint;
    FTexturePenWidth: single;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TChartDotLabelItem = class(TChartLabelItem)
  private
    FRadius: single;
    FPenColor: TBGRAPixel;
    FPenStyle: TPenStyle;
    FPenWidth: single;
    FTextureColor: TBGRAPixel;
    FTextureBackgroundColor: TBGRAPixel;
    FTextureWidth: longint;
    FTextureHeight: longint;
    FTexturePenWidth: single;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TChart = class
  private
    FBit: TBGRABitmap;
    FBackgroundColor: TBGRAPixel;
    FColor: TBGRAPixel;

    FLegendLineLength: longint;
    FLegendEnabled: boolean;

    FTitle: string;
    FTitleFontColor: TBGRAPixel;
    FTitleFontHeight: single;
    FTitleFontStyle: TFontStyles;

    FXAxisLabel: string;
    FXAxisLabelColor: TBGRAPixel;
    FXAxisFontColor: TBGRAPixel;
    FXAxisFontHeight: single;
    FXAxisFontStyle: TFontStyles;
    FXAxisLineColor: TBGRAPixel;
    FXAxisLineStyle: TPenStyle;
    FXAxisLineWidth: single;
    FXGridLineColor: TBGRAPixel;
    FXGridLineStyle: TPenStyle;
    FXGridLineWidth: single;

    FYAxisLabel: string;
    FYAxisLabelLength: longint;
    FYAxisLabelColor: TBGRAPixel;
    FYAxisFontColor: TBGRAPixel;
    FYAxisFontHeight: single;
    FYAxisFontStyle: TFontStyles;
    FYAxisLineColor: TBGRAPixel;
    FYAxisLineStyle: TPenStyle;
    FYAxisLineWidth: single;
    FYGridLineColor: TBGRAPixel;
    FYGridLineStyle: TPenStyle;
    FYGridLineWidth: single;

    FCurrentFontHeight: single;
    FCurrentFontColor: TBGRAPixel;
    FCurrentFontStyle: TFontStyles;
    FCurrentFontQuality: TBGRAFontQuality;
    FCurrentPenColor: TBGRAPixel;
    FCurrentPenStyle: TPenStyle;
    FCurrentPenWidth: single;
    FCurrentTextureColor: TBGRAPixel;
    FCurrentTextureBackgroundColor: TBGRAPixel;
    FCurrentTextureWidth: longint;
    FCurrentTextureHeight: longint;
    FCurrentTexturePenWidth: single;

    FXAxisLabelCount, FYAxisLabelCount: longint;

    FDrawingArea: TDrawingArea;
    FDataArea: TArea;

    FXIncrementF, FYIncrementF: single;
    FXScaleF, FYScaleF: single;

    FItems: TList;

    FWidth, FHeight: longint;
    FSpacer: longint;
    FScale: single;

    FAdjustXMin: boolean;
    FAdjustXMax: boolean;
    FAdjustYMin: boolean;
    FAdjustYMax: boolean;

    FIsNeededUpdateDrawingArea: boolean;
    FIsNeededCalcXDeltaF: boolean;
    FIsNeededCalcYDeltaF: boolean;
    FIsNeededCalcXLabelCount: boolean;
    FIsNeededCalcYLabelCount: boolean;

    function PxFromAreaFToArea(const AX: single): single;
    function PyFromAreaFToArea(const AY: single): single;

    procedure CalculateDataArea;

    function GetXMinF: single;
    function GetXMaxF: single;
    function GetYMinF: single;
    function GetYMaxF: single;

    procedure SetXMaxF(Value: single);
    procedure SetXMinF(Value: single);
    procedure SetYMaxF(Value: single);
    procedure SetYMinF(Value: single);
    procedure SetXDeltaF(Value: single);
    procedure SetYDeltaF(Value: single);
    procedure SetXCount(Value: longint);
    procedure SetYCount(Value: longint);

    procedure SetCurrentFontAntialias(AValue: boolean);
    procedure SetCurrentFontQuality(AValue: TBGRAFontQuality);
    procedure SetCurrentFontName(AValue: string);
    procedure SetCurrentFontStyle(AValue: TFontStyles);
    procedure SetCurrentFontColor(AValue: TBGRAPixel);
    procedure SetCurrentFontHeight(AValue: longint);
    procedure SetCurrentJoinStyle(AValue: TPenJoinStyle);
    procedure SetCurrentLineCap(AValue: TPenEndCap);
    procedure SetCurrentPenStyle(AValue: TPenStyle);

    function GetTextSize(const AText: string): TSize;

    function GetTitleSize: TSize;
    function GetLegendSize: TSize;
    function GetXAxisLabelSize: TSize;
    function GetYAxisLabelSize: TSize;

    function XToCanvas(X: single): single;
    function YToCanvas(Y: single): single;
    procedure DrawLine(x0, y0, x1, y1: single; aPenColor: TBGRAPixel; aPenWidth: single);
    procedure DrawText(X, Y: single; const aText: string; const aTextColor: TBGRAPixel; aAlign: TAlignment; aVertAlign: TVerticalAlignment);

    procedure DrawGrid;
    procedure DrawLegend;
    procedure DrawItems;
    procedure DrawPixel(AItem: TChartPixelItem);
    procedure DrawPolygon(AItem: TChartPolygonItem);
    procedure DrawPolyLine(AItem: TChartPolyLineItem);
    procedure DrawDotLabel(AItem: TChartDotLabelItem);
    procedure DrawLabel(AItem: TChartLabelItem);

  public
    constructor Create;
    destructor Destroy; override;

    procedure AddPolyLine(const APoints: ArrayOfTPointF; AExtend: boolean; const ACaption: string);
    procedure AddPolygon(const APoints: ArrayOfTPointF; const ACaption: string);

    procedure AddLabel(AX, AY: single; AShiftX, AShiftY: longint; AAlign: TAlignment; AVertAlign: TVerticalAlignment; const ACaption: string);

    procedure AddPixel(AX, AY: single; AColor: TBGRAPixel);

    procedure AddDotLabel(AX, AY, ARadius: single; AShiftX, AShiftY: longint; AAlign: TAlignment; AVertAlign: TVerticalAlignment; const ACaption: string);

    procedure Draw(ABitmap: TBGRABitmap; AWidth, AHeight: longint; AOpaque: boolean = True);
    procedure Clear;

    function GetDrawingRect: TRect;
  public
    function GetXAxisLabelSize(const ALabel: string): TSize;
    function GetYAxisLabelSize(const ALabel: string): TSize;

    property Title: string read FTitle write FTitle;
    property TitleFontHeight: single read FTitleFontHeight write FTitleFontHeight;
    property TitleFontColor: TBGRAPixel read FTitleFontColor write FTitleFontColor;
    property TitleFontStyle: TFontStyles read FTitleFontStyle write FTitleFontStyle;

    property XAxisLabel: string read FXAxisLabel write FXAxisLabel;
    property XAxisLabelColor: TBGRAPixel read FXAxisLabelColor write FXAxisLabelColor;
    property XAxisFontHeight: single read FXAxisFontHeight write FXAxisFontHeight;
    property XAxisFontColor: TBGRAPixel read FXAxisFontColor write FXAxisFontColor;
    property XAxisFontStyle: TFontStyles read FXAxisFontStyle write FXAxisFontStyle;
    property XAxisLineColor: TBGRAPixel read FXAxisLineColor write FXAxisLineColor;
    property XAxisLineStyle: TPenStyle read FXAxisLineStyle write FXAxisLineStyle;
    property XAxisLineWidth: single read FXAxisLineWidth write FXAxisLineWidth;
    property XGridLineColor: TBGRAPixel read FXGridLineColor write FXGridLineColor;
    property XGridLineStyle: TPenStyle read FXGridLineStyle write FXGridLineStyle;
    property XGridLineWidth: single read FXGridLineWidth write FXGridLineWidth;

    property YAxisLabel: string read FYAxisLabel write FYAxisLabel;
    property YAxisLabelLength: longint read FYAxisLabelLength write FYAxisLabelLength;
    property YAxisLabelColor: TBGRAPixel read FYAxisLabelColor write FYAxisLabelColor;
    property YAxisFontHeight: single read FYAxisFontHeight write FYAxisFontHeight;
    property YAxisFontColor: TBGRAPixel read FYAxisFontColor write FYAxisFontColor;
    property YAxisFontStyle: TFontStyles read FYAxisFontStyle write FYAxisFontStyle;
    property YAxisLineColor: TBGRAPixel read FYAxisLineColor write FYAxisLineColor;
    property YAxisLineStyle: TPenStyle read FYAxisLineStyle write FYAxisLineStyle;
    property YAxisLineWidth: single read FYAxisLineWidth write FYAxisLineWidth;
    property YGridLineColor: TBGRAPixel read FYGridLineColor write FYGridLineColor;
    property YGridLineStyle: TPenStyle read FYGridLineStyle write FYGridLineStyle;
    property YGridLineWidth: single read FYGridLineWidth write FYGridLineWidth;

    property BackgroundColor: TBGRAPixel read FBackgroundColor write FBackgroundColor;
    property Color: TBGRAPixel read FColor write FColor;

    property LegendLineLength: longint read FLegendLineLength write FLegendLineLength;
    property LegendEnabled: boolean read FLegendEnabled write FLegendEnabled;

    property FontHeight: single read FCurrentFontHeight write FCurrentFontHeight;
    property FontColor: TBGRAPixel read FCurrentFontColor write FCurrentFontColor;
    property FontStyle: TFontStyles read FCurrentFontStyle write FCurrentFontStyle;

    property PenColor: TBGRAPixel read FCurrentPenColor write FCurrentPenColor;
    property PenStyle: TPenStyle read FCurrentPenStyle write FCurrentPenStyle;
    property PenWidth: single read FCurrentPenWidth write FCurrentPenWidth;

    property TextureColor: TBGRAPixel read FCurrentTextureColor write FCurrentTextureColor;
    property TextureBackgroundColor: TBGRAPixel read FCurrentTextureBackgroundColor write FCurrentTextureBackgroundColor;
    property TextureWidth: longint read FCurrentTextureWidth write FCurrentTextureWidth;
    property TextureHeight: longint read FCurrentTextureHeight write FCurrentTextureHeight;
    property TexturePenWidth: single read FCurrentTexturePenWidth write FCurrentTexturePenWidth;

    property XMaxF: single read GetXMaxF write SetXMaxF;
    property XMinF: single read GetXMinF write SetXMinF;
    property YMaxF: single read GetYMaxF write SetYMaxF;
    property YMinF: single read GetYMinF write SetYMinF;
    property XDeltaF: single write SetXDeltaF;
    property YDeltaF: single write SetYDeltaF;
    property XCount: longint read FXAxisLabelCount write SetXCount;
    property YCount: longint read FYAxisLabelCount write SetYCount;

    property Spacer: longint read FSpacer write FSpacer;
    property Scale: single read FScale write FScale;

    property AdjustXMin: boolean read FAdjustXMin write FAdjustXMin;
    property AdjustXMax: boolean read FAdjustXMax write FAdjustXMax;
    property AdjustYMin: boolean read FAdjustYMin write FAdjustYMin;
    property AdjustYMax: boolean read FAdjustYMax write FAdjustYMax;
  end;

  TReportTable = class
  private
    FBit: TBGRABitmap;
    FFontName: string;
    FFontHeight: single;
    FFontColor: TBGRAPixel;
    FFontStyle: TFontStyles;
    FFontQuality: TBGRAFontQuality;

    FBorderWidth: longint;
    FBackgroundColor: TBGRAPixel;
    FPenColor: TBGRAPixel;
    FPenStyle: TPenStyle;
    FPenWidth: single;

    FLeft: longint;
    FTop: longint;
    FScale: single;
    FAutosize: boolean;
    FRowSpacer: longint;
    FRowCount: longint;
    FRowAlignments: array of TVerticalAlignment;
    FColumnSpacer: longint;
    FColumnCount: longint;
    FColumnAlignments: array of TAlignment;

    FTable: array of array of string;
    FIsNeededUpdateSize: boolean;
    FWidth: longint;
    FHeight: longint;

    function GetWidth: longint;
    function GetHeight: longint;

    function GetItem(aRow, aColumn: longint): string;
    procedure SetItem(aRow, aColumn: longint; const S: string);
    procedure SetColumnCount(Value: longint);
    procedure SetRowCount(Value: longint);
    function GetRowAlignment(Index: longint): TVerticalAlignment;
    function GetColumnAlignment(Index: longint): TAlignment;
    procedure SetRowAlignment(Index: longint; Value: TVerticalAlignment);
    procedure SetColumnAlignment(Index: longint; Value: TAlignment);
    procedure SetSize(aRowCount, aColumnCount: longint);

    procedure SetFontName(const Value: string);
    procedure SetFontHeight(const Value: single);
    procedure SetFontStyle(const Value: TFontStyles);
    procedure CalculateDrawindArea;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Draw(ABitmap: TBGRABitmap; AWidth, AHeight: longint; AOpaque: boolean = True);
  public
    property Autosize: boolean read FAutosize write FAutosize;
    property RowCount: longint read FRowCount write SetRowCount;
    property ColumnCount: longint read FColumnCount write SetColumnCount;
    property Items[Row, Column: longint]: string read GetItem write SetItem; default;

    property BorderWidth: longint read FBorderWidth write FBorderWidth;
    property BackgroundColor: TBGRAPixel read FBackgroundColor write FBackgroundColor;

    property FontName: string read FFontName write SetFontName;
    property FontHeight: single read FFontHeight write SetFontHeight;
    property FontColor: TBGRAPixel read FFontColor write FFontColor;
    property FontStyle: TFontStyles read FFontStyle write SetFontStyle;
    property PenColor: TBGRAPixel read FPenColor write FPenColor;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
    property PenWidth: single read FPenWidth write FPenWidth;

    property RowSpacer: longint read FRowSpacer write FRowSpacer;
    property RowAlignments[Row: longint]: TVerticalAlignment read GetRowAlignment write SetRowAlignment;
    property ColumnSpacer: longint read FColumnSpacer write FColumnSpacer;
    property ColumnAlignments[Column: longint]: TAlignment read GetColumnAlignment write SetColumnAlignment;

    property Zoom: single read FScale write FScale;

    property Left: longint read FLeft write FLeft;
    property Top: longint read FTop write FTop;
    property Width: longint read GetWidth;
    property Height: longint read GetHeight;
  end;

  TSpringDrawing = class
  private
    Fd: double;
    FDm: double;
    fClockWise: boolean;
    FLc: double;
    FLx: double;
    Fn: double;
    Fnt1: double;
    Fnt2: double;
    FPitchF: double;
    fClosedEnds: boolean;
    FGroundEnds: boolean;

    FCaption: string;
    FAutoFit: boolean;
    FAutoScale: double;
    FScale: single;
    FSpacer: longint;

    FBit: TBGRABitmap;
    FBitCharSize: TSize;
    FBackgroundColor: TBGRAPixel;
    FFontName: string;
    FFontHeight: single;
    FFontColor: TBGRAPixel;
    FFontStyle: TFontStyles;

    FCenterLineColor: TBGRAPixel;
    FCenterLineStyle: TPenStyle;
    FCenterLineWidth: single;

    FPenColor: TBGRAPixel;
    FPenStyle: TPenStyle;
    FPenWidth: single;
    FTextureColor: TBGRAPixel;
    FTextureBackgroundColor: TBGRAPixel;
    FTextureWidth: longint;
    FTextureHeight: longint;
    FTexturePenWidth: single;
    FWidth, FHeight: longint;

    function xt(const t: double): double;
    function yt(const t: double): double;
    function zt(const t: double): double;
    function PreCheck: boolean;

    function XToCanvas(X: single): single;
    function YToCanvas(Y: single): single;

  public
    constructor Create;
    destructor Destroy; override;
    procedure DrawInSection(ABitmap: TBGRABitmap);
    procedure DrawInSection(ABitmap: TBGRABitmap; aWidth, aHeight: longint);

    procedure DrawInProfile(ABitmap: TBGRABitmap);
    procedure DrawInProfile(ABitmap: TBGRABitmap; aWidth, aHeight: longint);
  public
    property d: double read Fd write Fd;
    property Dm: double read FDm write FDm;
    property Lc: double read FLc write FLc;
    property Lx: double read FLx write FLx;
    property n: double read Fn write Fn;
    property nt1: double read Fnt1 write Fnt1;
    property nt2: double read Fnt2 write Fnt2;
    property ClockWise: boolean read fClockWise write fClockWise;
    property GroundEnds: boolean read FGroundEnds write FGroundEnds;
    property ClosedEnds: boolean read fClosedEnds write fClosedEnds;

    property Caption: string read FCaption write FCaption;
    property AutoFit: boolean read fAutoFit write fAutoFit;

    property FontName: string read FFontName write FFontName;
    property FontHeight: single read FFontHeight write FFontHeight;
    property FontColor: TBGRAPixel read FFontColor write FFontColor;
    property FontStyle: TFontStyles read FFontStyle write FFontStyle;

    property CenterLineColor: TBGRAPixel read FCenterLineColor write FCenterLineColor;
    property CenterLineStyle: TPenStyle read FCenterLineStyle write FCenterLineStyle;
    property CenterLineWidth: single read FCenterLineWidth write FCenterLineWidth;

    property PenColor: TBGRAPixel read FPenColor write FPenColor;
    property PenStyle: TPenStyle read FPenStyle write FPenStyle;
    property PenWidth: single read FPenWidth write FPenWidth;

    property TextureColor: TBGRAPixel read FTextureColor write FTextureColor;
    property TextureBackgroundColor: TBGRAPixel read FTextureBackgroundColor write FTextureBackgroundColor;
    property TextureWidth: longint read FTextureWidth write FTextureWidth;
    property TextureHeight: longint read FTextureHeight write FTextureHeight;
    property TexturePenWidth: single read FTexturePenWidth write FTexturePenWidth;

    property BackgroundColor: TBGRAPixel read FBackgroundColor write FBackgroundColor;

    property Spacer: longint read FSpacer write FSpacer;
    property Height: longint read FHeight write FHeight;
    property Width: longint read FWidth write FWidth;

    property Scale: single read FScale write FScale;
  end;

procedure DrawLogo(ABitmap: TBGRABitmap; aWidth, aHeight: longint);

const
  DefaultSpacer = 16;

var
  DefaultFontName: string = '';
  DefaultFontFileName: string = '';

implementation

uses Math, SyncObjs;

var
  ChartTextLock: TCriticalSection;

// Initialize

// Common routines

function GetDelta(Count: longint; const Range: double): double;
var
  k: double = 0.001;
begin
  while True do
  begin
    Result := 0.01 * k; if (Result * Count) >= (Range) then Break;
    Result := 0.02 * k; if (Result * Count) >= (Range) then Break;
    Result := 0.04 * k; if (Result * Count) >= (Range) then Break;
    Result := 0.05 * k; if (Result * Count) >= (Range) then Break;
    Result := 0.06 * k; if (Result * Count) >= (Range) then Break;
    Result := 0.10 * k; if (Result * Count) >= (Range) then Break;
    Result := 0.15 * k; if (Result * Count) >= (Range) then Break;
    Result := 0.20 * k; if (Result * Count) >= (Range) then Break;
    Result := 0.25 * k; if (Result * Count) >= (Range) then Break;
    Result := 0.50 * k; if (Result * Count) >= (Range) then Break;
    k := k * 10;
  end;
end;

function GetMin(const MinValue: double): double;
var
  k: longint;
begin
  Result := 0;
  if MinValue > 0 then
  begin
    k := Trunc(Log10(MinValue));
    if k > 1 then
      Result := Trunc(MinValue / Power(10, k - 1)) * Power(10, k - 1)
    else
      Result := MinValue;
  end else
    if MinValue < 0 then
    begin
      k := Trunc(Log10(Abs(MinValue)));
      if k > 1 then
        Result := -Trunc(Abs(MinValue) / Power(10, k - 1) + 1) * Power(10, k - 1)
      else
        Result := MinValue;
    end;
end;

function GetString(const AValue: double): string;
begin
  if AValue >= 100000 then
    Result := FloatToStrF(AValue, ffGeneral, 6, 0)
  else
    if AValue >= 10000 then
      Result := FloatToStrF(AValue, ffGeneral, 5, 0)
    else
      Result := FloatToStrF(AValue, ffGeneral, 4, 0);
end;

// TDrawingArea

function TDrawingArea.Width: integer;
begin
  Result := Right - Left;
end;

function TDrawingArea.Height: integer;
begin
  Result := Top - Bottom;
end;

procedure TDrawingArea.Clear;
begin
  Left   := 0;
  Right  := 0;
  Top    := 0;
  Bottom := 0;
end;

// TArea

function TArea.Width: single;
begin
  Result := Right - Left;
end;

function TArea.Height: single;
begin
  Result := Top - Bottom;
end;

procedure TArea.Clear;
begin
  Left   := 0;
  Right  := 0;
  Top    := 0;
  Bottom := 0;
end;

// TChartItem

constructor TChartItem.Create;
begin
  inherited Create;
end;

destructor TChartItem.Destroy;
begin
  FCaption  := '';
  FFontName := '';
  inherited Destroy;
end;

// TChartPixelItem

constructor TChartPixelItem.Create;
begin
  inherited Create;
end;

destructor TChartPixelItem.Destroy;
begin
  inherited destroy;
end;

// TChartLabelItem

constructor TChartLabelItem.Create;
begin
  inherited Create;
end;

destructor TChartLabelItem.Destroy;
begin
  inherited Destroy;
end;

// TChartPolyLineItem

constructor TChartPolyLineItem.Create;
begin
  inherited Create;
  FPoints := nil;
end;

destructor TChartPolyLineItem.Destroy;
begin
  FPoints := nil;
  inherited Destroy;
end;

// TChartPoligonItem

constructor TChartPolygonItem.Create;
begin
  inherited Create;
  FPoints := nil;
end;

destructor TChartPolygonItem.Destroy;
begin
  FPoints := nil;
  inherited Destroy;
end;

// TChartDotLabelItem

constructor TChartDotLabelItem.Create;
begin
  inherited Create;
end;

destructor TChartDotLabelItem.Destroy;
begin
  inherited Destroy;
end;

// TChart

constructor TChart.Create;
begin
  inherited Create;
  FBit := TBGRABitmap.Create;
  if (DefaultFontName <> '') and FileExists(DefaultFontFileName) then
  begin
    FontCollection.AddFile(DefaultFontFileName);
    FBit.FontRenderer := TBGRAFreeTypeFontRenderer.Create;
  end;
  FItems := TList.Create;
  Clear;
end;

destructor TChart.Destroy;
begin
  Clear;
  FItems.Free;
  FBit.Free;
end;

procedure TChart.Clear;
var
  I: longint;
begin
  FColor := clWindow;
  FBackgroundColor := clBtnFace;

  FTitle := 'Chart';
  FTitleFontColor := clBlack;
  FTitleFontHeight := 0;
  FTitleFontStyle := [fsBold];

  FXAxisLabel := 'X Axis';

  FXAxisLabelColor := clGray;
  FXAxisFontColor := clBlack;
  FXAxisFontHeight := 0;
  FXAxisFontStyle := [fsBold];
  FXAxisLineColor := clBlack;
  FXAxisLineStyle := psSolid;
  FXAxisLineWidth := 1.0;
  FXGridLineColor := clSilver;
  FXGridLineStyle := psSolid;
  FXGridLineWidth := 0.5;

  FYAxisLabel := 'Y';
  FYAxisLabelLength := 0;
  FYAxisLabelColor := clGray;
  FYAxisFontColor := clBlack;
  FYAxisFontHeight := 0;
  FYAxisFontStyle := [fsBold];
  FYAxisLineColor := clBlack;
  FYAxisLineStyle := psSolid;
  FYAxisLineWidth := 1.0;
  FYGridLineColor := clSilver;
  FYGridLineStyle := psSolid;
  FYGridLineWidth := 0.5;

  FLegendLineLength := 16;
  FLegendEnabled := True;

  FCurrentFontHeight := 0;
  FCurrentFontColor := clRed;
  FCurrentFontStyle := [fsBold];
  FCurrentPenColor := clRed;
  FCurrentPenStyle := psSolid;
  FCurrentFontQuality := fqFineAntialiasing;

  FCurrentPenWidth := 1.0;
  FCurrentTextureColor := clRed;
  FCurrentTextureBackgroundColor := clYellow;
  FCurrentTextureWidth    := 8;
  FCurrentTextureHeight   := 8;
  FCurrentTexturePenWidth := 1.0;

  FDrawingArea.Clear;
  FDataArea.Clear;

  FXIncrementF := 0;
  FYIncrementF := 0;
  FXScaleF := 1;
  FYScaleF := 1;
  FXAxisLabelCount  := 1;
  FYAxisLabelCount  := 1;

  FWidth   := 0;
  FHeight  := 0;

  FAdjustXMin := True;
  FAdjustXMax := True;
  FAdjustYMin := True;
  FAdjustYMax := True;

  FIsNeededUpdateDrawingArea  := True;
  FIsNeededCalcXDeltaF := True;
  FIsNeededCalcYDeltaF := True;
  FIsNeededCalcXLabelCount  := True;
  FIsNeededCalcYLabelCount  := True;

  FSpacer := DefaultSpacer;
  FScale  := 1.0;

  for I := 0 to FItems.Count -1 do
    TChartItem(FItems[I]).Destroy;
  FItems.Clear;
end;

procedure TChart.AddPolyLine(const APoints: ArrayOfTPointF;
  AExtend: boolean; const ACaption: string);
var
  I: longint;
  Item: TChartPolyLineItem;
begin
  Item := TChartPolyLineItem.Create;
  SetLength(Item.FPoints, Length(APoints));
  for I := Low(APoints) to High(APoints) do
    Item.FPoints[I] := APoints[I];

  Item.FCaption    := ACaption;
  Item.FFontName   := DefaultFontName;
  Item.FFontHeight := FCurrentFontHeight;
  Item.FFontColor  := FCurrentFontColor;
  Item.FFontStyle  := FCurrentFontStyle;
  Item.FPenColor   := FCurrentPenColor;
  Item.FPenStyle   := FCurrentPenStyle;
  Item.FPenWidth   := FCurrentPenWidth;
  Item.FExtend     := AExtend;
  FItems.Add(Item);
end;

procedure TChart.AddPolygon(const APoints: ArrayOfTPointF; const ACaption: string);
var
  I: longint;
  Item: TChartPolygonItem;
begin
  Item := TChartPolygonItem.Create;
  SetLength(Item.FPoints, Length(APoints));
  for I := Low(APoints) to High(APoints) do
    Item.FPoints[I] := APoints[I];

  Item.FCaption                := ACaption;
  Item.FFontName               := DefaultFontName;
  Item.FFontHeight             := FCurrentFontHeight;
  Item.FFontColor              := FCurrentFontColor;
  Item.FFontStyle              := FCurrentFontStyle;
  Item.FPenColor               := FCurrentPenColor;
  Item.FPenStyle               := FCurrentPenStyle;
  Item.FPenWidth               := FCurrentPenWidth;
  Item.FTextureColor           := FCurrentTextureColor;
  Item.FTextureBackgroundColor := FCurrentTextureBackgroundColor;
  Item.FTextureWidth           := FCurrentTextureWidth;
  Item.FTextureHeight          := FCurrentTextureHeight;
  Item.FTexturePenWidth        := FCurrentTexturePenWidth;
  FItems.Add(Item);
end;

procedure TChart.AddLabel(AX, AY: single; AShiftX, AShiftY: longint;
  AAlign: TAlignment; AVertAlign: TVerticalAlignment; const ACaption: string);
var
  Item: TChartLabelItem;
begin
  Item := TChartLabelItem.Create;
  Item.FCaption    := ACaption;
  Item.FFontName   := DefaultFontName;
  Item.FFontHeight := FCurrentFontHeight;
  Item.FFontColor  := FCurrentFontColor;
  Item.FFontStyle  := FCurrentFontStyle;
  Item.FX          := AX;
  Item.FY          := AY;
  Item.FShiftX     := AShiftX;
  Item.FShiftY     := AShiftY;
  Item.FAlign      := AAlign;
  Item.FVertAlign  := AVertAlign;
  FItems.Add(Item);
end;

procedure TChart.AddPixel(AX, AY: single; AColor: TBGRAPixel);
var
  Item: TChartPixelItem;
begin
  Item := TChartPixelItem.Create;
  Item.FX := AX;
  Item.FY := AY;
  Item.FPenColor := AColor;
  Fitems.Add(Item);
end;

procedure TChart.AddDotLabel(AX, AY, ARadius: single; AShiftX, AShiftY: longint;
  AAlign: TAlignment; AVertAlign: TVerticalAlignment; const ACaption: string);
var
  Item: TChartDotLabelItem;
begin
  Item                         := TChartDotLabelItem.Create;
  Item.FCaption                := ACaption;
  Item.FFontName               := DefaultFontName;
  Item.FFontHeight             := FCurrentFontHeight;
  Item.FFontColor              := FCurrentFontColor;
  Item.FFontStyle              := FCurrentFontStyle;
  Item.FPenColor               := FCurrentPenColor;
  Item.FPenStyle               := FCurrentPenStyle;
  Item.FPenWidth               := FCurrentPenWidth;
  Item.FTextureColor           := FCurrentTextureColor;
  Item.FTextureBackgroundColor := FCurrentTextureBackgroundColor;
  Item.FTextureWidth           := FCurrentTextureWidth;
  Item.FTextureHeight          := FCurrentTextureHeight;
  Item.FTexturePenWidth        := FCurrentTexturePenWidth;
  Item.FX                      := AX;
  Item.FY                      := AY;
  Item.FRadius                 := ARadius;
  Item.FShiftX                 := AShiftX;
  Item.FShiftY                 := AShiftY;
  Item.FAlign                  := AAlign;
  Item.FVertAlign              := AVertAlign;
  FItems.Add(Item);
end;

procedure TChart.CalculateDataArea;
var
  i, j: longint;
  Item: TChartItem;
begin
  if FItems.Count = 0 then
  begin
    FDataArea.Clear;
  end else
  begin
    if FIsNeededUpdateDrawingArea then
    begin
      if AdjustXMin then FDataArea.Left   := + MaxSingle;
      if AdjustXMax then FDataArea.Right  := - MaxSingle;
      if AdjustYMin then FDataArea.Bottom := + MaxSingle;
      if AdjustYMax then FDataArea.Top    := - MaxSingle;

      for i := 0 to FItems.Count -1 do
      begin
        Item := TChartItem(FItems[i]);
        if (Item is TChartPolyLineItem) then
        begin
          for j := 0 to High(TChartPolyLineItem(Item).FPoints) do
          begin
            if AdjustXMin then FDataArea.Left   := Min(FDataArea.Left,   TChartPolyLineItem(Item).FPoints[j].X);
            if AdjustXMax then FDataArea.Right  := Max(FDataArea.Right,  TChartPolyLineItem(Item).FPoints[j].X);
            if AdjustYMin then FDataArea.Bottom := Min(FDataArea.Bottom, TChartPolyLineItem(Item).FPoints[j].Y);
            if AdjustYMax then FDataArea.Top    := Max(FDataArea.Top,    TChartPolyLineItem(Item).FPoints[j].Y);
          end;
        end else
        if (Item is TChartPixelItem) then
        begin
          if AdjustXMin then FDataArea.Left   := Min(FDataArea.Left,   TChartPixelItem(Item).FX);
          if AdjustXMax then FDataArea.Right  := Max(FDataArea.Right,  TChartPixelItem(Item).FX);
          if AdjustYMin then FDataArea.Bottom := Min(FDataArea.Bottom, TChartPixelItem(Item).FY);
          if AdjustYMax then FDataArea.Top    := Max(FDataArea.Top,    TChartPixelItem(Item).FY);
        end;
      end;
      if AdjustXMin then FDataArea.Left   := GetMin(FDataArea.Left);
      if AdjustYMin then FDataArea.Bottom := GetMin(FDataArea.Bottom);
    end;
  end;
end;

function TChart.GetTextSize(const AText: string): TSize;
begin
  ChartTextLock.Enter;
  try
    result := FBit.TextSize(AText);
  finally
    ChartTextLock.Leave;
  end;
end;

function TChart.GetDrawingRect: TRect;
begin
  result.Left   :=           FDrawingArea.Left   + 1;
  result.Right  :=           FDrawingArea.Right  - 1;
  result.Top    := FHeight - FDrawingArea.Top    + 1;
  result.Bottom := FHeight - FDrawingArea.Bottom - 1;
end;

function TChart.GetTitleSize: TSize;
begin
  if FTitle = '' then
    Exit(TSize.Create(0, 0));

  SetCurrentFontAntialias(False);
  SetCurrentFontQuality(FCurrentFontQuality);
  SetCurrentFontName(DefaultFontName);
  SetCurrentFontStyle(FTitleFontStyle);
  SetCurrentFontHeight(Trunc(FTitleFontHeight * FScale));
  result := GetTextSize(FTitle);
end;

function TChart.GetLegendSize: TSize;
var
  I: longint;
  Item: TChartItem;
  TxtSize: TSize;
begin
  if not FLegendEnabled then
    Exit(TSize.Create(0, 0));

  for I := 0 to FItems.Count -1 do
  begin
    Item := TChartItem(FItems[I]);

    if (Item is TChartPolygonItem) or
       (Item is TChartPolyLineItem) then
    begin
      if Item.FCaption <> '' then
      begin
        SetCurrentFontAntialias(False);
        SetCurrentFontQuality(FCurrentFontQuality);
        SetCurrentFontName(Item.FFontName);
        SetCurrentFontStyle(Item.FFontStyle);
        SetCurrentFontHeight(Trunc(Item.FFontHeight*FScale));

        TxtSize := GetTextSize(Item.FCaption);

        Result.Width  := Max(Result.Width,  TxtSize.Width);
        Result.Height := Max(Result.Height, TxtSize.Height);
      end;
    end;
  end;

  if Result.Width > 0 then
  begin
    Result.Width := Result.Width + Trunc(FSpacer*FScale*2.5 + FLegendLineLength*FScale);
  end;

  if Result.Height > 0 then
  begin
    Result.Height := Result.Height + Trunc(FSpacer*FScale);
  end;
end;

function TChart.GetXAxisLabelSize: TSize;
var
  I: longint;
  Size: TSize;
begin
  SetCurrentFontAntialias(False);
  SetCurrentFontQuality(FCurrentFontQuality);
  SetCurrentFontName(DefaultFontName);
  SetCurrentFontStyle(FXAxisFontStyle);
  SetCurrentFontHeight(Trunc(FXAxisFontHeight * FScale));

  Result := GetTextSize(FXAxisLabel);
  if FIsNeededCalcXLabelCount then
  begin
    FXAxisLabelCount := FWidth div Max(Result.Width, 2);
  end;

  Size := GetTextSize(GetString(FDataArea.Left));
  Result.Height := Max(Result.Height, Size.Height);
  Result.Width  := Max(Result.Width,  Size.Width);

  Size := GetTextSize(GetString(FDataArea.Right));
  Result.Height := Max(Result.Height, Size.Height);
  Result.Width  := Max(Result.Width,  Size.Width);
end;

function TChart.GetXAxisLabelSize(const ALabel: string): TSize;
begin
  SetCurrentFontAntialias(False);
  SetCurrentFontQuality(FCurrentFontQuality);
  SetCurrentFontName(DefaultFontName);
  SetCurrentFontStyle(FXAxisFontStyle);
  SetCurrentFontHeight(Trunc(FXAxisFontHeight * FScale));

  Result := GetTextSize(ALabel);
end;

function TChart.GetYAxisLabelSize: TSize;
var
  Size: TSize;
begin
  SetCurrentFontAntialias(False);
  SetCurrentFontQuality(FCurrentFontQuality);
  SetCurrentFontName(DefaultFontName);
  SetCurrentFontStyle(FYAxisFontStyle);
  SetCurrentFontHeight(Trunc(FYAxisFontHeight * FScale));

  Result := GetTextSize(FYAxisLabel);
  if FIsNeededCalcYLabelCount then
  begin
    FYAxisLabelCount := FHeight div Max(Result.Height, 2);
  end;

  Size := GetTextSize(GetString(FDataArea.Bottom));
  Result.Height := Max(Result.Height, Size.Height);
  Result.Width  := Max(Result.Width,  Size.Width);

  Size := GetTextSize(GetString(FDataArea.Top));
  Result.Height := Max(Result.Height, Size.Height);
  Result.Width  := Max(Result.Width,  Size.Width);

  Result.Width := Max(FYAxisLabelLength, Result.Width);
end;

function TChart.GetYAxisLabelSize(const ALabel: string): TSize;
begin
  SetCurrentFontAntialias(False);
  SetCurrentFontQuality(FCurrentFontQuality);
  SetCurrentFontName(DefaultFontName);
  SetCurrentFontStyle(FYAxisFontStyle);
  SetCurrentFontHeight(Trunc(FYAxisFontHeight * FScale));

  Result := GetTextSize(ALabel);
end;

procedure TChart.SetCurrentFontAntialias(AValue: boolean);
begin
  if FBit.FontAntialias <> AValue then
    FBit.FontAntialias := AValue;
end;

procedure TChart.SetCurrentFontQuality(AValue: TBGRAFontQuality);
begin
  if FBit.FontQuality <> AValue then
    FBit.FontQuality := AValue;
end;

procedure TChart.SetCurrentFontName(AValue: string);
begin
  if FBit.FontName <> AValue then
    FBit.FontName := AValue;
end;

procedure TChart.SetCurrentFontStyle(AValue: TFontStyles);
begin
  if FBit.FontStyle <> AValue then
    FBit.FontStyle := AValue;
end;

procedure TChart.SetCurrentFontColor(AValue: TBGRAPixel);
begin
  // nothing to do
end;

procedure TChart.SetCurrentFontHeight(AValue: longint);
begin
  if FBit.FontHeight <> AValue then
    FBit.FontHeight := AValue;
end;

procedure TChart.SetCurrentJoinStyle(AValue: TPenJoinStyle);
begin
  if FBit.JoinStyle <> AValue then
    FBit.JoinStyle := AValue;
end;

procedure TChart.SetCurrentLineCap(AValue: TPenEndCap);
begin
  if FBit.LineCap <> AValue then
    FBit.LineCap := AValue;
end;

procedure TChart.SetCurrentPenStyle(AValue: TPenStyle);
begin
  if FBit.PenStyle <> AValue then
    FBit.PenStyle := AValue;
end;

procedure TChart.DrawGrid;
var
  I: longint;
  X, XShift, XSpacing: double;
  Y, YShift, YSpacing: double;
begin
  XShift   := -(FSpacer * FScale) * 0.5;
  YShift   := -(FSpacer * FScale) * 0.5;
  XSpacing :=  (FDrawingArea.Right - FDrawingArea.Left  ) / FXAxisLabelCount;
  YSpacing :=  (FDrawingArea.Top   - FDrawingArea.Bottom) / FYAxisLabelCount;

  // Draw Y secondary axis and X labels
  SetCurrentFontAntialias(False);
  SetCurrentFontQuality(FCurrentFontQuality);
  SetCurrentFontName(DefaultFontName);
  SetCurrentFontStyle(FXAxisFontStyle);
  SetCurrentFontColor(FXAxisFontColor);
  SetCurrentFontHeight(Trunc(FXAxisFontHeight * FScale));
  SetCurrentJoinStyle(pjsRound);
  SetCurrentLineCap(pecRound);
  SetCurrentPenStyle(FYGridLineStyle);

  for I := 0 to FXAxisLabelCount do
  begin
    X := FDrawingArea.Left + XSpacing * I;
    DrawLine(X, FDrawingArea.Bottom, X, FDrawingArea.Top, FYGridLineColor, FYGridLineWidth * FScale);
    DrawText(X, FDrawingArea.Bottom + YShift, GetString(FDataArea.Left + FXIncrementF * I), FXAxisFontColor, taCenter, taAlignTop);
  end;

  if FXAxisLabel <> '' then
  begin
    DrawText(FDrawingArea.Right, FDrawingArea.Bottom + YShift - GetTextSize(FXAxisLabel).Height,
      FXAxisLabel, FXAxisLabelColor, taCenter, taAlignTop)
  end;

  // Draw X secondary axis and Y labels
  SetCurrentFontAntialias(False);
  SetCurrentFontQuality(FCurrentFontQuality);
  SetCurrentFontName(DefaultFontName);
  SetCurrentFontStyle(FYAxisFontStyle);
  SetCurrentFontColor(FYAxisFontColor);
  SetCurrentFontHeight(Trunc(FYAxisFontHeight * FScale));
  SetCurrentJoinStyle(pjsRound);
  SetCurrentLineCap(pecRound);
  SetCurrentPenStyle(FXGridLineStyle);

  for I := 0 to FYAxisLabelCount do
  begin
    Y := FDrawingArea.Bottom + YSpacing * I;
    DrawLine(FDrawingArea.Left, Y, FDrawingArea.Right, Y, FXGridLineColor, FXGridLineWidth * FScale);
    DrawText(FDrawingArea.Left + XShift, Y, GetString(FDataArea.Bottom + FYIncrementF * I), FYAxisFontColor, taRightJustify, taVerticalCenter);
  end;

  if FYAxisLabel <> '' then
  begin
    DrawText(FDrawingArea.Left + XShift, FDrawingArea.Top + GetTextSize(FYAxisLabel).Height,
      FYAxisLabel, FYAxisLabelColor, taRightJustify, taVerticalCenter);
  end;

  // Draw Chart Title
  SetCurrentFontAntialias(False);
  SetCurrentFontQuality(FCurrentFontQuality);
  SetCurrentFontName(DefaultFontName);
  SetCurrentFontStyle(FTitleFontStyle);
  SetCurrentFontColor(FTitleFontColor);
  SetCurrentFontHeight(Trunc(FTitleFontHeight * FScale));

  YShift := (FSpacer * FScale) * 0.5;
  DrawText((FDrawingArea.Left + FDrawingArea.Right) * 0.5, FDrawingArea.Top + YShift, FTitle, FTitleFontColor, taCenter, taAlignBottom);
end;

procedure TChart.DrawLine(x0, y0, x1, y1: single; APenColor: TBGRAPixel; APenWidth: single);
begin
  if SameValue(APenWidth, 0) then Exit;

  FBit.DrawLineAntialias(
    XToCanvas(x0),
    YToCanvas(y0),
    XToCanvas(x1),
    YToCanvas(y1),
    APenColor, APenWidth);
end;

procedure TChart.DrawText(X, Y: single; const AText: string; const ATextColor: TBGRAPixel; AAlign: TAlignment; AVertAlign: TVerticalAlignment);
var
  ShiftX, ShiftY: double;
  TxtSize: TSize;
begin
  TxtSize := GetTextSize(AText);

  case AAlign of
    taLeftJustify:    ShiftX := 0;
    taRightJustify:   ShiftX := - TxtSize.Width;
    taCenter:         ShiftX := - TxtSize.Width / 2;
  end;

  case AVertAlign of
    taAlignTop:       ShiftY := 0;
    taAlignBottom:    ShiftY := + TxtSize.Height;
    taVerticalCenter: ShiftY := + TxtSize.Height / 2;
  end;

  ChartTextLock.Enter;
  try
    FBit.TextOut(
      XToCanvas(X + ShiftX),
      YToCanvas(Y + ShiftY),
      AText, ATextColor, taLeftJustify);
  finally
    ChartTextLock.Leave;
  end;
end;

procedure TChart.DrawLegend;
var
  I: longint;
  Item: TChartItem;
  X, Y: single;
  TxtSize: TSize;
begin
  if not FLegendEnabled then Exit;

  X := FDrawingArea.Right;
  Y := FDrawingArea.Top;
  for I := 0 to FItems.Count -1 do
  begin
    Item := TChartItem(FItems[I]);

    if (Item is TChartPolygonItem) or
       (Item is TChartPolyLineItem) then
    begin
      if Item.FCaption <> '' then
      begin
        SetCurrentJoinStyle(pjsRound);
        SetCurrentLineCap(pecRound);
        SetCurrentPenStyle(TChartPolyLineItem(Item).FPenStyle);

        DrawLine(
          XToCanvas(X + FSpacer * FScale),
          YToCanvas(Y),
          XToCanvas(X + FSpacer * FScale + FLegendLineLength * FScale),
          YToCanvas(Y),
          TChartPolyLineItem(Item).FPenColor,
          TChartPolyLineItem(Item).FPenWidth * FScale);

        SetCurrentFontAntialias(False);
        SetCurrentFontQuality(FCurrentFontQuality);
        SetCurrentFontName(Item.FFontName);
        SetCurrentFontHeight(Trunc(Item.FFontHeight * FScale));
        SetCurrentFontStyle(Item.FFontStyle);
        SetcurrentFontcolor(Item.FFontColor);

        TxtSize := GetTextSize(Item.FCaption);

        DrawText(
          X + (FSpacer * FScale * 1.5) + FLegendLineLength * FScale,
          Y,
          Item.FCaption,
          Item.FFontColor,
          taLeftJustify,
          taVerticalCenter);

        Y := Y - TxtSize.Height - (FSpacer * FScale) * 0.25;
      end;
    end;

  end;
end;

function TChart.XToCanvas(X: single): single;
begin
  Result := X;
end;

function TChart.YToCanvas(Y: single): single;
begin
  Result := FHeight - Y;
end;

function GetCoefficent(const P1, P2: TPointF; var m, q: single): boolean;
begin
  Result := P2.x <> P1.x;
  if Result then
  begin
    m := (P2.y - P1.y) / (P2.x - P1.x);
    q := (P2.y - P2.x * m);
  end;
end;

function TChart.PxFromAreaFToArea(const AX: single): single;
begin
  result := FDrawingArea.Left + (AX - FDataArea.Left) * FXScaleF;
end;

function TChart.PyFromAreaFToArea(const AY: single): single;
begin
  result := FDrawingArea.Bottom + (AY - FDataArea.Bottom) * FYScaleF;
end;

procedure TChart.DrawPixel(AItem: TChartPixelItem);
begin
  FBit.SetPixel(
    Trunc(XToCanvas(PxFromAreaFToArea(AItem.FX))),
    Trunc(YToCanvas(PyFromAreaFToArea(AItem.FY))), AItem.FPenColor);
end;

procedure TChart.DrawPolygon(AItem: TChartPolygonItem);
var
  i: longint;
  T: array of TPointF = nil;
  Tex: TBGRABitmap;
begin
  SetCurrentJoinStyle(pjsRound);
  SetCurrentLineCap(pecRound);
  SetCurrentPenStyle(AItem.FPenStyle);

  SetLength(T, Length(AItem.FPoints));
  for i := Low(AItem.FPoints) to High(AItem.FPoints) do
  begin
    T[i].X := XToCanvas(PxFromAreaFToArea(AItem.FPoints[i].X));
    T[i].Y := YToCanvas(PyFromAreaFToArea(AItem.FPoints[i].Y));
  end;

  Tex := FBit.CreateBrushTexture(
           bsFDiagonal,
           AItem.FTextureColor,
           AItem.FTextureBackgroundColor,
           Trunc(AItem.FTextureWidth*FScale),
           Trunc(AItem.FTextureHeight*FScale),
           AItem.FTexturePenWidth*FScale) as TBGRABitmap;

  FBit.FillPolyAntialias(T, Tex);
  FBit.DrawPolygonAntialias(T, AItem.FPenColor, AItem.FPenWidth * FScale, BGRA(255, 255, 255, 0));
  Tex.Free;
end;

procedure TChart.DrawPolyLine(AItem: TChartPolyLineItem);
var
  i: longint;
  T: ArrayOfTPointF = nil;
  m, q: single;
begin
  SetCurrentJoinStyle(pjsRound);
  SetCurrentLineCap(pecRound);
  SetCurrentPenStyle(AItem.FPenStyle);

  SetLength(T, Length(AItem.FPoints));
  for i := Low(AItem.FPoints) to High(AItem.FPoints) do
  begin
    T[i].X := AItem.FPoints[i].X;
    T[i].Y := AItem.FPoints[i].Y;
  end;

  if AItem.FExtend then
  begin
    i := Low(T);
    if SameValue(T[i + 1].Distance(T[i]), 0) then
    begin
      if GetCoefficent(T[i + 1], T[i], m, q) then
      begin
        T[i].X := FDataArea.Left;
        T[i].Y := Max(Min(m * FDataArea.Left + q, FDataArea.Top), FDataArea.Bottom);

        if m = 0 then
          T[i].X := FDataArea.Left
        else
          T[i].X := (T[i].Y - q) / m;
      end else
      begin
        T[i].Y := FDataArea.Bottom;
      end;
    end;

    i := High(T);
    if SameValue(T[i - 1].Distance(T[i]), 0) then
    begin
      if GetCoefficent(T[i - 1], T[i], m, q) then
      begin
        T[i].X := FDataArea.Right;
        T[i].Y := Max(Min(m * FDataArea.Right + q, FDataArea.Top), FDataArea.Bottom);

        if m = 0 then
          T[i].X := FDataArea.Right
        else
          T[i].X := (T[i].Y - q) / m;
      end else
      begin
        T[i].Y := FDataArea.Top;
      end;
    end;
  end;

  for i := Low(T) to High(T) do
  begin
    T[i].X := XToCanvas(PxFromAreaFToArea(T[i].X));
    T[i].Y := YToCanvas(PyFromAreaFToArea(T[i].Y));
  end;

  FBit.DrawPolyLineAntialias(T, AItem.FPenColor, AItem.FPenWidth * FScale, BGRA(255, 255, 255, 0));
  T := nil;
end;

procedure TChart.DrawDotLabel(AItem: TChartDotLabelItem);
begin
  SetCurrentJoinStyle(pjsRound);
  SetCurrentLineCap(pecRound);
  SetCurrentPenStyle(AItem.FPenStyle);

  FBit.EllipseAntialias(
    XToCanvas(PxFromAreaFToArea(AItem.FX)),
    YToCanvas(PyFromAreaFToArea(AItem.FY)),
    AItem.FRadius*FScale,
    AItem.FRadius*FScale,
    AItem.FPenColor,
    AItem.FPenWidth,
    AItem.FPenColor);

  SetCurrentFontAntialias(False);
  SetCurrentFontQuality(FCurrentFontQuality);
  SetCurrentFontName(AItem.FFontName);
  SetCurrentFontHeight(Trunc(AItem.FFontHeight * FScale));
  SetCurrentFontStyle(AItem.FFontStyle);
  SetcurrentFontColor(AItem.FFontColor);

  DrawText(
    PxFromAreaFToArea(AItem.FX) + AItem.FShiftX * FScale,
    PyFromAreaFToArea(AItem.FY) + AItem.FShiftY * FScale,
    AItem.FCaption,
    AItem.FFontColor,
    AItem.FAlign,
    AItem.FVertAlign);
end;

procedure TChart.DrawLabel(AItem: TChartLabelItem);
begin
  SetCurrentFontAntialias(False);
  SetCurrentFontQuality(FCurrentFontQuality);
  SetCurrentFontName(AItem.FFontName);
  SetCurrentFontHeight(Trunc(AItem.FFontHeight * FScale));
  SetCurrentFontStyle(AItem.FFontStyle);
  SetCurrentFontColor(AItem.FFontColor);

  DrawText(
    PxFromAreaFToArea(AItem.FX) + AItem.FShiftX * FScale,
    PyFromAreaFToArea(AItem.FY) + AItem.FShiftY * FScale,
    AItem.FCaption,
    AItem.FFontColor,
    AItem.FAlign,
    AItem.FVertAlign);
end;

procedure TChart.DrawItems;
var
  I: longint;
  Item: TChartItem;
begin
  for I := 0 to FItems.Count -1 do
  begin
    Item := TChartItem(FItems[I]);
    if Item.ClassType = TChartPixelItem then
    begin
      DrawPixel(Item as TChartPixelItem);
    end else
    if Item.ClassType = TChartPolygonItem then
    begin
      DrawPolygon(Item as TChartPolygonItem);
    end else
    if Item is TChartPolyLineItem then
    begin
      DrawPolyLine(Item as TChartPolyLineItem);
    end else
    if Item is TChartDotLabelItem then
    begin
      DrawDotLabel(Item as TChartDotLabelItem);
    end else
    if Item is TChartLabelItem then
    begin
      DrawLabel(Item as TChartLabelItem);
    end;
  end;
end;

procedure TChart.Draw(ABitmap: TBGRABitmap; AWidth, AHeight: longint; AOpaque: boolean = True);
var
  Size1, Size2, Size3: TSize;
begin
  FWidth  := AWidth;
  FHeight := AHeight;
  FBit.SetSize(FWidth, FHeight);
  FBit.Fill(FBackgroundColor);

  CalculateDataArea;

  Size1 := GetXAxisLabelSize;
  Size2 := GetYAxisLabelSize;
  Size3 := GetTitleSize;

  FDrawingArea.Top    := FHeight - Size3.Height      - Trunc(FSpacer * FScale);
  FDrawingArea.Bottom :=           Size1.Height *  2 + Trunc(FSpacer * FScale);
  FDrawingArea.Left   :=           Size2.Width       + Trunc(FSpacer * FScale);
  FDrawingArea.Right  := FWidth  - Size1.Width div 2 - Trunc(FSpacer * FScale);

  if (FDrawingArea.Width  > 0) and
     (FDrawingArea.Height > 0) then
  begin
    if FIsNeededCalcXLabelCount then FXAxisLabelCount := FDrawingArea.Height div (Size2.Height + Trunc(FSpacer * FScale));
    if FIsNeededCalcYLabelCount then FYAxisLabelCount := FDrawingArea.Width  div (Size1.Width  + Trunc(FSpacer * FScale));

    if (FXAxisLabelCount > 0) and
       (FYAxisLabelCount > 0) then
    begin
      if FIsNeededCalcXDeltaF then FXIncrementF := GetDelta(FXAxisLabelCount, FDataArea.Right - FDataArea.Left);
      if FIsNeededCalcYDeltaF then FYIncrementF := GetDelta(FYAxisLabelCount, FDataArea.Top   - FDataArea.Bottom);

      if (FXIncrementF > 0) and
         (FYIncrementF > 0) then
      begin
        while (FDataArea.Left   + ((FXAxisLabelCount -1) * FXIncrementF) > FDataArea.Right) do Dec(FXAxisLabelCount);
        while (FDataArea.Bottom + ((FYAxisLabelCount -1) * FYIncrementF) > FDataArea.Top  ) do Dec(FYAxisLabelCount);

        FDataArea.Right := FDataArea.Left   + FXIncrementF * FXAxisLabelCount;
        FDataArea.Top   := FDataArea.Bottom + FYIncrementF * FYAxisLabelCount;
        FXScaleF := (FDrawingArea.Right - FDrawingArea.Left  ) / (FDataArea.Right - FDataArea.Left);
        FYScaleF := (FDrawingArea.Top   - FDrawingArea.Bottom) / (FDataArea.Top   - FDataArea.Bottom);

        DrawGrid;
        DrawItems;
        DrawLegend;
      end;
    end;
  end;

  SetCurrentJoinStyle(pjsRound);
  SetCurrentLineCap(pecRound);
  SetCurrentPenStyle(FXAxisLineStyle);
  DrawLine(
    FDrawingArea.Left,
    FDrawingArea.Bottom,
    FDrawingArea.Right,
    FDrawingArea.Bottom,
    FXAxisLineColor,
    FXAxisLineWidth * FScale);

  SetCurrentJoinStyle(pjsRound);
  SetCurrentLineCap(pecRound);
  SetCurrentPenStyle(FYAxisLineStyle);
  DrawLine(
    FDrawingArea.Left,
    FDrawingArea.Bottom,
    FDrawingArea.Left,
    FDrawingArea.Top,
    FYAxisLineColor,
    FYAxisLineWidth * FScale);

  ABitmap.PutImage(0, 0, FBit, dmSet);
end;

function TChart.GetXMinF: single;
begin
  result := FDataArea.Left;
end;

function TChart.GetXMaxF: single;
begin
  result := FDataArea.Right;
end;

function TChart.GetYMinF: single;
begin
  result := FDataArea.Bottom;
end;

function TChart.GetYMaxF: single;
begin
  result := FDataArea.Top;
end;

procedure TChart.SetXMaxF(Value: single);
begin
  FIsNeededUpdateDrawingArea := True;
  FAdjustXMax := False;
  FDataArea.Right := Value;
end;

procedure TChart.SetXMinF(Value: single);
begin
  FIsNeededUpdateDrawingArea := True;
  FAdjustXMin := False;
  FDataArea.Left := Value;
end;

procedure TChart.SetYMaxF(Value: single);
begin
  FIsNeededUpdateDrawingArea := True;
  FAdjustYMax := False;
  FDataArea.Top := Value;
end;

procedure TChart.SetYMinF(Value: single);
begin
  FIsNeededUpdateDrawingArea := True;
  FAdjustYMin := False;
  FDataArea.Bottom := Value;
end;

procedure TChart.SetXDeltaF(Value: single);
begin
  FXIncrementF := Value;
  FIsNeededCalcXDeltaF := False;
end;

procedure TChart.SetYDeltaF(Value: single);
begin
  FYIncrementF := Value;
  FIsNeededCalcYDeltaF := False;
end;

procedure TChart.SetXCount(Value: longint);
begin
  FXAxisLabelCount := Value;
  FIsNeededCalcXLabelCount := False;
end;

procedure TChart.SetYCount(Value: longint);
begin
  FYAxisLabelCount := Value;
  FIsNeededCalcYLabelCount := False;
end;

// TReportTable

constructor TReportTable.Create;
begin
  inherited Create;
  FBit := TBGRABitmap.Create;
  FBit.FontRenderer := TBGRATextEffectFontRenderer.Create;

  FFontName    := 'default';
  FFontHeight  := 13;
  FFontColor   := BGRA(0, 0, 0, 255);
  FFontStyle   := [fsBold];
  FFontQuality := fqSystemClearType;

  FIsNeededUpdateSize := True;
  FWidth  := 0;
  FHeight := 0;

  FPenColor   := BGRA(0, 0, 0, 255);
  FPenStyle   := psSolid;
  FPenWidth   := 1.0;

  FAutosize        := True;
  FBorderWidth     := DefaultSpacer;
  FBackgroundColor := BGRA(255, 255, 255, 255);

  FLeft  := 0;
  FTop   := 0;
  FScale := 1.0;
  FColumnSpacer := 0;
  FRowSpacer := 0;

  SetSize(0, 0);
end;

destructor TReportTable.Destroy;
begin
  SetSize(0, 0);
  FBit.Destroy;
  inherited Destroy;
end;

function TReportTable.GetItem(aRow, aColumn: longint): string;
begin
  Result := FTable[aRow][aColumn];
end;

procedure TReportTable.SetItem(aRow, aColumn: longint; const S: string);
begin
  FTable[aRow][aColumn] := S;
end;

procedure TReportTable.SetColumnCount(Value: longint);
begin
  SetSize(FRowCount, Value);
end;

procedure TReportTable.SetRowCount(Value: longint);
begin
  SetSize(Value, FColumnCount);
end;

function TReportTable.GetRowAlignment(Index: longint): TVerticalAlignment;
begin
  Result := FRowAlignments[Index];
end;

function TReportTable.GetColumnAlignment(Index: longint): TAlignment;
begin
  Result := FColumnAlignments[Index];
end;

procedure TReportTable.SetRowAlignment(Index: longint; Value: TVerticalAlignment);
begin
  FRowAlignments[Index] := Value;
end;

procedure TReportTable.SetColumnAlignment(Index: longint; Value: TAlignment);
begin
  FColumnAlignments[Index] := Value;
end;

procedure TReportTable.SetSize(aRowCount, aColumnCount: longint);
var
  i: longint;
begin
  SetLength(FRowAlignments, 0);
  SetLength(FColumnAlignments, 0);
  for i := Low(FTable) to High(FTable) do
    SetLength(FTable[i], 0);
  SetLength(FTable, 0);

  FRowCount := aRowCount;
  FColumnCount := aColumnCount;
  if (FRowCount > 0) and (FColumnCount > 0) then
  begin
    SetLength(FTable, FRowCount);
    for i := Low(FTable) to High(FTable) do
    begin
      SetLength(FTable[i], FColumnCount);
    end;

    SetLength(FColumnAlignments, FColumnCount);
    for I := Low(FColumnAlignments) to High(FColumnAlignments) do
    begin
      FColumnAlignments[I] := taLeftJustify;
    end;

    SetLength(FRowAlignments, FRowCount);
    for I := Low(FRowAlignments) to High(FRowAlignments) do
    begin
      FRowAlignments[I] := taVerticalCenter;
    end;
  end;
end;

procedure TReportTable.SetFontName(const Value: string);
begin
  FIsNeededUpdateSize := FFontName <> Value;
  if FIsNeededUpdateSize then
  begin
    FFontName := Value;
  end;
end;

procedure TReportTable.SetFontHeight(const Value: single);
begin
  FIsNeededUpdateSize := FFontHeight <> Value;
  if FIsNeededUpdateSize then
  begin
    FFontHeight := Value;
  end;
end;

procedure TReportTable.SetFontStyle(const Value: TFontStyles);
begin
  FIsNeededUpdateSize := FFontStyle <> Value;
  if FIsNeededUpdateSize then
  begin
    FFontStyle := Value;
  end;
end;

procedure TReportTable.CalculateDrawindArea;
var
  i: longint;
  j: longint;
  CellWidthList: array of longint = nil;
  CellHeightList: array of longint = nil;
  CellSize: TSize;
begin
  FBit.FontAntialias  := True;
  FBit.FontQuality    := FFontQuality;
  FBit.FontName       := FFontName;
  FBit.FontStyle      := FFontStyle;
  FBit.FontHeight     := Trunc(FFontHeight * FScale);
  FIsNeededUpdateSize := False;

  FWidth  := Ceil(2*FBorderWidth*FScale);
  FHeight := Ceil(2*FBorderWidth*FScale);
  if (FRowCount > 0) and (FColumnCount > 0) then
  begin
    SetLength(CellHeightList, FRowCount);
    for i := Low(CellHeightList) to High(CellHeightList) do CellHeightList[i] := 0;

    SetLength(CellWidthList, FColumnCount);
    for i := Low(CellWidthList) to High(CellWidthList) do CellWidthList[i] := 0;

    for i := Low(FTable) to High(FTable) do
      for j := Low(FTable[i]) to High(FTable[i]) do
      begin
        CellSize          := FBit.TextSize(GetItem(i, j));
        CellHeightList[i] := Max(CellHeightList[i], CellSize.Height);
        CellWidthList [j] := Max(CellWidthList [j], CellSize.Width );
      end;

    for i := Low(CellHeightList) to High(CellHeightList) do
      Inc(FHeight, CellHeightList[i] + Ceil(FRowSpacer*FScale));

    for i := Low(CellWidthList ) to High(CellWidthList ) do
      Inc(FWidth, CellWidthList[i] + Ceil(FColumnSpacer*FScale));

    CellHeightList := nil;
    CellWidthList := nil;
  end;
end;

function TReportTable.GetWidth: longint;
begin
  if FIsNeededUpdateSize then
  begin
    CalculateDrawindArea;
  end;
  result := FWidth;
end;

function TReportTable.GetHeight: longint;
begin
  if FIsNeededUpdateSize then
  begin
    CalculateDrawindArea;
  end;
  result := FHeight;
end;

procedure TReportTable.Draw(ABitmap: TBGRABitmap; AWidth, AHeight: longint; AOpaque: boolean);
var
  i: longint;
  j: longint;
  x: array of longint = nil;
  y: array of longint = nil;
  xsum, xoffset: single;
  ysum, yoffset: single;
begin
  if FIsNeededUpdateSize then CalculateDrawindArea;
  if FAutosize then
    FBit.SetSize(GetWidth, GetHeight)
  else
    FBit.SetSize(AWidth, AHeight);
  FBit.Fill(FBackgroundColor);

  SetLength(y, fRowCount    + 1);
  SetLength(x, fColumnCount + 1);
  for i := Low(y) to High(y) do y[i] := Trunc(FBorderWidth*FScale);
  for i := Low(x) to High(x) do x[i] := Trunc(FBorderWidth*FScale);

  for i := Low(fTable) to High(fTable) do
  begin
    for j := Low(fTable[i]) to High(fTable[i]) do
    begin
      x[j + 1] := Max(x[j + 1], FBit.TextSize(GetItem(i, j)).Width  + Trunc(FColumnSpacer*FScale));
      y[i + 1] := Max(y[i + 1], FBit.TextSize(GetItem(i, j)).Height + Trunc(FRowSpacer   *FScale));
    end;
  end;

  FBit.JoinStyle := pjsRound;
  FBit.LineCap   := pecRound;
  FBit.PenStyle  := FPenStyle;

  // Draw horizontal lines
  xsum := 0;
  ysum := 0;
  for i := Low(x) to High(x) do xsum := xsum + x[i];
  for i := Low(y) to High(y) do
  begin
    ysum := ysum + y[i];

    FBit.DrawLineAntialias(
      Trunc(FBorderWidth * FScale),
      ysum,
      xsum,
      ysum,
      FPenColor, FPenWidth * FScale);
  end;

  // Draw vertical lines
  xsum := 0;
  ysum := 0;
  for i := Low(y) to High(y) do ysum := ysum + y[i];
  for i := Low(x) to High(x) do
  begin
    xsum := xsum + x[i];

    FBit.DrawLineAntialias(
      xsum,
      Trunc(FBorderWidth * FScale),
      xsum,
      ysum,
      FPenColor, FPenWidth * FScale);
  end;

  xsum := 0;
  ysum := 0;
  for i := Low(y) to High(y) -1 do
  begin
    xsum := 0;
    ysum := ysum + y[i];
    for j := Low(x) to High(x) -1 do
    begin
      xsum := xsum + x[j];

      case FRowAlignments[i] of
        taAlignTop      : yoffset := (FRowSpacer*FScale)*0.5;
        taAlignBottom   : yoffset := (FRowSpacer*FScale)*0.5;
        taVerticalCenter: yoffset := (FRowSpacer*FScale)*0.5;
      end;

      case FColumnAlignments[j] of
        taLeftJustify   : xoffset :=            (FColumnSpacer*FScale)*0.5;
        taRightJustify  : xoffset := (x[j+1]) - (FColumnSpacer*FScale)*0.5;
        taCenter        : xoffset := (x[j+1])/2;
      end;

      FBit.TextOut(
        xsum + xoffset,
        ysum + yoffset,
        FTable[i, j], FFontColor, FColumnAlignments[j]);
    end;
  end;
  x := nil;
  y := nil;

  // Draw
  FBit.InvalidateBitmap;
  ABitmap.PutImage(0, 0, FBit, dmSet);
end;

// TSpringDrawing

constructor TSpringDrawing.Create;
begin
  inherited Create;
  Fd := 0;
  FDm := 0;
  FLc := 0;
  FLx := 0;
  Fn := 0;
  Fnt1 := 0;
  Fnt2 := 0;
  FPitchF := 0;

  FFontHeight := 12;

  fClockWise := True;
  FGroundEnds := True;
  fClosedEnds := True;
  FCaption := '';
  FScale := 1.0;
  FAutoFit := True;
  FAutoScale := 1.0;
end;

destructor TSpringDrawing.Destroy;
begin
  inherited Destroy;
end;

function TSpringDrawing.xt(const t: double): double;
begin
  if fClockWise then
    Result := (FDm / 2) * System.Cos(2*pi*t + pi)
  else
    Result := (FDm / 2) * System.Cos(2*pi*t);
end;

function TSpringDrawing.yt(const t: double): double;
begin
  Result := (FDm / 2) * System.Sin(2*pi*t);
end;

function TSpringDrawing.zt(const t: double): double;
begin
  Result := 0;
  if (t > 0) and (t <= Fnt2) then
  begin
    Result := t * Fd;
  end;

  if (t > Fnt2) and (t <= (Fnt2 + Fn)) then
  begin
    Result := Fnt2 * Fd + (t - Fnt2) * FPitchF;
  end;

  if (t > (Fnt2 + Fn)) then
  begin
    Result := Fn * FPitchF + (t - Fn) * Fd;
  end;
end;

function TSpringDrawing.PreCheck: boolean;
begin
  Result := True;
  if Fd     <= 0  then Result := False;
  if FDm    <= 0  then Result := False;
  if FLc    <= 0  then Result := False;
  if FLx    < FLc then Result := False;
  if Fn     <= 0  then Result := False;
  if Fnt1   <  0  then Result := False;
  if Fnt2   <  0  then Result := False;
  if FScale <= 0  then Result := False;
end;

procedure TSpringDrawing.DrawInSection(ABitmap: TBGRABitmap; aWidth, aHeight: longint);
begin
  FWidth  := aWidth;
  FHeight := aHeight;
  DrawInSection(ABitmap);
end;

procedure TSpringDrawing.DrawInProfile(ABitmap: TBGRABitmap; aWidth, aHeight: longint);
begin
  FWidth  := aWidth;
  FHeight := aHeight;
  DrawInProfile(ABitmap);
end;

function TSpringDrawing.XToCanvas(X: single): single;
begin
  Result := X;
end;

function TSpringDrawing.YToCanvas(Y: single): single;
begin
  Result := FHeight - Y;
end;

procedure TSpringDrawing.DrawInSection(ABitmap: TBGRABitmap);
var
  CenterPosition: double;
  x0, x1: double;
  y0, y1: double;
  alpha, t: double;
  Tex: TBGRABitmap;

  FXMin: longint;
  FYMin: longint;
  FXMax: longint;
  FYMax: longint;

  FXMinF: double;
  FYMinF: double;
  FXMaxF: double;
  FYMaxF: double;
begin
  CenterPosition := FWidth div 2;
  FBit := TBGRABitmap.Create;
  FBit.SetSize(FWidth, FHeight);
  FBit.Fill(FBackgroundColor);

  FBit.FontAntialias := True;
  FBit.FontQuality   := fqSystemClearType;
  FBit.FontName      := FFontName;
  FBit.FontStyle     := FFontStyle;
  FBit.FontHeight    := Trunc(FFontHeight * FScale);
  FBitCharSize       := FBit.TextSize('M');


  FXMin := FSpacer;
  FXMax := FWidth - Trunc(FSpacer*FScale * 0.5);

  FYMin := FBitCharSize.Height + Trunc(FSpacer*FScale * 1.5);
  FYMax := FHeight - Trunc(FSpacer*FScale * 0.5);

  if PreCheck then
  begin
    FPitchF := (Lx - Lc) / Fn + Fd;
    FXMinF  := 0;
    FXMaxF  := FDm + Fd;
    FYMinF  := 0;
    FYMaxF  := zt(Fnt2 + Fn + Fnt1) + Fd;

    if FAutoFit then
    begin
      FAutoScale := Min((FXMax - FXMin)/(FXMaxF - FXMinF), (FYMax - FYMin)/(FYMaxF - FYMinF));
    end;

    x0 := 0;
    y0 := 0;
    t  := -0.5;
    while t < (Fnt2 + Fn + Fnt1) do
    begin
      t := t + 0.5;
      if fClockWise then
        x1 := CenterPosition + xt(t + 0.5) * FAutoScale
      else
        x1 := CenterPosition + xt(t) * FAutoScale;
      y1 := FYMin + (zt(t) + Fd / 2) * FAutoScale;

      Tex := FBit.CreateBrushTexture(bsFDiagonal, FTextureColor, FTextureBackgroundColor,
        Trunc(FTextureWidth*FScale), Trunc(FTextureHeight*FScale), FTexturePenWidth * Min(2, FScale)) as TBGRABitmap;
      FBit.FillEllipseAntialias(
        XToCanvas(x1),
        YToCanvas(y1),
        Fd / 2 * FAutoScale, Fd / 2 * FAutoScale, Tex);
      Tex.Destroy;

      FBit.EllipseAntialias(
        XToCanvas(x1),
        YToCanvas(y1),
        Fd / 2 * FAutoScale,
        Fd / 2 * FAutoScale,
        FPenColor,
        FPenWidth * Min(2, FScale), BGRA(255, 255, 255, 0));

      if (t > 0) and ((t mod 1) > 0) then
      begin
        alpha := arctan2((y1 - y0), -(x1 - x0));

        FBit.DrawLineAntialias(
          XToCanvas(x0 + (Fd/2) * System.Sin(alpha) * FAutoScale),
          YToCanvas(y0 + (Fd/2) * System.Cos(alpha) * FAutoScale),
          XToCanvas(x1 + (Fd/2) * System.Sin(alpha) * FAutoScale),
          YToCanvas(y1 + (Fd/2) * System.Cos(alpha) * FAutoScale),
          FPenColor, FPenWidth * Min(2.0, FScale), False);

        FBit.DrawLineAntialias(
          XToCanvas(x0 - (Fd/2) * System.Sin(alpha) * FAutoScale),
          YToCanvas(y0 - (Fd/2) * System.Cos(alpha) * FAutoScale),
          XToCanvas(x1 - (Fd/2) * System.Sin(alpha) * FAutoScale),
          YToCanvas(y1 - (Fd/2) * System.Cos(alpha) * FAutoScale),
          FPenColor, FPenWidth * Min(2.0, FScale), False);
      end;
      x0 := x1;
      y0 := y1;
    end;

    // Draw Ends
    x0 := CenterPosition - (FDm + Fd) / 2 * FAutoScale;
    x1 := CenterPosition + (FDm + Fd) / 2 * FAutoScale;
    if FGroundEnds then
      y0 := FYMin + Fd / 2 * FAutoScale
    else
      y0 := FYMin;
    y1 := y0;

    if FGroundEnds then
    begin
      FBit.FillRect(
        Trunc(XToCanvas(x0 - 2)),
        Trunc(YToCanvas(0)),
        Trunc(XToCanvas(x1 + 2)),
        Trunc(YToCanvas(y1)),
        FBackgroundColor);

      FBit.DrawLineAntialias(
        XToCanvas(x0),
        YToCanvas(y0),
        XToCanvas(x1),
        YToCanvas(y1),
        FPenColor, FPenWidth * Min(2, FScale), False);
    end;

    x0 := CenterPosition - (FDm + Fd) / 2 * FAutoScale;
    x1 := CenterPosition + (FDm + Fd) / 2 * FAutoScale;
    if FGroundEnds then
      y0 := FYMin + (zt(Fnt2 + Fn + Fnt1)) * FAutoScale
    else
      y0 := FYMin + (zt(Fnt2 + Fn + Fnt1) + Fd / 2) * FAutoScale;
    y1 := y0;

    if FGroundEnds then
    begin
      FBit.FillRect(
        Trunc(XToCanvas(x0 - 2)),
        Trunc(YToCanvas(fHeight)),
        Trunc(XToCanvas(x1 + 2)),
        Trunc(YToCanvas(y1)),
        FBackgroundColor);

      FBit.DrawLineAntialias(
        XToCanvas(x0),
        YToCanvas(y0),
        XToCanvas(x1),
        YToCanvas(y1),
        FPenColor, FPenWidth * Min(2, FScale), False);
    end;
    // Draw center line
    FBit.PenStyle := psDashDot;
    FBit.DrawLineAntialias(
      XToCanvas(CenterPosition),
      YToCanvas(FYMin),
      XToCanvas(CenterPosition),
      YToCanvas(FYMin + (FYMaxF - FYMinF) * FAutoScale),
      FCenterLineColor, FCenterLineWidth * Min(2, FScale), False);
    // Draw caption
    x0 := CenterPosition;
    y0 := FSpacer*FScale + FBitCharSize.Height;
    FBit.TextOut(
      XToCanvas(x0),
      YToCanvas(y0),
      FCaption, FFontColor, taCenter);
    // Draw
  end;

  FBit.InvalidateBitmap;
  ABitmap.PutImage(0, 0, FBit, dmSet);
  FBit.Destroy;
end;

procedure TSpringDrawing.DrawInProfile(ABitmap: TBGRABitmap);
var
  CenterPosition: double;
  x0, x1: double;
  y0, y1: double;
  nx: double;
  alpha, beta: double;
  ctx: TBGRACanvas2D;

  FXMin: longint;
  FYMin: longint;
  FXMax: longint;
  FYMax: longint;

  FXMinF: double;
  FYMinF: double;
  FXMaxF: double;
  FYMaxF: double;
begin
  CenterPosition := FWidth div 2;
  FBit := TBGRABitmap.Create;
  FBit.SetSize(FWidth, FHeight);
  FBit.Fill(FBackgroundColor);

  FBit.FontAntialias := True;
  FBit.FontQuality   := fqSystemClearType;
  FBit.FontName      := FFontName;
  FBit.FontStyle     := FFontStyle;
  FBit.FontHeight    := Trunc(FFontHeight * FScale);
  FBitCharSize       := FBit.TextSize('M');


  FXMin := FSpacer;
  FXMax := FWidth - Trunc(FSpacer*FScale * 0.5);

  FYMin := FBitCharSize.Height + Trunc(FSpacer*FScale * 1.5);
  FYMax := FHeight - Trunc(FSpacer*FScale * 0.5);

  if PreCheck then
  begin
    FPitchF := (Lx - Lc) / Fn + Fd;
    FXMinF  := 0;
    FXMaxF  := FDm + Fd;
    FYMinF  := 0;
    FYMaxF  := zt(Fnt2 + Fn + Fnt1) + Fd;

    if FAutoFit then
    begin
      FAutoScale :=
        Min((FXMax - FXMin)/(FXMaxF - FXMinF),
            (FYMax - FYMin)/(FYMaxF - FYMinF));
    end;

    nx := 0.0;
    while nx < (Fnt2 + Fn + Fnt1) do
    begin
      x0 := CenterPosition + xt(nx      ) * FAutoScale;
      x1 := CenterPosition + xt(nx + 0.5) * FAutoScale;

      y0 := FYMin + (zt(nx      ) + Fd/2) * FAutoScale;
      y1 := FYMin + (zt(nx + 0.5) + Fd/2) * FAutoScale;

      alpha := arctan2((y1 - y0), -(x1 - x0));

      ctx := FBit.Canvas2D;
      ctx.fillStyle(FTextureBackgroundColor);
      ctx.strokeStyle(FPenColor);
      ctx.lineWidth := FPenWidth * Min(2.0, FScale);
      ctx.beginPath();

      ctx.moveTo(
        XToCanvas(x0 + Fd/2 * System.Sin(alpha) * FAutoScale),
        YToCanvas(y0 + Fd/2 * System.Cos(alpha) * FAutoScale));

      ctx.arc(
        XToCanvas(x0),
        YToCanvas(y0),
        Fd/2 * FAutoScale,
        1.5*pi +alpha,
        0.5*pi +alpha);

      ctx.lineTo(
        XToCanvas(x1 - Fd/2 * System.Sin(alpha) * FAutoScale),
        YToCanvas(y1 - Fd/2 * System.Cos(alpha) * FAutoScale));

      ctx.arc(
        XToCanvas(x1),
        YToCanvas(y1),
        Fd/2 * FAutoScale,
        0.5*pi +alpha,
        1.5*pi +alpha);

      ctx.closePath();
      ctx.fill();
      ctx.stroke();

      if nx = 0.0 then
      begin
        x0 := CenterPosition + xt(0) * FAutoScale;
        y0 := FYMin + (zt(0) + Fd/2) * FAutoScale;

        ctx.beginPath();
        ctx.moveTo(
          XToCanvas(x0),
          YToCanvas(y0 + Fd/2 * FAutoScale));

        ctx.arc(
          XToCanvas(x0),
          YToCanvas(y0),
          Fd/2 * FAutoScale,
          1.5*pi,
          0.5*pi, FClockWise);

        ctx.lineTo(
          XToCanvas(CenterPosition),
          YToCanvas(FYMin));

        ctx.lineTo(
          XToCanvas(CenterPosition),
          YToCanvas(FYMin + Fd*FAutoScale));

        ctx.closePath();
        ctx.fill();
        ctx.stroke();
      end else
      begin
        x0 := CenterPosition + xt(nx - 0.5) * FAutoScale;
        x1 := CenterPosition + xt(nx      ) * FAutoScale;

        y0 := FYMin + (zt(nx - 0.5) + Fd/2) * FAutoScale;
        y1 := FYMin + (zt(nx      ) + Fd/2) * FAutoScale;

        alpha := arctan2((y1 - y0), +(x1 - x0));

        ctx.beginPath();
        ctx.moveTo(
          XToCanvas(x0 + Fd/2 * System.Sin(alpha) * FAutoScale),
          YToCanvas(y0 - Fd/2 * System.Cos(alpha) * FAutoScale));

        ctx.arc(
          XToCanvas(x0),
          YToCanvas(y0),
          Fd/2 * FAutoScale,
          0.5*pi -alpha,
          1.5*pi -alpha);

        ctx.lineTo(
          XToCanvas(x1 - Fd/2 * System.Sin(alpha) * FAutoScale),
          YToCanvas(y1 + Fd/2 * System.Cos(alpha) * FAutoScale));

        ctx.arc(
          XToCanvas(x1),
          YToCanvas(y1),
          Fd/2 * FAutoScale,
          1.5*pi -alpha,
          0.5*pi -alpha);

        ctx.closePath();
        ctx.fill();
        ctx.stroke();
      end;
      nx := nx + 1.0;
    end;

    if nx > 0 then
    begin
      x0 := CenterPosition + xt(nx - 0.5) * FAutoScale;
      y0 := FYMin + (zt(nx - 0.5) + Fd/2) * FAutoScale;

      ctx.beginPath();
      ctx.moveTo(
        XToCanvas(x0),
        YToCanvas(y0 - Fd/2 * FAutoScale));

      ctx.arc(
        XToCanvas(x0),
        YToCanvas(y0),
        Fd/2 * FAutoScale,
        0.5*pi,
        1.5*pi, FClockWise);

      ctx.lineTo(
        XToCanvas(CenterPosition),
        YToCanvas(y0 + Fd/2 * FAutoScale));

      ctx.lineTo(
        XToCanvas(CenterPosition),
        YToCanvas(y0 - Fd/2 * FAutoScale));

      ctx.closePath();
      ctx.fill();
      ctx.stroke();
    end;
    if FGroundEnds then
    begin




    end;
    // Draw center line
    FBit.PenStyle := psDashDot;
    FBit.DrawLineAntialias(
      XToCanvas(CenterPosition),
      YToCanvas(FYMin),
      XToCanvas(CenterPosition),
      YToCanvas(FYMin + (FYMaxF - FYMinF) * FAutoScale),
      FCenterLineColor, FCenterLineWidth * Min(2, FScale), False);
    // Draw caption
    x0 := CenterPosition;
    y0 := FSpacer*FScale + FBitCharSize.Height;
    FBit.TextOut(
      XToCanvas(x0),
      YToCanvas(y0),
      FCaption, FFontColor, taCenter);
  end;

  FBit.InvalidateBitmap;
  ABitmap.PutImage(0, 0, FBit, dmSet);
  FBit.Destroy;
end;

// DrawLogo

procedure DrawLogo(ABitmap: TBGRABitmap; aWidth, aHeight: longint);
var
  Bit: TBGRABitmap;
  x, y: longint;
begin
  Bit := TBGRABitmap.Create;
  Bit.SetSize(aWidth, aHeight);
  Bit.Fill(BGRA(127, 127, 127, 255));

  Bit.FontHeight := 30;
  Bit.FontAntialias := True;
  Bit.FontStyle := [fsBold];

  x := -50;
  while x < (aWidth + 50) do
  begin
    y := -50;
    while y < (aHeight + 50) do
    begin
      Bit.TextOutAngle(x, y, 250, ApplicationName, BGRA(150, 150, 150, 255), taLeftJustify);
      Inc(y, Bit.TextSize(ApplicationName).Height + DefaultSpacer div 4);
    end;
    Inc(x, Bit.TextSize(ApplicationName).Width + DefaultSpacer div 4);
  end;

  Bit.InvalidateBitmap;
  ABitmap.PutImage(0, 0, Bit, dmSet);
  Bit.Destroy;
end;

initialization
  ChartTextLock := TCriticalSection.Create;

finalization
  ChartTextLock.Free;

end.
