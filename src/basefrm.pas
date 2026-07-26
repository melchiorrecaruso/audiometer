{
  Description: Base form with persistent geometry.

  Copyright (C) 2026 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit BaseFrm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Forms, Controls, IniFiles;

type

  { TBaseForm }

  TBaseForm = class(TForm)
  private
    FConfigFile: string;
    FGeometryRestored: Boolean;
    FSavedPPI: Integer;
    FLastWindowState: TWindowState;
    function CurrentPPI: Integer;
    function ScalePPI(AValue, AFromPPI, AToPPI: Integer): Integer;
    procedure ClampToDesktop;
    procedure RestoreGeometry;
    procedure SaveGeometry;
  protected
    procedure DoCreate; override;
    procedure DoShow; override;
    procedure DoDestroy; override;
    procedure Resize; override;
    function SectionName: string; virtual;
  public
    function ReadSetting(const AKey, ADefault: string): string; overload;
    function ReadSetting(const AKey: string; ADefault: Integer): Integer; overload;
    function ReadSetting(const AKey: string; ADefault: Boolean): Boolean; overload;
    procedure WriteSetting(const AKey, AValue: string); overload;
    procedure WriteSetting(const AKey: string; AValue: Integer); overload;
    procedure WriteSetting(const AKey: string; AValue: Boolean); overload;
    property ConfigFile: string read FConfigFile;
  end;

implementation

const
  KeyPPI    = 'PPI';
  KeyLeft   = 'Left';
  KeyTop    = 'Top';
  KeyWidth  = 'Width';
  KeyHeight = 'Height';

{ TBaseForm }

function TBaseForm.SectionName: string;
begin
  Result := Name;
  if Result = '' then
    Result := ClassName;
end;

function TBaseForm.CurrentPPI: Integer;
var
  M: TMonitor;
begin
  M := Monitor;
  if M <> nil then
    Result := M.PixelsPerInch
  else
    Result := Screen.PixelsPerInch;
  if Result <= 0 then
    Result := 96;
end;

function TBaseForm.ScalePPI(AValue, AFromPPI, AToPPI: Integer): Integer;
begin
  if (AFromPPI <= 0) or (AFromPPI = AToPPI) then
    Result := AValue
  else
    Result := Round(Int64(AValue) * AToPPI / AFromPPI);
end;

function TBaseForm.ReadSetting(const AKey, ADefault: string): string;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FConfigFile);
  try
    Result := Ini.ReadString(SectionName, AKey, ADefault);
  finally
    Ini.Free;
  end;
end;

function TBaseForm.ReadSetting(const AKey: string; ADefault: Integer): Integer;
begin
  Result := StrToIntDef(ReadSetting(AKey, IntToStr(ADefault)), ADefault);
end;

function TBaseForm.ReadSetting(const AKey: string; ADefault: Boolean): Boolean;
begin
  Result := ReadSetting(AKey, Ord(ADefault)) <> 0;
end;

procedure TBaseForm.WriteSetting(const AKey, AValue: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FConfigFile);
  try
    Ini.WriteString(SectionName, AKey, AValue);
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure TBaseForm.WriteSetting(const AKey: string; AValue: Integer);
begin
  WriteSetting(AKey, IntToStr(AValue));
end;

procedure TBaseForm.WriteSetting(const AKey: string; AValue: Boolean);
begin
  WriteSetting(AKey, Ord(AValue));
end;

procedure TBaseForm.ClampToDesktop;
var
  R: TRect;
  M: TMonitor;
begin
  M := Screen.MonitorFromRect(BoundsRect);
  if M = nil then
    M := Screen.PrimaryMonitor;
  if M = nil then
    Exit;
  R := M.WorkareaRect;

  if Width > R.Right - R.Left then
    Width := R.Right - R.Left;
  if Height > R.Bottom - R.Top then
    Height := R.Bottom - R.Top;

  if Left + Width > R.Right then
    Left := R.Right - Width;
  if Top + Height > R.Bottom then
    Top := R.Bottom - Height;
  if Left < R.Left then
    Left := R.Left;
  if Top < R.Top then
    Top := R.Top;
end;

procedure TBaseForm.RestoreGeometry;
var
  Ini: TIniFile;
  OldPPI, NewPPI, L, T, W, H: Integer;
begin
  NewPPI := CurrentPPI;
  FSavedPPI := NewPPI;
  FLastWindowState := wsNormal;

  Ini := TIniFile.Create(FConfigFile);
  try
    W := Ini.ReadInteger(SectionName, KeyWidth, 0);
    H := Ini.ReadInteger(SectionName, KeyHeight, 0);
    if (W <= 0) or (H <= 0) then
      Exit;
    OldPPI := Ini.ReadInteger(SectionName, KeyPPI, NewPPI);
    L := Ini.ReadInteger(SectionName, KeyLeft, Left);
    T := Ini.ReadInteger(SectionName, KeyTop, Top);
  finally
    Ini.Free;
  end;

  SetBounds(ScalePPI(L, OldPPI, NewPPI), ScalePPI(T, OldPPI, NewPPI),
            ScalePPI(W, OldPPI, NewPPI), ScalePPI(H, OldPPI, NewPPI));
  ClampToDesktop;
  SetRestoredBounds(Left, Top, Width, Height);
end;

procedure TBaseForm.SaveGeometry;
var
  Ini: TIniFile;
begin
  if not FGeometryRestored then
    Exit;

  Ini := TIniFile.Create(FConfigFile);
  try
    if (RestoredWidth > 0) and (RestoredHeight > 0) then
    begin
      Ini.WriteInteger(SectionName, KeyPPI, FSavedPPI);
      Ini.WriteInteger(SectionName, KeyLeft, RestoredLeft);
      Ini.WriteInteger(SectionName, KeyTop, RestoredTop);
      Ini.WriteInteger(SectionName, KeyWidth, RestoredWidth);
      Ini.WriteInteger(SectionName, KeyHeight, RestoredHeight);
    end;
    Ini.UpdateFile;
  finally
    Ini.Free;
  end;
end;

procedure TBaseForm.Resize;
begin
  inherited Resize;
  if FGeometryRestored and (WindowState in [wsNormal, wsMaximized]) then
    FLastWindowState := WindowState;
end;

procedure TBaseForm.DoCreate;
begin
  Position := poDesigned;
  DefaultMonitor := dmDesktop;
  FLastWindowState := wsNormal;

  FConfigFile := GetAppConfigFile(False);
  ForceDirectories(ExtractFilePath(FConfigFile));

  inherited DoCreate;
end;

procedure TBaseForm.DoShow;
begin
  if not FGeometryRestored then
  begin
    FGeometryRestored := True;
    RestoreGeometry;
  end;
  inherited DoShow;
end;

procedure TBaseForm.DoDestroy;
begin
  Application.RemoveAsyncCalls(Self);
  SaveGeometry;
  inherited DoDestroy;
end;

end.
