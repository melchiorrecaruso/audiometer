{
  Description: Report Form.

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

unit ReportFrm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  ExtCtrls, IniPropStorage;

type

  { TReportForm }

  TReportForm = class(TForm)
    BtnPanel: TPanel;
    PropStorage: TIniPropStorage;
    Memo: TMemo;
    SaveDialog: TSaveDialog;
    BorderShape: TShape;
    BkgShape: TShape;
    SaveBtn: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private

  public

  end;

var
  ReportForm: TReportForm;

implementation

{$R *.lfm}

uses
  Common;

{ TReportForm }

procedure TReportForm.FormCreate(Sender: TObject);
begin
  PropStorage.IniFileName := GetAppFile('audiometer.ini');
  PropStorage.Active := True;
  //
  Color := clBlack;
  {$ifdef UNIX}
  Memo.Font.Name := 'Liberation Mono';
  {$endif}
  {$ifdef WINDOWS}
  Memo.Font.Name := 'Courier New';
  {$endif}
  Memo.Font.Color := clYellow;
  Memo.Color := clBlack;
end;

procedure TReportForm.SaveBtnClick(Sender: TObject);
begin
  if SaveDialog.Execute then
  begin
    Memo.Lines.SaveToFile(SaveDialog.FileName);
  end;
end;

end.

