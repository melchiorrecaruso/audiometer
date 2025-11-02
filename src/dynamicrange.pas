{
  Description: Dynamic Range Measurement Routines.

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

unit DynamicRange;

{$mode objfpc}{$h+}
{$modeswitch advancedrecords}

interface

uses
  Common, Sysutils;

type
  TDynamicRangeMeter = record
  private
    FBlocksize: longint;
    FBlockCount: longint;
    FChannelCount: longint;
    FRms2: TDoubleMatrix;
    FPeak: TDoubleMatrix;
    FDynamicRange: TDoubleVector;
  public
    procedure Init;
    procedure Finalize;

    procedure Process(const AChannels: TDoubleMatrix; ASampleCount, ASampleRate: longint);

    function Rms2(AChannel, ABlock: longint): double;
    function Rms2(AChannel: longint): double;
    function Rms2: double;

    function Rms(AChannel: longint): double;
    function Rms: double;

    function Peak(AChannel, ABlock: longint): double;
    function Peak(AChannel: longint): double;
    function Peak: double;

    function DR(AChannel: longint): double;
    function DR: double;

    property BlockCount: longint read FBlockCount;
    property ChannelCount: longint read FChannelCount;
  end;


implementation

uses
  Math;

// TDynamicRangeMeter

function TDynamicRangeMeter.Rms2(AChannel, ABlock: longint): double;
begin
  result := FRms2[AChannel][ABlock];
end;

function TDynamicRangeMeter.Rms2(AChannel: longint): double;
var
  i: longint;
begin
  if FBlockCount = 0 then Exit(0);

  result := 0;
  for i := 0 to FBlockCount -1 do
  begin
    result := result + FRms2[AChannel][i];
  end;
  result := result / FBlockCount;
end;

function TDynamicRangeMeter.Rms2: double;
var
  ch: longint;
begin
  if FChannelCount = 0 then Exit(0);

  result := 0;
  for ch := 0 to FChannelCount -1 do
  begin
    result := result + Rms2(ch);
  end;
  result := result / FChannelCount;
end;

function TDynamicRangeMeter.Peak(AChannel, ABlock: longint): double;
begin
  result := FPeak[AChannel][ABlock];
end;

function TDynamicRangeMeter.Peak(AChannel: longint): double;
var
  i: longint;
begin
  if FBlockCount = 0 then Exit(0);

  result := 0;
  for i := 0 to FBlockCount -1 do
  begin
    result := Max(result, FPeak[AChannel][i]);
  end;
end;

function TDynamicRangeMeter.Peak: double;
var
  ch: longint;
begin
  if FChannelCount = 0 then Exit(0);

  result := 0;
  for ch := 0 to FChannelCount -1 do
  begin
    result := max(result, Peak(ch));
  end;
end;

function TDynamicRangeMeter.Rms(AChannel: longint): double;
begin
  result := Sqrt(Rms2(AChannel));
end;

function TDynamicRangeMeter.Rms: double;
var
  ch: longint;
begin
  if FChannelCount = 0 then Exit(0);

  result := 0;
  for ch := 0 to FChannelCount -1 do
  begin
    result := result + Rms2(ch);
  end;
  result := sqrt(result / ChannelCount);
end;

function TDynamicRangeMeter.DR(AChannel: longint): double;
begin
  result := FDynamicRange[AChannel];
end;

function TDynamicRangeMeter.DR: double;
var
  ch: longint;
begin
  if FChannelCount = 0 then Exit(0);

  result := 0;
  for ch := 0 to FChannelCount -1 do
  begin
    result := result + FDynamicRange[ch];
  end;
  result := RoundTo(result / FChannelCount, -1);
end;

procedure TDynamicRangeMeter.Init;
begin
  Finalize;
end;

procedure TDynamicRangeMeter.Finalize;
begin
  FBlockSize    := 0;
  FBlockCount   := 0;
  FChannelCount := 0;

  SetLength(FRms2, 0, 0);
  SetLength(FPeak, 0, 0);
  SetLength(FDynamicRange, 0);
end;

procedure TDynamicRangeMeter.Process(const AChannels: TDoubleMatrix; ASampleCount, ASampleRate: longint);
var
  ch, i, index, Num: longint;
  CurrEnergy, Peak2nd: TDouble;
  RmsVec, PeakVec: TDoubleVector;
  TailSize: longint;
begin
  Finalize;
  FChannelCount := Length(AChannels);
  if FChannelCount = 0 then Exit;

  FBlockSize  := 3 * ASampleRate;
  FBlockCount := ASampleCount div FBlockSize;
  TailSize    := ASampleCount mod FBlockSize;

  if FBlockCount = 0 then Exit;

  SetLength(FRms2, FChannelCount, FBlockCount + Ord(TailSize > 0));
  SetLength(FPeak, FChannelCount, FBlockCount + Ord(TailSize > 0));
  SetLength(FDynamicRange, FChannelCount);

  for ch := 0 to FChannelCount - 1 do
  begin
    for i := 0 to FBlockCount - 1 do
    begin
      index := i * FBlockSize;
      FRms2[ch][i] := Common.Rms2(@AChannels[ch][index], FBlockSize);
      FPeak[ch][i] := Common.Peak(@AChannels[ch][index], FBlockSize);
    end;

    if TailSize > 0 then
    begin
      index := FBlockCount * FBlockSize;
      FRms2[ch][FBlockCount] := Common.Rms2(@AChannels[ch][index], TailSize);
      FPeak[ch][FBlockCount] := Common.Peak(@AChannels[ch][index], TailSize);
    end;
  end;

  // compute dynamic range values
  SetLength(RmsVec,  FBlockCount + Ord(TailSize > 0));
  SetLength(PeakVec, FBlockCount + Ord(TailSize > 0));
  for ch := 0 to FChannelCount - 1 do
  begin
    for i := Low(RmsVec) to High(RmsVec) do
    begin
      RmsVec [i] := Sqrt(2 * FRms2[ch][i]);
      PeakVec[i] := FPeak[ch][i];
    end;

    QuickSort(RmsVec,  Low(RmsVec), High(RmsVec));
    QuickSort(PeakVec, Low(RmsVec), High(PeakVec));

    Num := Trunc(0.2 * Length(RmsVec));
    if Num < 1 then Num := 1;

    if High(PeakVec) >= 1 then
      Peak2nd := PeakVec[High(PeakVec) - 1]
    else
      Peak2nd := PeakVec[0];

    CurrEnergy := 0;
    for index := Length(RmsVec) - Num to High(RmsVec) do
    begin
      CurrEnergy := CurrEnergy + Sqr(RmsVec[index]);
    end;
    CurrEnergy := CurrEnergy / Num;

    if Peak2nd > 1e-10 then
      FDynamicRange[ch] := -Decibel(Sqrt(CurrEnergy) / Peak2nd)
    else
      FDynamicRange[ch] := 0;
  end;
end;

end.
