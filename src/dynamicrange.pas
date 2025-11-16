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
  Common, SysUtils;

type
  TDynamicRangeMeter = record
  private
    FBlocksize: longint;
    FBlockCount: longint;
    FChannelCount: longint;
    FRms2: TDoubleMatrix;
    FPeak: TDoubleMatrix;
    FDynamicRange: TDoubleVector;
    FTick: TTickMethod;
  public
    procedure Init(const ATick: TTickMethod = nil);
    procedure Finalize;

    function EstimatedTicks(AChannelCount, ASampleCount, ASampleRate: longint): longint;
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
    result := Max(result, Peak(ch));
  end;
end;

function TDynamicRangeMeter.Rms(AChannel: longint): double;
begin
  result := Sqrt(2 * Rms2(AChannel));
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
  result := Sqrt(2 * result / FChannelCount);
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
  result := SimpleRoundTo(result / FChannelCount, 0);
end;

procedure TDynamicRangeMeter.Init(const ATick: TTickMethod = nil);
begin
  Finalize;
  FTick := ATick;
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

function TDynamicRangeMeter.EstimatedTicks(AChannelCount, ASampleCount, ASampleRate: longint): longint;
begin
  result := (ASampleCount div (3 * ASampleRate)) * AChannelCount;
end;

procedure TDynamicRangeMeter.Process(const AChannels: TDoubleMatrix; ASampleCount, ASampleRate: longint);
var
  ch, i, index, Num: longint;
  CurrEnergy, Peak2nd: TDouble;
  Rms2Vec, PeakVec: TDoubleVector;
  TailSize: longint;
begin
  Finalize;
  FChannelCount := Length(AChannels);
  if FChannelCount = 0 then Exit;

  case ASampleRate of
     44100: FBlockSize := 3 * ASampleRate + 180;
     48000: FBlockSize := 3 * ASampleRate;
     88200: FBlockSize := 3 * ASampleRate + 360;
     96000: FBlockSize := 3 * ASampleRate;
    176400: FBlockSize := 3 * ASampleRate + 720;
    192000: FBlockSize := 3 * ASampleRate;
    else    FBlockSize := 3 * ASampleRate;
  end;

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

      if Assigned(FTick) then FTick;
    end;

    if TailSize > 0 then
    begin
      index := FBlockCount * FBlockSize;
      FRms2[ch][FBlockCount] := Common.Rms2(@AChannels[ch][index], TailSize);
      FPeak[ch][FBlockCount] := Common.Peak(@AChannels[ch][index], TailSize);
    end;
  end;
  FBlockCount := FBlockCount + Ord(TailSize > 0);

  // compute dynamic range values
  SetLength(Rms2Vec, FBlockCount);
  SetLength(PeakVec, FBlockCount);
  for ch := 0 to FChannelCount - 1 do
  begin
    for i := 0 to FBlockCount -1 do
    begin
      Rms2Vec[i] := FRms2[ch][i];
      PeakVec[i] := FPeak[ch][i];
    end;

    QuickSort(Rms2Vec, 0, FBlockCount -1);
    QuickSort(PeakVec, 0, FBlockCount -1);

    Num := Trunc(0.2 * FBlockCount);
    if Num < 1 then Num := 1;

    if FBlockCount > 1 then
      Peak2nd := PeakVec[FBlockCount -2]
    else
      Peak2nd := PeakVec[0];

    CurrEnergy := 0;
    for index := FBlockCount - Num to FBlockCount -1 do
    begin
      CurrEnergy := CurrEnergy + Rms2Vec[index];
    end;
    CurrEnergy := CurrEnergy / Num;

    if Peak2nd > 1e-10 then
      FDynamicRange[ch] := -Decibel(Sqrt(2 * CurrEnergy) / Peak2nd)
    else
      FDynamicRange[ch] := 0;
  end;
end;

end.
