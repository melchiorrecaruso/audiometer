{
  Description: Dynamic Range Measurement Routines.

  Copyright (C) 2025-2026 Melchiorre Caruso <melchiorrecaruso@gmail.com>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  You should have received a copy of the GNU General Public License along with
  this program. If not, see <https://www.gnu.org/licenses/>.
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
    FBlockSize: longint;
    FBlockCount: longint;
    FSampleCount: longint;
    FTailSize: longint;
    FChannelCount: longint;
    FRms2: TDoubleMatrix;
    FPeak: TDoubleMatrix;
    FDynamicRange: TDoubleVector;
    FTick: TTickMethod;
  public
    procedure Init(const ATick: TTickMethod = nil);
    procedure Finalize;

    function EstimatedTicks(AChannelCount, ASampleCount,
      ASampleRate: longint): longint;
    procedure Process(const AChannels: TDoubleMatrix;
      ASampleCount, ASampleRate: longint);

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
  if FBlockCount = 0 then
    Exit(0);

  result := 0;
  for i := 0 to FBlockCount - 1 do
  begin
    if (i = FBlockCount - 1) and (FTailSize > 0) then
      result := result + FRms2[AChannel][i] * FTailSize
    else
      result := result + FRms2[AChannel][i] * FBlockSize;
  end;
  result := result / FSampleCount;
end;

function TDynamicRangeMeter.Rms2: double;
var
  ch: longint;
begin
  if FChannelCount = 0 then
    Exit(0);

  result := 0;
  for ch := 0 to FChannelCount - 1 do
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
  if FBlockCount = 0 then
    Exit(0);

  result := 0;
  for i := 0 to FBlockCount - 1 do
  begin
    result := Max(result, FPeak[AChannel][i]);
  end;
end;

function TDynamicRangeMeter.Peak: double;
var
  ch: longint;
begin
  if FChannelCount = 0 then
    Exit(0);

  result := 0;
  for ch := 0 to FChannelCount - 1 do
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
  if FChannelCount = 0 then
    Exit(0);

  result := 0;
  for ch := 0 to FChannelCount - 1 do
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
  if FChannelCount = 0 then
    Exit(0);

  result := 0;
  for ch := 0 to FChannelCount - 1 do
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
  FSampleCount := 0;
  FTailSize := 0;
  FBlockSize := 0;
  FBlockCount := 0;
  FChannelCount := 0;

  SetLength(FRms2, 0, 0);
  SetLength(FPeak, 0, 0);
  SetLength(FDynamicRange, 0);
end;

function TDynamicRangeMeter.EstimatedTicks(AChannelCount, ASampleCount,
  ASampleRate: longint): longint;
var
  BlockSize, Ticks: Int64;
begin
  if (AChannelCount <= 0) or (ASampleRate <= 0) or (ASampleCount <= 0) then
    Exit(0);

  if ASampleRate > High(longint) div 3 - 60 then
    raise EArgumentException.Create('Invalid sample rate');

  BlockSize := Int64(3) * ASampleRate;
  if ASampleRate = 44100 then
    Inc(BlockSize, 180);

  Ticks := (ASampleCount div BlockSize) * AChannelCount;
  if Ticks > High(longint) then
    raise EArgumentException.Create('Estimated tick count exceeds LongInt range');

  result := longint(Ticks);
end;

procedure TDynamicRangeMeter.Process(const AChannels: TDoubleMatrix;
  ASampleCount, ASampleRate: longint);
var
  ch, i, index, Num, NumChannels: longint;
  CurrEnergy, Peak2nd: TDouble;
  Rms2Vec, PeakVec: TDoubleVector;
  TailSize: longint;
begin
  Finalize;

  NumChannels := Length(AChannels);
  if (NumChannels = 0) or (ASampleCount <= 0) then
    Exit;

  if (ASampleRate <= 0) or (ASampleRate > High(longint) div 3 - 60) then
    raise EArgumentException.Create('Invalid sample rate');

  for ch := 0 to NumChannels - 1 do
    if Length(AChannels[ch]) < ASampleCount then
      raise EArgumentException.Create('Sample count exceeds channel length');

  FChannelCount := NumChannels;
  FSampleCount := ASampleCount;

  case ASampleRate of
    44100:
      FBlockSize := 3 * ASampleRate + 180;
    else
      FBlockSize := 3 * ASampleRate;
  end;

  FBlockCount := ASampleCount div FBlockSize;
  TailSize := ASampleCount mod FBlockSize;

  FTailSize := TailSize;

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

      if Assigned(FTick) then
        FTick;
    end;

    if TailSize > 0 then
    begin
      index := FBlockCount * FBlockSize;
      FRms2[ch][FBlockCount] := Common.Rms2(@AChannels[ch][index], TailSize);
      FPeak[ch][FBlockCount] := Common.Peak(@AChannels[ch][index], TailSize);
    end;
  end;
  FBlockCount := FBlockCount + Ord(TailSize > 0);

  // Compute dynamic range values. By convention a partial tail counts as
  // one block in the top-20% energy and second-peak selections, regardless
  // of its duration. Only the global RMS uses sample-count weighting.
  SetLength(Rms2Vec, FBlockCount);
  SetLength(PeakVec, FBlockCount);
  for ch := 0 to FChannelCount - 1 do
  begin
    for i := 0 to FBlockCount - 1 do
    begin
      Rms2Vec[i] := FRms2[ch][i];
      PeakVec[i] := FPeak[ch][i];
    end;

    QuickSort(Rms2Vec, 0, FBlockCount - 1);
    QuickSort(PeakVec, 0, FBlockCount - 1);

    Num := Trunc(0.2 * FBlockCount);
    if Num < 1 then
      Num := 1;

    if FBlockCount > 1 then
      Peak2nd := PeakVec[FBlockCount - 2]
    else
      Peak2nd := PeakVec[0];

    CurrEnergy := 0;
    for index := FBlockCount - Num to FBlockCount - 1 do
    begin
      CurrEnergy := CurrEnergy + Rms2Vec[index];
    end;
    CurrEnergy := CurrEnergy / Num;

    if Peak2nd > 0 then
      FDynamicRange[ch] := -Decibel(Sqrt(2 * CurrEnergy) / Peak2nd)
    else
      FDynamicRange[ch] := 0;
  end;
end;

end.
