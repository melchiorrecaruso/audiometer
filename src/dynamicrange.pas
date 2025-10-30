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
    FRms2: arrayofarrayofdouble;
    FPeak: arrayofarrayofdouble;
    FDynamicRange: arrayofdouble;
  public
    procedure Init;
    procedure Finalize;

    procedure Process(const AChannels: TChannels; ASamplecount, ASamplerate: longint);

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
  Block: longint;
begin
  if FBlockCount = 0 then Exit(0);

  result := 0;
  for Block := 0 to FBlockCount -1 do
  begin
    result := result + FRms2[AChannel][Block];
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
  Block: longint;
begin
  result := 0;
  for Block := 0 to FBlockCount -1 do
  begin
    result := max(result, FPeak[AChannel][Block]);
  end;
end;

function TDynamicRangeMeter.Peak: double;
var
  ch: longint;
begin
  result := 0;
  for ch := 0 to FChannelCount -1 do
  begin
    result := max(result, Peak(ch));
  end;
end;

function TDynamicRangeMeter.Rms(AChannel: longint): double;
begin
  result := sqrt(Rms2(AChannel));
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
var
  j: longint;
begin
  FBlocksize    := 0;
  FBlockCount   := 0;
  FChannelCount := 0;

  for j := low(FRms2) to high(FRms2) do
    SetLength(FRms2[j], 0);
  SetLength(FRms2, 0);

  for j := low(FPeak) to high(FPeak) do
    SetLength(FPeak[j], 0);
  SetLength(FPeak, 0);
  SetLength(FDynamicRange, 0);
end;

procedure TDynamicRangeMeter.Process(const AChannels: TChannels; ASamplecount, ASamplerate: longint);
var
  ch, i, j, k, index, Num: longint;
  CurrEnergy, CurrPeak, Peak2nd, CurrSample: double;
  Rms2Vec, PeakVec: array of double;
  TailSize: longint;
begin
  Finalize;
  FChannelCount := Length(AChannels);
  if FChannelCount = 0 then Exit;

  FBlockSize  := 3 * ASamplerate;
  FBlockCount := ASamplecount div FBlockSize;
  TailSize    := ASampleCount mod FBlockSize;

  if FBlockCount = 0 then Exit;

  SetLength(FRms2, FChannelCount, FBlockCount + Ord(TailSize > 0));
  SetLength(FPeak, FChannelCount, FBlockCount + Ord(TailSize > 0));
  SetLength(FDynamicRange, FChannelCount);

  for ch := 0 to FChannelCount - 1 do
  begin
    for i := 0 to FBlockCount - 1 do
    begin
      CurrEnergy := 0;
      CurrPeak   := 0;
      k := i * FBlockSize;
      for j := 0 to FBlockSize - 1 do
      begin
        index := k + j;
        CurrSample := AChannels[ch][index];
        CurrEnergy := CurrEnergy + Sqr(CurrSample);
        CurrPeak   := Max(CurrPeak, Abs(CurrSample));
      end;
      FRms2[ch][i] := CurrEnergy / FBlockSize;
      FPeak[ch][i] := CurrPeak;
    end;
  end;

  if TailSize > 0 then
  begin
    for ch := 0 to FChannelCount - 1 do
    begin
      CurrEnergy := 0;
      CurrPeak   := 0;
      k := FBlockCount * FBlockSize;
      for j := 0 to TailSize - 1 do
      begin
        index := k + j;

        CurrEnergy := CurrEnergy + Sqr(AChannels[ch][index]);
        CurrPeak   := Max(CurrPeak, Abs(AChannels[ch][index]));
      end;
      FRms2[ch][FBlockCount] := CurrEnergy / TailSize;
      FPeak[ch][FBlockCount] := CurrPeak;
    end;
  end;

  for ch := 0 to FChannelCount - 1 do
  begin
    SetLength(Rms2Vec, FBlockCount + Ord(TailSize > 0));
    SetLength(PeakVec, FBlockCount + Ord(TailSize > 0));

    for i := 0 to High(Rms2Vec) do
    begin
      Rms2Vec [i] := Sqrt(2 * FRms2[ch][i]);
      PeakVec[i]  := FPeak[ch][i];
    end;

    QuickSort(Rms2Vec, 0, High(Rms2Vec));
    QuickSort(PeakVec, 0, High(PeakVec));

    Num := Trunc(0.2 * Length(Rms2Vec));
    if Num < 1 then Num := 1;

    if High(PeakVec) >= 1 then
      Peak2nd := PeakVec[High(PeakVec) - 1]
    else
      Peak2nd := PeakVec[0];

    CurrEnergy := 0;
    for index := Length(Rms2Vec) - Num to High(Rms2Vec) do
    begin
      CurrEnergy := CurrEnergy + Sqr(Rms2Vec[index]);
    end;
    CurrEnergy := CurrEnergy / Num;

    if Peak2nd <> 0 then
      FDynamicRange[ch] := -Decibel(Sqrt(CurrEnergy) / Peak2nd)
    else
      FDynamicRange[ch] := 0;
  end;
end;

end.
