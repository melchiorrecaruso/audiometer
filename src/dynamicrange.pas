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
  Classes, Common, Fgl, Sysutils;

type
  TDynamicrangerMeter = class
  private
    FBlocksize: longint;
    FBlockCount: longint;
    FChannelCount: longint;
    FRms2: tarrayofarrayofdouble;
    FPeak: tarrayofarrayofdouble;
    FDR: tarrayofdouble;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    procedure Analyze(const AChannels: TChannels; ASampleCount, ASampleRate: longint);

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

// TDynamicrangerMeter

function TDynamicrangerMeter.Rms2(AChannel, ABlock: longint): double;
begin
  result := FRms2[AChannel][ABlock];
end;

function TDynamicrangerMeter.Rms2(AChannel: longint): double;
var
  Block: longint;
begin
  result := 0;
  if FBlockCount > 0 then
  begin
    for Block := 0 to FBlockCount -1 do
    begin
      result := result + FRms2[AChannel][Block];
    end;
    result := result / FBlockCount;
  end;
end;

function TDynamicrangerMeter.Rms2: double;
var
  ch: longint;
begin
  result := 0;
  if FChannelCount > 0 then
  begin
    for ch := 0 to FChannelCount -1 do
    begin
      result := result + Rms2(ch);
    end;
    result := result / FChannelCount;
  end;
end;

function TDynamicrangerMeter.Peak(AChannel, ABlock: longint): double;
begin
  result := FPeak[AChannel][ABlock];
end;

function TDynamicrangerMeter.Peak(AChannel: longint): double;
var
  Block: longint;
begin
  result := 0;
  for Block := 0 to FBlockCount -1 do
  begin
    result := max(result, FPeak[AChannel][Block]);
  end;
end;

function TDynamicrangerMeter.Peak: double;
var
  ch: longint;
begin
  result := 0;
  if FChannelCount > 0 then
  begin
    for ch := 0 to FChannelCount -1 do
    begin
      result := max(result, Peak(ch));
    end;
  end;
end;

function TDynamicrangerMeter.Rms(AChannel: longint): double;
begin
  result := sqrt(Rms2(AChannel));
end;

function TDynamicrangerMeter.Rms: double;
var
  ch: longint;
begin
  result := 0;
  if FChannelCount > 0 then
  begin
    for ch := 0 to FChannelCount -1 do
      result := result + Rms2(ch);

    result := sqrt(result / ChannelCount);
  end;
end;

function TDynamicrangerMeter.DR(AChannel: longint): double;
begin
  result := FDR[AChannel];
end;

function TDynamicrangerMeter.DR: double;
var
  ch: longint;
begin
  result := 0;
  if FChannelCount > 0 then
  begin
    for ch := 0 to FChannelCount -1 do
      result := result + FDR[ch];

    result := trunc(result / FChannelCount + 1);
  end;
end;

constructor TDynamicrangerMeter.Create;
begin
  inherited create;
end;

destructor TDynamicrangerMeter.Destroy;
begin
  Clear;
  inherited destroy;
end;

procedure TDynamicrangerMeter.Clear;
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
  SetLength(FDR, 0);
end;

procedure TDynamicrangerMeter.Analyze(const achannels: TChannels; asamplecount, asamplerate: longint);
var
  index, Block, ch, num: longint;
   rmslist: tlistofdouble;
  peaklist: tlistofdouble;
  peak2nd, sum2, pk: double;
begin
  inherited create;
  FChannelCount := length(achannels);
  if FChannelCount = 0 then exit;

  FBlocksize  := 3 * asamplerate;
  FBlockCount := asamplecount div FBlocksize;

  if FBlockCount = 0 then exit;

  SetLength(FRms2, FChannelCount);
  SetLength(FPeak, FChannelCount);
  for ch := 0 to FChannelCount -1 do
  begin
    SetLength(FRms2[ch], FBlockCount);
    SetLength(FPeak[ch], FBlockCount);
  end;

  for ch := 0 to FChannelCount -1 do
  begin
    for Block := 0 to FBlockCount -1 do
    begin
      sum2 := 0;
      pk   := 0;
      for index := (Block * FBlocksize) to ((Block + 1) * FBlocksize) -1 do
      begin
        sum2 :=  sum2 + sqr(achannels[ch][index]);
        pk   := max(pk, abs(achannels[ch][index]));
      end;
      FRms2[ch][Block] := sum2 / FBlocksize;
      FPeak[ch][Block] := pk;
    end;
  end;

  SetLength(FDR, FChannelCount);
  for ch := 0 to FChannelCount -1 do
  begin
     rmslist := tlistofdouble.create;
    peaklist := tlistofdouble.create;
    for Block := 0 to FBlockCount -1 do
    begin
       rmslist.add(sqrt(2*FRms2[ch][Block]));
      peaklist.add(       FPeak[ch][Block]);
    end;
     rmslist.sort(@compare);
    peaklist.sort(@compare);

    num  := trunc(0.2 * rmslist.count);
    if num > 1 then
    begin
      peak2nd := peaklist[1];

      sum2 := 0;
      for index := 0 to num -1 do
      begin
        sum2 := sum2 + sqr(rmslist[index]);
      end;
      FDR[ch] := -Decibel(sqrt(sum2/num) / peak2nd);
    end;
     rmslist.destroy;
    peaklist.destroy
  end;
end;

end.
