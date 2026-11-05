{
  Description: Text report of the analyzed tracks.

  Copyright (C) 2020-2026 Melchiorre Caruso <melchiorrecaruso@gmail.com>

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

unit TrackReport;

{$mode objfpc}

interface

uses
  Classes, AudioTrack;

// Writes the DR report of every track in AList into S (cleared first).
procedure WriteTrackReport(AList: TTrackList; S: TStrings);


implementation

uses
  Common, Math, SysUtils;

procedure WriteTrackReport(AList: TTrackList; S: TStrings);
const
  Splitter = '--------------------------------------------------------------------------------';
var
  DR: double;
  i, DRCount: longint;
  Track: TTrack;
begin
  S.Clear;
  S.Add(APPTITLE);
  S.Add(Splitter);
  S.Add(Format('Log date : %s', [DateTimeToStr(now)]));
  S.Add(Splitter);
  S.Add('');

  DR := 0;
  DrCount := 0;
  if AList.Count > 0 then
  begin
    S.Add('DR      Peak        RMS     bps  chs      SR  Duration  Track');
    S.Add(Splitter);
    for i := 0 to AList.Count - 1 do
    begin
      Track := AList[i];
      S.Add(Format('DR%2.0f %7.2f dB %7.2f dB %4.0d %4.0d %7.0d %-s     %s',
        [Track.DRMeter.DR,
         Decibel(Track.DRMeter.Peak),
         Decibel(Track.DRMeter.Rms),
         Track.Bitspersample,
         Track.Channelcount,
         Track.Samplerate,
         Format('%3.2d:%2.2d', [
           Track.Duration div (60),
           Track.Duration mod (60)]),
           ExtractFileName(Track.Filename)]));

      if Track.DRMeter.ChannelCount > 0 then
      begin
        DR := DR + Track.DRMeter.DR;
        Inc(DRCount);
      end;
    end;
    if DRCount > 0 then
    begin
      DR := SimpleRoundTo(DR / DRCount, 0);
    end;

    S.Add(Splitter);
    S.Add('');
    S.Add(Format('Number of tracks:  %d', [AList.Count]));
    if DR >  0 then S.Add(Format('Official DR value: DR%1.0f', [DR]));
    if DR <= 0 then S.Add('Official DR value: ---');

    S.Add('');
    S.Add(Splitter);
  end;
end;

end.
