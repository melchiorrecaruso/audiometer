unit drawers;

{$mode ObjFPC}

interface

uses
  bgrabitmap, bgrabitmaptypes, basegraphics, classes, soundwav, sysutils;

type
  tdrawer = class(tthread)
  private
    fbit: tbgrabitmap;
    findex: longint;
    ftrack: ttrack;
    fonredraw: tthreadmethod;
    procedure drawblocks;
    procedure drawspectrum;
    procedure drawwave;
    procedure drawspectrogram;
  public
    constructor create(atrack: ttrack; abit: tbgrabitmap; aindex: longint);
    procedure execute; override;
  public
    property onredraw: tthreadmethod read fonredraw write fonredraw;
  end;

var
 drawer: tdrawer = nil;

implementation

uses
  graphics, math;

function getcolor(factor: double): tbgrapixel;
var
  r1, g1, b1, r2, g2, b2: byte;
  color1: tbgrapixel;
  color2: tbgrapixel;
begin
  if factor < 0 then exit(clblack);
  if factor > 1 then exit(clwhite);

  if factor < 1/5 then
  begin
    color1 := clblack;
    color2 := clnavy;
    factor := (factor -   0) / (1/5);
  end else
  if factor < 2/5 then
  begin
    color1 := clnavy;
    color2 := clpurple;
    factor := (factor - 1/5) / (1/5);
  end else
  if factor < 3/5 then
  begin
    color1 := clpurple;
    color2 := clred;
    factor := (factor - 2/5) / (1/5);
  end else
  if factor < 4/5 then
  begin
    color1 := clred;
    color2 := clyellow;
    factor := (factor - 3/5) / (1/5);
  end else
  begin
    color1 := clyellow;
    color2 := clwhite;
    factor := (factor - 4/5) / (1/5);
  end;

  r1 := color1.red;
  g1 := color1.green;
  b1 := color1.blue;

  r2 := color2.red;
  g2 := color2.green;
  b2 := color2.blue;

  result.red   := trunc(r1 + (r2 - r1) * factor);
  result.green := trunc(g1 + (g2 - g1) * factor);
  result.blue  := trunc(b1 + (b2 - b1) * factor);
  result.alpha := 255;
end;

// drawer

constructor tdrawer.create(atrack: ttrack; abit: tbgrabitmap; aindex: longint);
begin
  fbit    := abit;
  findex  := aindex;
  ftrack  := atrack;
  fonredraw := nil;

  freeonterminate := true;
  inherited create(true);
end;

procedure tdrawer.drawblocks;
begin

end;

procedure tdrawer.drawspectrogram;
begin

end;

procedure tdrawer.drawwave;
var
  i, j, k: longint;
  windowxsize: longint;
  windowysize: longint;
  windowxcount: longint;
  windowycount: longint;
  zmax, zmin, zdelta: double;

  p1,p2: tpointf;
  chart: basegraphics.tchart;
begin
  chart := basegraphics.tchart.create;
  chart.legendenabled := false;
  chart.title := '';
  chart.xaxislabel := 'blocknum';
  chart.yaxislabel := '%';
  chart.scale := 1.0;
  chart.backgroundcolor := clblack;
  chart.xaxisfontcolor:= clwhite;
  chart.yaxisfontcolor:= clwhite;
  chart.xaxislinecolor := clwhite;
  chart.yaxislinecolor := clwhite;
  chart.xaxislabelpos := true;
  chart.yaxislabelpos := true;
  chart.YCount  := 8;
  chart.YDeltaF := 0.25;

  if ftrack.channelcount > 0 then
  begin
    windowxcount := fbit.width;
    windowycount := ftrack.channelcount;

    for i := 0 to windowycount -1 do
    begin
      windowxsize  := length(ftrack.channels[i].samples) div windowxcount;
      windowysize  := fbit.height div windowycount;

      for j := 0 to windowxcount -1 do
      begin
        zmin :=  maxint;
        zmax := -maxint;
        for k := 0 to windowxsize -1 do
        begin
          zmin :=  min(zmin, ftrack.channels[i].samples[j * windowxsize + k]);
          zmax :=  max(zmax, ftrack.channels[i].samples[j * windowxsize + k]);
        end;

        zdelta := (zmax-zmin)/(1 shl ftrack.bitspersample);

        p1.x := j;
        p1.y := -zdelta;
        p2.x := j;
        p2.y := +zdelta;

        chart.addpolyline([p1, p2],false,'');
      end;
    end;
  end;

  chart.ymaxf := +1.0;
  chart.yminf := -1.0;
  chart.xminf := 0;
  chart.xmaxf := max(1, windowxcount);

  chart.adjustxmin := false;
  chart.adjustxmax := false;
  chart.adjustymin := false;
  chart.adjustymax := false;

  chart.draw(fbit.canvas, fbit.width, fbit.height);
  chart.destroy;

  if assigned(fonredraw) then
    synchronize(fonredraw);
end;

procedure tdrawer.drawspectrum;
var
  index: longint;
  i, j, k: longint;
  amp, maxamp: double;
  windowsize: longint;
  windowcount: longint;
begin
  if ftrack.channelcount > 0 then
  begin
    maxamp := 0;
    for i := 0 to ftrack.channelcount -1 do
      for j := 0 to length(ftrack.channels[i].spectrum) -1 do
      begin
        maxamp := max(maxamp, ftrack.channels[i].spectrum[j]);
      end;

    windowsize  := ftrack.spectrumws div 2;
    for i := 0 to fbit.width -1 do
      for j := 0 to fbit.height -1 do
      begin
        amp := 0;
        for k := 0 to ftrack.channelcount -1 do
        begin
          windowcount := length(ftrack.channels[k].spectrum) div windowsize;

          index := trunc(j/fbit.height*windowcount)*windowsize + trunc(i/fbit.width*windowsize);
          // skip DC component
          if index mod windowsize <> 0 then
          begin
            amp := max(amp, db(ftrack.maxamp*ftrack.channels[k].spectrum[index]/maxamp)/db(1 shl ftrack.bitspersample));
          end;
        end;
        fbit.setpixel(i, j, getcolor(amp));
      end;
  end;

  if assigned(fonredraw) then
    synchronize(fonredraw);
end;

procedure tdrawer.execute;
begin
  case findex of
    1: drawwave;
    2: drawwave;
    3: drawwave;
  else drawwave;
  end;
end;

end.

