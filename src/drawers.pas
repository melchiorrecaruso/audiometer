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
var
  chart: basegraphics.tchart;
  i, j: longint;
  norm: double;
  rmsi: double;
  peaki: double;
  points: arrayoftpointf;
begin
  setlength(points, 4);
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
  //chart.YCount  := 8;
  //chart.YDeltaF := 0.25;
  chart.XMinF := 0;
  chart.TextureHeight := 1;
  chart.TextureWidth := 1;
  chart.TextureBackgroundColor := clblack;
  chart.PenColor := clblack;

  // load rms and peak
  if ftrack.channelcount > 0 then
  begin
    norm := 1 shl (ftrack.bitspersample -1);
    for i := 0 to ftrack.channels[0].count -1 do
    begin
      rmsi := 0;
      for j := 0 to ftrack.channelcount -1 do
        rmsi := rmsi + sqrt(ftrack.channels[j].rms2[i]);

      points[0].X := (i + 1) - 0.4;
      points[0].Y := 0;

      points[1].X := (i + 1) - 0.4;
      points[1].Y := db(rmsi/ftrack.channelcount*norm);

      points[2].X := (i + 1) + 0.4;
      points[2].Y := db(rmsi/ftrack.channelcount*norm);

      points[3].X := (i + 1) + 0.4;
      points[3].Y := 0;

      chart.texturecolor := clyellow;
      chart.addpolygon(points, '');

      peaki := 0;
      for j := 0 to ftrack.channelcount -1 do
        peaki := peaki + ftrack.channels[j].peak[i];

      points[0].X := (i + 1) - 0.4;
      points[0].Y := db(rmsi/ftrack.channelcount*norm);

      points[1].X := (i + 1) - 0.4;
      points[1].Y := db(peaki/ftrack.channelcount*norm);

      points[2].X := (i + 1) + 0.4;
      points[2].Y := db(peaki/ftrack.channelcount*norm);

      points[3].X := (i + 1) + 0.4;
      points[3].Y := db(rmsi/ftrack.channelcount*norm);

      chart.texturecolor := clred;
      chart.addpolygon(points, '');
    end;

    chart.draw(fbit.canvas, fbit.width, fbit.height);
    chart.destroy;
  end;
  setlength(points, 0);
end;

procedure tdrawer.drawspectrum;
var
  chart: basegraphics.tchart;
  i, j, k: longint;
  maxamp: double;
  windowsize: longint;
  windowcount: longint;
  arr: array of double = nil;
  points: arrayoftpointf;
begin
  if ftrack.channelcount > 0 then
  begin
    maxamp := minfloat;
    for i := 0 to ftrack.channelcount -1 do
      for j := 0 to length(ftrack.channels[i].spectrum) -1 do
      begin
        maxamp := max(maxamp, ftrack.channels[i].spectrum[j]);
      end;

    setlength(arr, ftrack.spectrumws div 2);
    for i := 0 to length(arr) -1 do arr[i] := 0;

    windowsize := ftrack.spectrumws div 2;
    for i := 0 to ftrack.channelcount -1 do
    begin
      windowcount := length(ftrack.channels[i].spectrum) div windowsize;

      for j := 0 to length(arr) -1 do
      begin
        arr[j] := 0;
        if j mod windowsize <> 0 then
        begin
          for k := 0 to windowcount -1 do
          begin
            arr[j] := max(arr[j], ftrack.channels[i].spectrum[k*windowsize + j]/maxamp);
          end;
        end;
      end;
    end;

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
    //chart.YCount  := 8;
    //chart.YDeltaF := 0.25;
    chart.XMinF := 0;
    chart.TextureHeight := 1;
    chart.TextureWidth := 1;
    chart.TextureBackgroundColor := clblack;
    chart.PenColor := clyellow;

    setlength(points, 4);
    for i := 0 to length(arr) -1 do
    begin
      if i mod windowsize <> 0 then
      begin
        points[0].X := (i + 0.75)*ftrack.samplerate/ftrack.spectrumws;
        points[0].Y := 0;

        points[1].X := (i + 0.75)*ftrack.samplerate/ftrack.spectrumws;
        points[1].Y := db(ftrack.maxamp*arr[i]/ftrack.channelcount);

        points[2].X := (i + 1.25)*ftrack.samplerate/ftrack.spectrumws;
        points[2].Y := db(ftrack.maxamp*arr[i]/ftrack.channelcount);

        points[3].X := (i + 1.25)*ftrack.samplerate/ftrack.spectrumws;
        points[3].Y := 0;

        chart.texturecolor := clyellow;
        chart.addpolygon(points, '');
      end;
    end;
    chart.draw(fbit.canvas, fbit.width, fbit.height);
    chart.destroy;
    points := nil;
    arr := nil;
  end;
end;

procedure tdrawer.drawwave;
var
  chart: basegraphics.tchart;
  i, j, k: longint;
  windowxsize: longint;
  windowysize: longint;
  windowxcount: longint;
  windowycount: longint;
  zmax, zmin, zdelta: double;

  p1,p2: tpointf;

  bit: array of tbgrabitmap;
begin
  if ftrack.channelcount > 0 then
  begin
    setlength(bit, ftrack.channelcount);
    for i := low(bit) to high(bit) do
      bit[i] := tbgrabitmap.create;

    windowxcount := fbit.width;
    windowycount := ftrack.channelcount;

    for i := 0 to windowycount -1 do
    begin
      windowxsize  := length(ftrack.channels[i].samples) div windowxcount;
      windowysize  := fbit.height div windowycount;

      bit[i].setsize(fbit.width, fbit.height div ftrack.channelcount);
      bit[i].filltransparent;

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

      chart.ymaxf := +1.0;
      chart.yminf := -1.0;
      chart.xminf := 0;
      chart.xmaxf := max(1, windowxcount);

      chart.adjustxmin := false;
      chart.adjustxmax := false;
      chart.adjustymin := false;
      chart.adjustymax := false;

      chart.draw(bit[i].canvas, bit[i].width, bit[i].height);
      chart.destroy;
    end;

    for i := low(bit) to high(bit) do
    begin
      fbit.PutImage(0, trunc(i*(fbit.height/ftrack.channelcount)), bit[i], dmset);
      bit[i].destroy;
    end;
    bit := nil;
  end;
end;

procedure tdrawer.drawspectrogram;
var
  chart: basegraphics.tchart;
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
end;

procedure tdrawer.execute;
begin
  case findex of
    0: drawblocks;
    1: drawspectrum;
    2: drawspectrogram;
    3: drawwave;
  else drawblocks;
  end;

  if assigned(fonredraw) then
    synchronize(fonredraw);
end;

end.

