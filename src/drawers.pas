unit drawers;

{$mode objfpc}

interface

uses
  BGRABitmap, BGRABitmapTypes, basegraphics, classes, fpimage, graphics, soundwav, sysutils;


type
  tvirtualscreens = array[0..3] of tbitmap;

  tscreendrawer = class(tthread)
  private
    fonstart: tthreadmethod;
    fonstop: tthreadmethod;
    fontick: tthreadmethod;
    fonwait: tthreadmethod;
    fscreens: tvirtualscreens;
    fscreenheight: integer;
    fscreenwidth: integer;
    fticktime: tdatetime;
    ftrack: ttrack;
    procedure dotick;
    procedure drawblocks(atrack: ttrack; ascreen: tbitmap);
    procedure drawspectrum(atrack: ttrack; ascreen: tbitmap);
    procedure drawspectrogram(atrack: ttrack; ascreen: tbitmap);
    procedure drawwave(atrack: ttrack; ascreen: tbitmap);
    function getscreen(aindex: longint):tbitmap;
  public
    constructor create(atrack: ttrack);
    destructor destroy; override;
    procedure execute; override;
  public
    property onstart: tthreadmethod read fonstart write fonstart;
    property onstop: tthreadmethod read fonstop write fonstop;
    property ontick: tthreadmethod read fontick write fontick;
    property onwait: tthreadmethod read fonwait write fonwait;
    property screens[index: longint]: tbitmap read getscreen;
    property screenwidth: integer read fscreenwidth write fscreenwidth;
    property screenheight: integer read fscreenheight write fscreenheight;
  end;


var
  screendrawer: tscreendrawer = nil;

implementation

uses
  math, dateutils;

function getcolor(factor: double): tcolor;
var
  r1, g1, b1, r2, g2, b2: word;
  color1: tcolor;
  color2: tcolor;
  color3: tfpcolor;
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

  r1 := TColorToFPColor(color1).red;
  g1 := TColorToFPColor(color1).green;
  b1 := TColorToFPColor(color1).blue;

  r2 := TColorToFPColor(color2).red;
  g2 := TColorToFPColor(color2).green;
  b2 := TColorToFPColor(color2).blue;

  color3.red   := trunc(r1 + (r2 - r1) * factor);
  color3.green := trunc(g1 + (g2 - g1) * factor);
  color3.blue  := trunc(b1 + (b2 - b1) * factor);
  result := FPColorToTColor(color3);
end;

// tscreendrawer

constructor tscreendrawer.create(atrack: ttrack);
var
  i: longint;
begin
  fonstart := nil;
  fonstop  := nil;
  fonwait  := nil;
  fontick  := nil;

  for i := low(fscreens)to high(fscreens) do
    fscreens[i] := tbitmap.create;
  fscreenwidth  := 0;
  fscreenheight := 0;
  ftrack := atrack;

  freeonterminate := true;
  inherited create(true);
end;

destructor tscreendrawer.destroy;
var
  i: longint;
begin
  for i := low(fscreens)to high(fscreens) do
    freeandnil(fscreens[i]);
  inherited destroy;
end;

procedure tscreendrawer.execute;
begin
  if assigned(fonstart) then
    synchronize(fonstart);

  fticktime := now;
  if assigned(fonwait) then
    while (fscreenwidth = 0) or (fscreenheight = 0) do
    begin
      if millisecondsbetween(now, fticktime) > 500 then
      begin
        queue(fonwait);
        fticktime := now;
      end;
    end;

  if (fscreenwidth > 0) and (fscreenheight > 0) then
  begin
    fscreens[0].setsize(fscreenwidth, fscreenheight);
    fscreens[1].setsize(fscreenwidth, fscreenheight);
    fscreens[2].setsize(fscreenwidth, fscreenheight);
    fscreens[3].setsize(fscreenwidth, fscreenheight);

    if assigned(ftrack) then
    begin
      drawblocks     (ftrack, fscreens[0]);
      drawspectrum   (ftrack, fscreens[1]);
      drawspectrogram(ftrack, fscreens[2]);
      drawwave       (ftrack, fscreens[3]);
    end;
 end;

  if assigned(fonstop) then
    synchronize(fonstop);
end;

procedure tscreendrawer.dotick;
begin
  if millisecondsbetween(now, fticktime) > 250 then
  begin
    queue(fontick);
    fticktime := now;
  end;
end;

procedure tscreendrawer.drawblocks(atrack: ttrack; ascreen: tbitmap);
var
  i, j, blocknum: longint;
  rmsi, peaki, maxdB: double;
  points: arrayoftpointf = nil;
  chart: tchart;
begin
  // create and configure the chart
  chart := tchart.create;
  chart.legendenabled := false;
  chart.title := '';
  chart.xaxislabel := 'blocknum';
  chart.yaxislabel := 'audio [dB]';
  chart.xgridlinewidth := 0;
  chart.ygridlinewidth := 0;
  chart.scale := 1.0;
  chart.backgroundcolor := clblack;
  chart.titlefontcolor := clwhite;
  chart.xaxisfontcolor := clwhite;
  chart.yaxisfontcolor := clwhite;
  chart.xaxislinecolor := clwhite;
  chart.yaxislinecolor := clwhite;
  chart.textureheight := 1;
  chart.texturewidth := 1;
  chart.texturebackgroundcolor := clblack;
  chart.pencolor := clblack;
  chart.xminf := 0;
  chart.yminf := 0;
  chart.ycount := 8;
  chart.ydeltaf := 0.75*atrack.bitspersample;

  maxdB    := 6*atrack.bitspersample;
  blocknum := length(atrack.channels[0].rms2);

  // loop through each block
  setlength(points, 4);
  for i := 0 to blocknum - 1 do
  begin
    rmsi := 0;
    // calculate average rms across channels
    for j := 0 to atrack.channelcount - 1 do
    begin
      if i < length(atrack.channels[j].rms2) then
        rmsi := rmsi + sqrt(max(0, atrack.channels[j].rms2[i]));
    end;
    rmsi := rmsi / atrack.channelcount;

    // draw yellow block for rms level
    points[0].x := (i + 1) - 0.35;
    points[0].y := 0;
    points[1].x := (i + 1) - 0.35;
    points[1].y := maxdB + db(rmsi);
    points[2].x := (i + 1) + 0.35;
    points[2].y := maxdB + db(rmsi);
    points[3].x := (i + 1) + 0.35;
    points[3].y := 0;

    chart.texturecolor := clyellow;
    chart.pencolor := clblack;
    chart.addpolygon(points, '');

    peaki := 0;
    // calculate average peak across channels
    for j := 0 to atrack.channelcount - 1 do
    begin
      if i < length(atrack.channels[j].peak) then
        peaki := peaki + atrack.channels[j].peak[i];
    end;
    peaki := peaki / atrack.channelcount;

    // draw red block from rms to peak
    points[0].x := (i + 1) - 0.35;
    points[0].y := maxdB + db(rmsi);
    points[1].x := (i + 1) - 0.35;
    points[1].y := maxdB + db(peaki);
    points[2].x := (i + 1) + 0.35;
    points[2].y := maxdB + db(peaki);
    points[3].x := (i + 1) + 0.35;
    points[3].y := maxdB + db(rmsi);

    chart.texturecolor := clred;
    chart.pencolor := clblack;
    chart.addpolygon(points, '');
    dotick;
  end;
  points := nil;

  chart.draw(ascreen, ascreen.width, ascreen.height, true);
  chart.free;
end;

procedure tscreendrawer.drawspectrum(atrack: ttrack; ascreen: tbitmap);
var
  chart: tchart;
  i, j, k: longint;
  windowsize: longint;
  windowcount: longint;
  points: arrayoftpointf = nil;
  index: longint;
  maxdB: double;
  x, y: single;
  factor: single;
begin
  // create and configure the chart
  chart := tchart.create;
  chart.legendenabled := false;
  chart.title := '';
  chart.xaxislabel := 'freq [Hz]';
  chart.yaxislabel := 'audio [dB]';
  chart.xgridlinewidth := 0;
  chart.ygridlinewidth := 0;
  chart.scale := 1.0;
  chart.backgroundcolor := clblack;
  chart.xaxisfontcolor := clwhite;
  chart.yaxisfontcolor := clwhite;
  chart.xaxislinecolor := clwhite;
  chart.yaxislinecolor := clwhite;
  chart.xminf := 0;
  chart.textureheight := 1;
  chart.texturewidth := 1;
  chart.texturebackgroundcolor := clblack;
  chart.texturecolor := clyellow;
  chart.pencolor := clyellow;

  maxdB := 6*atrack.bitspersample;

  // initialize frequency bin array (half of fft size)
  windowsize  := spectrumwindowsize div 2;
  windowcount := length(atrack.channels[0].spectrum) div windowsize;
  factor      := 0.5 * atrack.samplerate / (windowsize -1);

  setlength(points, 4);
  for i := 1 to windowsize -1 do
  begin
    x := i * factor;
    y := 0;

    for j := 0 to windowcount -1 do
    begin
      index := j * windowsize + i;
      for k := 0 to ftrack.channelcount -1 do
      begin
        y := max(y, maxdB + dB(atrack.channels[k].spectrum[index]));
      end;
    end;

    points[0].x := x -0.25 * factor;
    points[0].y := 0;
    points[1].x := x -0.25 * factor;
    points[1].y := y;
    points[2].x := x +0.25 * factor;
    points[2].y := y;
    points[3].x := x +0.25 * factor;
    points[3].y := 0;
    chart.addpolygon(points, '');
    dotick;
  end;
  points := nil;

  chart.draw(ascreen, ascreen.width, ascreen.height, true);
  chart.free;
end;

procedure tscreendrawer.drawspectrogram(atrack: ttrack; ascreen: tbitmap);
var
  chart: tchart;
  index: longint;
  x, y, ch: longint;
  amp, maxdB: double;
  windowsize: longint;
  windowcount: longint;
  xfactor, yfactor: single;
  bit: tbitmap;
begin
  // create chart
  chart := tchart.create;
  chart.legendenabled := false;
  chart.title := '';
  chart.xaxislabel := 'freq [Hz]';
  chart.yaxislabel := 'time [s]';
  chart.xgridlinewidth := 0;
  chart.ygridlinewidth := 0;
  chart.scale := 1.0;
  chart.backgroundcolor := clblack;
  chart.xaxisfontcolor := clwhite;
  chart.yaxisfontcolor := clwhite;
  chart.xaxislinecolor := clwhite;
  chart.yaxislinecolor := clwhite;
  chart.xminf   := 0;
  chart.yminf   := 0;
  chart.addpixel(atrack.samplerate div 2, atrack.duration, clblack);
  chart.draw(ascreen, ascreen.width, ascreen.height, true);

  bit := tbitmap.create;
  bit.setsize(
    trunc(chart.getdrawingrect.width *((atrack.samplerate div 2) / (chart.xmaxf - chart.xminf))),
    trunc(chart.getdrawingrect.height*((atrack.duration        ) / (chart.ymaxf - chart.yminf))));

  maxdB := 6*atrack.bitspersample;

  // set fft analysis window size (half of total window size)
  windowsize  := spectrumwindowsize div 2;
  windowcount := length(atrack.channels[0].spectrum) div windowsize;
  xfactor := (windowsize  -1) / (bit.width  -1);
  yfactor := (windowcount -1) / (bit.height -1);

  // loop over output bitmap pixels
  for x := 0 to bit.width -1 do
    for y := 0 to bit.height -1 do
    begin
      amp := 0;
      // compute fft bin index for this pixel
      for ch := 0 to atrack.channelcount - 1 do
      begin
        index := trunc(y * yfactor) * windowsize + trunc(x * xfactor);
        // convert amplitude to dB scale and normalize
        if (index mod windowsize) <> 0 then
        begin
          amp := max(amp, 1 + dB(atrack.channels[ch].spectrum[index]) / maxdB);
        end;
      end;
      // map amplitude to color and set pixel
      if amp <> 0 then
      begin
        bit.canvas.pixels[x, bit.height - y] := getcolor(amp);
      end;
      dotick;
    end;
  ascreen.canvas.draw(
    chart.getdrawingrect.left,
    chart.getdrawingrect.top + (chart.GetDrawingRect.Height - bit.Height), bit);

  bit.free;
  chart.free;
end;

procedure tscreendrawer.drawwave(atrack: ttrack; ascreen: tbitmap);
var
  ch, i, x, sampleindex: longint;
  windowxsize, windowysize: longint;
  windowxcount, windowycount: longint;
  zmax, zmin: double;
  p1, p2: tpointf;
  bit: array of tbitmap = nil;
  chart: tchart;
begin
  // create a bitmap for each audio channel
  setlength(bit, atrack.channelcount);
  for ch := low(bit) to high(bit) do
    bit[ch] := tbitmap.create;

  windowxcount := ascreen.width;       // horizontal resolution (pixels)
  windowycount := atrack.channelcount; // one row per channel

  // loop through each channel
  for ch := low(bit) to high(bit) do
  begin
    chart := tchart.create;
    chart.legendenabled := false;
    chart.title := '';
    chart.xaxislabel := 'time [s]';
    chart.yaxislabel := '1';
    chart.scale := 1.0;
    chart.backgroundcolor := clblack;
    chart.xaxisfontcolor := clwhite;
    chart.yaxisfontcolor := clwhite;
    chart.xaxislinecolor := clwhite;
    chart.yaxislinecolor := clwhite;
    chart.xgridlinewidth := 0;
    chart.ygridlinewidth := 0;
    chart.ycount  := 8;
    chart.ydeltaf := 0.25;
    chart.penwidth := 1;
    chart.pencolor := clred;
    chart.texturecolor := clred;

    // calculate number of samples per horizontal pixel
    windowxsize := length(atrack.channels[ch].samples) div windowxcount;
    // calculate vertical size per channel section
    windowysize := ascreen.height div windowycount;

    // prepare per-channel bitmap
    bit[ch].setsize(ascreen.width, windowysize);
    // loop through horizontal pixels (time segments)
    for x := 0 to windowxcount - 1 do
    begin
      zmin :=  infinity;
      zmax := -infinity;

      // find min and max sample values in this time segment
      for i := 0 to windowxsize -1 do
      begin
        sampleindex := x * windowxsize + i;
        zmin := min(zmin, atrack.channels[ch].samples[sampleindex]);
        zmax := max(zmax, atrack.channels[ch].samples[sampleindex]);
      end;

      // create a vertical line for waveform range at this segment
      p1.x := x / (windowxcount -1) * atrack.duration;
      p1.y := zmin;
      p2.x := x / (windowxcount -1) * atrack.duration;
      p2.y := zmax;

      chart.addpolyline([p1, p2], false, '');
      dotick;
    end;
    // set visible bounds for chart
    chart.ymaxf := +1.0;
    chart.yminf := -1.0;
    chart.xminf := 0;
    chart.xmaxf := max(1, atrack.duration);
    // draw chart on bitmap
    chart.draw(bit[ch], bit[ch].width, bit[ch].height);
    chart.free;
  end;

  // composite each channel's bitmap into the final output
  for ch := low(bit) to high(bit) do
  begin
    ascreen.canvas.draw(0, trunc(ch * (ascreen.height / atrack.channelcount)), bit[ch]);
    bit[ch].free;
  end;
  bit := nil;
end;

function tscreendrawer.getscreen(aindex: longint):tbitmap;
begin
  result := fscreens[aindex];
end;

end.

