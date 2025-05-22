unit drawers;

{$mode objfpc}

interface

uses
  bgrabitmap, bgrabitmaptypes, basegraphics, classes, graphics, soundwav, sysutils;

type
  tscreens = array[0..3] of tbgrabitmap;

  tdrawer = class(tthread)
  private
    ftrack: ttrack;
    fscreens: tscreens;
    fonstart: tthreadmethod;
    fontick: tthreadmethod;
    fonstop: tthreadmethod;
    procedure drawblocks(ascreen: tbgrabitmap);
    procedure drawspectrum(ascreen: tbgrabitmap);
    procedure drawspectrogram(ascreen: tbgrabitmap);
    procedure drawwave(ascreen: tbgrabitmap);
  public
    constructor create(atrack: ttrack; ascreens: tscreens);
    procedure execute; override;
  public
    property onstart: tthreadmethod read fonstart write fonstart;
    property ontick: tthreadmethod read fontick write fontick;
    property onstop: tthreadmethod read fonstop write fonstop;
  end;

var
  drawer: tdrawer = nil;

implementation

uses
  math;

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

// tdrawer

constructor tdrawer.create(atrack: ttrack; ascreens: tscreens);
begin
  ftrack   := atrack;
  fscreens := ascreens;
  fonstart := nil;
  fontick  := nil;
  fonstop  := nil;

  freeonterminate := true;
  inherited create(true);
  //inherited create;
end;

procedure tdrawer.drawblocks(ascreen: tbgrabitmap);
var
  chart: basegraphics.tchart;
  i, j: longint;
  rmsi: double;
  peaki: double;
  points: array[0..3] of tpointf;
  blocknum: longint;
  maxdB: double;
begin
  if not assigned(ftrack) then exit;
  if ftrack.channelcount = 0 then exit;

  // create and configure the chart
  chart := basegraphics.tchart.create;
  chart.legendenabled := false;
  chart.title := '';
  chart.xaxislabel := 'blocknum';
  chart.yaxislabel := 'audio [dB]';
  chart.xgridlinewidth :=0;
  chart.ygridlinewidth :=0;
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
  chart.ydeltaf := 12;
  chart.ycount := 8;

  // ensure we have channels to process
  if (ftrack.channelcount > 0) then
  begin
    maxdB    := 6*ftrack.bitspersample;
    blocknum := length(ftrack.channels[0].rms2);

    // loop through each block
    for i := 0 to blocknum - 1 do
    begin
      rmsi := 0;
      // calculate average rms across channels
      for j := 0 to ftrack.channelcount - 1 do
      begin
        if i < length(ftrack.channels[j].rms2) then
          rmsi := rmsi + sqrt(max(0, ftrack.channels[j].rms2[i]));
      end;
      rmsi := rmsi / ftrack.channelcount;

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
      chart.addpolygon(points, '');

      peaki := 0;
      // calculate average peak across channels
      for j := 0 to ftrack.channelcount - 1 do
      begin
        if i < length(ftrack.channels[j].peak) then
          peaki := peaki + ftrack.channels[j].peak[i];
      end;
      peaki := peaki / ftrack.channelcount;

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
      chart.addpolygon(points, '');
    end;

    chart.ydeltaf := 0.75*ftrack.bitspersample;
  end;

  // render chart to main output bitmap
  chart.draw(ascreen.canvas, ascreen.width, ascreen.height);
  chart.free;
end;

procedure tdrawer.drawspectrum(ascreen: tbgrabitmap);
var
  chart: basegraphics.tchart;
  i, j, k: longint;
  windowsize: longint;
  windowcount: longint;
  arr: array of double;
  points: array[0..3] of tpointf;
  index: longint;
begin
  if not assigned(ftrack) then exit;
  if ftrack.channelcount = 0 then exit;

  // create and configure the chart
  chart := basegraphics.tchart.create;
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
  chart.pencolor := clyellow;

  // initialize frequency bin array (half of fft size)
  windowsize := spectrumwindowsize div 2;
  setlength(arr, windowsize);
  for i := 0 to length(arr) - 1 do
    arr[i] := 0;

  // accumulate spectrum over all channels
  for i := 0 to ftrack.channelcount - 1 do
  begin
    windowcount := length(ftrack.channels[i].spectrum) div windowsize;

    for j := 0 to length(arr) - 1 do
    begin
      arr[j] := 0;

      // skip dc component (usually index 0 of each window)
      if j mod windowsize <> 0 then
      begin
        for k := 0 to windowcount - 1 do
        begin
          index := k * windowsize + j;
          if index < length(ftrack.channels[i].spectrum) then
            arr[j] := max(arr[j], ftrack.channels[i].spectrum[index]);
        end;
      end;
    end;
  end;

  // allocate space for frequency bar polygon
  for i := 0 to length(arr) - 1 do
  begin
    if (i mod windowsize <> 0) and (ftrack.channelcount > 0) then
    begin
      // compute horizontal frequency position (in hz)
      points[0].x := (i + 0.75) * ftrack.samplerate / spectrumwindowsize;
      points[1].x := points[0].x;
      points[2].x := (i + 1.25) * ftrack.samplerate / spectrumwindowsize;
      points[3].x := points[2].x;

      // draw from 0 up to db magnitude
      points[0].y := 0;
      points[3].y := 0;

      // convert amplitude to dbfs
      points[1].y := db(arr[i] / ftrack.channelcount);
      points[2].y := points[1].y;

      chart.texturecolor := clyellow;
      chart.addpolygon(points, '');
    end;
  end;
  // cleanup
  arr := nil;

  // render chart to output bitmap
  chart.draw(ascreen.canvas, ascreen.width, ascreen.height);
  chart.free;
end;

procedure tdrawer.drawspectrogram(ascreen: tbgrabitmap);
var
  chart: basegraphics.tchart;
  index: longint;
  i, j, k: longint;
  amp, maxamp: double;
  scale: double;
  windowsize: longint;
  windowcount: longint;
  pixelxratio, pixelyratio: double;
begin
  if not assigned(ftrack) then exit;
  if ftrack.channelcount = 0 then Exit;

  // create chart
  chart := basegraphics.tchart.create;
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
  //chart.ycount  := 8;     // number of vertical grid lines
  //chart.ydeltaf := 0.25;  // distance between grid lines

  // find the maximum amplitude in all spectrum data
  maxamp := 0;
  for i := 0 to ftrack.channelcount - 1 do
    for j := 0 to length(ftrack.channels[i].spectrum) - 1 do
      maxamp := max(maxamp, ftrack.channels[i].spectrum[j]);

  // find the maximum raw sample value (for normalization)
  scale := 0;
  for i := 0 to ftrack.channelcount - 1 do
    for j := 0 to length(ftrack.channels[i].samples) - 1 do
      scale := max(scale, ftrack.channels[i].samples[j]);

  // prevent division by zero
  if (maxamp = 0) or (scale = 0) or (ftrack.bitspersample = 0) then Exit;

  // set fft analysis window size (half of total window size)
  windowsize := spectrumwindowsize div 2;

  // precompute ratios for coordinate scaling
  pixelxratio := windowsize / ascreen.width;
  pixelyratio := (length(ftrack.channels[0].spectrum) / windowsize) / ascreen.height;

  // loop over output bitmap pixels
  for i := 0 to ascreen.width - 1 do
    for j := 0 to ascreen.height - 1 do
    begin
      amp := 0;

      // compute fft bin index for this pixel
      for k := 0 to ftrack.channelcount - 1 do
      begin
        windowcount := length(ftrack.channels[k].spectrum) div windowsize;

        index := trunc(j * pixelyratio) * windowsize + trunc(i * pixelxratio);

        // skip dc component (first index of each window)
        if (index mod windowsize <> 0) then
        begin
          // convert amplitude to db scale and normalize
          amp := max(amp, 1 + db((scale * ftrack.channels[k].spectrum[index]) / maxamp) / (6 * ftrack.bitspersample));
        end;
      end;

      // map amplitude to color and set pixel
      chart.AddPixel(i, j, getcolor(amp));
    end;

  // draw chart on bitmap
  chart.draw(ascreen.canvas, ascreen.width, ascreen.height);
  chart.free;
end;

procedure tdrawer.drawwave(ascreen: tbgrabitmap);
var
  i, j, k, sampleindex: longint;
  windowxsize, windowysize: longint;
  windowxcount, windowycount: longint;
  zmax, zmin: double;
  p1, p2: tpointf;
  bit: array of tbgrabitmap = nil;
  chart: basegraphics.tchart;
begin
  if not assigned(ftrack) then exit;
  if ftrack.channelcount = 0 then Exit;
  if ascreen.width < 100 then exit;
  if ascreen.height < 100 then exit;

  // create chart for waveform drawing
  chart := basegraphics.tchart.create;

  // create a bitmap for each audio channel
  setlength(bit, ftrack.channelcount);
  for i := low(bit) to high(bit) do
    bit[i] := tbgrabitmap.create;

  windowxcount := ascreen.width;       // horizontal resolution (pixels)
  windowycount := ftrack.channelcount; // one row per channel

  // loop through each channel
  for i := 0 to windowycount - 1 do
  begin
    chart.clear;
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

    // calculate number of samples per horizontal pixel
    windowxsize := length(ftrack.channels[i].samples) div windowxcount;
    // calculate vertical size per channel section
    windowysize := ascreen.height div windowycount;

    // prepare per-channel bitmap
    bit[i].setsize(ascreen.width, windowysize);
    bit[i].filltransparent;

    // loop through horizontal pixels (time segments)
    for j := 0 to windowxcount - 1 do
    begin
      zmin :=  infinity;
      zmax := -infinity;

      // find min and max sample values in this time segment
      for k := 0 to windowxsize -1 do
      begin
        sampleindex := j * windowxsize + k;
        zmin := min(zmin, ftrack.channels[i].samples[sampleindex]);
        zmax := max(zmax, ftrack.channels[i].samples[sampleindex]);
      end;

      // create a vertical line for waveform range at this segment
      p1.x := (j + 1) / windowxcount * ftrack.duration;
      p1.y := zmin;
      p2.x := (j + 1) / windowxcount * ftrack.duration;
      p2.y := zmax;

      chart.addpolyline([p1, p2], false, '');
    end;
    // set visible bounds for chart
    chart.ymaxf := +1.0;
    chart.yminf := -1.0;
    chart.xminf := 0;
    chart.xmaxf := max(1, ftrack.duration);
    // draw chart on bitmap
    chart.draw(bit[i].canvas, bit[i].width, bit[i].height);
  end;
  chart.free;

  // composite each channel's bitmap into the final output
  for i := low(bit) to high(bit) do
  begin
    bit[i].draw(ascreen.canvas, 0, trunc(i * (ascreen.height / ftrack.channelcount)));
    bit[i].free;
  end;
  setlength(bit, 0);
end;

procedure tdrawer.execute;
begin
  writeln('tdrawer.start');
  if assigned(fonstart) then fonstart;
  //  synchronize();

  drawblocks(fscreens[0]);
  if assigned(fontick) then fontick;
  //  synchronize();

  drawspectrum(fscreens[1]);
  if assigned(fontick) then fontick;
  //  synchronize();

  drawspectrogram(fscreens[2]);
  if assigned(fontick) then fontick;
  //  synchronize();

  drawwave(fscreens[3]);
  if assigned(fontick) then fontick;
  //  synchronize();

  if assigned(fonstop) then fonstop;
  //  synchronize();
  writeln('tdrawer.stop');
end;

end.

