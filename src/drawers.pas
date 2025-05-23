unit drawers;

{$mode objfpc}

interface

uses
  bgrabitmap, bgrabitmaptypes, basegraphics, classes, graphics, soundwav, sysutils;


procedure drawblocks(atrack: ttrack; ascreen: tbgrabitmap);
procedure drawspectrum(atrack: ttrack; ascreen: tbgrabitmap);
procedure drawspectrogram(atrack: ttrack; ascreen: tbgrabitmap);
procedure drawwave(atrack: ttrack; ascreen: tbgrabitmap);


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

procedure drawblocks(atrack: ttrack; ascreen: tbgrabitmap);
var
  chart: basegraphics.tchart;
  i, j: longint;
  rmsi: double;
  peaki: double;
  points: array[0..3] of tpointf;
  blocknum: longint;
  maxdB: double;
begin
  if not assigned(atrack) then exit;
  if atrack.channelcount = 0 then exit;

  // create and configure the chart
  chart := tchart.create;
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
  if (atrack.channelcount > 0) then
  begin
    maxdB    := 6*atrack.bitspersample;
    blocknum := length(atrack.channels[0].rms2);

    // loop through each block
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
      chart.addpolygon(points, '');
    end;

    chart.ydeltaf := 0.75*atrack.bitspersample;
  end;

  chart.draw(ascreen.canvas, ascreen.width, ascreen.height, true);
  chart.destroy;
end;

procedure drawspectrum(atrack: ttrack; ascreen: tbgrabitmap);
var
  chart: basegraphics.tchart;
  i, j, k: longint;
  windowsize: longint;
  windowcount: longint;
  arr: array of double;
  points: array[0..3] of tpointf;
  index: longint;
begin
  if not assigned(atrack) then exit;
  if atrack.channelcount = 0 then exit;

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
  chart.pencolor := clyellow;

  // initialize frequency bin array (half of fft size)
  windowsize := spectrumwindowsize div 2;
  setlength(arr, windowsize);
  for i := 0 to length(arr) - 1 do
    arr[i] := 0;

  // accumulate spectrum over all channels
  for i := 0 to atrack.channelcount - 1 do
  begin
    windowcount := length(atrack.channels[i].spectrum) div windowsize;

    for j := 0 to length(arr) - 1 do
    begin
      arr[j] := 0;

      // skip dc component (usually index 0 of each window)
      if j mod windowsize <> 0 then
      begin
        for k := 0 to windowcount - 1 do
        begin
          index := k * windowsize + j;
          if index < length(atrack.channels[i].spectrum) then
            arr[j] := max(arr[j], atrack.channels[i].spectrum[index]);
        end;
      end;
    end;
  end;

  // allocate space for frequency bar polygon
  for i := 0 to length(arr) - 1 do
  begin
    if (i mod windowsize <> 0) and (atrack.channelcount > 0) then
    begin
      // compute horizontal frequency position (in hz)
      points[0].x := (i + 0.75) * atrack.samplerate / spectrumwindowsize;
      points[1].x := points[0].x;
      points[2].x := (i + 1.25) * atrack.samplerate / spectrumwindowsize;
      points[3].x := points[2].x;

      // draw from 0 up to db magnitude
      points[0].y := 0;
      points[3].y := 0;

      // convert amplitude to dbfs
      points[1].y := db(arr[i] / atrack.channelcount);
      points[2].y := points[1].y;

      chart.texturecolor := clyellow;
      chart.addpolygon(points, '');
    end;
  end;
  // cleanup
  arr := nil;

  chart.draw(ascreen.canvas, ascreen.width, ascreen.height, true);
  chart.destroy;
end;

procedure drawspectrogram(atrack: ttrack; ascreen: tbgrabitmap);
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
  if not assigned(atrack) then exit;
  if atrack.channelcount = 0 then Exit;

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
  //chart.ycount  := 8;     // number of vertical grid lines
  //chart.ydeltaf := 0.25;  // distance between grid lines

  // find the maximum amplitude in all spectrum data
  maxamp := 0;
  for i := 0 to atrack.channelcount - 1 do
    for j := 0 to length(atrack.channels[i].spectrum) - 1 do
      maxamp := max(maxamp, atrack.channels[i].spectrum[j]);

  // find the maximum raw sample value (for normalization)
  scale := 0;
  for i := 0 to atrack.channelcount - 1 do
    for j := 0 to length(atrack.channels[i].samples) - 1 do
      scale := max(scale, atrack.channels[i].samples[j]);

  // prevent division by zero
  if (maxamp = 0) or (scale = 0) or (atrack.bitspersample = 0) then Exit;

  // set fft analysis window size (half of total window size)
  windowsize := spectrumwindowsize div 2;

  // precompute ratios for coordinate scaling
  pixelxratio := windowsize / ascreen.width;
  pixelyratio := (length(atrack.channels[0].spectrum) / windowsize) / ascreen.height;

  // loop over output bitmap pixels
  for i := 0 to ascreen.width - 1 do
    for j := 0 to ascreen.height - 1 do
    begin
      amp := 0;

      // compute fft bin index for this pixel
      for k := 0 to atrack.channelcount - 1 do
      begin
        windowcount := length(atrack.channels[k].spectrum) div windowsize;

        index := trunc(j * pixelyratio) * windowsize + trunc(i * pixelxratio);

        // skip dc component (first index of each window)
        if (index mod windowsize <> 0) then
        begin
          // convert amplitude to db scale and normalize
          amp := max(amp, 1 + db((scale * atrack.channels[k].spectrum[index]) / maxamp) / (6 * atrack.bitspersample));
        end;
      end;

      // map amplitude to color and set pixel
      chart.AddPixel(i, j, getcolor(amp));
    end;

  chart.draw(ascreen.canvas, ascreen.width, ascreen.height, true);
  chart.destroy;
end;

procedure drawwave(atrack: ttrack; ascreen: tbgrabitmap);
var
  i, j, k, sampleindex: longint;
  windowxsize, windowysize: longint;
  windowxcount, windowycount: longint;
  zmax, zmin: double;
  p1, p2: tpointf;
  bit: array of tbgrabitmap = nil;
  chart: basegraphics.tchart;
begin
  writeln('drawwave.start');
  if not assigned(atrack) then exit;
  if atrack.channelcount = 0 then Exit;

  // create chart for waveform drawing
  chart := basegraphics.tchart.create;

  // create a bitmap for each audio channel
  setlength(bit, atrack.channelcount);
  for i := low(bit) to high(bit) do
    bit[i] := tbgrabitmap.create;

  windowxcount := ascreen.width;       // horizontal resolution (pixels)
  windowycount := atrack.channelcount; // one row per channel

  // loop through each channel
  for i := 0 to windowycount - 1 do
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

    // calculate number of samples per horizontal pixel
    windowxsize := length(atrack.channels[i].samples) div windowxcount;
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
        zmin := min(zmin, atrack.channels[i].samples[sampleindex]);
        zmax := max(zmax, atrack.channels[i].samples[sampleindex]);
      end;

      // create a vertical line for waveform range at this segment
      p1.x := (j + 1) / windowxcount * atrack.duration;
      p1.y := zmin;
      p2.x := (j + 1) / windowxcount * atrack.duration;
      p2.y := zmax;

      chart.addpolyline([p1, p2], false, '');
    end;
    // set visible bounds for chart
    chart.ymaxf := +1.0;
    chart.yminf := -1.0;
    chart.xminf := 0;
    chart.xmaxf := max(1, atrack.duration);
    // draw chart on bitmap
    chart.draw(bit[i].canvas, bit[i].width, bit[i].height);
    chart.destroy;
  end;

  // composite each channel's bitmap into the final output
  for i := low(bit) to high(bit) do
  begin
    bit[i].draw(ascreen.canvas, 0, trunc(i * (ascreen.height / atrack.channelcount)));
    bit[i].free;
  end;
  setlength(bit, 0);
  writeln('drawwave.end');
end;

end.

