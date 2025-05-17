unit drawers;

{$mode objfpc}

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
  fbit   := abit;
  findex := aindex;
  ftrack := atrack;
  fonredraw  := nil;

  freeonterminate := true;
  inherited create(true);
end;

procedure tdrawer.drawblocks;
var
  chart: basegraphics.tchart;
  i, j: longint;
  rmsi: double;
  peaki: double;
  points: array of tpointf;
  blocknum: longint;
  maxdB: double;
begin
  // prepare points array for drawing rectangles
  points := nil;
  setlength(points, 4);
  // create and configure the chart
  chart := basegraphics.tchart.create;
  chart.legendenabled := false;
  chart.title := '';
  chart.xaxislabel := 'block number';
  chart.yaxislabel := 'audio [dB]';
  //chart.xgridlinewidth :=0;
  //chart.ygridlinewidth :=0;
  chart.scale := 1.0;
  chart.backgroundcolor := clblack;
  chart.titlefontcolor := clwhite;
  chart.xaxisfontcolor := clwhite;
  chart.yaxisfontcolor := clwhite;
  chart.xaxislinecolor := clwhite;
  chart.yaxislinecolor := clwhite;

  chart.xminf := 0;
  chart.yminf := 0;
  chart.ydeltaf := 0.75*ftrack.bitspersample;
  chart.ycount := 8;
  chart.textureheight := 1;
  chart.texturewidth := 1;
  chart.texturebackgroundcolor := clblack;
  chart.pencolor := clblack;

  // ensure we have channels to process
  if ftrack.channelcount > 0 then
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

    // render chart to main output bitmap
    chart.draw(fbit.canvas, fbit.width, fbit.height);
    chart.free;
  end;

  // cleanup
  setlength(points, 0);
end;

procedure tdrawer.drawspectrum;
var
  chart: basegraphics.tchart;
  i, j, k: longint;
  windowsize: longint;
  windowcount: longint;
  arr: array of double;
  points: array of tpointf;
  index: longint;
begin
  if ftrack.channelcount > 0 then
  begin
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

    // create and configure the chart
    chart := basegraphics.tchart.create;
    chart.legendenabled := false;
    chart.title := '';
    chart.xaxislabel := 'Freq [Hz]';
    chart.yaxislabel := 'audio [dB]';
    //chart.xgridlinewidth :=0;
    //chart.ygridlinewidth :=0;
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

    // allocate space for frequency bar polygon
    setlength(points, 4);

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
        points[1].y := db(max(arr[i] / ftrack.channelcount, 1e-12));
        points[2].y := points[1].y;

        chart.texturecolor := clyellow;
        chart.addpolygon(points, '');
      end;
    end;

    // render chart to output bitmap
    chart.draw(fbit.canvas, fbit.width, fbit.height);
    chart.free;

    // cleanup
    setlength(points, 0);
    setlength(arr, 0);
  end;
end;

procedure tdrawer.drawwave;
var
  i, j, k, sampleindex: longint;
  windowxsize, windowysize: longint;
  windowxcount, windowycount: longint;
  zmax, zmin: double;
  p1, p2: tpointf;
  bit: array of tbgrabitmap = nil;
  chart: basegraphics.tchart;
begin
  if ftrack.channelcount > 0 then
  begin
    // create a bitmap for each audio channel
    setlength(bit, ftrack.channelcount);
    for i := low(bit) to high(bit) do
      bit[i] := tbgrabitmap.create;

    windowxcount := fbit.width;                  // horizontal resolution (pixels)
    windowycount := ftrack.channelcount;         // one row per channel

    // loop through each channel
    for i := 0 to windowycount - 1 do
    begin
      // calculate number of samples per horizontal pixel
      windowxsize := length(ftrack.channels[i].samples) div windowxcount;
      // calculate vertical size per channel section
      windowysize := fbit.height div windowycount;

      // prepare per-channel bitmap
      bit[i].setsize(fbit.width, windowysize);
      bit[i].filltransparent;

      // create chart for waveform drawing
      chart := basegraphics.tchart.create;
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

      chart.ycount := 8;      // number of vertical grid lines
      chart.ydeltaf := 0.25;  // distance between grid lines

      // loop through horizontal pixels (time segments)
      for j := 0 to windowxcount - 1 do
      begin
        zmin := infinity;
        zmax := -infinity;

        // find min and max sample values in this time segment
        for k := 0 to windowxsize - 1 do
        begin
          sampleindex := j * windowxsize + k;
          if sampleindex < length(ftrack.channels[i].samples) then
          begin
            zmin := min(zmin, ftrack.channels[i].samples[sampleindex]);
            zmax := max(zmax, ftrack.channels[i].samples[sampleindex]);
          end;
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

      // fix the axes (do not auto-scale)
      chart.adjustxmin := false;
      chart.adjustxmax := false;
      chart.adjustymin := false;
      chart.adjustymax := false;

      // draw chart on bitmap
      chart.draw(bit[i].canvas, bit[i].width, bit[i].height);
      chart.free;
    end;

    // composite each channel's bitmap into the final output
    for i := low(bit) to high(bit) do
    begin
      fbit.putimage(0, trunc(i * (fbit.height / ftrack.channelcount)), bit[i], dmset);
      bit[i].free;
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
  scale: double;
  windowsize: longint;
  windowcount: longint;
  pixelxratio, pixelyratio: double;
begin
  // create chart
  chart := basegraphics.tchart.create;
  chart.legendenabled := false;
  chart.title := '';
  chart.xaxislabel := 'Freq [Hz]';
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

  if ftrack.channelcount > 0 then
  begin
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
    if (maxamp = 0) or (scale = 0) or (ftrack.bitspersample = 0) then
      exit;

    // set fft analysis window size (half of total window size)
    windowsize := spectrumwindowsize div 2;

    // precompute ratios for coordinate scaling
    pixelxratio := windowsize / fbit.width;
    pixelyratio := (length(ftrack.channels[0].spectrum) / windowsize) / fbit.height;

    // loop over output bitmap pixels
    for i := 0 to fbit.width - 1 do
      for j := 0 to fbit.height - 1 do
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
        //fbit.setpixel(i, j, getcolor(amp));
        chart.AddPixel(i, j, getcolor(amp));
      end;


    // draw chart on bitmap
    chart.draw(fbit.canvas, fbit.width, fbit.height);
    chart.free;
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

