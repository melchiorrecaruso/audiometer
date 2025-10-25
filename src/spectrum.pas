unit spectrum;

{$mode objfpc}{$h+}

interface

uses
  classes, common, sysutils, ufft, utypes;

const
  SPECTRUMWINDOWSIZE = 1024;

type
  tspectrums = class
  private
    fspectrums: tarrayofarrayofdouble;
    fwindowcount: longint;
    fwindowsize: longint;

    function getsample(const channel, index: longint): TSample;
  public
    constructor create;
    destructor destroy; override;

    procedure analyze(const achannels: tchannels; asamplecount, asamplerate: longint);
    procedure clear;

    property samples[channel, index: longint]: TSample read getsample;

    property windowsize: longint read fwindowsize;
    property windowcount: longint read fwindowcount;
  end;


implementation

procedure getspectrum(asamples: pdouble; count: longint; aspectrum: pdouble);
var
  i: longint;
  buff: tcompvector = nil;
  freq: tcompvector = nil;
  window: double;
begin
  if count > 0 then
  begin
    setlength(buff, count);
    for i := 0 to count -1 do
    begin
      window := 0.5 - 0.5 * cos(2 * pi * i / (count - 1));
      buff[i].x := window * asamples^;
      buff[i].y := 0;
      inc(asamples);
    end;
    freq := FFT(count, buff);

    // bin 0 (DC)
    aspectrum^ := abs(freq[0].x)/count;
    inc(aspectrum);
    // bin da 1 a N/2 - 1
    for i := 1 to (count div 2) -1 do
    begin
      aspectrum^ := 2*sqrt(sqr(freq[i].x) + sqr(freq[i].y))/count;
      inc(aspectrum);
    end;
    // bin N/2 (Nyquist), if N is even
    if (count mod 2 = 0) then
    begin
      aspectrum^ := abs(freq[count div 2].x)/count;
    //inc(aspectrum);
    end;

    freq := nil;
    buff := nil;
  end;
end;

constructor tspectrums.create;
begin
  inherited create;
end;

destructor tspectrums.destroy;
begin
  clear;
  inherited destroy;
end;

procedure tspectrums.clear;
var
  i: longint;
begin
  for i := low(fspectrums) to high(fspectrums) do
  begin
    setlength(fspectrums[i], 0);
  end;
  setlength(fspectrums, 0);
end;

procedure tspectrums.analyze(const achannels: tchannels; asamplecount, asamplerate: longint);
var
  ch: longint;
  i: longint;
begin
  for ch := low(achannels) to high(achannels) do
  begin
     for i := 0 to (asamplecount div SPECTRUMWINDOWSIZE) -1 do
     begin
       getspectrum(@achannels [ch][(i * spectrumwindowsize)], spectrumwindowsize,
                   @fspectrums[ch][(i * spectrumwindowsize) div 2]);
     end;
   end;
end;

function tspectrums.getsample(const channel, index: longint): TSample;
begin
  result := fspectrums[channel][index];
end;

end.

