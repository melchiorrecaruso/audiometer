unit Spectrum;

{$mode objfpc}{$h+}
{$modeswitch advancedrecords}

interface

uses
  Common, SysUtils, ufft, utypes;

const
  SPECTRUMWINDOWSIZE = 1024;

type
  TSpectrums = record
  private
    FChannels: TDoubleMatrix;
    FWindowCount: longint;
    FWindowSize: longint;
    FHopSize: longint;
    FOutBins: longint;
  public
    procedure Init(AWindowSize, AHopSize: LongInt);
    procedure Finalize;

    procedure Process(const AChannels: TDoubleMatrix; ASampleCount, ASampleRate: longint);

    property WindowSize: longint read FWindowSize;
    property WindowCount: longint read FWindowCount;
    property HopSize: longint read FhopSize;
    property OutBins: longint read FOutBins;

    property Channels: TDoubleMatrix read FChannels;
  end;


implementation

uses
  Math;

procedure GetSpectrum(ASamples: PDouble; Count: longint; ASpectrum: PDouble);
var
  i: longint;
  Buff: TCompVector = nil;
  Freq: TCompVector = nil;
  Window: TDouble;
begin
  if Count > 0 then
  begin
    SetLength(Buff, Count);
    for i := 0 to Count -1 do
    begin
      Window := 0.5 - 0.5 * cos(2 * pi * i / (Count - 1));
      Buff[i].x := Window * ASamples^;
      Buff[i].y := 0;
      inc(ASamples);
    end;
    Freq := FFT(Count, Buff);

    // bin 0 (DC)
    ASpectrum^ := abs(Freq[0].x)/Count;
    Inc(ASpectrum);
    // bin da 1 a N/2 - 1
    for i := 1 to (Count div 2) -1 do
    begin
      ASpectrum^ := 2*Sqrt(sqr(Freq[i].x) + Sqr(Freq[i].y))/Count;
      inc(ASpectrum);
    end;
    // bin N/2 (Nyquist), if N is even
    if (Count mod 2 = 0) then
    begin
      ASpectrum^ := Abs(Freq[Count div 2].x)/Count;
    //Inc(ASpectrum);
    end;
  end;
end;

// TSpectrum

procedure TSpectrums.Init(AWindowSize, AHopSize: LongInt);
begin
  Finalize;
  FWindowSize := AWindowSize;
  FHopSize    := AHopSize;
end;

procedure TSpectrums.Finalize;
begin
  SetLength(FChannels, 0, 0);
end;

procedure TSpectrums.Process(const AChannels: TDoubleMatrix; ASampleCount, ASampleRate: longint);
var
  ch, i: longint;
begin
  FWindowCount := (ASampleCount - FWindowSize) div FHopSize + 1;
  FOutBins     := FWindowSize div 2 + 1;

  if FwindowCount > 0 then
  begin
    SetLength(FChannels, Length(AChannels), FWindowCount * OutBins);
    for ch := Low(AChannels) to High(AChannels) do
    begin
       for i := 0 to FWindowCount -1 do
       begin
         GetSpectrum(@AChannels[ch][(i * FHopSize)], FWindowSize,  @FChannels[ch][(i * OutBins)]);
       end;
     end;
  end;
end;

end.

