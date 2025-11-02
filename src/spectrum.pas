unit Spectrum;

{$mode objfpc}{$h+}
{$modeswitch advancedrecords}

interface

uses
  Common, SysUtils, ufft, utypes;

const
  DEFAULTWINDOWSIZE = 1024;
  DEFAULTHOPSIZE    = DEFAULTWINDOWSIZE div 2;

type
  TSpectrums = record
  private
    FChannels: TDoubleMatrix;
    FWindowCount: longint;
    FWindowSize: longint;
    FHopSize: longint;
    FOutBins: longint;
    procedure SmoothSpectrogram;
  public
    procedure Init(AWindowSize, AHopSize: LongInt);
    procedure Finalize;

    procedure Process(const AChannels: TDoubleMatrix; ASampleCount, ASampleRate: longint);

    property WindowSize: longint read FWindowSize;
    property WindowCount: longint read FWindowCount;
    property HopSize: longint read FHopSize;
    property OutBins: longint read FOutBins;

    property Channels: TDoubleMatrix read FChannels;
  end;


implementation

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
    ASpectrum^ := Abs(Freq[0].x)/Count;
    Inc(ASpectrum);
    // bin da 1 a N/2 - 1
    for i := 1 to (Count div 2) -1 do
    begin
      ASpectrum^ := 2*Sqrt(Sqr(Freq[i].x) + Sqr(Freq[i].y))/Count;
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

  if AWindowSize <= 0 then
    AWindowSize := DEFAULTWINDOWSIZE;
  if AHopSize <= 0 then
    AHopSize := DEFAULTHOPSIZE;

  FWindowSize := AWindowSize;
  FHopSize    := AHopSize;
end;

procedure TSpectrums.Finalize;
begin
  FWindowCount := 0;
  FWindowSize  := 0;
  FHopSize     := 0;
  FOutBins     := 0;
  SetLength(FChannels, 0, 0);
end;

procedure TSpectrums.Process(const AChannels: TDoubleMatrix; ASampleCount, ASampleRate: longint);
var
  ch, i: longint;
begin
  FWindowCount := (ASampleCount - FWindowSize) div (FWindowSize - FHopSize);

  if FWindowCount > 0 then
  begin
    FOutBins := FWindowSize div 2 + 1;

    SetLength(FChannels, Length(AChannels), FWindowCount * OutBins);
    for ch := Low(AChannels) to High(AChannels) do
    begin
      for i := 0 to FWindowCount -1 do
      begin
        GetSpectrum(@AChannels[ch][(i * FHopSize)], FWindowSize,  @FChannels[ch][(i * OutBins)]);
      end;
    end;
    SmoothSpectrogram;
  end else
    Finalize;
end;

procedure TSpectrums.SmoothSpectrogram;
var
  ch, i, j, index: longint;
  Tmp: TDoubleVector;
begin
  SetLength(Tmp, FWindowCount * FOutBins);

  // --- Smoothing sull'asse delle frequenze ---
  for ch := Low(FChannels) to High(FChannels) do
  begin
    Move(FChannels[ch][0], Tmp[0], Length(FChannels[ch]) * SizeOf(TDouble));

    for i := 0 to FWindowCount - 1 do
    begin
      for j := 1 to FOutBins - 2 do
      begin
        index := i * FOutBins + j;

        FChannels[ch][index] := (Tmp[index - 1] + 2 * Tmp[index] + Tmp[index + 1]) / 4;
      end;
    end;
  end;

  // --- Smoothing sull'asse del tempo ---
  for ch := Low(FChannels) to High(FChannels) do
  begin
    Move(FChannels[ch][0], Tmp[0], Length(FChannels[ch]) * SizeOf(TDouble));

    for i := 1 to FWindowCount - 2 do
    begin
      for j := 0 to FOutBins - 1 do
      begin
        index := i * FOutBins + j;

        FChannels[ch][index] := (Tmp[index - FOutBins] + 2 * Tmp[index] + Tmp[index + FOutBins]) / 4;
      end;
    end;
  end;
end;

end.

