unit AudioTrackRegression;
{$mode objfpc}{$H+}
interface
procedure AudioTrackTests;
implementation
uses Classes, SysUtils, Math, AudioTrack, WavReader, RegressionSupport;

function FormatFor(AChannels: word; ARate: longword): TWaveFormat;
begin
  Result:=Default(TWaveFormat);
  Result.Channels:=AChannels; Result.SampleRate:=ARate;
  Result.BitsPerSample:=16; Result.BlockAlign:=AChannels*2;
end;

procedure AudioTrackTests;
var
  T: TTrack; L: TTrackList; F, Bad: TWaveFormat;
  n,w,x,ch,i,First,Last: integer;
  Lo,Hi: double; Raised: boolean; Name: string;
begin
  Name:='/'+StringOfChar('a',150)+'/'+StringOfChar('b',150)+'.wav';
  T:=TTrack.Create(Name);
  try
    Check('long path preserved',Ord(T.Filename=Name),1,0);
    Check('initial samples',T.SampleCount,0,0);
    Check('initial duration',T.DurationSeconds,0,0);
    F:=FormatFor(2,48000);
    T.SetFormat(F,60000);
    Check('channel allocation',Length(T.Channels),2,0);
    Check('frame allocation',Length(T.Channels[1]),60000,0);
    Check('fractional duration',T.DurationSeconds,1.25,1e-12);
    Check('integer report duration',T.Duration,1,0);
    T.ClearChannels;
    Check('clear preserves duration',T.DurationSeconds,1.25,1e-12);
    Check('clear releases samples',Length(T.Channels),0,0);
    T.PrepareWaveformCache(3);
    Check('cache after release',T.WaveformMax(0,0),0,0);

    // Independent exhaustive small-grid oracle: every downsampled column
    // owns [floor(x*N/W), floor((x+1)*N/W)); empty columns repeat a sample.
    for n:=1 to 17 do begin
      T.SetFormat(F,n);
      for ch:=0 to 1 do for i:=0 to n-1 do
        T.Channels[ch][i]:=((i*7+ch*3) mod 19-9)/10;
      for w:=1 to 19 do begin
        T.PrepareWaveformCache(w);
        for ch:=0 to 1 do for x:=0 to w-1 do begin
          First:=Floor(x*n/w); Last:=Floor((x+1)*n/w)-1;
          if Last<First then Last:=First;
          Lo:=Infinity; Hi:=-Infinity;
          for i:=First to Last do begin
            Lo:=Min(Lo,T.Channels[ch][i]); Hi:=Max(Hi,T.Channels[ch][i]);
          end;
          Check('waveform minimum',T.WaveformMin(ch,x),Lo,0);
          Check('waveform maximum',T.WaveformMax(ch,x),Hi,0);
        end;
      end;
    end;

    T.SetFormat(F,10);
    T.Channels[0][9]:=1; T.Channels[1][9]:=-1;
    T.PrepareWaveformCache(3);
    Check('positive EOF peak',T.WaveformMax(0,2),1,0);
    Check('negative EOF peak',T.WaveformMin(1,2),-1,0);
    T.Channels[0][9]:=0.5;
    T.InvalidateWaveformCache;
    Check('explicit invalidation clears values',T.WaveformMax(0,2),0,0);
    T.PrepareWaveformCache(3);
    Check('cache after direct sample edit',T.WaveformMax(0,2),0.5,0);
    T.SetFormat(F,10);
    Check('fresh samples on reformat',T.Channels[0][9],0,0);
    T.PrepareWaveformCache(3);
    Check('reformat invalidates cache',T.WaveformMax(0,2),0,0);
    Check('invalid channel index',T.WaveformMin(-1,0),0,0);
    Check('invalid column index',T.WaveformMax(0,3),0,0);
    T.PrepareWaveformCache(-1);
    Check('negative width clears cache',T.WaveformMax(0,0),0,0);

    // Invalid calls must not partially overwrite a populated track or cache.
    T.Channels[0][9]:=0.75; T.PrepareWaveformCache(3);
    for i:=0 to 2 do begin
      Bad:=F; n:=10;
      case i of 0: Bad.SampleRate:=0; 1: Bad.Channels:=0; 2: n:=-1; end;
      Raised:=False;
      try T.SetFormat(Bad,n); except on E:EArgumentException do Raised:=True; end;
      Check('invalid format rejected',Ord(Raised),1,0);
      Check('invalid format preserves count',T.SampleCount,10,0);
      Check('invalid format preserves rate',T.SampleRate,48000,0);
      Check('invalid format preserves samples',T.Channels[0][9],0.75,0);
      Check('invalid format preserves cache',T.WaveformMax(0,2),0.75,0);
    end;
    T.SetFormat(F,32);
    T.Spectrums.Init(32,16); T.Spectrums.Process(T.Channels,32);
    Check('spectrum prepared',T.Spectrums.WindowCount,1,0);
    T.SetFormat(F,0);
    Check('zero frame count accepted',T.SampleCount,0,0);
    Check('zero frame duration',T.DurationSeconds,0,0);
    Check('reformat clears old spectrum',T.Spectrums.WindowCount,0,0);
    T.ClearChannels; T.ClearChannels;
  finally T.Free; end;

  L:=TTrackList.Create;
  try
    L.Sort; Check('empty list sort',L.Count,0,0);
    L.Add('z.wav'); L.Add('a.wav'); L.Sort;
    Check('list sort',Ord(L[0].Filename='a.wav'),1,0);
    L.Delete(0); Check('list delete',L.Count,1,0);
    Check('remaining track',Ord(L[0].Filename='z.wav'),1,0);
    Raised:=False;
    try L.Delete(10); except on E:EListError do Raised:=True; end;
    Check('invalid delete rejected',Ord(Raised),1,0);
    Check('invalid delete preserves list',L.Count,1,0);
    L.Add(Name); Check('list preserves long path',Ord(L[1].Filename=Name),1,0);
    L.Clear; L.Clear; Check('repeated list clear',L.Count,0,0);
  finally L.Free; end;
end;
end.
