{
  Description: EBU Tech 3341 / 3342 compliance validator.

  Scans a folder of EBU "minimum-requirements" test files, runs each one
  through the TLoudnessMeter (via TTrackAnalyzer) and compares the relevant
  measure against the expected value and tolerance from EBU Tech 3341
  (Table 1, test cases 1-23) and EBU Tech 3342 (Table 1, test cases 1-6).

  The test case for each file is recognized from its name: any file whose
  name contains "3341-N" and/or "3342-N" is matched to the corresponding
  case(s). The combined files (e.g. seq-3341-7_seq-3342-5) are checked twice.

  RunEBUSuite returns the number of failures; an empty fixture set fails.

  Copyright (C) 2026 Melchiorre Caruso <melchiorrecaruso@gmail.com>
  GPL v2 or later.
}

unit EBUCheck;

{$mode objfpc}{$h+}

interface

function RunEBUSuite(const AFolder: string): longint;

implementation

uses Classes, SysUtils, Math, Common, Loudness, AudioTrack, TrackAnalyzer;

type
  TMetric = (mI, mMaxM, mMaxS, mLRA, mTP, mSteadyM, mSteadyS, mRepeatedLRA);

  TCaseSpec = record
    Std: integer;       // 3341 or 3342
    Num: integer;       // test-case number within the table
    Metric: TMetric;
    Expected: double;
    TolLo, TolHi: double;   // allowed band: [Expected - TolLo, Expected + TolHi]
    Note: string;
  end;

  TResults = record
    Ok: boolean;
    Status: longint;
    I, MaxM, MaxS, MinM, MinS, SteadyMaxM, SteadyMaxS, LRA, TP: double;
    SteadyMCount, SteadySCount: integer;
    RepeatedLRA: double;
  end;

const
  // EBU Tech 3341, Table 1 (synthesized at 48 kHz). Loudness in LUFS, TP in dBTP.
  // EBU Tech 3342, Table 1. LRA in LU.
  Cases: array[0..42] of TCaseSpec = (
    // --- Tech 3341: integrated loudness ---
    (Std:3341; Num: 1; Metric:mI;    Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'M,S,I = -23.0 LUFS'),
    (Std:3341; Num: 2; Metric:mI;    Expected:-33.0; TolLo:0.1; TolHi:0.1; Note:'M,S,I = -33.0 LUFS'),
    (Std:3341; Num: 3; Metric:mI;    Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'3 tones'),
    (Std:3341; Num: 4; Metric:mI;    Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'5 tones, gating'),
    (Std:3341; Num: 5; Metric:mI;    Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'relative gate'),
    (Std:3341; Num: 6; Metric:mI;    Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'5.0 / 5.1 channels'),
    (Std:3341; Num: 7; Metric:mI;    Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'authentic NLR'),
    (Std:3341; Num: 8; Metric:mI;    Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'authentic WLR'),
    // --- Tech 3341: short-term (max) ---
    (Std:3341; Num: 9; Metric:mSteadyS; Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'S constant after 3 s'),
    (Std:3341; Num:10; Metric:mMaxS; Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'file-based, max S'),
    (Std:3341; Num:11; Metric:mMaxS; Expected:-19.0; TolLo:0.1; TolHi:0.1; Note:'LIVE test; checking peak segment'),
    // --- Tech 3341: momentary (max) ---
    (Std:3341; Num:12; Metric:mSteadyM; Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'M constant after 1 s'),
    (Std:3341; Num:13; Metric:mMaxM; Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'file-based, max M'),
    (Std:3341; Num:14; Metric:mMaxM; Expected:-19.0; TolLo:0.1; TolHi:0.1; Note:'LIVE test; checking peak segment'),
    // --- Tech 3341: true peak (asymmetric tolerance +0.2 / -0.4 dBTP) ---
    (Std:3341; Num:15; Metric:mTP;   Expected:-6.0;  TolLo:0.4; TolHi:0.2; Note:'fs/4, 0.50 FFS, 0 deg'),
    (Std:3341; Num:16; Metric:mTP;   Expected:-6.0;  TolLo:0.4; TolHi:0.2; Note:'fs/4, 0.50 FFS, 45 deg'),
    (Std:3341; Num:17; Metric:mTP;   Expected:-6.0;  TolLo:0.4; TolHi:0.2; Note:'fs/6, 0.50 FFS, 60 deg'),
    (Std:3341; Num:18; Metric:mTP;   Expected:-6.0;  TolLo:0.4; TolHi:0.2; Note:'fs/8, 0.50 FFS, 67.5 deg'),
    (Std:3341; Num:19; Metric:mTP;   Expected: 3.0;  TolLo:0.4; TolHi:0.2; Note:'fs/4, 1.41 FFS, 45 deg'),
    (Std:3341; Num:20; Metric:mTP;   Expected: 0.0;  TolLo:0.4; TolHi:0.2; Note:'embedded fs/4, offset 0'),
    (Std:3341; Num:21; Metric:mTP;   Expected: 0.0;  TolLo:0.4; TolHi:0.2; Note:'embedded fs/4, offset 1'),
    (Std:3341; Num:22; Metric:mTP;   Expected: 0.0;  TolLo:0.4; TolHi:0.2; Note:'embedded fs/4, offset 2'),
    (Std:3341; Num:23; Metric:mTP;   Expected: 0.0;  TolLo:0.4; TolHi:0.2; Note:'embedded fs/4, offset 3'),
    // --- Tech 3342: loudness range ---
    (Std:3342; Num: 1; Metric:mLRA;  Expected:10.0;  TolLo:1.0; TolHi:1.0; Note:'2 tones, 10 LU apart'),
    (Std:3342; Num: 2; Metric:mLRA;  Expected: 5.0;  TolLo:1.0; TolHi:1.0; Note:'2 tones, 5 LU apart'),
    (Std:3342; Num: 3; Metric:mLRA;  Expected:20.0;  TolLo:1.0; TolHi:1.0; Note:'2 tones, 20 LU apart'),
    (Std:3342; Num: 4; Metric:mLRA;  Expected:15.0;  TolLo:1.0; TolHi:1.0; Note:'5 tone-segments'),
    (Std:3342; Num: 5; Metric:mLRA;  Expected: 5.0;  TolLo:1.0; TolHi:1.0; Note:'authentic NLR'),
    (Std:3342; Num: 6; Metric:mLRA;  Expected:15.0;  TolLo:1.0; TolHi:1.0; Note:'authentic WLR'),
    // Table 1 requires M, S and I, not only I, for cases 1 and 2.
    (Std:3341; Num:1; Metric:mSteadyM; Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'all settled M readings'),
    (Std:3341; Num:1; Metric:mSteadyS; Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'all settled S readings'),
    (Std:3341; Num:2; Metric:mSteadyM; Expected:-33.0; TolLo:0.1; TolHi:0.1; Note:'all settled M readings'),
    (Std:3341; Num:2; Metric:mSteadyS; Expected:-33.0; TolLo:0.1; TolHi:0.1; Note:'all settled S readings'),
    // Supplementary package files, not numbered compliance cases.
    (Std:0; Num:1; Metric:mI; Expected:-20.0; TolLo:0.1; TolHi:0.1; Note:'package alignment tone'),
    (Std:0; Num:2; Metric:mI; Expected:-26.0; TolLo:0.1; TolHi:0.1; Note:'package alignment tone'),
    (Std:0; Num:3; Metric:mI; Expected:-40.0; TolLo:0.1; TolHi:0.1; Note:'package alignment tone'),
    (Std:0; Num:4; Metric:mI; Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'Tech 3343 section 8.2 pink noise; 0.1 LU test budget'),
    (Std:3342; Num:1; Metric:mRepeatedLRA; Expected:10; TolLo:1; TolHi:1; Note:'whole WAV repeated twice'),
    (Std:3342; Num:2; Metric:mRepeatedLRA; Expected:5; TolLo:1; TolHi:1; Note:'whole WAV repeated twice'),
    (Std:3342; Num:3; Metric:mRepeatedLRA; Expected:20; TolLo:1; TolHi:1; Note:'whole WAV repeated twice'),
    (Std:3342; Num:4; Metric:mRepeatedLRA; Expected:15; TolLo:1; TolHi:1; Note:'whole WAV repeated twice'),
    (Std:3342; Num:5; Metric:mRepeatedLRA; Expected:5; TolLo:1; TolHi:1; Note:'whole WAV repeated twice'),
    (Std:3342; Num:6; Metric:mRepeatedLRA; Expected:15; TolLo:1; TolHi:1; Note:'whole WAV repeated twice')
  );

function MetricName(M: TMetric): string;
begin
  case M of
    mI:    result := 'I  ';
    mMaxM, mSteadyM: result := 'M  ';
    mMaxS, mSteadyS: result := 'S  ';
    mLRA, mRepeatedLRA: result := 'LRA';
    mTP:   result := 'TP ';
  else     result := '?  ';
  end;
end;

function UnitName(M: TMetric): string;
begin
  case M of
    mLRA, mRepeatedLRA: result := 'LU';
    mTP:  result := 'dBTP';
  else    result := 'LUFS';
  end;
end;

// Find the test number that follows "<std>-" in the file name, or -1.
function ExtractTestNum(const AName: string; AStd: integer): integer;
var
  prefix, s: string;
  p, q: integer;
begin
  Result := -1;
  prefix := IntToStr(AStd) + '-';
  s := AName;
  p := Pos(prefix, s);
  if p = 0 then Exit;
  p := p + Length(prefix);
  q := p;
  while (q <= Length(s)) and (s[q] in ['0'..'9']) do Inc(q);
  if q > p then
    Result := StrToIntDef(Copy(s, p, q - p), -1);
end;

function MatchesCase(const AName: string; const C: TCaseSpec): boolean;
const
  ExtraNames: array[1..4] of string = (
    '1kHz Sine -20 LUFS-16bit.wav', '1kHz Sine -26 LUFS-16bit.wav',
    '1kHz Sine -40 LUFS-16bit.wav',
    'EBU-reference_listening_signal_pinknoise_500Hz_2kHz_R128.wav');
begin
  if C.Std = 0 then Result := AName = ExtraNames[C.Num]
  else Result := ExtractTestNum(AName, C.Std) = C.Num;
end;

// Run the full analysis pipeline on one file and collect the loudness measures.
function Analyze(const AFileName: string): TResults;
var
  Track: TTrack;
  Stream: TFileStream;
  Analyzer: TTrackAnalyzer;
  L, RepeatedMeter: TLoudnessMeter;
  Repeated: TDoubleMatrix;
  DurMs, t, ch, Count: longint;
  m, s: double;
begin
  Result.Ok := False;
  Result.Status := 0;
  Result.I := NegInfinity; Result.MaxM := NegInfinity;
  Result.MaxS := NegInfinity; Result.LRA := NegInfinity; Result.TP := NegInfinity;
  Result.MinM := Infinity; Result.MinS := Infinity;
  Result.SteadyMaxM := NegInfinity; Result.SteadyMaxS := NegInfinity;
  Result.SteadyMCount := 0; Result.SteadySCount := 0;
  Result.RepeatedLRA := NegInfinity;

  Track := TTrack.Create(AFileName);
  Stream := nil;
  try
    Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    // FFT off: we only need loudness; run synchronously via the worker thread.
    Analyzer := TTrackAnalyzer.Create(Track, Stream, False);
    Analyzer.FreeOnTerminate := False;
    try
      Analyzer.Start;
      Analyzer.WaitFor;
      Result.Status := Analyzer.Status;
    finally
      Analyzer.Free;
    end;

    if Result.Status = 0 then
    begin
      L := Track.Loudness;
      Result.I   := L.IntegratedLoudness;
      Result.LRA := L.LoudnessRange;
      Result.TP  := L.TruePeak;
      Result.MaxM := L.MaxMomentaryLoudness;   // sample-accurate maxima
      Result.MaxS := L.MaxShortTermLoudness;

      // Inspect the full settled 100 ms grid, not just its final value.
      // Non-finite readings must fail too, not be silently discarded.
      if Track.SampleRate > 0 then
        DurMs := (Int64(Track.SampleCount) * 1000) div Track.SampleRate
      else
        DurMs := 0;
      t := 0;
      while t <= DurMs do
      begin
        m := L.MomentaryLoudness(t);
        s := L.ShortTermLoudness(t);
        if (t >= 1000) and (t <= DurMs) then
        begin
          Inc(Result.SteadyMCount);
          if IsNan(m) or IsInfinite(m) then m := NegInfinity;
          Result.MinM := Min(Result.MinM, m);
          Result.SteadyMaxM := Max(Result.SteadyMaxM, m);
        end;
        if (t >= 3000) and (t <= DurMs) then
        begin
          Inc(Result.SteadySCount);
          if IsNan(s) or IsInfinite(s) then s := NegInfinity;
          Result.MinS := Min(Result.MinS, s);
          Result.SteadyMaxS := Max(Result.SteadyMaxS, s);
        end;
        Inc(t, 100);
      end;

      // Tech 3342 section 4: the expected LRA is unchanged if the entire
      // programme is repeated. No filter reset between the two copies.
      if ExtractTestNum(ExtractFileName(AFileName), 3342) in [1..6] then
      begin
        Count := Track.SampleCount;
        SetLength(Repeated,Track.ChannelCount,Count*2);
        for ch:=0 to Track.ChannelCount-1 do
        begin
          Move(Track.Channels[ch][0],Repeated[ch][0],SizeInt(Count)*SizeOf(double));
          Move(Track.Channels[ch][0],Repeated[ch][Count],SizeInt(Count)*SizeOf(double));
        end;
        RepeatedMeter := Default(TLoudnessMeter);
        RepeatedMeter.Init;
        try
          RepeatedMeter.Process(Repeated,Count*2,Track.SampleRate);
          Result.RepeatedLRA := RepeatedMeter.LoudnessRange;
        finally
          RepeatedMeter.Finalize;
        end;
      end;
      Result.Ok := True;
    end;
  finally
    Stream.Free;
    Track.Free;
  end;
end;

// Report the worst settled reading. Passing this value implies that every
// reading on the settled grid is within the symmetric loudness tolerance.
function WorstReading(ALow, AHigh, AExpected: double; ACount: integer): double;
begin
  if (ACount = 0) or IsInfinite(ALow) or IsInfinite(AHigh) then Exit(NegInfinity);
  if Abs(ALow - AExpected) > Abs(AHigh - AExpected) then Result := ALow
  else Result := AHigh;
end;

function Measured(const R: TResults; const C: TCaseSpec): double;
begin
  case C.Metric of
    mI: result := R.I;
    mMaxM: result := R.MaxM;
    mMaxS: result := R.MaxS;
    mSteadyM: result := WorstReading(R.MinM, R.SteadyMaxM, C.Expected, R.SteadyMCount);
    mSteadyS: result := WorstReading(R.MinS, R.SteadyMaxS, C.Expected, R.SteadySCount);
    mLRA: result := R.LRA;
    mRepeatedLRA: result := R.RepeatedLRA;
    mTP: result := R.TP;
  else result := NegInfinity;
  end;
end;

function RunEBUSuite(const AFolder: string): longint;
var
  Folder: string;
  Info: TSearchRec;
  Files: TStringList;
  i, k, j: integer;
  Covered: array[Low(Cases)..High(Cases)] of boolean;
  ShortVariants, MomentaryVariants: array[1..20] of boolean;
  R: TResults;
  C: TCaseSpec;
  meas, dev: double;
  pass, anyCase, HasFiveChannels, HasSixChannels: boolean;
  nTotal, nPass: integer;
  line, sdev: string;
begin
  HasFiveChannels := False;
  HasSixChannels := False;
  FillChar(Covered, SizeOf(Covered), 0);
  FillChar(ShortVariants, SizeOf(ShortVariants), 0);
  FillChar(MomentaryVariants, SizeOf(MomentaryVariants), 0);
  Folder := AFolder;
  Folder := IncludeTrailingPathDelimiter(Folder);

  Files := TStringList.Create;
  try
    if FindFirst(Folder + '*.wav', faAnyFile, Info) = 0 then
    begin
      repeat
        if (Info.Attr and faDirectory) = 0 then
          Files.Add(Info.Name);
      until FindNext(Info) <> 0;
      FindClose(Info);
    end;
    Files.Sort;

    Writeln('EBU Tech 3341 / 3342 compliance validation');
    Writeln('Folder: ', Folder);
    Writeln(StringOfChar('-', 96));
    Writeln(Format('%-40s %-4s %10s %10s %9s %14s  %s',
      ['File', 'Meas', 'measured', 'expected', 'dev', 'tolerance', 'result']));
    Writeln(StringOfChar('-', 96));

    nTotal := 0;
    nPass  := 0;

    for i := 0 to Files.Count - 1 do
    begin
      anyCase := False;
      R.Ok := False;
      if Files[i] = 'seq-3341-6-5channels-16bit.wav' then HasFiveChannels := True;
      if Files[i] = 'seq-3341-6-6channels-WAVEEX-16bit.wav' then HasSixChannels := True;

      for k := Low(Cases) to High(Cases) do
      begin
        C := Cases[k];
        if not MatchesCase(Files[i], C) then Continue;

        anyCase := True;
        Covered[k] := True;
        if (C.Std = 3341) and (C.Num in [10, 13]) then
          for j := 1 to 20 do
            if Pos(Format('3341-%d-%d-', [C.Num, j]), Files[i]) > 0 then
              if C.Num = 10 then ShortVariants[j] := True
              else MomentaryVariants[j] := True;

        // analyze once, lazily, the first time a case matches
        if not R.Ok then
          R := Analyze(Folder + Files[i]);

        if not R.Ok then
        begin
          Inc(nTotal);
          Writeln(Format('%-40s  read error (status %d)', [Files[i], R.Status]));
          Break;
        end;

        meas := Measured(R, C);
        dev  := meas - C.Expected;
        pass := (dev >= -C.TolLo - 1e-9) and (dev <= C.TolHi + 1e-9);

        Inc(nTotal);
        if pass then Inc(nPass);

        // FPC Format has no '+' flag, so build the signed deviation by hand.
        sdev := Format('%.2f', [dev]);
        if dev >= 0 then sdev := '+' + sdev;

        line := Format('%-40s %-4s %10.2f %10.2f %9s   +%.2f/-%.2f %-4s  %s',
          [Files[i], MetricName(C.Metric), meas, C.Expected, sdev,
           C.TolHi, C.TolLo, UnitName(C.Metric),
           BoolToStr(pass, 'PASS', 'FAIL')]);
        if C.Note <> '' then line := line + '   [' + C.Note + ']';
        if C.Metric = mSteadyM then line := line + Format(' (%d readings)', [R.SteadyMCount]);
        if C.Metric = mSteadyS then line := line + Format(' (%d readings)', [R.SteadySCount]);
        Writeln(line);
      end;

      if not anyCase then Writeln('UNMATCHED WAV: ', Files[i]);
    end;

    if not HasFiveChannels then
    begin
      Inc(nTotal); Writeln('MISSING 3341-6 5.0 PCM variant');
    end;
    if not HasSixChannels then
    begin
      Inc(nTotal); Writeln('MISSING 3341-6 5.1 WAVEEX variant');
    end;
    // Missing fixtures must not turn a partial reference run into "all pass".
    for k := Low(Cases) to High(Cases) do
      if not Covered[k] then
      begin
        Inc(nTotal);
        Writeln(Format('MISSING case %d-%d %s',
          [Cases[k].Std, Cases[k].Num, MetricName(Cases[k].Metric)]));
      end;
    for j := 1 to 20 do
    begin
      if not ShortVariants[j] then
      begin
        Inc(nTotal); Writeln('MISSING variant 3341-10-', j);
      end;
      if not MomentaryVariants[j] then
      begin
        Inc(nTotal); Writeln('MISSING variant 3341-13-', j);
      end;
    end;
    Writeln(StringOfChar('-', 96));
    Writeln(Format('Checks: %d   Passed: %d   Failed: %d', [nTotal, nPass, nTotal - nPass]));
    if (nTotal > 0) and (nPass = nTotal) then
      Writeln('RESULT: all checks within EBU tolerance.')
    else if nTotal = 0 then
      Writeln('RESULT: no EBU test files found in folder.')
    else
      Writeln('RESULT: some checks outside tolerance - see FAIL rows above.');
  finally
    Files.Free;
  end;

  // A missing/empty fixture directory is a failure, never a false green.
  if nTotal = 0 then Result := 1 else Result := nTotal - nPass;
end;

end.
