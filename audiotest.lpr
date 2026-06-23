{
  Description: EBU Tech 3341 / 3342 compliance validator.

  Scans a folder of EBU "minimum-requirements" test files, runs each one
  through the TLoudnessMeter (via TTrackAnalyzer) and compares the relevant
  measure against the expected value and tolerance from EBU Tech 3341
  (Table 1, test cases 1-23) and EBU Tech 3342 (Table 1, test cases 1-6).

  The test case for each file is recognized from its name: any file whose
  name contains "3341-N" and/or "3342-N" is matched to the corresponding
  case(s). The combined files (e.g. seq-3341-7_seq-3342-5) are checked twice.

  Usage:   ebuvalidate [folder]        (default folder: current directory)

  Copyright (C) 2026 Melchiorre Caruso <melchiorrecaruso@gmail.com>
  GPL v2 or later.
}

program AudioTest;

{$mode objfpc}{$h+}

uses
  {$ifdef unix} cthreads, {$endif}
  Classes, SysUtils, Math,
  Common, Loudness, DynamicRange, Spectrum, SoundWav;

type
  TMetric = (mI, mMaxM, mMaxS, mLRA, mTP);

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
    I, MaxM, MaxS, LastM, LastS, LRA, TP: double;
  end;

const
  // EBU Tech 3341, Table 1 (synthesized at 48 kHz). Loudness in LUFS, TP in dBTP.
  // EBU Tech 3342, Table 1. LRA in LU.
  Cases: array[0..28] of TCaseSpec = (
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
    (Std:3341; Num: 9; Metric:mMaxS; Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'S constant after 3 s'),
    (Std:3341; Num:10; Metric:mMaxS; Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'file-based, max S'),
    (Std:3341; Num:11; Metric:mMaxS; Expected:-19.0; TolLo:0.1; TolHi:0.1; Note:'LIVE test; checking peak segment'),
    // --- Tech 3341: momentary (max) ---
    (Std:3341; Num:12; Metric:mMaxM; Expected:-23.0; TolLo:0.1; TolHi:0.1; Note:'M constant after 1 s'),
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
    (Std:3342; Num: 6; Metric:mLRA;  Expected:15.0;  TolLo:1.0; TolHi:1.0; Note:'authentic WLR')
  );

function MetricName(M: TMetric): string;
begin
  case M of
    mI:    result := 'I  ';
    mMaxM: result := 'M  ';
    mMaxS: result := 'S  ';
    mLRA:  result := 'LRA';
    mTP:   result := 'TP ';
  else     result := '?  ';
  end;
end;

function UnitName(M: TMetric): string;
begin
  case M of
    mLRA: result := 'LU';
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

function FindCase(AStd, ANum: integer; out ACase: TCaseSpec): boolean;
var
  i: integer;
begin
  for i := Low(Cases) to High(Cases) do
    if (Cases[i].Std = AStd) and (Cases[i].Num = ANum) then
    begin
      ACase := Cases[i];
      Exit(True);
    end;
  Result := False;
end;

// Run the full analysis pipeline on one file and collect the loudness measures.
function Analyze(const AFileName: string): TResults;
var
  Track: TTrack;
  Stream: TFileStream;
  Analyzer: TTrackAnalyzer;
  L: TLoudnessMeter;
  DurMs, t: longint;
  m, s: double;
begin
  Result.Ok := False;
  Result.Status := 0;
  Result.I := NegInfinity; Result.MaxM := NegInfinity;
  Result.MaxS := NegInfinity; Result.LRA := NegInfinity; Result.TP := NegInfinity;
  Result.LastM := NegInfinity; Result.LastS := NegInfinity;

  Track := TTrack.Create(AFileName);
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    // FFT off: we only need loudness; run synchronously via the worker thread.
    Analyzer := TTrackAnalyzer.Create(Track, Stream, False);
    Analyzer.FreeOnTerminate := False;
    Analyzer.Start;
    Analyzer.WaitFor;
    Result.Status := Analyzer.Status;
    Analyzer.Free;

    if Result.Status = 0 then
    begin
      L := Track.Loudness;
      Result.I   := L.IntegratedLoudness;
      Result.LRA := L.LoudnessRange;
      Result.TP  := L.TruePeak;
      Result.MaxM := L.MaxMomentaryLoudness;   // sample-accurate maxima
      Result.MaxS := L.MaxShortTermLoudness;

      // Steady-state (settled) reading for the "constant after Xs" cases:
      // scan the 100 ms grid and keep the last finite M/S value.
      if Track.SampleRate > 0 then
        DurMs := (Int64(Track.SampleCount) * 1000) div Track.SampleRate
      else
        DurMs := 0;
      t := 0;
      while t <= DurMs do
      begin
        m := L.MomentaryLoudness(t);
        s := L.ShortTermLoudness(t);
        if not IsInfinite(m) then Result.LastM := m;
        if not IsInfinite(s) then Result.LastS := s;
        Inc(t, 100);
      end;

      Result.Ok := True;
    end;
  finally
    Stream.Free;
    Track.Free;
  end;
end;

function Measured(const R: TResults; M: TMetric; ASettle: boolean): double;
begin
  case M of
    mI:    result := R.I;
    mMaxM: if ASettle then result := R.LastM else result := R.MaxM;
    mMaxS: if ASettle then result := R.LastS else result := R.MaxS;
    mLRA:  result := R.LRA;
    mTP:   result := R.TP;
  else     result := NegInfinity;
  end;
end;

// Tests "constant after Xs" (3341-9 S, 3341-12 M) check the steady-state
// value, not the start-up transient: read the settled value, not the maximum.
function CaseSettles(const C: TCaseSpec): boolean;
begin
  Result := (C.Std = 3341) and ((C.Num = 9) or (C.Num = 12));
end;

var
  Folder: string;
  Info: TSearchRec;
  Files: TStringList;
  i, k, nStd: integer;
  R: TResults;
  C: TCaseSpec;
  meas, dev: double;
  pass, anyCase: boolean;
  nTotal, nPass: integer;
  line, sdev: string;
begin
  if ParamCount >= 1 then
    Folder := ParamStr(1)
  else
    Folder := GetCurrentDir;
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

      for nStd := 0 to 1 do   // check both 3341 and 3342 tokens in the name
      begin
        if nStd = 0 then k := ExtractTestNum(Files[i], 3341)
                     else k := ExtractTestNum(Files[i], 3342);
        if k < 0 then Continue;
        if not FindCase(3341 + nStd, k, C) then Continue;   // nStd 0->3341, 1->3342

        anyCase := True;

        // analyze once, lazily, the first time a case matches
        if not R.Ok then
          R := Analyze(Folder + Files[i]);

        if not R.Ok then
        begin
          Writeln(Format('%-40s  read error (status %d)', [Files[i], R.Status]));
          Break;
        end;

        meas := Measured(R, C.Metric, CaseSettles(C));
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
        Writeln(line);
      end;

      if (not anyCase) then
        ; // file is not part of the EBU test set -> silently skipped
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

  // exit code: 0 = all pass (or nothing to do), 1 = at least one failure
  if nTotal - nPass > 0 then
    Halt(1)
  else
    Halt(0);
end.
