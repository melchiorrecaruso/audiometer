program Test_loudness;
{$mode objfpc}{$H+}
uses RegressionSupport, loudnessregression;
begin
  Run('loudness', @LoudnessTests);
  Writeln(Checks, ' checks, ', Failures, ' failures');
  if Failures <> 0 then Halt(1);
end.
