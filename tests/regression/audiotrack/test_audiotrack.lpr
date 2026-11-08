program Test_audiotrack;
{$mode objfpc}{$H+}
uses RegressionSupport, AudioTrackRegression;
begin
  Run('audiotrack', @AudioTrackTests);
  Writeln(Checks, ' checks, ', Failures, ' failures');
  if Failures <> 0 then Halt(1);
end.
