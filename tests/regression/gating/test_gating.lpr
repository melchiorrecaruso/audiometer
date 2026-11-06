program Test_gating;
{$mode objfpc}{$H+}
uses RegressionSupport, gatingregression;
begin
  Run('gating', @GatingTests);
  Writeln(Checks, ' checks, ', Failures, ' failures');
  if Failures <> 0 then Halt(1);
end.
