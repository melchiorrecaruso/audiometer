program Test_boundaries;
{$mode objfpc}{$H+}
uses RegressionSupport, boundaryregression;
begin
  Run('boundaries', @BoundaryTests);
  Writeln(Checks, ' checks, ', Failures, ' failures');
  if Failures <> 0 then Halt(1);
end.
