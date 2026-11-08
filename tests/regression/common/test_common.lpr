program Test_common;
{$mode objfpc}{$H+}
uses RegressionSupport, commonregression;
begin
  Run('common', @CommonTests);
  Writeln(Checks, ' checks, ', Failures, ' failures');
  if Failures <> 0 then Halt(1);
end.
