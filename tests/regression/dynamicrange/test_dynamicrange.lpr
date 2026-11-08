program Test_dynamicrange;
{$mode objfpc}{$H+}
uses RegressionSupport, dynamicrangeregression;
begin
  Run('dynamicrange', @DRTests);
  Writeln(Checks, ' checks, ', Failures, ' failures');
  if Failures <> 0 then Halt(1);
end.
