program Test_slidingmax;
{$mode objfpc}{$H+}
uses RegressionSupport, slidingmaxregression;
begin
  Run('slidingmax', @SlidingMaxTests);
  Writeln(Checks, ' checks, ', Failures, ' failures');
  if Failures <> 0 then Halt(1);
end.
