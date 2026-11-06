program Test_spectrum;
{$mode objfpc}{$H+}
uses RegressionSupport, spectrumregression;
begin
  Run('spectrum', @SpectrumTests);
  Writeln(Checks, ' checks, ', Failures, ' failures');
  if Failures <> 0 then Halt(1);
end.
