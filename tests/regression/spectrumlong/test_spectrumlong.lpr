program Test_spectrumlong;
{$mode objfpc}{$H+}
uses SpectrumCheck;
begin
  if RunSpectrumSuite <> 0 then Halt(1);
end.
