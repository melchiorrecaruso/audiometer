program Test_ebu;
{$mode objfpc}{$H+}
uses
  {$ifdef unix} cthreads, {$endif}
  SysUtils, EBUCheck;
begin
  if ParamCount <> 1 then
  begin
    Writeln(StdErr, 'Usage: test_ebu reference-folder');
    Halt(2);
  end;
  try
    if RunEBUSuite(ExpandFileName(ParamStr(1))) <> 0 then Halt(1);
  except on E: Exception do
    begin
      Writeln(StdErr, 'FAIL: ', E.ClassName, ': ', E.Message);
      Halt(1);
    end;
  end;
end.
