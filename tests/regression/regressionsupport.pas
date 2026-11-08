unit RegressionSupport;
{$mode objfpc}{$H+}

interface

uses SysUtils;

var Checks, Failures: integer;

procedure Check(const Name: string; Value, Expected, Tol: double);
procedure Run(const Name: string; Test: TProcedure);

implementation

uses Math;

procedure Check(const Name: string; Value, Expected, Tol: double);
var OK: boolean;
begin
  Inc(Checks);
  if IsInfinite(Expected) then
    OK := IsInfinite(Value) and (Value = Expected)
  else
    OK := not IsNan(Value) and not IsInfinite(Value) and
      (Abs(Value - Expected) <= Tol);
  if not OK then begin
    Inc(Failures);
    Writeln('FAIL ', Name, ': got ', Value, ', expected ', Expected);
  end;
end;

procedure Run(const Name: string; Test: TProcedure);
begin
  Writeln(Name);
  try Test; except on E: Exception do begin
    Inc(Failures); Writeln('FAIL exception: ', E.ClassName, ': ', E.Message);
  end; end;
end;

end.
