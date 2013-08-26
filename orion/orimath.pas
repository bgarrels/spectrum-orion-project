unit OriMath;

{$mode objfpc}{$H+}

interface

{%region Float Number Formatting}
type
  TOriFloatFormatParams = record
    Exponent: Byte;
    Digits: Byte;
    Zerofill: Boolean;
  end;
  POriFloatFormatParams = ^TOriFloatFormatParams;

function StringFromFloat(Value: Extended; ExpTheshold, Digits: Byte; ZeroFill: Boolean): String; overload;
function StringFromFloat(Value: Extended; const FormatParams: TOriFloatFormatParams): String; overload;
function RefineFloatText(const AValue: String): String;
{%endregion}

implementation

uses
  SysUtils, Math,
  OriStrings;

{%region Float Number Formatting}
{ The function formats a float number in a way like style General in Mathcad.
  Paramater ExpThreshold:
    Results of magnitude greater than 10^n or smaller than 10^-n ,
    where n is the exponential threshold, are displayed in exponential notation.
  Parameter Digits:
    Defines a maximal number of digits after decimal point. When result is presented
    in exponetial notation, Digits defines a number of decimal points before exponent.
  For example:
    12456,7556 = 1,246e4 (expth = 3 or 4, digits =3)
    12456,7556 = 12456,756 (expth = 5, digits =3)
    0,0012 = 0,001 (digits = 3, expth = 4)
    0,0012 = 1,20E-3 (digits = 2, expth = 3)
}
function StringFromFloat(Value: Extended; ExpTheshold, Digits: Byte; ZeroFill: Boolean): String;
var
  Min, Max: Extended;
  i, j: Integer;
begin
  if IsNaN(Value) then
  begin
    Result := 'NaN';
    Exit;
  end;

  if IsInfinite(Value) then
  begin
    if Sign(Value) = -1
      then Result := '-Inf'
      else Result := 'Inf';
    Exit;
  end;

  if Value = 0 then
  begin
    if ZeroFill
      then Result := FloatToStrF(Value, ffFixed, 18, Digits)
      else Result := FloatToStrF(Value, ffGeneral, 18, Digits);
    Exit;
  end;

  case ExpTheshold of
    1:   begin Max := 1e01; Min := 1e-00; end;  // 10 - 1
    2:   begin Max := 1e02; Min := 1e-01; end;  // 100 - 0.01
    3:   begin Max := 1e03; Min := 1e-02; end;
    4:   begin Max := 1e04; Min := 1e-03; end;
    5:   begin Max := 1e05; Min := 1e-04; end;
    6:   begin Max := 1e06; Min := 1e-05; end;
    7:   begin Max := 1e07; Min := 1e-06; end;
    8:   begin Max := 1e08; Min := 1e-07; end;
    9:   begin Max := 1e09; Min := 1e-08; end;
    10:  begin Max := 1e10; Min := 1e-09; end;
    11:  begin Max := 1e11; Min := 1e-10; end;
    12:  begin Max := 1e12; Min := 1e-11; end;
    13:  begin Max := 1e13; Min := 1e-12; end;
    14:  begin Max := 1e14; Min := 1e-13; end;
    15:  begin Max := 1e15; Min := 1e-14; end;
    16:  begin Max := 1e16; Min := 1e-15; end;
    17:  begin Max := 1e17; Min := 1e-16; end;
    else begin Max := 1e18; Min := 1e-17; end;
  end;

  if Value > 0 then
    if (Value < Min) or (Value > Max)
      then Result := FloatToStrF(Value, ffExponent, 1+Digits, 1)
      else Result := FloatToStrF(Value, ffFixed, 18, Digits)
  else
    if (Value > -Min) or (Value < -Max)
      then Result := FloatToStrF(Value, ffExponent, 1+Digits, 1)
      else Result := FloatToStrF(Value, ffFixed, 18, Digits);

  if not ZeroFill then
  begin
    i := CharPos(Result, 'E');
    if i = 0 then
    begin
      i := Length(Result);
      while i > 0 do
        if Result[i] = '0' then
          Dec(i)
        else if Result[I] in [',','.'] then
        begin
         SetLength(Result, i-1);
         Break;
        end
        else
        begin
          SetLength(Result, i);
          Break;
        end;
    end
    else
    begin
      j := i-1;
      while j > 0 do
        if Result[j] = '0' then
          Dec(j)
        else if Result[I] in [',','.'] then
        begin
          Result := Copy(Result, 1, j-1) + Copy(Result, i, 256);
          Break;
        end
        else
        begin
          Result := Copy(Result, 1, j) + Copy(Result, i, 256);
          Break;
        end;
    end;
  end;
end;

function StringFromFloat(Value: Extended; const FormatParams: TOriFloatFormatParams): String;
begin
  with FormatParams do Result := StringFromFloat(Value, Exponent, Digits, Zerofill);
end;

function RefineFloatText(const AValue: String): String;
var i, j: Integer;
begin
  j := 1;
  SetLength(Result, Length(AValue));
  for i := 1 to Length(AValue) do
    if AValue[i] in ['0'..'9','-', '+','e','E'] then
    begin
      Result[j] := AValue[i];
      Inc(j);
    end
    else if AValue[i] in [',','.'] then
    begin
      Result[j] := DefaultFormatSettings.DecimalSeparator;
      Inc(j);
    end;
  SetLength(Result, j-1);
  if (Result = '-') or (Result = ',') or (Result = '.')
    then Result := '0';
end;
{%endregion}

end.

