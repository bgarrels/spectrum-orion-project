unit Tests_OriUtils_Strings;

{$mode objfpc}{$H+}

interface

uses
  FpcUnit, TestRegistry;

type
  TTest_OriUtils_Strings = class(TTestCase)
  published
    procedure SplitStr;
  end;

implementation

uses
  OriUtils_Strings;

procedure TTest_OriUtils_Strings.SplitStr;
var
  S: String;
  R: TStringArray;
begin
  S := 'C1 λ_min,рус; ЉϢȠЂӔӜ   ڝڶڥڰڇکگ';
  R := OriUtils_Strings.SplitStr(S, ';, ');
  AssertEquals(5, Length(R));
  AssertEquals('C1', R[0]);
  AssertEquals('λ_min', R[1]);
  AssertEquals('рус', R[2]);
  AssertEquals('ЉϢȠЂӔӜ', R[3]);
  AssertEquals('ڝڶڥڰڇکگ', R[4]);
end;

initialization
  RegisterTest(TTest_OriUtils_Strings);
end.

