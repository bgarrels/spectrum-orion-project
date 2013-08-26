unit Tests_OriDialogs;

{$mode objfpc}{$H+}

interface

uses
  FpcUnit, TestRegistry;

type
  TTest_OriDialogs = class(TTestCase)
  published
    procedure ErrorDlg;
  end;

implementation

uses
  OriDialogs;

procedure TTest_OriDialogs.ErrorDlg;
begin
  OriDialogs.ErrorDlg('Test error message');
end;

initialization
  RegisterTest(TTest_OriDialogs);
end.

