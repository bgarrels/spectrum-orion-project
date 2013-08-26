unit Tests_OriUtils_Gui;

{$mode objfpc}{$H+}

interface

uses
  FpcUnit, TestRegistry;

type
  TTest_OriUtils_Gui = class(TTestCase)
  published
    procedure ErrorDlg;
  end;

implementation

uses
  OriUtils_Gui;

procedure TTest_OriUtils_Gui.ErrorDlg;
begin
  OriUtils_Gui.ErrorDlg('Test error message');
end;

initialization
  RegisterTest(TTest_OriUtils_Gui);
end.

