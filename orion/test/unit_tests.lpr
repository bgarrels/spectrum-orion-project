program Unit_Tests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, tachartlazaruspkg, OriTabs,
  WinPropEditor, Tests_OriControls, oridebugconsole, oriutils_gui, OriGraphics,
  OriMath, OriStrings, oriutils_tchart, OriEditors, Tests_OriMath,
  tests_oristrings, tests_oriutils_gui, WinTestColors;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
