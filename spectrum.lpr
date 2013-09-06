program spectrum;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, WinMain, DlgParamsRandom, DlgFormulaEditor,
  spectrumtypes, plotmath, plotreaders, plotreadersaux, SpectrumStrings, Plots,
  SpectrumControls, FrmRangePoints, FrmRange, diagram, spectrumsettings,
  spectrumundo;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainWnd, MainWnd);
  Application.Run;
end.

