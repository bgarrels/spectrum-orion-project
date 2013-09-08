program spectrum;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, WinMain, DlgParamsRandom, DlgFormulaEditor,
  SpectrumTypes, PlotMath, PlotReaders, PlotReadersAux, SpectrumStrings, Plots,
  SpectrumControls, FrmRangePoints, FrmRange, Diagram, SpectrumSettings,
  SpectrumUndo, PlotReader;

{$R *.res}

begin
  Application.Title:='Spectrum';
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainWnd, MainWnd);
  Application.Run;
end.

