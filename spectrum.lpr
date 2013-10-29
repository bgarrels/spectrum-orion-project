program spectrum;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  LResources, Interfaces, // this includes the LCL widgetset
  Forms, tachartlazaruspkg, WinMain, DlgParamsRandom, DlgFormulaEditor,
  SpectrumTypes, PlotMath, PlotReaders, PlotReadersAux, SpectrumStrings, Plots,
  SpectrumControls, FrmRangePoints, FrmRange, Diagram, SpectrumSettings,
  SpectrumUndo, PlotReader, PlotControls;

{$R *.res}

begin
  RequireDerivedFormResource := True;

  {$I images.lrs}

  Application.Initialize;
  Application.CreateForm(TMainWnd, MainWnd);
  Application.Run;
end.

