unit WinMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, TAGraph, TAChartImageList, TASources, TASeries,
  Forms, Controls, Graphics, Dialogs, ActnList, ComCtrls, Menus, ExtCtrls,
  Buttons,
  SpectrumControls;

type
  { TMainWnd }

  TMainWnd = class(TForm)
    ActionDiagramSaveAsImage: TAction;
    ActionDiagramSaveAsProject: TAction;
    ActionDiagramDelete: TAction;
    ActionDiagramRename: TAction;
    ActionDiagramNew: TAction;
    ActionEditPasteFormat: TAction;
    ActionEditCopyFormat: TAction;
    ActionEditPasteTable: TAction;
    ActionEditPaste: TAction;
    ActionEditCut: TAction;
    ActionEditCopy: TAction;
    ActionEditRedo: TAction;
    ActionEditUndo: TAction;
    ActionAddFormula: TAction;
    ActionProjectSaveAs: TAction;
    ActionProjectSave: TAction;
    ActionProjectNew: TAction;
    ActionProjectOpen: TAction;
    ActionAddRandom: TAction;
    ActionList: TActionList;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart2: TChart;
    Chart2LineSeries1: TLineSeries;
    Images24: TImageList;
    Images16: TImageList;
    PageAdd: TPage;
    PageEdit: TPage;
    PageDiagram: TPage;
    ToolBarAdd: TToolBar;
    ToolBarAdd1: TToolBar;
    ToolBarEdit: TToolBar;
    ToolbarBook: TNotebook;
    PageProject: TPage;
    PlotsBook: TNotebook;
    Page1: TPage;
    Page2: TPage;
    RandomChartSource1: TRandomChartSource;
    RandomChartSource2: TRandomChartSource;
    StatusBar: TStatusBar;
    ToolBar1: TToolBar;
    ToolBarProject: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure ActionAddFormulaExecute(Sender: TObject);
    procedure ActionAddRandomExecute(Sender: TObject);
    procedure ActionDiagramDeleteExecute(Sender: TObject);
    procedure ActionDiagramNewExecute(Sender: TObject);
    procedure ActionDiagramRenameExecute(Sender: TObject);
    procedure ActionDiagramSaveAsImageExecute(Sender: TObject);
    procedure ActionDiagramSaveAsProjectExecute(Sender: TObject);
    procedure ActionEditCopyExecute(Sender: TObject);
    procedure ActionEditCopyFormatExecute(Sender: TObject);
    procedure ActionEditCutExecute(Sender: TObject);
    procedure ActionEditPasteExecute(Sender: TObject);
    procedure ActionEditPasteFormatExecute(Sender: TObject);
    procedure ActionEditPasteTableExecute(Sender: TObject);
    procedure ActionEditRedoExecute(Sender: TObject);
    procedure ActionEditUndoExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    FToolbarTabs: TNotebookTabs;
    FPlotTabs: TNotebookTabs;
  public
    { public declarations }
  end;

var
  MainWnd: TMainWnd;

implementation

uses
  SpectrumStrings,
  DlgParamsRandom, DlgFormulaEditor;

{$R *.lfm}

{ TMainWnd }

procedure TMainWnd.FormCreate(Sender: TObject);
begin
  FToolbarTabs := TNotebookTabs.Create(ToolbarBook);
  FToolbarTabs.TabsPosition := ntpTop;
  FToolbarTabs.Align := alTop;
  FToolbarTabs.Parent := Self;

  FPlotTabs := TNotebookTabs.Create(PlotsBook);
  FPlotTabs.TabsPosition := ntpBottom;
  FPlotTabs.Align := alBottom;
  FPlotTabs.Parent := Self;
end;

procedure TMainWnd.FormShow(Sender: TObject);
begin
end;

{%region Diagram Actions}
procedure TMainWnd.ActionDiagramDeleteExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionDiagramNewExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionDiagramRenameExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionDiagramSaveAsImageExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionDiagramSaveAsProjectExecute(Sender: TObject);
begin
  //
end;
{%endregion}

{%region Edit Actions}
procedure TMainWnd.ActionEditCopyExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionEditCopyFormatExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionEditCutExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionEditPasteExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionEditPasteFormatExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionEditPasteTableExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionEditRedoExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionEditUndoExecute(Sender: TObject);
begin
  //
end;
{%endregion}

{%region Add Actions}
procedure TMainWnd.ActionAddRandomExecute(Sender: TObject);
begin
  with TRandomParamsDlg.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TMainWnd.ActionAddFormulaExecute(Sender: TObject);
begin
  with TFormulaEditorDlg.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;
{%endregion}

end.

