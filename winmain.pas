unit WinMain;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Controls, Forms, Dialogs, ActnList, ComCtrls, ExtCtrls, TAGraph,
  Plots, Diagram, SpectrumControls;

type
  { TMainWnd }

  TMainWnd = class(TForm, IPlotNotification)
  {%region published}
    ActionPlotSaveAsImage: TAction;
    ActionPlotSaveAsProject: TAction;
    ActionPlotDelete: TAction;
    ActionPlotRename: TAction;
    ActionPlotNew: TAction;
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
    procedure ActionPlotDeleteExecute(Sender: TObject);
    procedure ActionPlotNewExecute(Sender: TObject);
    procedure ActionPlotRenameExecute(Sender: TObject);
    procedure ActionPlotSaveAsImageExecute(Sender: TObject);
    procedure ActionPlotSaveAsProjectExecute(Sender: TObject);
    procedure ActionEditCopyExecute(Sender: TObject);
    procedure ActionEditCopyFormatExecute(Sender: TObject);
    procedure ActionEditCutExecute(Sender: TObject);
    procedure ActionEditPasteExecute(Sender: TObject);
    procedure ActionEditPasteFormatExecute(Sender: TObject);
    procedure ActionEditPasteTableExecute(Sender: TObject);
    procedure ActionEditRedoExecute(Sender: TObject);
    procedure ActionEditUndoExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  {%endregion}
  private
    FToolbarTabs: TNotebookTabs;
    FPlotTabs: TNotebookTabs;
    FDiagrams: TDiagramList;

    procedure Notify(AOperation: TPlotOperation; APlot: TPlot; AGraph: TGraph);
    procedure ProcessPlotAdded(APlot: TPlot);

    function GetCurPlot: TPlot;
    function GetCurChart: TChart;
    function GetCurDiagram: TDiagram;
  public
    property CurPlot: TPlot read GetCurPlot;
    property CurChart: TChart read GetCurChart;
    property CurDiagram: TDiagram read GetCurDiagram;
  end;

var
  MainWnd: TMainWnd;

implementation

uses
  OriIniFile, OriUtils_Gui,
  SpectrumTypes, SpectrumSettings,
  PlotMath, DlgFormulaEditor;

{$R *.lfm}

{%region TMainWnd}
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

  // initialize plot set
  FDiagrams := TDiagramList.Create;
  PlotSet := TPlots.Create;
  PlotSet.RegisterNotifyClient(Self);
  PlotSet.AddPlot('');
end;

procedure TMainWnd.FormDestroy(Sender: TObject);
var
  Diagram: TDiagram;
  Ini: TOriIniFile;
begin
  Ini := TOriIniFile.Create;
  try
    SaveAllStates(Ini);
  finally
    Ini.Free;
  end;

  for Diagram in FDiagrams do
    Diagram.Free;
  FDiagrams.Free;
end;

{%region Form Events}
procedure TMainWnd.FormShow(Sender: TObject);
begin
end;
{%endregion}

{%region Plot Actions}
procedure TMainWnd.ActionPlotDeleteExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionPlotNewExecute(Sender: TObject);
begin
  PlotSet.AddPlot('');
end;

procedure TMainWnd.ActionPlotRenameExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionPlotSaveAsImageExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionPlotSaveAsProjectExecute(Sender: TObject);
begin
  //
end;
{%endregion Plot Actions}

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
{%endregion Edit Actions}

{%region Add Actions}
procedure TMainWnd.ActionAddRandomExecute(Sender: TObject);
var
  Params: TRandomSampleParams;
begin
  if AskRandomSampleParams(Params) then
    CurPlot.AddGraph(TGraph.Create(PlotMath.GetSampleGraph(Params), 'Sample'));
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
{%endregion Add Actions}

{%region Plot Accessors}
function TMainWnd.GetCurPlot: TPlot;
begin
  Result := CurDiagram.Plot;
end;

function TMainWnd.GetCurChart: TChart;
begin
  Result := CurDiagram.Chart;
end;

function TMainWnd.GetCurDiagram: TDiagram;
begin
  Result := TDiagram(FPlotTabs.ActiveTab.Feature);
end;
{%endregion}

{%region Plot Events}
procedure TMainWnd.Notify(AOperation: TPlotOperation; APlot: TPlot; AGraph: TGraph);
begin
  case AOperation of
    poPlotAdded: ProcessPlotAdded(APlot);
  end;
end;

procedure TMainWnd.ProcessPlotAdded(APlot: TPlot);
var
  Index: Integer;
  Diagram: TDiagram;
begin
  Index := PlotsBook.Pages.Add(APlot.Title);
  Diagram := TDiagram.Create(APlot, PlotsBook.Page[Index]);
  FPlotTabs.AddTab(APlot.Title, Index, Diagram);
  FDiagrams.Add(Diagram);
end;
{%endregion Plot Events}

{%endregion TMainWnd}
end.

