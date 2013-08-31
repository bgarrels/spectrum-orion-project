unit WinMain;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Controls, Forms, Dialogs, ActnList, ComCtrls, ExtCtrls, Menus,
  TAGraph, OriUndo, Plots, Diagram, SpectrumControls, Classes;

type
  { TMainWnd }

  TMainWnd = class(TForm, IPlotNotification)
    ActionEditDeleteAll: TAction;
    ActionEditDelete: TAction;
    ActionEditInvertSelection: TAction;
    ActionEditSelectAll: TAction;
    ActionEditSelectGraphs: TAction;
    ActionDiagramPrinterSetup: TAction;
    ActionDiagramPrintWithPreview: TAction;
    ActionDiagramPrint: TAction;
    ActionAddTable: TAction;
    ActionAddFolder: TAction;
    ActionAddFile: TAction;
  {%region published}
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
    Images24: TImageList;
    Images16: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    PageAdd: TPage;
    PageEdit: TPage;
    PageDiagram: TPage;
    MenuRecentProjects: TPopupMenu;
    MenuProjectSave: TPopupMenu;
    MenuRecentFiles: TPopupMenu;
    MenuPlotPrint: TPopupMenu;
    MenuEditSelect: TPopupMenu;
    MenuEditDelete: TPopupMenu;
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
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton26: TToolButton;
    ToolButton27: TToolButton;
    ToolButton28: TToolButton;
    ToolButton29: TToolButton;
    ToolButton3: TToolButton;
    ToolButton30: TToolButton;
    ToolButton31: TToolButton;
    ToolButton32: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    procedure ActionAddFileExecute(Sender: TObject);
    procedure ActionAddFolderExecute(Sender: TObject);
    procedure ActionAddFormulaExecute(Sender: TObject);
    procedure ActionAddRandomExecute(Sender: TObject);
    procedure ActionAddTableExecute(Sender: TObject);
    procedure ActionDiagramDeleteExecute(Sender: TObject);
    procedure ActionDiagramNewExecute(Sender: TObject);
    procedure ActionDiagramPrinterSetupExecute(Sender: TObject);
    procedure ActionDiagramPrintExecute(Sender: TObject);
    procedure ActionDiagramPrintWithPreviewExecute(Sender: TObject);
    procedure ActionDiagramRenameExecute(Sender: TObject);
    procedure ActionDiagramSaveAsImageExecute(Sender: TObject);
    procedure ActionDiagramSaveAsProjectExecute(Sender: TObject);
    procedure ActionEditCopyExecute(Sender: TObject);
    procedure ActionEditCopyFormatExecute(Sender: TObject);
    procedure ActionEditCutExecute(Sender: TObject);
    procedure ActionEditDeleteAllExecute(Sender: TObject);
    procedure ActionEditDeleteExecute(Sender: TObject);
    procedure ActionEditInvertSelectionExecute(Sender: TObject);
    procedure ActionEditPasteExecute(Sender: TObject);
    procedure ActionEditPasteFormatExecute(Sender: TObject);
    procedure ActionEditPasteTableExecute(Sender: TObject);
    procedure ActionEditRedoExecute(Sender: TObject);
    procedure ActionEditSelectAllExecute(Sender: TObject);
    procedure ActionEditSelectGraphsExecute(Sender: TObject);
    procedure ActionEditUndoExecute(Sender: TObject);
    procedure ActionProjectNewExecute(Sender: TObject);
    procedure ActionProjectOpenExecute(Sender: TObject);
    procedure ActionProjectSaveAsExecute(Sender: TObject);
    procedure ActionProjectSaveExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PlotsBookChangeBounds(Sender: TObject);
  {%endregion}
  private
    FToolbarTabs: TNotebookTabs;
    FPlotTabs: TNotebookTabs;
    FDiagrams: TDiagramList;

    procedure Notify(AOperation: TPlotOperation; APlot: TPlot; AGraph: TGraph);
    procedure ProcessPlotAdded(APlot: TPlot);

    procedure UndoChanged(Sender: TObject; CmdUndo, CmdRedo: TOriUndoCommand);
    procedure UndoUndone(Sender: TObject; Cmd: TOriUndoCommand);

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
  SpectrumTypes, SpectrumSettings, SpectrumStrings,
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

  History.OnChanged := @UndoChanged;
  History.OnUndone := @UndoUndone;
  UndoChanged(History, nil, nil);
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

procedure TMainWnd.PlotsBookChangeBounds(Sender: TObject);
begin

end;

{%endregion}

{%region Plot Actions}
procedure TMainWnd.ActionDiagramDeleteExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionDiagramNewExecute(Sender: TObject);
begin
  PlotSet.AddPlot('');
end;

procedure TMainWnd.ActionDiagramPrinterSetupExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionDiagramPrintExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionDiagramPrintWithPreviewExecute(Sender: TObject);
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

procedure TMainWnd.ActionEditDeleteAllExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionEditDeleteExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionEditInvertSelectionExecute(Sender: TObject);
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
  History.Redo;
end;

procedure TMainWnd.ActionEditSelectAllExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionEditSelectGraphsExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionEditUndoExecute(Sender: TObject);
begin
  History.Undo;
end;
{%endregion Edit Actions}

{%region Project Actions}
procedure TMainWnd.ActionProjectOpenExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionProjectNewExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionProjectSaveAsExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionProjectSaveExecute(Sender: TObject);
begin
  //
end;
{%endregion Project Actions}

{%region Add Actions}
procedure TMainWnd.ActionAddRandomExecute(Sender: TObject);
var
  Params: TRandomSampleParams;
begin
  if AskRandomSampleParams(Params) then
    CurPlot.AddGraph(TGraph.Create(PlotMath.GetSampleGraph(Params), 'Sample'));
end;

procedure TMainWnd.ActionAddTableExecute(Sender: TObject);
begin
  //
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

procedure TMainWnd.ActionAddFileExecute(Sender: TObject);
begin
  //
end;

procedure TMainWnd.ActionAddFolderExecute(Sender: TObject);
begin
  //
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

{%region Undo Commands}
procedure TMainWnd.UndoChanged(Sender: TObject; CmdUndo, CmdRedo: TOriUndoCommand);
begin
  ActionEditUndo.Enabled := CmdUndo <> nil;
  if CmdUndo <> nil
    then ActionEditUndo.Caption := SpectrumStrings.Action_Undo + ': ' + CmdUndo.Title
    else ActionEditUndo.Caption := SpectrumStrings.Action_Undo;
  ActionEditUndo.Hint := ActionEditUndo.Caption;

  ActionEditRedo.Enabled := CmdRedo <> nil;
  if CmdRedo <> nil
    then ActionEditRedo.Caption := SpectrumStrings.Action_Redo + ': ' + CmdRedo.Title
    else ActionEditRedo.Caption := SpectrumStrings.Action_Redo;
  ActionEditRedo.Hint := ActionEditRedo.Caption;
end;

procedure TMainWnd.UndoUndone(Sender: TObject; Cmd: TOriUndoCommand);
begin

end;
{%endregion}

{%endregion TMainWnd}
end.

