unit WinMain;

{$mode objfpc}{$H+}

interface

uses
  Controls, Forms, ActnList, ComCtrls, ExtCtrls, Menus, TAGraph, OriUndo,
  {$ifdef LINUX} BGRAImageList, {$endif}
  Plots, Diagram, SpectrumControls, PlotControls;

type
  {$ifdef LINUX}
  TImageList = class(TBGRAImageList) end;
  {$endif}

  { TMainWnd }

  TMainWnd = class(TForm, IPlotNotification)
  {%region published}
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
    ToolTabsImages: TImageList;
    Images24: TImageList;
    Images16: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PageDataTable: TPage;
    PanelTabs: TPanel;
    PlotsBook: TNotebook;
    Splitter1: TSplitter;
    ToolsBook: TNotebook;
    PageAdd: TPage;
    PageEdit: TPage;
    PageDiagram: TPage;
    MenuRecentProjects: TPopupMenu;
    MenuProjectSave: TPopupMenu;
    MenuRecentFiles: TPopupMenu;
    MenuPlotPrint: TPopupMenu;
    MenuEditSelect: TPopupMenu;
    MenuEditDelete: TPopupMenu;
    PanelContent: TPanel;
    ToolBarAdd: TToolBar;
    ToolBarAdd1: TToolBar;
    ToolBarEdit: TToolBar;
    ToolbarBook: TNotebook;
    PageProject: TPage;
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
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PlotsBookChangeBounds(Sender: TObject);
  {%endregion}
  private
    FToolbarTabs: TNotebookTabs;
    FPlotTabs: TNotebookTabs;
    FToolTabs: TNotebookTabs;
    FDiagrams: TDiagramList;
    FStatusBar: TSpectrumStatusBar;
    FGraphGrid: TGraphGrid;

    procedure InitComponents;

    procedure Notify(AOperation: TPlotOperation; APlot: TPlot; AGraph: TGraph);
    procedure ProcessProjectModified;
    procedure ProcessPlotAdded(APlot: TPlot);
    procedure ProcessPlotChanged(APlot: TPlot);
    procedure ProcessGraphAdded(APlot: TPlot; AGraph: TGraph);
    procedure ProcessGraphDeleted(APlot: TPlot; AGraph: TGraph);

    procedure UndoChanged(Sender: TObject; CmdUndo, CmdRedo: TOriUndoCommand);
    procedure UndoUndone(Sender: TObject; Cmd: TOriUndoCommand);

    procedure PlotsTabActivated(Sender: TObject);

    function GetCurPlot: TPlot;
    function GetCurChart: TChart;
    function GetCurDiagram: TDiagram;
    function GetDiagram(APlot: TPlot): TDiagram;
  public
    property CurPlot: TPlot read GetCurPlot;
    property CurChart: TChart read GetCurChart;
    property CurDiagram: TDiagram read GetCurDiagram;
  end;

var
  MainWnd: TMainWnd;

implementation

uses
  OriIniFile,
  SpectrumTypes, SpectrumSettings, SpectrumStrings,
  PlotMath, PlotReader, DlgFormulaEditor;

{$R *.lfm}

{%region TMainWnd}

{%region (De)Initialization}
procedure TMainWnd.FormCreate(Sender: TObject);
var
  Ini: TOriIniFile;
begin
  InitComponents;

  // initialize plot set
  FDiagrams := TDiagramList.Create;
  PlotSet := TPlots.Create;
  PlotSet.RegisterNotifyClient(Self);
  PlotSet.AddPlot;

  Ini := TOriIniFile.Create;
  try
    Preferences.Load(Ini);
    Preferences.LoadStates(Ini);
  finally
    Ini.Free;
  end;

  History.OnChanged := @UndoChanged;
  History.OnUndone := @UndoUndone;
  UndoChanged(History, nil, nil);
end;

procedure TMainWnd.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  Diagram: TDiagram;
  Ini: TOriIniFile;
begin
  Ini := TOriIniFile.Create;
  try
    Preferences.SaveStates(Ini);
    SaveAllStates(Ini);
  finally
    Ini.Free;
  end;

  for Diagram in FDiagrams do
    Diagram.Free;
  FDiagrams.Free;
end;

procedure TMainWnd.InitComponents;

  procedure InitToolTab(AIndex, AImageIndex: Integer; AHint: String);
  begin
    with FToolTabs.Tabs[AIndex] do
    begin
      Title := '';
      Hint := AHint;
      ShowHint := True;
      ImageIndex := AImageIndex;
      Padding := 6;
    end;
  end;

var
  Header: TToolPanelHeader;
begin
  FToolbarTabs := TNotebookTabs.Create(ToolbarBook);
  FToolbarTabs.TabsPosition := ntpTop;
  FToolbarTabs.Align := alTop;
  FToolbarTabs.Parent := Self;

  FToolTabs := TNotebookTabs.Create(ToolsBook);
  FToolTabs.TabsPosition := ntpBottom;
  FToolTabs.Align := alLeft;
  FToolTabs.Parent := PanelTabs;
  FToolTabs.Images := ToolTabsImages;
  InitToolTab(0, 0, SpectrumStrings.Tooltab_DataTable);

  Header := TToolPanelHeader.Create(PageDataTable);
  Header.Caption := SpectrumStrings.Tooltab_DataTable;
  Header.Images := ToolTabsImages;
  Header.ImageIndex := 0;

  FGraphGrid := TGraphGrid.Create(PageDataTable);

  FPlotTabs := TNotebookTabs.Create(PlotsBook);
  FPlotTabs.TabsPosition := ntpBottom;
  FPlotTabs.Align := alClient;
  FPlotTabs.Parent := PanelTabs;
  FPlotTabs.OnTabActivated := @PlotsTabActivated;

  FStatusBar := TSpectrumStatusBar.Create(Self);
  FStatusBar.Top := PanelTabs.Top + PanelTabs.Height;
end;
{%endregion}

{%region Form Events}
procedure TMainWnd.FormShow(Sender: TObject);
begin
end;

procedure TMainWnd.PlotsBookChangeBounds(Sender: TObject);
begin

end;

{%endregion}

{%region Controls Events}
procedure TMainWnd.PlotsTabActivated(Sender: TObject);
begin
  FStatusBar.ShowGraphCount(CurDiagram.CountTotal, CurDiagram.CountVisible);
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
  CurDiagram.Rename;
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
  TPlotReader.AddFileDialog(CurPlot);
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

function TMainWnd.GetDiagram(APlot: TPlot): TDiagram;
begin
  for Result in FDiagrams do
    if Result.Plot = APlot then exit;
  Result := nil;
end;
{%endregion}

{%region Plot Events}
procedure TMainWnd.Notify(AOperation: TPlotOperation; APlot: TPlot; AGraph: TGraph);
begin
  case AOperation of
    poModified: ProcessProjectModified;
    poPlotAdded: ProcessPlotAdded(APlot);
    poPlotChanged: ProcessPlotChanged(APlot);
    poGraphAdded: ProcessGraphAdded(APlot, AGraph);
    poGraphDeleted: ProcessGraphDeleted(APlot, AGraph);
  end;
end;

procedure TMainWnd.ProcessProjectModified;
begin
  FStatusBar.ShowModified(PlotSet.Modified);
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

procedure TMainWnd.ProcessPlotChanged(APlot: TPlot);
begin
  FPlotTabs.RenameTab(APlot.Title, GetDiagram(APlot));
end;

procedure TMainWnd.ProcessGraphAdded(APlot: TPlot; AGraph: TGraph);
var
  Diagram: TDiagram;
begin
  Diagram := CurDiagram;
  if APlot = Diagram.Plot then
    FStatusBar.ShowGraphCount(Diagram.CountTotal, Diagram.CountVisible);
  // TODO Add to MRU
  if Assigned(FGraphGrid) then
    FGraphGrid.Graph := AGraph;
end;

procedure TMainWnd.ProcessGraphDeleted(APlot: TPlot; AGraph: TGraph);
var
  Diagram: TDiagram;
begin
  Diagram := CurDiagram;
  if APlot = Diagram.Plot then
    FStatusBar.ShowGraphCount(Diagram.CountTotal, Diagram.CountVisible);
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

