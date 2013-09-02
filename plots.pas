unit Plots;

interface

uses
  Classes, Contnrs, FGL,
  SpectrumTypes;

type
  TGraph = class;
  TPlot = class;
  TPlots = class;

  TPlotOperation = (poProjectSaved, poProjectLoaded, poProjectReseting, poProjectReset,
    poPlotAdded, poPlotChanged, poPlotDestroying, poPlotDestroyed, poModified,
    poGraphAdded, poGraphChanged, poGraphDeleted, poGraphChangedValues,
    poGraphDestroying);
  TPlotOperations = set of TPlotOperation;

  IPlotNotification = interface
  ['{57D01E74-C0FF-4AFC-B211-FF5610FDA085}']
    procedure Notify(AOperation: TPlotOperation; APlot: TPlot; AGraph: TGraph);
  end;

  TGraphKind = (gkNormal, gkExpression);

  // Diagram line
  TGraph = class
  private
    FOwner: TPlot;
    FTitle: String;
    FValuesX: TValueArray;
    FValuesY: TValueArray;

    // Title is generated automatically. It is True for a new graph.
    // Some operations can generate a graph's title (e.g. formula editing)
    // if this flag is True. But when user renames graph the flag is reset.
    FAutoTitle: Boolean;

    // Data source of graph. It can be file path, expression or function name
    // (e.g. Derivative(smth)). This value is just an information. It is
    // better to store a true data source in FParams for following usage.
    FSource: String;

    // Kind of graph particularly determines a way to store graph in project
    // file. E.g. it controls what type of additional parameters (FParams)
    // must be created while graph is read from project file.
    FKind: TGraphKind;

    // Any additional graph params and properties. E.g. for expression graph
    // stores a plotting function and parameters (TFormulaParams).
    FParams: TObject;

    //FSeries: TLineSeries;
    //FSeriesIndex: Integer;

    function GetIndex: Integer;
    function GetValueX(Index: Integer): TValue;
    function GetValueY(Index: Integer): TValue;
    procedure SetValueX(Index: Integer; const Value: TValue);
    procedure SetValueY(Index: Integer; const Value: TValue);
    procedure SetTitle(const Value: String);
    procedure SetValueCount(Value: Integer);
    function GetValueCount: Integer;

  public
    constructor Create(AOwner: TPlot); overload;
    constructor Create(AValues: TGraphRec; const ATitle: String); overload;
    destructor Destroy; override;

    function Rename: Boolean; // requests a new name for graph
    function EditFormula: Boolean;

    //procedure SaveXML(Node: IXMLNode);
    //procedure LoadXML(Node: IXMLNode);
    //procedure Load(Params: TGraphAppendParams);
    procedure UpdateValues;
    procedure FillValues;
    procedure FillSampleValues(ACount: Integer); overload;
    procedure FillSampleValues(var Param: TRandomSampleParams); overload;

    procedure CreateLine;
    procedure DeleteLine;
    procedure UpdateLine;

    procedure GetMinMaxX(out MinValue, MaxValue: TValue);
    procedure GetMinMaxY(out MinValue, MaxValue: TValue);

    procedure Trim(var Param: TTrimParams);
    procedure Offset(var Param: TOffsetParams);
    procedure Normalize(var Param: TNormalizeParams);
    procedure SwapXY;
    procedure FlipX;
    procedure FlipY;
    procedure Sort;

    property Index: Integer read GetIndex;
    property Title: String read FTitle write SetTitle;
    property AutoTitle: Boolean read FAutoTitle write FAutoTitle;
    property ValueCount: Integer read GetValueCount write SetValueCount;
    property ValuesX: TValueArray read FValuesX;
    property ValuesY: TValueArray read FValuesY;
    property ValueX[Indx: Integer]: TValue read GetValueX write SetValueX;
    property ValueY[Indx: Integer]: TValue read GetValueY write SetValueY;
    property Source: String read FSource;
    property Owner: TPlot read FOwner;
    property Kind: TGraphKind read FKind;
    property Params: TObject read FParams;

    procedure SetValuesXY(const Xs, Ys: TValueArray);

    procedure Notify(AOperation: TPlotOperation);

    //property Series: TLineSeries read FSeries write FSeries;
  end;

  TGraphList = specialize TFPGList<TGraph>;
  TGraphArray = array of TGraph;
  PGraphArray = ^TGraphArray;

  // Graphs collection
  TPlot = class
  private
    FTitle: String;
    FOwner: TPlots;
    FItems: TGraphList;
    FFactorX, FFactorY: Integer;

    //FChart: TChart;
    //FSelector: TMultiSelectorTool;
    //FHintValuesTool: TMarksTipTool;

    //function CreateGraph(Node: IXMLNode): TGraph; overload;
    //function CreateGraph(Params: TGraphAppendParams): TGraph; overload;
    //function CreateGraph(ATitle: String = ''): TGraph; overload;

    //function CreateNote(Node: IXMLNode): TAnnotationTool;

    procedure SetTitle(Value: String);
    function GetCount: Integer;
    function GetItems(Index: Integer): TGraph;

    procedure CreateUndoGroup(var Graphs: TGraphArray; const Title: String);

  protected
    procedure DoGraphAdded(AGraph: TGraph);
    procedure DoGraphChanged(AGraph: TGraph);
    procedure DoGraphDeleted(AGraph: TGraph);
    procedure DoGraphChangedValues(AGraph: TGraph);

  public
    BackupIndex: Integer;  // номер выделенного графика, для переключения м-у диаграммами

    constructor Create(AOwner: TPlots; const ATitle: String = '');
    destructor Destroy; override;
    procedure Clear;

    //function AddGraph: TGraph; overload;                                         // add empty graph
    procedure AddGraph(AGraph: TGraph; AIndex: Integer = -1; AUndoable: Boolean = True); overload;
    //procedure AddGraph(AGraph: TGraphRec; const ATitle: String); overload;

    // Основная процедура создания графиков по имеющимся данным
    // FileNames - список файлов, если графики создаются из файлов. Может не задаваться.
    // Params - предположительные параметры загрузки каждого файла. Параметры затем
    //    уточняются на основании типа файла или чего-то еще. Если список файлов не
    //    задан, предполагается что параметры содержат достаточно данных для создания
    //    графика (задан поток, или массивы готовых данных).
    // Использование объекта-параметра TGraphAppendParams см. в TGraph.Load.
    //function AddGraph(Params: TGraphAppendParams; FileNames: TStrings = nil): TGraph; overload;
    //function AddGraph(Params: TGraphAppendParams; const FileName: String): TGraph; overload;
    //
    //function AddSampleGraph(ATitle: String): TGraph; overload;
    //function AddSampleGraph(ATitle: String; AParams: TRandomSampleParams): TGraph; overload;
    //procedure ExtractGraph(AGraph: TGraph);
    procedure DeleteGraph(AGraph: TGraph; AUndoable: Boolean = True); overload;
    //procedure DeleteGraph(ASeries: TChartSeries); overload;
    //
    //procedure SaveXML(Node: IXMLNode);
    //procedure LoadXML(Node: IXMLNode);

    procedure Trim(var Graphs: TGraphArray; var Param: TTrimParams);
    procedure Offset(var Graphs: TGraphArray; var Param: TOffsetParams);
    procedure Flip(var Graphs: TGraphArray; var Param: TMiscParams);
    procedure Normalize(var Graphs: TGraphArray; var Param: TNormalizeParams);
    procedure Scale(var Graphs: TGraphArray; var Param: TScaleParams);
    procedure Inverse(var Graphs: TGraphArray; var Param: TMiscParams);
    procedure Variance(var Graphs: TGraphArray; var Param: TVarianceParams);
    procedure SwapXY(var Graphs: TGraphArray);
    procedure FlipX(var Graphs: TGraphArray);
    procedure FlipY(var Graphs: TGraphArray);
    procedure Differentiate(var Graphs: TGraphArray);
    procedure Differentiate2(var Graphs: TGraphArray);
    procedure Regularity(var Graphs: TGraphArray);
    procedure Midline(var Graphs: TGraphArray);
    procedure Envelope(var Graphs: TGraphArray; IsTop: Boolean);
    procedure Despike(var Graphs: TGraphArray; var Params: TDespikeParams);
    procedure GroupOperation(var Graphs: TGraphArray; Operation: TGroupOperation);

    function IsEmpty: Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TGraph read GetItems; default;
    property Title: String read FTitle write SetTitle;
    property Owner: TPlots read FOwner;
    property FactorX: Integer read FFactorX write FFactorX;
    property FactorY: Integer read FFactorY write FFactorY;

    procedure ApplyChartSettings;

    procedure Notify(AOperation: TPlotOperation; AGraph: TGraph = nil);

    //property Chart: TChart read FChart write FChart;
    //property Selector: TMultiSelectorTool read FSelector write FSelector;
    //property HintValuesTool: TMarksTipTool read FHintValuesTool write FHintValuesTool;
  end;

  TPlotSetStatuses = (pssLoading);
  TPlotSetStatus = set of TPlotSetStatuses;
  TPlotList = specialize TFPGList<TPlot>;

  TPlots = class
  private
    FItems: TPlotList;
    FUntitled: Boolean;
    FPacked: Boolean;
    FFileName: String;
    FStatus: TPlotSetStatus;
    FModified: Boolean;

    FNotifyClients: TObjectList;

    function CreatePlot(const ATitle: String = ''): TPlot;
    function GetCount: Integer;
    function GetItems(Index: Integer): TPlot;
    procedure SetModified(Value: Boolean);
    //procedure SaveXML(APlot: TPlot); overload;
    //procedure SaveXML(Node: IXMLNode; APlot: TPlot); overload;
    //procedure LoadXML(const AFileName: String; APacked: Boolean); overload;
    //procedure LoadXML(Node: IXMLNode); overload;
  protected
    //procedure DoPlotAdded(APlot: TPlot);
    //procedure DoPlotDeleting(APlot: TPlot);
    //procedure DoPlotDeleted(APlot: TPlot);
    //procedure DoPlotChanged(APlot: TPlot);
    //procedure DoProjectSaved;
    //procedure DoProjectLoaded;
    //procedure DoProjectReseting;
    //procedure DoProjectReset;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;

    //function AddPlot(Node: IXMLNode): TPlot; overload;
    function AddPlot(const ATitle: String = ''): TPlot; overload;
    procedure DeletePlot(Index: Integer); overload;
    procedure DeletePlot(APlot: TPlot); overload;

    property Items[Index: Integer]: TPlot read GetItems; default;
    function IndexOf(APlot: TPlot): Integer;
    property Count: Integer read GetCount;

    procedure RegisterNotifyClient(AObject: TObject);
    procedure UnRegisterNotifyClient(AObject: TObject);
    function GetNotifyClient(AClass: TClass): TObject;
    procedure Notify(AOperation: TPlotOperation; APlot: TPlot = nil; AGraph: TGraph = nil);

    function CanReset: Boolean;
    function Reset(Silent: Boolean = False; Mode: Integer = 2): Boolean;
    function Save(APlot: TPlot = nil): Boolean;
    function SaveAs(APlot: TPlot = nil): Boolean;
    function Load: Boolean; overload;
    procedure Load(const AFileName: String; APacked: Boolean); overload;
    property FileName: String read FFileName;

    property Status: TPlotSetStatus read FStatus;
    property Modified: Boolean read FModified write SetModified;
  end;

function IsProjectPacked(const FileName: String): Boolean;

procedure IncreaseArray(var Arr: TGraphArray; Value: TGraph);
procedure IncreaseArrayUnique(var Arr: TGraphArray; Value: TGraph);
procedure DecreaseArray(var Arr: TGraphArray; Index: Integer);

var
  PlotSet: TPlots;

const
  CDefProjectExt = 'sprj';
  PROJECT_VER = 1;

implementation

uses
  SysUtils,
  OriUndo,
//  Controls, Dialogs, Variants, Graphics, Zlib,
//  OriUndo, OriUtils, OriDialogs, TeeStoreXML,
//  Common, UndoRedo, PlotMath, WinOpenArchive;
  PlotMath, SpectrumStrings, SpectrumUndo;

var
  UntitledIndex: Integer = 1;

const
  // Set of operations making a plot modified. FModified flag is set.
  ModifyingOperations: TPlotOperations = [poPlotChanged, poPlotDestroyed, poPlotAdded,
    poGraphChangedValues];

  // Set of operations making a plot unmodified. FModified flag is reset.
  SavingOperations: TPlotOperations = [poProjectSaved, poProjectLoaded, poProjectReset];

{%region Helpers}
function IsProjectPacked(const FileName: String): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), '.' + CDefProjectExt + 'z');
end;

procedure IncreaseArray(var Arr: TGraphArray; Value: TGraph);
begin
  SetLength(Arr, Length(Arr)+1);
  Arr[Length(Arr)-1] := Value;
end;

procedure IncreaseArrayUnique(var Arr: TGraphArray; Value: TGraph);
var
  I: Integer;
begin
  for I := 0 to Length(Arr)-1 do
    if Arr[I] = Value then Exit;
  SetLength(Arr, Length(Arr)+1);
  Arr[Length(Arr)-1] := Value;
end;

procedure DecreaseArray(var Arr: TGraphArray; Index: Integer);
var
  I: Integer;
begin
  if (Index > -1) and (Index < Length(Arr)) then
  begin
    for I := Index+1 to Length(Arr)-1 do Arr[I-1] := Arr[I];
    SetLength(Arr, Length(Arr)-1);
  end;
end;
{%endregion}

{%region TGraph}
constructor TGraph.Create(AOwner: TPlot);
begin
  FOwner := AOwner;
  FAutoTitle := True;
  CreateLine;
end;

constructor TGraph.Create(AValues: TGraphRec; const ATitle: String);
begin
  FTitle := ATitle;
  FAutoTitle := True;
  FValuesX := AValues.X;
  FValuesY := AValues.Y;
end;

destructor TGraph.Destroy;
begin
  //DeleteLine;
  //FreeAndNil(FSeries);
  FreeAndNil(FParams);
  Notify(poGraphDestroying);
  inherited;         
end;

procedure TGraph.Notify(AOperation: TPlotOperation);
begin
  if Assigned(Owner) then Owner.Notify(AOperation, Self);
end;

procedure TGraph.CreateLine;
begin
  //if FSeries = nil then
  //begin
  //  FSeriesIndex := -1;
  //  FSeries := TLineSeries.Create(Owner.Chart);
  //  FSeries.Tag := Integer(Self);
  //  FSeries.Title := FTitle;
  //  FSeries.XValues.Order := loNone;
  //  FSeries.YValues.Order := loNone;
  //end;
  //Owner.Chart.AddSeries(FSeries);
  //if FSeriesIndex > -1 then // Move т.к. почему-то SeriesList.Insert дает AV
  //  Owner.Chart.SeriesList.Move(Owner.Chart.SeriesList.Count-1, FSeriesIndex);
end;

procedure TGraph.DeleteLine;
begin
  // FSeriesIndex нужен чтобы при undo удаления графика
  // вернуть ряд в то положение, где он стоял до удаления
  //FSeriesIndex := Owner.Chart.SeriesList.IndexOf(FSeries);
  //Owner.Selector.SeriesList.Extract(FSeries);
  //Owner.Chart.RemoveSeries(FSeries);
end;

procedure TGraph.UpdateLine;
var i: Integer;
begin
  //FSeries.BeginUpdate;
  //try
  //  FSeries.Clear;
  //  for i := 0 to ValueCount-1 do
  //    FSeries.AddXY(ValueX[i], ValueY[i]);
  //finally
  //  FSeries.EndUpdate;
  //end;
end;

procedure TGraph.SetValuesXY(const Xs, Ys: TValueArray);
begin
  if Length(Xs) <> Length(Ys) then raise
    ESpectrumError.Create(Err_XYLengthsDiffer);
  FValuesX := Copy(ValuesX);
  FValuesY := Copy(ValuesY);
  UpdateLine;
  if Assigned(FOwner) then FOwner.DoGraphChangedValues(Self);
end;

function TGraph.GetValueX(Index: Integer): TValue;
begin
  Result := FValuesX[Index];
end;

function TGraph.GetValueY(Index: Integer): TValue;
begin
  Result := FValuesY[Index];
end;

function TGraph.GetValueCount: Integer;
begin
  Result := Length(FValuesX);
end;

procedure TGraph.SetValueCount(Value: Integer);
begin
  SetLength(FValuesX, Value);
  SetLength(FValuesY, Value);
end;

function TGraph.GetIndex: Integer;
begin
  Result := FOwner.FItems.IndexOf(Self);
end;

// процедура для внутреннего использования, поэтому Undo не надо
procedure TGraph.SetValueX(Index: Integer; const Value: TValue);
begin
  if (Index > -1) and (Index < Length(FValuesX)) then
  begin
    FValuesX[Index] := Value;
    UpdateLine;
    if Assigned(FOwner) then FOwner.DoGraphChangedValues(Self);
  end;
end;

// процедура для внутреннего использования, поэтому Undo не надо
procedure TGraph.SetValueY(Index: Integer; const Value: TValue);
begin
  if (Index > -1) and (Index < Length(FValuesY)) then
  begin
    FValuesY[Index] := Value;
    UpdateLine;
    if Assigned(FOwner) then FOwner.DoGraphChangedValues(Self);
  end;
end;

{%region Title}
function TGraph.Rename: Boolean;
var
  TmpTitle: String;
begin
  TmpTitle := FTitle;
  //Result := InputStringDlgDK(TmpTitle, 'EnterText_GraphTitle');
  //if Result then
  //begin
  //  History.Append(TTitleEditCommand.Create(Self));
  //  SetTitle(TmpTitle); // here call Owner.OnGraphChanged
  //  FAutoTitle := False;
  //end;
end;

procedure TGraph.SetTitle(const Value: String);
begin
  if Value <> FTitle then
  begin
    FTitle := Value;
    //FSeries.Title := Value;
    if Assigned(FOwner) then FOwner.DoGraphChanged(Self);
  end;
end;
{%endregion}

function TGraph.EditFormula: Boolean;
//var
//  UndoCmd: TOriUndoCommand;
begin
  //Result := False;
  //case FKind of
  //  gkExpression:
  //  begin
  //    UndoCmd := TEditFormulaCommand.Create(Self);
  //    if EditFormulaParams(TFormulaParams(FParams)) then
  //    begin
  //      UpdateValues;
  //      History.Append(UndoCmd);
  //      Result := True;
  //    end
  //    else UndoCmd.Free;
  //  end;
  //end;
end;

{procedure TGraph.Load(Params: TGraphAppendParams);

  procedure LoadFormula(Params: TFormulaParams);
  begin
    FKind := gkExpression;
    FParams := Params;
    FSource := FormulaExpression(Params.Formula);
    FAutoTitle := True;
    FTitle := FSource;
    FillValues;
  end;

begin
  FSource := Params.Source;
  if Params.Title = ''
    then FTitle := ExtractFileName(Params.Source)
    else FTitle := Params.Title;
  if Assigned(Params.Reader) then
  begin
    Params.ValuesX := @FValuesX;
    Params.ValuesY := @FValuesY;
    with Params.Reader.Create(Params) do
    try
      ReadValues;
    finally
      Free;
    end;
  end
  else if Assigned(Params.AuxParam) then
  begin
    // График выражения y = f(x)
    if Params.AuxParam is TFormulaParams then
      LoadFormula(Params.AuxParam as TFormulaParams);
  end
  else // данные уже загружены
    if (Params.ValuesX <> nil) and (Params.ValuesY <> nil) then
    begin
      FValuesX := Params.ValuesX^;
      FValuesY := Params.ValuesY^;
    end;
  ValueCount := Min(Length(FValuesX), Length(FValuesY));
  if ValueCount < 2 then
    raise ESpectrumError.CreateFmtDK('Err_TooFewPoints', [FTitle]);
end;}

{procedure TGraph.SaveXML(Node: IXMLNode);

  procedure SaveValues;
  var
    i: Integer;
    child, value: IXMLNode;
  begin
    child := Node.AddChild('Values');
    child.Attributes['Count'] := ValueCount;
    for i := 0 to ValueCount-1 do
    begin
      value := child.AddChild('Point');
      value.Attributes['X'] := FValuesX[i];
      value.Attributes['Y'] := FValuesY[i];
    end;
  end;

  procedure SaveFormula(Params: TFormulaParams);
  var
    child, range: IXMLNode;
  begin
    child := Node.AddChild('Formula');

    range := child.AddChild('Range');
    range.Attributes['Min'] := Params.Range.Min;
    range.Attributes['Max'] := Params.Range.Max;
    range.Attributes['Step'] := Params.Range.Step;
    range.Attributes['Points'] := Params.Range.Points;
    range.Attributes['UseStep'] := Params.Range.UseStep;

    child.AddChild('Code').Text := Params.Formula;
  end;

var
  OldDec: Char;
begin
  Node.Attributes['Title'] := FTitle;
  Node.Attributes['Kind'] := Ord(FKind);
  Node.Attributes['AutoTitle'] := FAutoTitle;
  Node.AddChild('Source').Text := FSource;

  WriteLineSeries(Node, 'Line', FSeries);

  OldDec := DecimalSeparator;
  DecimalSeparator := '.';
  try
    case FKind of
      gkNormal: SaveValues;
      gkExpression: SaveFormula(TFormulaParams(FParams));
    end;
  finally
    DecimalSeparator := OldDec;
  end;
end; }

{procedure TGraph.LoadXML(Node: IXMLNode);

  procedure LoadValues;
  var
    i, count: Integer;
    child, value: IXMLNode;
    nodes: IXMLNodeList;
  begin
    child := Node.ChildNodes.FindNode('Values');
    if child <> nil then
    begin
      nodes := child.ChildNodes;
      count := nodes.Count;
      SetLength(FValuesX, count);
      SetLength(FValuesY, count);
      for i := 0 to count-1 do
      begin
        value := nodes[i];
        FValuesX[i] := VarAsType(value.Attributes['X'], varDouble);
        FValuesY[i] := VarAsType(value.Attributes['Y'], varDouble);
      end;
    end;
  end;

  procedure LoadFormula;
  var
    Params: TFormulaParams;
    child, range: IXMLNode;
  begin
    Params := TFormulaParams.Create;

    child := Node.ChildNodes.Nodes['Formula'];

    range := child.ChildNodes.Nodes['Range'];
    Params.Range.Min := VarAsType(range.Attributes['Min'], varDouble);
    Params.Range.Max := VarAsType(range.Attributes['Max'], varDouble);
    Params.Range.Step := VarAsType(range.Attributes['Step'], varDouble);
    Params.Range.Points := VarAsType(range.Attributes['Points'], varInteger);
    Params.Range.UseStep := VarAsType(range.Attributes['UseStep'], varBoolean);

    Params.Formula := child.ChildNodes.Nodes['Code'].Text;

    FParams := Params;
    FillValues;
  end;

var
  OldSep: Char;
begin
  FTitle := Node.Attributes['Title'];
  if not VarIsNull(Node.Attributes['Kind'])
    then FKind := Node.Attributes['Kind']
    else FKind := gkNormal;
  if not VarIsNull(Node.Attributes['AutoTitle'])
    then FAutoTitle := Node.Attributes['AutoTitle']
    else FAutoTitle := True;
  FSource := Node.ChildNodes.Nodes['Source'].Text;

{$ifndef DBG_IGNORE_LOAD_ERRS}
  try
{$endif}
    ReadLineSeries(Node, 'Line', FSeries);
{$ifndef DBG_IGNORE_LOAD_ERRS}
  except
    on e: Exception do
      MsgBoxDK('PlotErr_LoadLine', [FTitle, e.ClassName, e.Message], mbError);
  end;
{$endif}

  OldSep := DecimalSeparator;
  DecimalSeparator := '.';
  try
    case FKind of
      gkNormal: LoadValues;
      gkExpression: LoadFormula;
    end;
  finally
    DecimalSeparator := OldSep;
  end;
end; }

procedure TGraph.UpdateValues;
begin
  //case FKind of
  //  gkExpression:
  //  begin
  //    FSource := FormulaExpression(TFormulaParams(FParams).Formula);
  //    if FAutoTitle then SetTitle(FSource); // fire OnGraphChanged
  //  end;
  //end;
  //FillValues;
  //UpdateLine;
  //if Assigned(FOwner) then FOwner.DoGraphChangedValues(Self);
end;

procedure TGraph.FillValues;
begin
  //case FKind of
  //  gkExpression:
  //    PlotMath.PlotFormula(TFormulaParams(FParams), FValuesX, FValuesY);
  //end;
end;

procedure TGraph.FillSampleValues(ACount: Integer);
begin
  PlotMath.FillSampleValues(FValuesX, FValuesY, ACount);
end;

procedure TGraph.FillSampleValues(var Param: TRandomSampleParams);
begin
  PlotMath.FillSampleValues(FValuesX, FValuesY, Param);
end;

procedure TGraph.GetMinMaxX(out MinValue, MaxValue: TValue);
begin
  PlotMath.GetMinMax(FValuesX, MinValue, MaxValue);
end;

procedure TGraph.GetMinMaxY(out MinValue, MaxValue: TValue);
begin
  PlotMath.GetMinMax(FValuesY, MinValue, MaxValue);
end;

{%region Editing}
procedure TGraph.Trim(var Param: TTrimParams);
var
  Result: Boolean;
  BoundValue: TValue;
begin
  //Result := False;
  //if Param.TrimLeft then
  //begin
  //  if Param.VisibleLeft
  //    then BoundValue := FSeries.GetHorizAxis.Minimum
  //    else BoundValue := Param.ValueLeft;
  //  Result := TrimValuesLeft(FValuesX, FValuesY, BoundValue, Param.RefineLeft);
  //end;
  //if Param.TrimRight then
  //begin
  //  if Param.VisibleRight
  //    then BoundValue := FSeries.GetHorizAxis.Maximum
  //    else BoundValue := Param.ValueRight;
  //  Result := TrimValuesRight(FValuesX, FValuesY, BoundValue, Param.RefineRight) or Result;
  //end;
  //if Result and Assigned(FOwner) then FOwner.DoGraphChangedValues(Self);
end;

procedure TGraph.Offset(var Param: TOffsetParams);
begin
  case Param.Direction of
    adirX:
    begin
      case Param.Kind of
        ofkMax: Param.Value := -FValuesX[MaxValueIndex(FValuesY)];
        ofkMin: Param.Value := -FValuesX[MinValueIndex(FValuesY)];
        ofkAvg: Param.Value := -AverageValue(FValuesX);
      end;
      OffsetValues(FValuesX, Param.Value);
    end;
    adirY: OffsetValues(FValuesY, Param.Kind, Param.Value);
  end;
  if Assigned(FOwner) then FOwner.DoGraphChangedValues(Self);
end;

procedure TGraph.Normalize(var Param: TNormalizeParams);
begin
  case Param.Direction of
    adirX: NormalizeValues(FValuesX, Param.Value, Param.PerMaximum);
    adirY: NormalizeValues(FValuesY, Param.Value, Param.PerMaximum);
  end;
  if Assigned(FOwner) then FOwner.DoGraphChangedValues(Self);
end;

procedure TGraph.SwapXY;
var
  I: Integer;
  Tmp: TValue;
begin
  for I := 0 to Length(FValuesX)-1 do
  begin
    Tmp := FValuesX[I];
    FValuesX[I] := FValuesY[I];
    FValuesY[I] := Tmp;
  end;
  if Assigned(FOwner) then FOwner.DoGraphChangedValues(Self);
end;

procedure TGraph.FlipX;
var
  I, C, C1: Integer;
  Tmp: TValue;
begin
  C := Length(FValuesX) - 1;
  C1 := Length(FValuesX) div 2 - 1;
  for I := 0 to C1 do
  begin
    Tmp := FValuesY[I];
    FValuesY[I] := FValuesY[C-I];
    FValuesY[C-I] := Tmp;
  end;
  if Assigned(FOwner) then FOwner.DoGraphChangedValues(Self);
end;

procedure TGraph.FlipY;
var
  I: Integer;
  MinValue, MaxValue: TValue;
begin
  for I := 0 to Length(FValuesY)-1 do
    FValuesY[I] := -1 * FValuesY[I]; // перевернуть
  GetMinMaxY(MinValue, MaxValue);
  for I := 0 to Length(FValuesY)-1 do // на дельту min-max вверх
    FValuesY[I] := FValuesY[I] - MinValue - MaxValue;
  if Assigned(FOwner) then FOwner.DoGraphChangedValues(Self);
end;

procedure TGraph.Sort;
begin
  // TODO: Undo
  PlotMath.Sort(FValuesX, FValuesY);
  if Assigned(FOwner) then FOwner.DoGraphChangedValues(Self);
end;
{%endregion Editing}

{%endregion TGraph}

//------------------------------------------------------------------------------

{%region TPlot}
constructor TPlot.Create(AOwner: TPlots; const ATitle: String = '');
begin
  FOwner := AOwner;
  FItems := TGraphList.Create;

  if ATitle = '' then
  begin
    FTitle := Plot_DefTitle + ' ' + IntToStr(UntitledIndex);
    Inc(UntitledIndex);
  end
  else FTitle := ATitle;
end;

destructor TPlot.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

procedure TPlot.Notify(AOperation: TPlotOperation; AGraph: TGraph);
begin
  if Assigned(Owner) then Owner.Notify(AOperation, Self, AGraph);
end;

//procedure TPlot.CreateChart;
//begin
//  //FChart := TChart.Create(nil);
//  //FChart.Visible := False;
//  //FChart.Align := alClient;
//  //FChart.BevelInner := bvNone;
//  //FChart.BevelOuter := bvNone;
//  //FChart.View3D := False;
//  //FChart.Legend.LegendStyle := lsSeries;
//  //FChart.BackWall.Color := clWhite;
//  //FChart.BackWall.Transparent := False;
//  //FChart.Zoom.Pen.Color := clGray;
//  //FChart.Zoom.Pen.SmallDots := True;
//  //FChart.Zoom.Pen.SmallSpace := 2;
//  //FChart.BottomAxis.AxisValuesFormat := '#,##0.###############';
//  //FChart.LeftAxis.AxisValuesFormat := '#,##0.###############';
//  //
//  //FSelector := TMultiSelectorTool.Create(FChart);
//  //FSelector.AllowMultiSelect := True;
//  //FSelector.AllowRightSelect := True;
//  //FChart.Tools.Add(FSelector);
//  //
//  //FHintValuesTool := TMarksTipTool.Create(FChart);
//  //FHintValuesTool.Style := smsXY;
//  //FChart.Tools.Add(FHintValuesTool);
//  //
//  //ApplyChartSettings;
//end;

procedure TPlot.ApplyChartSettings;
//var
//  I: Integer;
//  T: TAxisScrollTool;
begin
  //FChart.Zoom.Animated := Preferences.AnimatedChartZoom;
  //if Preferences.ScrollAxesByMouse then
  //begin
  //  T := TAxisScrollTool.Create(FChart);
  //  T.Axis := FChart.BottomAxis;
  //  FChart.Tools.Add(T);
  //
  //  T := TAxisScrollTool.Create(FChart);
  //  T.Axis := FChart.LeftAxis;
  //  FChart.Tools.Add(T);
  //end
  //else
  //  for I := FChart.Tools.Count-1 downto 0 do
  //    if FChart.Tools[I] is TAxisScrollTool then
  //    begin
  //      T := TAxisScrollTool(FChart.Tools[I]);
  //      FChart.Tools.Delete(I);
  //      T.Free;
  //    end;
end;

procedure TPlot.Clear;
var
  I: Integer;
begin
  if FItems.Count > 0 then
  begin
    for I := 0 to FItems.Count-1 do
      TGraph(FItems[I]).Free;
    FItems.Clear;
    DoGraphDeleted(nil);
  end;
  // TODO Remove from history all commands referencing graphs of this plot
end;

//function TPlot.AddGraph(Params: TGraphAppendParams; const FileName: String): TGraph;
//var
//  InputFiles: TStringList;
//begin
//  InputFiles := TStringList.Create;
//  try
//    InputFiles.Add(FileName);
//    Result := AddGraph(Params, InputFiles);
//  finally
//    InputFiles.Free;
//  end;
//end;
(*
function TPlot.AddGraph(Params: TGraphAppendParams; FileNames: TStrings = nil): TGraph;

  procedure ProcessMultiReader(AReader: TDataReaderClass);
  begin
    // Создаем мультиридер прямо здесь, а не в TGraph.Load.
    // Результатом работы ReadValues является еще несколько вызовов
    // этой процедуры (TPlot.AddGraph), но с уже готовыми данными в
    // объекте-параметре (TGraphAppendParams.ValuesX / ValuesY)
    with AReader.Create(Params) do
    try
      if Configure then ReadValues;
    finally
      Free;
    end;
  end;

var
  I: Integer;
begin
  Result := nil;
  if Assigned(Params) then
  try
    Params.Plot := Self;
    // Если задан список файлов, то предполагается, что еще никаких потоков не
    // открыто (Params.Stream не задан, Params.Source не задействован).
    // В этом случае источником данных является файл.
    if Assigned(FileNames) then
    begin
      for I := 0 to FileNames.Count-1 do
      try
        Params.Source := FileNames[I];
        {if FileIsArchive(Params.Source) then
        begin
          // Если источник данных является архивным файлом, то в данный момент
          // создания графика не происходит. Просто открываем окно просмотра архива.
          TwndOpenArchive.Create(Params.Source, Self).Show;
        end
        else} if Assigned(Params.Reader) then
        // Если ридер задан, то это значит что в диалоге открытия файлов был
        // выбран конкретный фильтр и все файлы пытаемся открыть этим ридером.
        // Привязка к фильтру нужна из-за того что одно и то же расширение
        // может соотвествовать разным форматам данных (разным ридерам).
        begin
          if Params.Reader.IsMulti then
            // Если ридер может посторить несколько графиков из одного источника
            // то график создается не сразу, а в процессе работы мультиридера.
            ProcessMultiReader(Params.Reader)
          else
            // Если это простой ридер, то сразу создаем график
            Result := CreateGraph(Params);
        end
        else
        // Если ридер не задан это означает что в диалоге был выбран фильтр
        // "Все файлы". Тогда пытаемся определить нужный рилер на основании
        // расширения файла. Но т.к. одно и то же расширение может соотвествовать
        // разным ридерам, то не обязательно будет использоваться тот ридер,
        // кторый нужен для конкретного файла.
        begin
          Params.Reader := TFileReaders.ReaderByExt(Params.Source);
          Result := CreateGraph(Params);
        end;
      except
        on e: Exception do
        {$ifndef DBG_IGNORE_LOAD_ERRS}
          if I < FileNames.Count-1 then
          begin // если не последний файл, то нужно ли открывать оставшиеся
            if not MsgBoxYesNo(Constant('Err_ReadData') + #13#13 + e.Message +
              #13#13 + Constant('Err_FileTryNext'), [FileNames[I]]) then Break;
          end
          else
            MsgBox(Constant('Err_ReadData') + #13#13 + e.Message, [FileNames[I]], mbError);
        {$else}
          raise;
        {$endif}
      end
    end

    // Если список файлов не задан, то предполагается, что должен быть задан
    // либо поток Params.Stream, либо в параметрах передаются уже готовые данные,
    // либо ридер сам знает, откуда ему взять данные (как TClipboardDataReader).
    else
    try
      if Assigned(Params.Reader) and Params.Reader.IsMulti then
        ProcessMultiReader(Params.Reader)
      else
        Result := CreateGraph(Params);
    except
      on e: Exception do
      {$ifndef DBG_IGNORE_LOAD_ERRS}
        MsgBox(Constant('Err_ReadData') + #13#13 + e.Message, [Params.Source], mbError);
      {$else}
        raise;
      {$endif}
    end;
  finally
    Params.Free;
  end;
end;
 *)
//function TPlot.AddGraph: TGraph;
//begin
//  Result := CreateGraph;
//  DoGraphAdded(Result);
//end;

procedure TPlot.AddGraph(AGraph: TGraph; AIndex: Integer = -1; AUndoable: Boolean = True);
begin
  if AIndex > -1
    then FItems.Insert(AIndex, AGraph)
    else FItems.Add(AGraph);
  AGraph.FOwner := Self;
  if AUndoable then
    History.Append(TGraphAppendCommand.Create(AGraph));
  DoGraphAdded(AGraph);
end;

//procedure TPlot.AddGraph(AGraph: TGraphRec; const ATitle: String);
//begin
//
//end;
//
//function TPlot.AddSampleGraph(ATitle: String): TGraph;
//begin
//  Result := CreateGraph(ATitle);
//  Result.FillSampleValues(100);
//  DoGraphAdded(Result);
//end;
//
//function TPlot.AddSampleGraph(ATitle: String; AParams: TRandomSampleParams): TGraph;
//begin
//  Result := CreateGraph(ATitle);
//  Result.FillSampleValues(AParams);
//  DoGraphAdded(Result);
//end;

//function TPlot.CreateGraph(Params: TGraphAppendParams): TGraph;
//begin
//  try
//    Result := CreateGraph;
//    Result.Load(Params);
//    DoGraphAdded(Result);
//  except
//    FItems.Extract(Result);
//    FreeAndNil(Result);
//    raise;
//  end;
//end;
//
//function TPlot.CreateGraph(Node: IXMLNode): TGraph;
//begin
//  try
//    Result := CreateGraph;
//    Result.LoadXML(Node);
//    DoGraphAdded(Result);
//  except
//    FItems.Extract(Result);
//    FreeAndNil(Result);
//    raise;
//  end;
//end;

//function TPlot.CreateGraph(ATitle: String = ''): TGraph;
//begin
//  Result := TGraph.Create(Self);
//  Result.FTitle := ATitle;
//  Result.FSource := ATitle;
//  FItems.Add(Result);
//  History.Append(TGraphAppendCommand.Create(Result));
//end;

//procedure TPlot.ExtractGraph(AGraph: TGraph);
//begin
//  FItems.Extract(AGraph);
//  DoGraphDeleted(AGraph);
//end;

procedure TPlot.DeleteGraph(AGraph: TGraph; AUndoable: Boolean = True);
begin
  if AUndoable then
    History.Append(TGraphDeleteCommand.Create(AGraph, FItems.IndexOf(AGraph)));
  FItems.Extract(AGraph);
  DoGraphDeleted(AGraph);
  // Do not destroy graph, because of it is required to undo the deletion
end;

{%region Events}
procedure TPlot.DoGraphAdded(AGraph: TGraph);
begin
  if Assigned(FOwner) then
  begin
    FOwner.FModified := True;
    FOwner.Notify(poGraphAdded, Self, AGraph);
  end;
end;

procedure TPlot.DoGraphDeleted(AGraph: TGraph);
begin
  if Assigned(FOwner) then
  begin
    FOwner.FModified := True;
    FOwner.Notify(poGraphDeleted, Self, AGraph);
  end;
end;

procedure TPlot.DoGraphChanged(AGraph: TGraph);
begin
  if Assigned(FOwner) then
  begin
    FOwner.FModified := True;
    FOwner.Notify(poGraphChanged, Self, AGraph);
  end;
end;

procedure TPlot.DoGraphChangedValues(AGraph: TGraph);
begin
  if Assigned(FOwner) then
  begin
    FOwner.FModified := True;
    FOwner.Notify(poGraphChangedValues, Self, AGraph);
  end;
end;
{%endregion}

{%region Title}
procedure TPlot.SetTitle(Value: String);
begin
  if Value <> FTitle then
  begin
    FTitle := Value;
    //  History.Append(TTitleEditCommand.Create(Self));
    Notify(poPlotChanged);
  end;
end;
{%endregion}

{$region 'Индексация элементов'}
function TPlot.IsEmpty: Boolean;
begin
  Result := FItems.Count = 0;
end;

function TPlot.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TPlot.GetItems(Index: Integer): TGraph;
begin
  Result := TGraph(FItems[Index]);
end;
{$endregion}

{$region 'Операции редактирования'}
procedure TPlot.CreateUndoGroup(var Graphs: TGraphArray; const Title: String);
var I: Integer;
begin
  //if Length(Graphs) > 1 then
  //  History.BeginGroup(Constant(Title));
  //try
  //  for I := 0 to Length(Graphs)-1 do
  //    History.Append(TFullDataGraphCommand.Create(Graphs[I], Title));
  //finally
  //  if History.Groupped then History.EndGroup;
  //end;
end;

procedure TPlot.Trim(var Graphs: TGraphArray; var Param: TTrimParams);
var I: Integer;
begin
  CreateUndoGroup(Graphs, 'Undo_Trim');
  for I := 0 to Length(Graphs)-1 do Graphs[I].Trim(Param);
end;

procedure TPlot.Offset(var Graphs: TGraphArray; var Param: TOffsetParams);
var I: Integer;
begin
  CreateUndoGroup(Graphs, 'Undo_Offset');
  for I := 0 to Length(Graphs)-1 do Graphs[I].Offset(Param);
end;

procedure TPlot.Flip(var Graphs: TGraphArray; var Param: TMiscParams);
var I: Integer;
begin
  CreateUndoGroup(Graphs, 'Undo_Flip');
  for I := 0 to Length(Graphs)-1 do
  begin
    case Param.Direction of
      adirX:
      begin
        FlipValues(Graphs[I].FValuesX, Param.Value);
        // При воплнении опрерации Const-X все значения X оказываются
        // переставленными в обратном порядке max -> min. В принципе, линия
        // графика не сортированная и для отображения ничего страшного в этом
        // нет. Но все-таки принято, чтобы график шел от большего к меньшему.
        // Поэтому переставляем значения в обратном порядке.
        ReorderValues(Graphs[I].FValuesX, Graphs[I].FValuesY);
        // TODO: селектор странно себя ведет при обратном расположении
        // точек - выделяются все точки. Пока не разбирался.
      end;
      adirY: FlipValues(Graphs[I].FValuesY, Param.Value);
    end;
    DoGraphChangedValues(Graphs[I]);
  end;
end;

procedure TPlot.Normalize(var Graphs: TGraphArray; var Param: TNormalizeParams);
var I: Integer;
begin
  CreateUndoGroup(Graphs, 'Undo_Normalize');
  for I := 0 to Length(Graphs)-1 do Graphs[I].Normalize(Param);
end;

procedure TPlot.Scale(var Graphs: TGraphArray; var Param: TScaleParams);
var I: Integer;
begin
  CreateUndoGroup(Graphs, 'Undo_Scale');
  for I := 0 to Length(Graphs)-1 do
  begin
    with Param do
      case Direction of
        adirX: ScaleValues(Graphs[I].FValuesX, CenterKind, CenterValue, Value);
        adirY: ScaleValues(Graphs[I].FValuesY, CenterKind, CenterValue, Value);
      end;
    DoGraphChangedValues(Graphs[I]);
  end;
end;

procedure TPlot.Inverse(var Graphs: TGraphArray; var Param: TMiscParams);
var I: Integer;
begin
  CreateUndoGroup(Graphs, 'Undo_Inverse');
  for I := 0 to Length(Graphs)-1 do
  begin
    case Param.Direction of
      adirX:
      begin
        InverseValues(Graphs[I].FValuesX, Graphs[I].FValuesY, Param.Value);
        // При воплнении опрерации Const/X все значения X оказываются
        // переставленными в обратном порядке max -> min. В принципе, линия
        // графика не сортированная и для отображения ничего страшного в этом
        // нет. Но все-таки принято, чтобы график шел от большего к меньшему.
        // Поэтому переставляем значения в обратном порядке.
        ReorderValues(Graphs[I].FValuesX, Graphs[I].FValuesY);
        // TODO: селектор странно себя ведет при обратном расположении
        // точек - выделяются все точки. Пока не разбирался.
      end;
      adirY: InverseValues(Graphs[I].FValuesY, Graphs[I].FValuesX, Param.Value);
    end;
    DoGraphChangedValues(Graphs[I]);
  end;
end;

procedure TPlot.SwapXY(var Graphs: TGraphArray);
var I: Integer;
begin
  // TODO: make resort X values after swapping
  for I := 0 to Length(Graphs)-1 do Graphs[I].SwapXY;
  //History.Append(TReversibleGraphCommand.Create(Graphs, rgkSwapXY));
end;

procedure TPlot.FlipX(var Graphs: TGraphArray);
var I: Integer;
begin
  for I := 0 to Length(Graphs)-1 do Graphs[I].FlipX;
  //History.Append(TReversibleGraphCommand.Create(Graphs, rgkFlipX));
end;

procedure TPlot.FlipY(var Graphs: TGraphArray);
var I: Integer;
begin
  CreateUndoGroup(Graphs, 'Undo_FlipY');
  for I := 0 to Length(Graphs)-1 do Graphs[I].FlipY;
end;

procedure TPlot.Despike(var Graphs: TGraphArray; var Params: TDespikeParams);
var
  I: Integer;
begin
  CreateUndoGroup(Graphs, 'Undo_Despike');
  for I := 0 to Length(Graphs)-1 do
  begin
    case Params.Kind of
      drPercent: DespikePer(Graphs[I].FValuesY, Params.Min, Params.Max);
      drAbsolute: DespikeAbs(Graphs[I].FValuesY, Params.Min, Params.Max);
    end;
    DoGraphChangedValues(Graphs[I]);
  end;
end;
{$endregion}

{$region 'Сохранение/Загрузка'}
{procedure TPlot.SaveXML(Node: IXMLNode);
var
  i: Integer;
  child, tmp: IXMLNode;
begin
  // save common plot settings
  Node.Attributes['Title'] := FTitle;
  with Node.AddChild('Factors') do
  begin
    Attributes['X'] := FactorX;
    Attributes['Y'] := FactorY;
  end;

  // chart settings
  child := Node.AddChild('PlotSettings');
  TeeStoreXML.WriteChartSettings(child, FChart);
  child := Node.AddChild('AxesLimits');
  TeeStoreXML.WriteAxesLimits(child, FChart);

  // save each graph
  child := Node.AddChild('Graphs');
  for i := 0 to FItems.Count-1 do
  begin
    tmp := child.AddChild('Graph');
    TGraph(FItems[i]).SaveXML(tmp);
  end;

  // save chart notes
  child := Node.AddChild('Annotations');
  for i := 0 to Chart.Tools.Count-1 do
    if Chart.Tools[i] is TAnnotationTool then
      TeeStoreXML.WriteNoteSettings(child,
        'Annotation', TAnnotationTool(Chart.Tools[i]));
end;   }

{procedure TPlot.LoadXML(Node: IXMLNode);
var
  i: Integer;
  child: IXMLNode;
  nodes: IXMLNodeList;
begin
  // load common plot settings
  FTitle := Node.Attributes['Title'];

  child := Node.ChildNodes.FindNode('Factors');
  if child <> nil then
  begin
    FactorX := child.Attributes['X'];
    FactorY := child.Attributes['Y'];
  end;

  // chart settings
{$ifndef DBG_IGNORE_LOAD_ERRS}
  try
{$endif}
    child := Node.ChildNodes.FindNode('PlotSettings');
    if child <> nil then ReadChartSettings(child, FChart);
    child := Node.ChildNodes.FindNode('AxesLimits');
    if child <> nil then ReadAxesLimits(child, FChart);
{$ifndef DBG_IGNORE_LOAD_ERRS}
  except
    on e: Exception do
      MsgBoxDK('PlotErr_LoadFormat', [FTitle, e.ClassName, e.Message], mbError);
    // TODO: ошибки надо отлавливать и где-то хранить, а сообщение
    // выдавать только по окончание загрузки всего проекта
  end;
{$endif}

  // load each graph
  child := Node.ChildNodes.FindNode('Graphs');
  if child <> nil then
  begin
    nodes := child.ChildNodes;
    for i := 0 to nodes.Count-1 do CreateGraph(nodes[i]);
    // TODO: хорошо ли это - прекращать загрузку диаграммы, если
    // один из графиков загрузился с ошибкой
  end;

  // load chart notes
  child := Node.ChildNodes.FindNode('Annotations');
  if child <> nil then
  begin
    nodes := child.ChildNodes;
    for i := 0 to nodes.Count-1 do CreateNote(nodes[i]);
    // TODO: отловить исключения, т.к. ошибки при загрузки
    // примечаний не критичны для загрузки всего проекта
  end;

end;  }
{$endregion}

{$region 'Chart tools work'}
{function TPlot.CreateNote(Node: IXMLNode): TAnnotationTool;
begin
  try
    Result := TAnnotationTool.Create(Chart);
    ReadNoteSettings(Node, Result);
    Chart.Tools.Add(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;}
{$endregion}

{$region 'Calc and Draw procs'}
procedure TPlot.Differentiate(var Graphs: TGraphArray);
var
  I: Integer;
  DiffY: TValueArray;
  Graph: TGraph;
begin
  //if Length(Graphs) > 1 then
  //  History.BeginGroup(Constant('Undo_AddGraph'));
  try
    for I := 0 to Length(Graphs)-1 do
    begin
      PlotMath.Differentiate(Graphs[I].ValuesX, Graphs[I].ValuesY, DiffY);
     // Graph := CreateGraph(Format('Derivative(%s)', [Graphs[I].Title]));
      Graph.SetValuesXY(Graphs[I].ValuesX, DiffY);
      DoGraphAdded(Graph);
    end;
  finally
//    if History.Groupped then History.EndGroup;
  end;
end;

procedure TPlot.Differentiate2(var Graphs: TGraphArray);
var
  I: Integer;
  DiffY: TValueArray;
  Graph: TGraph;
begin
  //if Length(Graphs) > 1 then
  //  History.BeginGroup(Constant('Undo_AddGraph'));
  try
    for I := 0 to Length(Graphs)-1 do
    begin
      PlotMath.Differentiate2(Graphs[I].ValuesX, Graphs[I].ValuesY, DiffY);
      if Length(DiffY) > 1 then
      begin
      //  Graph := CreateGraph(Format('Derivative2(%s)', [Graphs[I].Title]));
        Graph.SetValuesXY(
          Copy(Graphs[I].ValuesX, 1, Length(Graphs[I].ValuesX)-2), DiffY);
        DoGraphAdded(Graph);
      end;
    end;
  finally
//    if History.Groupped then History.EndGroup;
  end;
end;

procedure TPlot.Regularity(var Graphs: TGraphArray);
var
  I: Integer;
  X, Y: TValueArray;
  Graph: TGraph;
begin
  //if Length(Graphs) > 1 then
  //  History.BeginGroup(Constant('Undo_AddGraph'));
  try
    for I := 0 to Length(Graphs)-1 do
    begin
      PlotMath.Regularity(Graphs[I].ValuesX, X, Y);
    //  Graph := CreateGraph(Format('Regularity(%s)', [Graphs[I].Title]));
      Graph.SetValuesXY(X, Y);
      DoGraphAdded(Graph);
    end;
  finally
//    if History.Groupped then History.EndGroup;
  end;
end;

procedure TPlot.Midline(var Graphs: TGraphArray);
begin

end;

procedure TPlot.Envelope(var Graphs: TGraphArray; IsTop: Boolean);
//var
//  I, J: Integer;
//  overlap: TGraphRec2;
//  gr1, gr2: TGraphRec;
begin
  //gr1 := GraphToRec(Graphs[0]);
  //for I := 1 to Length(Graphs)-1 do
  //begin
  //  gr2 := GraphToRec(Graphs[I]);
  //  overlap := CalcOverlapGraph(gr1, gr2, True);
  //  gr1.X := overlap.X;
  //  SetLength(gr1.Y, Length(overlap.X));
  //  if IsTop then
  //    for J := 0 to Length(overlap.X)-1 do
  //      gr1.Y[J] := Max(overlap.Y1[J], overlap.Y2[J])
  //  else
  //    for J := 0 to Length(overlap.X)-1 do
  //      gr1.Y[J] := Min(overlap.Y1[J], overlap.Y2[J]);
  //end;
  //AddGraph(TGraphAppendParams.Create(@gr1.X, @gr1.Y, 'Envelope'));
end;

procedure TPlot.GroupOperation(var Graphs: TGraphArray; Operation: TGroupOperation);
const
  FuncName: array[TGroupOperation] of String = ('Sum', 'Difference', 'Product', 'Quotient');
//var
//  I, J: Integer;
//  gr1, gr2: TGraphRec;
//  Overlap: TGraphRec2;
begin
  //gr1 := GraphToRec(Graphs[0]);
  //for I := 1 to Length(Graphs)-1 do
  //begin
  //  gr2 := GraphToRec(Graphs[I]);
  //  Overlap := PlotMath.CalcOverlapGraph(gr1, gr2);
  //  gr1.X := Overlap.X;
  //  SetLength(gr1.Y, Length(Overlap.X));
  //  case Operation of
  //    gropSum:
  //      for J := 0 to Length(Overlap.X)-1 do
  //        gr1.Y[J] := Overlap.Y1[J] + Overlap.Y2[J];
  //    gropDiff:
  //      for J := 0 to Length(Overlap.X)-1 do
  //        gr1.Y[J] := Overlap.Y1[J] - Overlap.Y2[J];
  //    gropProd:
  //      for J := 0 to Length(Overlap.X)-1 do
  //        gr1.Y[J] := Overlap.Y1[J] * Overlap.Y2[J];
  //    gropQuot:
  //      for J := 0 to Length(Overlap.X)-1 do
  //        gr1.Y[J] := Overlap.Y1[J] / Overlap.Y2[J];
  //  end;
  //end;
  //AddGraph(TGraphAppendParams.Create(@gr1.X, @gr1.Y, FuncName[Operation]));
end;

procedure TPlot.Variance(var Graphs: TGraphArray; var Param: TVarianceParams);
var
  I: Integer;
  Plot: TPlot;
  Graph: TGraph;
  AllanX, AllanY: TValueArray;
begin
  Plot := Owner.AddPlot;
  //if Length(Graphs) > 1 then
  //  History.BeginGroup(Constant('Undo_AddGraph'));
  try
    for I := 0 to Length(Graphs)-1 do
    begin
      PlotMath.Variance(Graphs[I].ValuesX, Graphs[I].ValuesY, AllanX, AllanY);
      //Graph := Plot.CreateGraph(Format('%s(%s)',
        //[VarianceNames[Param.Kind], Graphs[I].Title]));
      Graph.SetValuesXY(AllanX, AllanY);
      Plot.DoGraphAdded(Graph);
    end;
  finally
//    if History.Groupped then History.EndGroup;
  end;
end;
{%endregion}
{%endregion TPlot}

//------------------------------------------------------------------------------

{%region TPlots}
constructor TPlots.Create;
begin
  FUntitled := True;
  FItems := TPlotList.Create;
end;

destructor TPlots.Destroy;
begin
  Clear;
  FItems.Free;
  FNotifyClients.Free;
  inherited;
end;

procedure TPlots.SetModified(Value: Boolean);
begin
  if FModified <> Value then
  begin
    FModified := Value;
    Notify(poModified);
  end;
end;

procedure TPlots.Clear;
var
  I: Integer;
begin
  if FItems.Count > 0 then
  begin
    for I := FItems.Count-1 downto 0 do
    begin
      Notify(poPlotDestroyed, FItems[I], nil);
      FItems[I].Clear;
    end;
    for I := FItems.Count-1 downto 0 do FItems[I].Free;
    FItems.Clear;
    Notify(poPlotDestroyed);
  end;
end;

function TPlots.AddPlot(const ATitle: String = ''): TPlot;
begin
  Result := CreatePlot(ATitle);
  Notify(poPlotAdded, Result);
  // First diagram is added by default and
  // it does not mean that project was modified
  if Count = 1 then Modified := False;
end;

//function TPlots.AddPlot(Node: IXMLNode): TPlot;
//begin
//  try
//    Result := CreatePlot;
//    Result.LoadXML(Node);
//    DoPlotAdded(Result);
//  except
//    FItems.Extract(Result);
//    FreeAndNil(Result);
//    raise;
//  end;
//end;

function TPlots.CreatePlot(const ATitle: String = ''): TPlot;
begin
  Result := TPlot.Create(Self, ATitle);
  FItems.Add(Result);
end;

procedure TPlots.DeletePlot(Index: Integer);
begin
  DeletePlot(TPlot(FItems[Index]));
end;

procedure TPlots.DeletePlot(APlot: TPlot);
begin
  Notify(poPlotDestroying, APlot);
  FItems.Extract(APlot);
  Notify(poPlotDestroyed, APlot);
  APlot.Free;
end;

{%region Events}
{
procedure TPlots.DoPlotAdded(APlot: TPlot);
begin
  if Count > 1 then FModified := True;
  Notify(poPlotAdded, APlot, nil);
end;

procedure TPlots.DoPlotDeleting(APlot: TPlot);
begin
  Notify(poPlotDestroying, APlot, nil);
end;

procedure TPlots.DoPlotDeleted(APlot: TPlot);
begin
  FModified := True;
  Notify(poPlotDestroyed, APlot, nil);
end;

procedure TPlots.DoPlotChanged(APlot: TPlot);
begin
  FModified := True;
  Notify(poPlotChanged, APlot, nil);
end;
 }
{procedure TPlots.DoProjectSaved;
begin
  FModified := False;
  Notify(poProjectSaved, nil, nil);
end;

procedure TPlots.DoProjectLoaded;
begin
  FModified := False;
  Notify(poProjectLoaded, nil, nil);
end;

procedure TPlots.DoProjectReseting;
begin
  Notify(poProjectReseting, nil, nil);
end;

procedure TPlots.DoProjectReset;
begin
  FModified := False;
  Notify(poProjectReset, nil, nil);
end;}
{%endregion}

{%region Enumerate Plots}
function TPlots.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TPlots.GetItems(Index: Integer): TPlot;
begin
  Result := TPlot(FItems[Index]);
end;

function TPlots.IndexOf(APlot: TPlot): Integer;
begin
  Result := FItems.IndexOf(APlot);
end;
{%endregion}

{%region Load/Save}
function TPlots.Load: Boolean;
begin
  //with TOpenDialog.Create(nil) do
  //try
  //  Title := Constant('Proj_LoadDlg');
  //  Filter := Constant('Proj_FileFilter');
  //  FilterIndex := Preferences.ProjectOpenFilter;
  //  InitialDir := Preferences.ProjectOpenCurDir;
  //  DefaultExt := CDefProjectExt;
  //  Options := Options + [ofOverwritePrompt];
  //  Result := Execute;
  //  if Result then
  //  begin
  //    Preferences.ProjectOpenFilter := FilterIndex;
  //    Preferences.ProjectOpenCurDir := ExtractFilePath(FileName);
  //    Load(FileName, IsProjectPacked(FileName));
  //  end;
  //finally
  //  Free;
  //end;
end;

procedure TPlots.Load(const AFileName: String; APacked: Boolean);
//var
//  SavedCur: TCursor;
begin
  //SavedCur := Screen.Cursor;
  //Screen.Cursor := crHourGlass;
  //Include(FStatus, pssLoading);
  //History.Active := False;
  //try
  //  Reset(True);
  //  LoadXML(AFileName, APacked);
  //  FUntitled := False;
  //  FPacked := APacked;
  //  FFileName := AFileName;
  //  DoProjectLoaded;
  //finally
  //  History.Active := True;
  //  Exclude(FStatus, pssLoading);
  //  Screen.Cursor := SavedCur;
  //end;
  //History.Clear;
end;

function TPlots.Save(APlot: TPlot = nil): Boolean;
//var
//  SavedCur: TCursor;
begin
  //if not FUntitled then
  //begin
  //  SavedCur := Screen.Cursor;
  //  Screen.Cursor := crHourGlass;
  //  try
  //    SaveXML(APlot);
  //  finally
  //    Screen.Cursor := SavedCur;
  //  end;
  //  if APlot = nil then
  //    DoProjectSaved;
  //  Result := True;
  //end
  //else Result := SaveAs(APlot);
end;

function TPlots.SaveAs(APlot: TPlot = nil): Boolean;
begin
  //with TSaveDialog.Create(nil) do
  //try
  //  Title := Constant('Proj_SaveDlg');
  //  Filter := Constant('Proj_FileFilter');
  //  FilterIndex := Preferences.ProjectOpenFilter;
  //  InitialDir := Preferences.ProjectOpenCurDir;
  //  DefaultExt := CDefProjectExt;
  //  Options := Options + [ofOverwritePrompt];
  //  FileName := ExtractFileName(FFileName);
  //  Result := Execute;
  //  if Result then
  //  begin
  //    Preferences.ProjectOpenFilter := FilterIndex;
  //    Preferences.ProjectOpenCurDir := ExtractFilePath(FileName);
  //    FUntitled := False;
  //    FPacked := FilterIndex = 2;
  //    FFileName := FileName;
  //    Save(APlot);
  //  end;
  //finally
  //  Free;
  //end;
end;

//procedure TPlots.SaveXML(APlot: TPlot);
//var
//  ProjRoot: IXMLNode;
//  OutFile: TFileStream;
//  Zip: TCompressionStream;
//begin
//  with TXMLDocument.Create(Application) do
//  try
//    Active := True;
//    Version := '1.0';
//    Encoding := 'utf-8';
//    StandAlone := 'yes';
//    if not FPacked then
//      Options := Options + [doNodeAutoIndent];
//
//    ProjRoot := Node.AddChild('SpectrumProject');
//    ProjRoot.Attributes['Version'] := PROJECT_VER;
//    SaveXML(ProjRoot, APlot);
//
//    if FPacked then
//    begin
//      OutFile := TFileStream.Create(FFileName, fmCreate, fmShareExclusive);
//      Zip := TCompressionStream.Create(clDefault, outFile);
//      try
//        XML.SaveToStream(Zip);
//      finally
//        Zip.Free;
//        OutFile.Free;
//      end;
//    end
//    else XML.SaveToFile(FFileName);
//  finally
//    Free;
//  end;
//end;

//procedure TPlots.SaveXML(Node: IXMLNode; APlot: TPlot);
//var
//  i: Integer;
//  child: IXMLNode;
//begin
//  // save common project settings
//
//  if Assigned(APlot) then
//  begin
//    child := Node.AddChild('Plot');
//    APlot.SaveXML(child);
//  end
//  else
//    for i := 0 to FItems.Count-1 do
//    begin
//      child := Node.AddChild('Plot');
//      TPlot(Items[i]).SaveXML(child);
//    end;
//end;

{procedure TPlots.LoadXML(const AFileName: String; APacked: Boolean);
var
  ProjRoot: IXMLNode;
  InFile: TFileStream;
  Zip: TDecompressionStream;
begin
{$ifndef DBG_IGNORE_LOAD_ERRS}
  try
{$endif}
    if not FileExists(AFileName) then
      raise ESpectrumError.CreateFmtDK('ProjErr_DoesNotExist', [AFileName]);

    with TXMLDocument.Create(Application) do
    try
      if APacked then
      begin
        InFile := TFileStream.Create(AFileName, fmOpenRead, fmShareDenyWrite);
        Zip := TDecompressionStream.Create(InFile);
        try
          XML.LoadFromStream(Zip);
        finally
          Zip.Free;
          InFile.Free;
        end;
      end
      else XML.LoadFromFile(AFileName);

      Active := True;
      ProjRoot := ChildNodes.FindNode('SpectrumProject');
      if ProjRoot = nil then
        raise ESpectrumError.CreateDK('ProjErr_NoRoot');

      if ProjRoot.Attributes['Version'] <> PROJECT_VER then
        raise ESpectrumError.CreateDK('ProjErr_WrongVersion');

      LoadXML(ProjRoot);
    finally
      Free;
    end;
{$ifndef DBG_IGNORE_LOAD_ERRS}
  except
    on e: Exception do
      raise ESpectrumError.CreateFmtDK('ProjErr_Load', [AFileName, e.Message]);
  end;
{$endif}
end;    }

//procedure TPlots.LoadXML(Node: IXMLNode);
//var
//  i: Integer;
//  child: IXMLNode;
//  nodes: IXMLNodeList;
//begin
//  // load common project settings
//
//  // load each plot
//  nodes := Node.ChildNodes;
//  for i := 0 to nodes.Count-1 do
//  begin
//    child := nodes[i];
//    if WideSameText(child.NodeName, 'Plot') then AddPlot(child);
//  end;
//end;
{%endregion}

function TPlots.Reset(Silent: Boolean; Mode: Integer): Boolean;
begin
  {if not Silent then
    with TwndProjReset.Create(Application) do
    try
      Result := ShowModal = mrOk;
      if not Result then Exit;
      Mode := ResetMode;
    finally
      Free;
    end
  else Result := True;}

  {case Mode of
      1: // keep plots
        for i := 0 to FItems.Count-1 do
          TPlot(FItems[i]).Clear;
      2: // delete all
        Clear;
    end;}

  Notify(poProjectReseting);
  Clear;
  //History.Clear;
  FFileName := '';
  FUntitled := True;
  FPacked := False;
  UntitledIndex := 1;
  Notify(poProjectReset);
  Result := True;
end;

function TPlots.CanReset: Boolean;
var
  Msg: String;
begin
  //Result := True;
  //if Modified then
  //begin
  //  if FUntitled
  //    then Msg := Constant('Proj_SaveReq1')
  //    else Msg := Format(Constant('Proj_SaveReq'), [FFileName]);
  //  case AppMessageDlg(Msg, mtConfirmation, mbYesNoCancel) of
  //    mrCancel: Result := False;
  //    mrYes: Result := Save;
  //  end;
  //end;
end;

{%region Notifications}
procedure TPlots.RegisterNotifyClient(AObject: TObject);
begin
  if FNotifyClients = nil then
  begin
    FNotifyClients := TObjectList.Create(False);
    FNotifyClients.Add(AObject);
  end
  else if FNotifyClients.IndexOf(AObject) = -1 then
    FNotifyClients.Add(AObject)
end;

procedure TPlots.UnRegisterNotifyClient(AObject: TObject);
begin
  if Assigned(FNotifyClients) then
  begin
    FNotifyClients.Extract(AObject);
    if FNotifyClients.Count = 0 then
      FreeAndNil(FNotifyClients);
  end;
end;

function TPlots.GetNotifyClient(AClass: TClass): TObject;
var I: Integer;
begin
  Result := nil;
  if Assigned(FNotifyClients) then
    for I := 0 to FNotifyClients.Count-1 do
      if FNotifyClients[I] is AClass then
      begin
        Result := FNotifyClients[I];
        Break;
      end;
end;

procedure TPlots.Notify(AOperation: TPlotOperation; APlot: TPlot; AGraph: TGraph);
var
  I: Integer;
  Intf: IPlotNotification;
begin
  if AOperation in ModifyingOperations then
    Modified := True
  else if AOperation in SavingOperations then
    Modified := False;

  if Assigned(FNotifyClients) then
    for I := FNotifyClients.Count-1 downto 0 do
      if FNotifyClients[I].GetInterface(IPlotNotification, Intf) then
        Intf.Notify(AOperation, APlot, AGraph);
end;
{%endregion}

{%endregion TPlots}
end.