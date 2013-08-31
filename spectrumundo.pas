unit SpectrumUndo;

interface

uses
  Classes, Contnrs,
  OriUndo,
  SpectrumTypes, Plots;

type
  TSpectrumUndoCommand = class(TOriUndoCommand)
  protected
    FSource: TObject;
    FOwnSource: Boolean;
  public
    destructor Destroy; override;
  end;

  {TTitleEditCommand = class(TOriUndoCommand)
  private
    FSource: TObject;
    FAutoTitle: Boolean;
    FBackup: String;
  protected
    procedure Swap; override;
  public
    constructor Create(AGraph: TGraph); overload;
    constructor Create(APlot: TPlot); overload;
  end;}

  TGraphAppendCommand = class(TSpectrumUndoCommand)
  public
    constructor Create(AGraph: TGraph);
    procedure Undo; override;
    procedure Redo; override;
  end;

  TGraphDeleteCommand = class(TSpectrumUndoCommand)
  private
    FIndex: Integer;
  public
    constructor Create(AGraph: TGraph; AIndex: Integer);
    procedure Undo; override;
    procedure Redo; override;
  end;

  {TReversibleGraphCmdKind = (rgkSwapXY, rgkFlipX, rgkFlipY);

  TReversibleGraphCommand = class(TOriUndoCommand)
  private
    const CTitles: array [TReversibleGraphCmdKind] of String =
      ('Undo_SwapXY', 'Undo_FlipX', 'Undo_FlipY');
  private
    FKind: TReversibleGraphCmdKind;
    FGraphs: TGraphArray;
  protected
    procedure Swap; override;
  public
    constructor Create(var AGraphs: TGraphArray; AKind: TReversibleGraphCmdKind); overload;
  end;

  TFullDataGraphCommand = class(TOriUndoCommand)
  private
    FSource: TGraph;
    FValuesX, FValuesY: TValueArray;
  protected
    procedure Swap; override;
  public
    constructor Create(AGraph: TGraph; const ATitle: String);
  end;

  TEditFormulaCommand = class(TOriUndoCommand)
  private
    FSource: TGraph;
    FRange: TRangeParams;
    FFormula: String;
  protected
    procedure Swap; override;
  public
    constructor Create(AGraph: TGraph);
  end; }

implementation

uses
  SpectrumStrings;

{%region TSpectrumUndoCommand}
destructor TSpectrumUndoCommand.Destroy;
begin
  if FOwnSource then FSource.Free;
  inherited;
end;
{%endregion}

(*
{$region 'TTitleEditCommand'}
constructor TTitleEditCommand.Create(AGraph: TGraph);
begin
  FSource := AGraph;
  FBackup := AGraph.Title;
  FAutoTitle := AGraph.AutoTitle;
  FTitle := Constant('Undo_TitleGraph');
end;

constructor TTitleEditCommand.Create(APlot: TPlot);
begin
  FSource := APlot;
  FBackup := APlot.Title;
  FTitle := Constant('Undo_TitlePlot');
end;

procedure TTitleEditCommand.Swap;
var
  Tmp: TOriString;
  TmpF: Boolean;
begin
  if FSource is TGraph then
  begin
    Tmp := TGraph(FSource).Title;
    TmpF := TGraph(FSource).AutoTitle;
    TGraph(FSource).Title := FBackup;
    TGraph(FSource).AutoTitle := FAutoTitle;
    FBackup := Tmp;
    FAutoTitle := TmpF;
  end
  else if FSource is TPlot then
  begin
    Tmp := TPlot(FSource).Title;
    TPlot(FSource).Title := FBackup;
    FBackup := Tmp;
  end;
end;
{$endregion 'TTitleEditCommand'}   *)

{%region TGraphAppendCommand}
constructor TGraphAppendCommand.Create(AGraph: TGraph);
begin
  FSource := AGraph;
  FOwnSource := False;
  FTitle := SpectrumStrings.Undo_AddGraph;
end;

procedure TGraphAppendCommand.Undo;
begin
  TGraph(FSource).Owner.DeleteGraph(TGraph(FSource), False);
  FOwnSource := True;
end;

procedure TGraphAppendCommand.Redo;
begin
  TGraph(FSource).Owner.AddGraph(TGraph(FSource), -1, False);
  FOwnSource := False;
end;
{%endregion}

{%region TGraphDeleteCommand}
constructor TGraphDeleteCommand.Create(AGraph: TGraph; AIndex: Integer);
begin
  FSource := AGraph;
  FIndex := AIndex;
  FOwnSource := True;
  FTitle := SpectrumStrings.Undo_DeleteGraph;
end;

procedure TGraphDeleteCommand.Undo;
begin
  TGraph(FSource).Owner.AddGraph(TGraph(FSource), FIndex, False);
  FOwnSource := False;
end;

procedure TGraphDeleteCommand.Redo;
begin
  TGraph(FSource).Owner.DeleteGraph(TGraph(FSource), False);
  FOwnSource := True;
end;
{%endregion}
(*
{$region 'TReversibleGraphCommand'}
constructor TReversibleGraphCommand.Create(var AGraphs: TGraphArray; AKind: TReversibleGraphCmdKind);
begin
  FKind := AKind;
  FGraphs := AGraphs;
  FTitle := LangManager.ConstantValue[CTitles[FKind]];
end;

procedure TReversibleGraphCommand.Swap;
var I: Integer;
begin
  case FKind of
    rgkSwapXY: for I := 0 to Length(FGraphs)-1 do FGraphs[i].SwapXY;
    rgkFlipX: for I := 0 to Length(FGraphs)-1 do FGraphs[i].FlipX;
    rgkFlipY: for I := 0 to Length(FGraphs)-1 do FGraphs[i].FlipY;
  end;
end;
{$endregion 'TReversibleGraphCommand'}

{$region 'TFullDataGraphCommand'}
constructor TFullDataGraphCommand.Create(AGraph: TGraph; const ATitle: String);
begin
  FSource := AGraph;
  if ATitle <> '' then FTitle := Constant(ATitle);
  FValuesX := Copy(AGraph.ValuesX, 0, AGraph.ValueCount);
  FValuesY := Copy(AGraph.ValuesY, 0, AGraph.ValueCount);
end;

procedure TFullDataGraphCommand.Swap;
var
  TmpX, TmpY: TValueArray;
begin
  TmpX := Copy(FSource.ValuesX, 0, FSource.ValueCount);
  TmpY := Copy(FSource.ValuesY, 0, FSource.ValueCount);
  FSource.SetValuesXY(FValuesX, FValuesY);
  FValuesX := TmpX;
  FValuesY := TmpY;
end;
{$endregion}

{$region 'TEditFormulaCommand'}
constructor TEditFormulaCommand.Create(AGraph: TGraph);
begin
  FSource := AGraph;
  FRange := TFormulaParams(AGraph.Params).Range;
  FFormula := TFormulaParams(AGraph.Params).Formula;
  FTitle := Constant('Undo_FormulaEdit');
end;

procedure TEditFormulaCommand.Swap;
var
  TmpRange: TRangeParams;
  TmpFormula: String;
begin
  with TFormulaParams(FSource.Params) do
  begin
    TmpRange := Range;
    TmpFormula := Formula;

    Range := FRange;
    Formula := FFormula;
    FSource.UpdateValues;
  end;
  FRange := TmpRange;
  FFormula := TmpFormula;
end;
{$endregion}  *)

initialization
  HistoryInit;

end.