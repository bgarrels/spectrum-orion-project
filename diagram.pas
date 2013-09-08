unit Diagram;

{$mode objfpc}{$H+}

interface

uses
  Controls, FGL, TAGraph, TASeries,
  Plots;

type
  TDiagram = class;
  TDiagramList = specialize TFPGList<TDiagram>;
  TGraphLines = specialize TFPGMap<Pointer, TLineSeries>;

  // An intermediate between TPlot and TChart.
  // It is Controller in MVC-terms, while TPlot is Model and TChart is View.
  TDiagram = class(TInterfacedObject, IPlotNotification)
  private
    FPlot: TPlot;
    FChart: TChart;
    FLines: TGraphLines;

    procedure Create(APlot: TPlot; AParent: TCustomControl);
    procedure CreateChart(AParent: TCustomControl);

    procedure Notify(AOperation: TPlotOperation; APlot: TPlot; AGraph: TGraph);
    procedure ProcessGraphAdded(AGraph: TGraph);
    procedure ProcessGraphDeleted(AGraph: TGraph);
    procedure ProcessGraphDestroying(AGraph: TGraph);
    procedure ProcessGraphChanged(AGraph: TGraph);

    procedure CreateLine(AGraph: TGraph);
    procedure UpdateLine(AGraph: TGraph);

    function GetLine(AGraph: TGraph): TLineSeries;
    function GetCountTotal: Integer;
    function GetCountVisible: Integer;

  public
    class function Create(APlot: TPlot; AParent: TCustomControl): TDiagram;
    procedure Free;

    procedure Rename; overload;
    procedure Rename(AGraph: TGraph); overload;
    property Plot: TPlot read FPlot;
    property Chart: TChart read FChart;
    property CountTotal: Integer read GetCountTotal;
    property CountVisible: Integer read GetCountVisible;
  end;

implementation

uses
  Dialogs, Graphics,
  OriUtils_TChart,
  SpectrumStrings;

class function TDiagram.Create(APlot: TPlot; AParent: TCustomControl): TDiagram;
begin
  Result := TDiagram(NewInstance);
  Result.Create(APlot, AParent);
end;

procedure TDiagram.Create(APlot: TPlot; AParent: TCustomControl);
begin
  FPlot := APlot;

  CreateChart(AParent);

  FLines := TGraphLines.Create;

  if Assigned(FPlot.Owner) then
    FPlot.Owner.RegisterNotifyClient(Self);
end;

procedure TDiagram.Free;
begin
  if Assigned(FPlot.Owner) then
    FPlot.Owner.UnRegisterNotifyClient(Self);

  FLines.Free;
  FPlot.Free;
  FChart.Free;
  FreeInstance;
end;

procedure TDiagram.CreateChart(AParent: TCustomControl);
var
  M: Integer;
begin
  FChart := TChart.Create(AParent);
  FChart.Align := alClient;
  FChart.Parent := AParent;
  FChart.BackColor := clWindow;

  //FChart.Margins.Left := 0;
  //FChart.Margins.Right := 0;
  //FChart.Margins.Top := 0;
  //FChart.Margins.Bottom := 0;
  FChart.LeftAxis.Grid.Style := psSolid;
  FChart.LeftAxis.Grid.Color := clSilver;
  FChart.BottomAxis.Grid.Style := psSolid;
  FChart.BottomAxis.Grid.Color := clSilver;

  M := FChart.Canvas.TextHeight('I');
  FChart.MarginsExternal.Left := M;
  FChart.MarginsExternal.Right := M;
  FChart.MarginsExternal.Top := M;
  FChart.MarginsExternal.Bottom := M;
end;

{%region Plot Notifications}
procedure TDiagram.Notify(AOperation: TPlotOperation; APlot: TPlot; AGraph: TGraph);
begin
  case AOperation of
    poGraphAdded: if APlot = FPlot then ProcessGraphAdded(AGraph);
    poGraphChanged: if APlot = FPlot then ProcessGraphChanged(AGraph);
    poGraphDeleted: if APlot = FPlot then ProcessGraphDeleted(AGraph);
    poGraphDestroying: if APlot = FPlot then ProcessGraphDestroying(AGraph);
  end;
end;

procedure TDiagram.ProcessGraphAdded(AGraph: TGraph);
begin
  CreateLine(AGraph);
  UpdateLine(AGraph);
end;

procedure TDiagram.ProcessGraphDeleted(AGraph: TGraph);
var
  Line: TLineSeries;
begin
  Line := GetLine(AGraph);
  if Assigned(Line) then
    FChart.DeleteSeries(Line);
  // Do not destroy line, only remove it from chart
  // Graph deletion can be undone, so line is still needed
end;

procedure TDiagram.ProcessGraphDestroying(AGraph: TGraph);
var
  Line: TLineSeries;
begin
  Line := GetLine(AGraph);
  if Assigned(Line) then
    FChart.DeleteSeries(Line);
  FLines.Remove(AGraph);
  Line.Free;
end;

procedure TDiagram.ProcessGraphChanged(AGraph: TGraph);
var
  Line: TLineSeries;
begin
  Line := GetLine(AGraph);
  if Assigned(Line) then
    Line.Title := AGraph.Title;
end;
{%endregion}

{%region Working With Line}
function TDiagram.GetLine(AGraph: TGraph): TLineSeries;
var
  Index: Integer;
begin
  Index := FLines.IndexOf(AGraph);
  if Index >= 0
    then Result := FLines.Data[Index]
    else Result := nil;
end;

procedure TDiagram.CreateLine(AGraph: TGraph);
var
  Line: TLineSeries;
begin
  Line := GetLine(AGraph);
  if not Assigned(Line) then
  begin
    Line := TLineSeries.Create(FChart);
    Line.Tag := Integer(AGraph);
    Line.Title := AGraph.Title;
    Line.SeriesColor := GetLineSeriesColor(FChart);
    FLines.Add(AGraph, Line);
    FChart.AddSeries(Line);
  end
  else if not FChart.HasSeries(Line) then
    FChart.AddSeries(Line);
end;

procedure TDiagram.UpdateLine(AGraph: TGraph);
var
  I: Integer;
  Line: TLineSeries;
begin
  Line := GetLine(AGraph);
  if Assigned(Line) then
  begin
    Line.BeginUpdate;
    try
      Line.Clear;
      for I := 0 to AGraph.ValueCount-1 do
        Line.AddXY(AGraph.ValuesX[I], AGraph.ValuesY[I]);
    finally
      Line.EndUpdate;
    end;
  end;
end;
{%endregion}

{%region Commands}
procedure TDiagram.Rename;
var
  Title: String;
begin
  Title := InputBox(Dlg_DiagramTitleCaption, Dlg_DiagramTitlePrompt, FPlot.Title);
  if Title <> FPlot.Title then FPlot.SetTitle(Title);
end;

procedure TDiagram.Rename(AGraph: TGraph);
var
  Title: String;
begin
  Title := InputBox(Dlg_GraphTitleCaption, Dlg_GraphTitlePrompt, AGraph.Title);
  if Title <> AGraph.Title then AGraph.SetTitle(Title);
end;
{%endregion}

{%region Properties}
function TDiagram.GetCountTotal: Integer;
begin
  Result := FPlot.Count;
end;

function TDiagram.GetCountVisible: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FPlot.Count-1 do
    if FLines[FPlot[I]].Active then Inc(Result);
end;
{%endregion}

end.

