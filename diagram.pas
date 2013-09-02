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
  //
  TDiagram = class(TInterfacedObject, IPlotNotification)
  private
    FPlot: TPlot;
    FChart: TChart;
    FLines: TGraphLines;
    procedure Notify(AOperation: TPlotOperation; APlot: TPlot; AGraph: TGraph);
    procedure ProcessGraphAdded(AGraph: TGraph);
    procedure ProcessGraphDeleted(AGraph: TGraph);
    procedure ProcessGraphDestroying(AGraph: TGraph);
    procedure CreateLine(AGraph: TGraph);
    procedure UpdateLine(AGraph: TGraph);
    function GetLine(AGraph: TGraph): TLineSeries;
  public
    constructor Create(APlot: TPlot; AParent: TCustomControl);
    destructor Destroy; override;
    property Plot: TPlot read FPlot;
    property Chart: TChart read FChart;
    procedure Rename;
  end;

implementation

uses
  Dialogs,
  OriUtils_TChart,
  SpectrumStrings;

constructor TDiagram.Create(APlot: TPlot; AParent: TCustomControl);
begin
  _AddRef; // Else, TInterfacedObject will destroyed by releasing of last interface

  FPlot := APlot;
  FChart := TChart.Create(AParent);
  FChart.Align := alClient;
  FChart.Parent := AParent;
  FLines := TGraphLines.Create;

  if Assigned(FPlot.Owner) then
    FPlot.Owner.RegisterNotifyClient(Self);
end;

destructor TDiagram.Destroy;
begin
  if Assigned(FPlot.Owner) then
    FPlot.Owner.UnRegisterNotifyClient(Self);

  FLines.Free;
  FPlot.Free;
  FChart.Free;
  inherited;
end;

{%region Plot Notifications}
procedure TDiagram.Notify(AOperation: TPlotOperation; APlot: TPlot; AGraph: TGraph);
begin
  case AOperation of
    poGraphAdded: if APlot = FPlot then ProcessGraphAdded(AGraph);
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
{%endregion}

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
    FLines.Add(AGraph, Line);
    FChart.AddSeries(Line);
  end
  else if not FChart.HasSeries(Line) then
    FChart.AddSeries(Line);
end;

procedure TDiagram.UpdateLine(AGraph: TGraph);
var
  I: Integer;
  Series: TLineSeries;
begin
  I := FLines.IndexOf(AGraph);
  if I < 0 then exit;
  Series := FLines.Data[I];
  Series.BeginUpdate;
  try
    Series.Clear;
    for I := 0 to AGraph.ValueCount-1 do
      Series.AddXY(AGraph.ValuesX[I], AGraph.ValuesY[i]);
  finally
    Series.EndUpdate;
  end;
end;

procedure TDiagram.Rename;
var
  Title: String;
begin
  Title := InputBox(Dlg_DiagramTitleCaption, Dlg_DiagramTitlePrompt, FPlot.Title);
  if Title <> FPlot.Title then FPlot.Title := Title;
end;

end.

