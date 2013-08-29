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
    procedure CreateLine(AGraph: TGraph);
    procedure UpdateLine(AGraph: TGraph);
  public
    constructor Create(APlot: TPlot; AParent: TCustomControl);
    destructor Destroy; override;
    property Plot: TPlot read FPlot;
    property Chart: TChart read FChart;
  end;

implementation

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

procedure TDiagram.Notify(AOperation: TPlotOperation; APlot: TPlot; AGraph: TGraph);
begin
  case AOperation of
    poGraphAdded: if APlot = FPlot then ProcessGraphAdded(AGraph);
  end;
end;

procedure TDiagram.ProcessGraphAdded(AGraph: TGraph);
begin
  CreateLine(AGraph);
  UpdateLine(AGraph);
end;

procedure TDiagram.CreateLine(AGraph: TGraph);
var
  Series: TLineSeries;
begin
  Series := TLineSeries.Create(FChart);
  Series.Tag := Integer(AGraph);
  Series.Title := AGraph.Title;
  FChart.AddSeries(Series);
  FLines.Add(AGraph, Series);
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

end.

