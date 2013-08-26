unit Diagram;

{$mode objfpc}{$H+}

interface

uses
  Controls, FGL, TAGraph,
  Plots;

type
  TDiagram = class
  private
    FPlot: TPlot;
    FChart: TChart;
  public
    constructor Create(APlot: TPlot; AParent: TCustomControl);
    destructor Destroy; override;
    property Plot: TPlot read FPlot;
    property Chart: TChart read FChart;
  end;

  TDiagramList = specialize TFPGList<TDiagram>;

implementation

constructor TDiagram.Create(APlot: TPlot; AParent: TCustomControl);
begin
  FPlot := APlot;
  FChart := TChart.Create(AParent);
  FChart.Align := alClient;
  FChart.Parent := AParent;
end;

destructor TDiagram.Destroy;
begin
  FPlot.Free;
  FChart.Free;
  inherited;
end;

end.

