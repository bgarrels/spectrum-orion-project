unit OriUtils_TChart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, TAGraph, TASeries;

procedure ChooseSeriesColor(Series: TLineSeries);
function GetLineSeriesColor(Chart: TChart): TColor;

implementation

uses
  OriGraphics;

const
  SeriesColors: array[0..12] of TColor = (clRed, clGreen, clBlue, clMaroon, clNavy, clOlive,
    clPurple, clTeal, clGray, clLime, clFuchsia, clAqua, clBlack);

procedure ChooseSeriesColor(Series: TLineSeries);
begin
  if Assigned(Series.Owner) and (Series.Owner is TChart)
    then Series.LinePen.Color := GetLineSeriesColor(TChart(Series.Owner))
    else Series.LinePen.Color := SeriesColors[Random(Length(SeriesColors))];
end;

function GetLineSeriesColor(Chart: TChart): TColor;
var
  I, J: Integer;
  Exist: Boolean;
begin
  for I := Low(SeriesColors) to High(SeriesColors) do
  begin
    Exist := False;
    for J := 0 to Chart.Series.Count-1 do
      if Chart.Series[J] is TLineSeries then
        if TLineSeries(Chart.Series[J]).LinePen.Color = SeriesColors[I] then
        begin
          Exist := True;
          Break;
        end;
    if not Exist then
    begin
      Result := SeriesColors[I];
      Exit;
    end;
  end;
  Result := SeriesColors[Random(Length(SeriesColors))];
end;

end.

