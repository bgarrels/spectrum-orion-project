unit OriUtils_TChart;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, TAGraph, TASeries;

type
  TChartHelper = class helper for TChart
  public
    function HasSeries(ASeries: TBasicChartSeries): Boolean;
  end;

procedure ChooseSeriesColor(Series: TLineSeries);
function GetLineSeriesColor(Chart: TChart): TColor;
function GetRandomLineSeriesColor(Chart: TChart): TColor;

implementation

uses
  OriGraphics;

{%region TChartHelper}
function TChartHelper.HasSeries(ASeries: TBasicChartSeries): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Self.Series.Count-1 do
    if Self.Series[I] = ASeries then
    begin
      Result := True;
      exit;
    end;
end;
{%endregion}

{%region Series Colors}
const
  SeriesColors: array[0..50] of TColor = (clRed, clGreen, clBlue, clBlack,
    clMaroon, clNavy, clOlive, clPurple, clTeal, clGray, clLime, clFuchsia,
    clAqua, clMediumVioletRed, clDarkRed, clBrown, clDarkGreen, clDarkCyan,
    clMidnightBlue, clDarkSlateGray, clDarkSlateBlue, clDarkOrange, clFireBrick,
    clDarkKhaki, clSienna, clPaleVioletRed, clSeaGreen, clCadetBlue, clRoyalBlue,
    clSlateBlue, clSlateGray, clDeepPink, clCrimson, clSaddleBrown, clDarkOliveGreen,
    clLightSeaGreen, clSteelBlue, clOrangeRed, clIndigo, clDimGray, clIndianRed,
    clDarkGoldenrod, clDarkSeaGreen, clTurquoise, clDodgerBlue, clDarkViolet,
    clDarkSalmon, clRosyBrown, clMediumSpringGreen, clMediumAquamarine, clViolet);

procedure ChooseSeriesColor(Series: TLineSeries);
begin
  if Assigned(Series.Owner) and (Series.Owner is TChart)
    then Series.LinePen.Color := GetLineSeriesColor(TChart(Series.Owner))
    else Series.LinePen.Color := SeriesColors[Random(Length(SeriesColors))];
end;

function LineSeriesColorExists(Chart: TChart; Color: TColor): Boolean;
var
  I: Integer;
begin
  for I := 0 to Chart.Series.Count-1 do
    if Chart.Series[I] is TLineSeries then
      if TLineSeries(Chart.Series[I]).SeriesColor = Color then
      begin
        Result := True;
        exit;
      end;
  Result := False;
end;

function GetLineSeriesColor(Chart: TChart): TColor;
var
  I: Integer;
begin
  for I := Low(SeriesColors) to High(SeriesColors) do
    if not LineSeriesColorExists(Chart, SeriesColors[I]) then
    begin
      Result := SeriesColors[I];
      exit;
    end;
  Result := SeriesColors[Random(Length(SeriesColors))];
end;

function GetRandomLineSeriesColor(Chart: TChart): TColor;
var
  I, Start: Integer;
begin
  Start := Random(Length(SeriesColors));
  for I := Start to High(SeriesColors) do
    if not LineSeriesColorExists(Chart, SeriesColors[I]) then
    begin
      Result := SeriesColors[I];
      exit;
    end;
  for I := Low(SeriesColors) to Start-1 do
    if not LineSeriesColorExists(Chart, SeriesColors[I]) then
    begin
      Result := SeriesColors[I];
      exit;
    end;
  Result := SeriesColors[Start];
end;
{%endregion}

end.
