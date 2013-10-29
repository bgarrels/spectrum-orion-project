unit PlotControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, Grids,
  Plots;

type
  TGraphGrid = class(TCustomGrid)
  private
    FGraph: TGraph;
    FAlternateRows: Boolean;
    procedure SetGraph(Value: TGraph);
    procedure AdjustColSizes;
  protected
    procedure Resize; override;
    procedure DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState); override;
  public
    constructor Create(AOwner: TWinControl);
    property Graph: TGraph read FGraph write SetGraph;
  end;

implementation

uses
  StdCtrls, Graphics, Math,
  OriGraphics,
  SpectrumTypes;

const
  SourceDPI = 96;

{%region TGraphGrid}
constructor TGraphGrid.Create(AOwner: TWinControl);
begin
  inherited Create(AOwner);

  Align := alClient;
  Parent := AOwner;
  BorderSpacing.Around := ScaleX(3, SourceDPI);
  ColCount := 3;
  RowCount := 2;
  ScrollBars := ssAutoVertical;
  Options := Options - [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,
    goEditing, goRowSizing, goColSizing] + [goRangeSelect, goTabs, goThumbTracking];

  FAlternateRows := True;
end;

procedure TGraphGrid.SetGraph(Value: TGraph);
begin
  if FGraph <> Value then
  begin
    FGraph := Value;
    if Assigned(FGraph)
      then RowCount := Max(FGraph.ValueCount+1, 2)
      else RowCount := 2;
    AdjustColSizes;
    Invalidate;
  end;
end;

procedure TGraphGrid.AdjustColSizes;
var
  S: String;
begin
  if Assigned(FGraph)
    then S := IntToStr(FGraph.ValueCount)
    else S := '9999';
  Canvas.Font := Self.Font;
  ColWidths[0] := Canvas.TextWidth(S) + 6;
  ColWidths[1] := (ClientWidth - ColWidths[0]) div 2;
  ColWidths[2] := ClientWidth - ColWidths[0] - ColWidths[1];
end;

procedure TGraphGrid.Resize;
begin
  if Assigned(Parent) then AdjustColSizes;
end;

procedure TGraphGrid.DrawCell(ACol, ARow: Integer; ARect: TRect; AState: TGridDrawState);
var
  TxtRect: TRect;
  Str: String;
  TS: TTextStyle;
  Values: TValueArray;
begin
  TxtRect := ARect;
  Inc(TxtRect.Left, 2);
  Dec(TxtRect.Right, 2);
  TS := Canvas.TextStyle;
  TS.Layout := tlCenter;
  TS.Alignment := taLeftJustify;
  TS.Opaque := False;
  TS.Clipping := True;
  TS.SystemFont := Canvas.Font.IsDefault;
  with Canvas do
  begin
    Brush.Style := bsSolid;
    if gdFixed in AState then
    begin
      Brush.Color := clBtnFace;
      FillRect(ARect);
      Pen.Color := clBtnShadow;
      MoveTo(ARect.Left, ARect.Bottom-1);
      LineTo(ARect.Right-1, ARect.Bottom-1);
      if ACol < 2 then
        LineTo(ARect.Right-1, ARect.Top-1);
      case ACol of
        1: begin TS.Alignment := taCenter; Str := 'X'; end;
        2: begin TS.Alignment := taCenter; Str := 'Y'; end;
        else if ARow > 0 then Str := IntToStr(ARow) else Str := '';
      end;
    end
    else
    begin
      if gdSelected	in AState then
      begin
        if FAlternateRows and (ARow mod 2 = 0)
          then Brush.Color := Blend(clHighlight, clWindow, 40)
          else Brush.Color := Blend(clHighlight, clWindow, 30);
        if gdFocused in AState
          then Pen.Color := clHighlight
          else Pen.Color := Brush.Color;
        Rectangle(ARect);
      end
      else begin
        if FAlternateRows and (ARow mod 2 = 0)
          then Brush.Color := Lighten(clWindow, -10)
          else Brush.Color := clWindow;
        FillRect(ARect);
      end;
      if Assigned(FGraph) then
      begin
        if ACol = 1
          then Values := FGraph.ValuesX
          else Values := FGraph.ValuesY;
        Str := FloatToStr(Values[ARow-1]);
      end
      else Str := '';
    end;
    Font := Self.Font;
    TextRect(TxtRect, TxtRect.Left, TxtRect.Top, Str, TS);
  end;
end;
{%endregion}

end.

