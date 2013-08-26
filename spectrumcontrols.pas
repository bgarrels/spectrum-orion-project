unit SpectrumControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, Buttons, ExtCtrls, FGL;

type
  TNotebookTab = class;
  TNotebookTabList = specialize TFPGList<TNotebookTab>;

  TNotebookTabPosition = (ntpTop, ntpBottom);

  TNotebookTabs = class(TCustomPanel)
  private
    FTabs: TNotebookTabList;
    FNotebook: TNotebook;
    FTabsPosition: TNotebookTabPosition;
    procedure InvalidateTabs;
    procedure SetTabsPosition(Value: TNotebookTabPosition);
  protected
    procedure Paint; override;
  public
    constructor Create(ANotebook: TNotebook); reintroduce;
    destructor Destroy; override;
    property TabsPosition: TNotebookTabPosition read FTabsPosition write SetTabsPosition;
    function AddTab(const Title: String; Index: Integer): TNotebookTab; overload;
    procedure AddTab(const Title: String; Index: Integer; Feature: TObject); overload;
    procedure ShowTab(Index: Integer);
  end;

  TNotebookTab = class(TCustomSpeedButton)
  private
    FOwner: TNotebookTabs;
    FChecked: Boolean;
    FFeature: TObject;
    FTabIndex: Integer;
    procedure SetChecked(Value: Boolean);
    procedure UpdateBorderSpacing;
  protected
    procedure PaintBackground(var PaintRect: TRect); override;
  public
    constructor Create(AOwner: TNotebookTabs); reintroduce;
    procedure Click; override;
    property Checked: Boolean read FChecked write SetChecked;
    property Feature: TObject read FFeature;
    property TabIndex: Integer read FTabIndex;
  end;

implementation

uses
  Graphics, Controls,
  OriStrings;

const
  SourceDPI = 96;

{%region TNotebookTabs}
constructor TNotebookTabs.Create(ANotebook: TNotebook);
var
  I: Integer;
  S: String;
begin
  inherited Create(ANotebook.Owner);

  FNotebook := ANotebook;
  FTabs := TNotebookTabList.Create;

  Color := cl3DHiLight;
  BevelOuter := bvNone;
  BorderSpacing.InnerBorder := ScaleX(3, SourceDPI);

  for I := 0 to FNotebook.PageCount-1 do
  begin
    S := FNotebook.Page[I].Name;
    if StartsWith(S, 'Page') then
      S := Copy(S, 5, MaxInt);
    AddTab(S, I);
  end;
  if FTabs.Count > 0 then ShowTab(0);

  AutoSize := True;
end;

destructor TNotebookTabs.Destroy;
begin
  FTabs.Free;
  inherited;
end;

procedure TNotebookTabs.SetTabsPosition(Value: TNotebookTabPosition);
var
  Tab: TNotebookTab;
begin
  if Value <> FTabsPosition then
  begin
    FTabsPosition := Value;
    for Tab in FTabs do
      Tab.UpdateBorderSpacing;
    Invalidate;
    InvalidateTabs;
  end;
end;

procedure TNotebookTabs.InvalidateTabs;
var
  Tab: TNotebookTab;
begin
  for Tab in FTabs do Tab.Invalidate;
end;

function TNotebookTabs.AddTab(const Title: String; Index: Integer): TNotebookTab;
var
  X: Integer = 0;
  Tab: TNotebookTab;
begin
  for Tab in FTabs do
    if Tab.Left > Left then
      X := Tab.Left + Tab.Width;

  Result := TNotebookTab.Create(Self);
  Result.Caption := '   ' + Title + '   ';
  Result.Left := X + 1;
  Result.Parent := Self;
  Result.FTabIndex := Index;
  FTabs.Add(Result);
end;

procedure TNotebookTabs.AddTab(const Title: String; Index: Integer; Feature: TObject);
var
  Tab: TNotebookTab;
begin
  Tab := AddTab(Title, Index);
  Tab.FFeature := Feature;
  ShowTab(Index);
end;

procedure TNotebookTabs.ShowTab(Index: Integer);
var
  I: Integer;
begin
  for I := 0 to FTabs.Count-1 do
    if FTabs[I].TabIndex = Index then
    begin
      FTabs[I].Checked := True;
      FNotebook.PageIndex := I;
    end
    else
      FTabs[I].Checked := False;
  Invalidate;
end;

procedure TNotebookTabs.Paint;
var
  Y: Integer;
begin
  inherited;

  if TabsPosition = ntpBottom then Y := 0 else Y := Height-1;
  Canvas.Pen.Color := cl3DShadow;
  Canvas.Line(0, Y, Width, Y);
end;
{%endregion}

{%region TNotebookTab}
constructor TNotebookTab.Create(AOwner: TNotebookTabs);
begin
  inherited Create(AOwner);

  FOwner := AOwner;

  AutoSize := True;
  Align := alLeft;

  UpdateBorderSpacing;
end;

procedure TNotebookTab.UpdateBorderSpacing;
var
  MarginY: Integer;
begin
  MarginY := ScaleY(3, SourceDPI);
  if FOwner.TabsPosition = ntpBottom then
  begin
    BorderSpacing.Top := 0;
    BorderSpacing.Bottom := MarginY
  end
  else
  begin
    BorderSpacing.Top := MarginY;
    BorderSpacing.Bottom := 0;
  end;
  BorderSpacing.Left := ScaleX(3, SourceDPI);
end;

procedure TNotebookTab.Click;
begin
  FOwner.ShowTab(TabIndex);
end;

procedure TNotebookTab.PaintBackground(var PaintRect: TRect);
begin
  if FChecked
    then Canvas.Brush.Color := cl3DFace
    else Canvas.Brush.Color := cl3DHiLight;
  Canvas.FillRect(PaintRect);
  Canvas.Pen.Color := cl3DShadow;
  if FChecked then
  begin
    if FOwner.TabsPosition = ntpBottom then
    begin
      Canvas.MoveTo(0, 0);
      Canvas.LineTo(0, PaintRect.Bottom-1);
      Canvas.LineTo(PaintRect.Right-1, PaintRect.Bottom-1);
      Canvas.LineTo(PaintRect.Right-1, -1);
    end
    else
    begin
      Canvas.MoveTo(0, PaintRect.Bottom);
      Canvas.LineTo(0, 0);
      Canvas.LineTo(PaintRect.Right-1, 0);
      Canvas.LineTo(PaintRect.Right-1, PaintRect.Bottom);
    end;
  end
  else
  begin
    if FOwner.TabsPosition = ntpBottom then
      Canvas.Line(0, 0, PaintRect.Right, 0)
    else
      Canvas.Line(0, PaintRect.Bottom-1, PaintRect.Right, PaintRect.Bottom-1);
  end;
end;

procedure TNotebookTab.SetChecked(Value: Boolean);
begin
  if FChecked <> Value then
  begin
    FChecked := Value;
    Invalidate;
  end;
end;
{%endregion}

end.

