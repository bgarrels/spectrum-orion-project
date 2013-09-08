unit SpectrumControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, ComCtrls, ExtCtrls, Buttons, FGL;

type
  TNotebookTab = class;
  TNotebookTabList = specialize TFPGList<TNotebookTab>;

  TNotebookTabPosition = (ntpTop, ntpBottom);

  TNotebookTabs = class(TCustomPanel)
  private
    FTabs: TNotebookTabList;
    FNotebook: TNotebook;
    FTabsPosition: TNotebookTabPosition;
    FOnTabActivated: TNotifyEvent;
    function GetActiveTab: TNotebookTab;
    procedure InvalidateTabs;
    procedure SetTabsPosition(Value: TNotebookTabPosition);
  protected
    procedure Paint; override;
  public
    constructor Create(ANotebook: TNotebook); reintroduce;
    destructor Destroy; override;
    property TabsPosition: TNotebookTabPosition read FTabsPosition write SetTabsPosition;
    property ActiveTab: TNotebookTab read GetActiveTab;
    function AddTab(const Title: String; Index: Integer): TNotebookTab; overload;
    procedure AddTab(const Title: String; Index: Integer; Feature: TObject); overload;
    procedure RenameTab(const Title: String; Feature: TObject);
    procedure ShowTab(Index: Integer);
    property OnTabActivated: TNotifyEvent read FOnTabActivated write FOnTabActivated;
  end;

  TNotebookTab = class(TCustomSpeedButton)
  private
    FOwner: TNotebookTabs;
    FChecked: Boolean;
    FFeature: TObject;
    FTabIndex: Integer;
    procedure SetChecked(Value: Boolean);
    procedure UpdateBorderSpacing;
    procedure SetTitle(const Value: String);
    function GetTitle: String;
  protected
    procedure PaintBackground(var PaintRect: TRect); override;
  public
    constructor Create(AOwner: TNotebookTabs); reintroduce;
    procedure Click; override;
    property Checked: Boolean read FChecked write SetChecked;
    property Feature: TObject read FFeature;
    property TabIndex: Integer read FTabIndex;
    property Title: String read GetTitle write SetTitle;
  end;

  TSpectrumStatusBar = class(TStatusBar)
  private
    FPanelGraphSource: TStatusPanel;
    FPanelGraphsCount: TStatusPanel;
    FPanelPointsCount: TStatusPanel;
    FPanelModified: TStatusPanel;
    FPanelFactorX: TStatusPanel;
    FPanelFactorY: TStatusPanel;
    procedure SetPanel(APanel: TStatusPanel; const AText: String);
  public
    constructor Create(AOwner: TWinControl); reintroduce;
    procedure ShowModified(Value: Boolean);
    procedure ShowGraphCount(TotalCount, VisibleCount: Integer);
  end;

implementation

uses
  SysUtils, Graphics,
  OriStrings,
  SpectrumStrings;

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
  Result.Title := Title;
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
      if Assigned(FOnTabActivated) then FOnTabActivated(Self);
    end
    else
      FTabs[I].Checked := False;
  Invalidate;
end;

procedure TNotebookTabs.RenameTab(const Title: String; Feature: TObject);
var
  Tab: TNotebookTab;
begin
  for Tab in FTabs do
    if Tab.Feature = Feature then
    begin
      Tab.Title := Title;
      exit;
    end;
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

function TNotebookTabs.GetActiveTab: TNotebookTab;
begin
  for Result in FTabs do
    if Result.Checked then
      Exit;
  Result := nil;
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
  MarginY := ScaleY(6, SourceDPI);
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
  BorderSpacing.Left := ScaleX(6, SourceDPI);
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

procedure TNotebookTab.SetTitle(const Value: String);
begin
  inherited Caption := '   ' + Value + '   ';
  InvalidatePreferredSize;
  AdjustSize;
end;

function TNotebookTab.GetTitle: String;
begin
  Result := Trim(inherited Caption);
end;
{%endregion}

{%region TSpectrumStatusBar}
constructor TSpectrumStatusBar.Create(AOwner: TWinControl);
begin
  inherited Create(AOwner);

  Parent := AOwner;
  SimplePanel := False;

  FPanelGraphsCount := Panels.Add;
  FPanelModified := Panels.Add;
  FPanelPointsCount := Panels.Add;
  FPanelFactorX := Panels.Add;
  FPanelFactorY := Panels.Add;
  FPanelGraphSource := Panels.Add;

  FPanelGraphsCount.Alignment := taCenter;
  FPanelPointsCount.Alignment := taCenter;
  FPanelModified.Alignment := taCenter;
  FPanelFactorX.Alignment := taCenter;
  FPanelFactorY.Alignment := taCenter;
end;

procedure TSpectrumStatusBar.ShowModified(Value: Boolean);
begin
  if Value
    then SetPanel(FPanelModified, Status_Modified)
    else SetPanel(FPanelModified, '');
end;

procedure TSpectrumStatusBar.ShowGraphCount(TotalCount, VisibleCount: Integer);
begin
  if TotalCount <> VisibleCount
    then SetPanel(FPanelGraphsCount, Format('%s: %d (%d)', [Status_Graphs, VisibleCount, TotalCount]))
    else SetPanel(FPanelGraphsCount, Format('%s: %d', [Status_Graphs, TotalCount]));
end;

procedure TSpectrumStatusBar.SetPanel(APanel: TStatusPanel; const AText: String);
begin
  APanel.Text := AText;
  APanel.Width := Canvas.TextWidth('   ' + AText + '   ');
  Invalidate;
end;
{%endregion}

end.

