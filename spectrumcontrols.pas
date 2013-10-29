unit SpectrumControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, ComCtrls, ExtCtrls, Buttons, FGL, Graphics, ImgList;

type
  TNotebookTab = class;
  TNotebookTabList = specialize TFPGList<TNotebookTab>;

  TNotebookTabPosition = (ntpTop, ntpBottom);

  TNotebookTabs = class(TCustomPanel)
  private
    FTabs: TNotebookTabList;
    FNotebook: TNotebook;
    FImages: TCustomImageList;
    FTabsPosition: TNotebookTabPosition;
    FOnTabActivated: TNotifyEvent;
    function GetActiveTab: TNotebookTab;
    function GetTab(Index: Integer): TNotebookTab;
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
    property Images: TCustomImageList read FImages write FImages;
    property Tabs[Index: Integer]: TNotebookTab read GetTab;
  end;

  TNotebookTab = class(TCustomSpeedButton)
  private
    FOwner: TNotebookTabs;
    FTitle: String;
    FChecked: Boolean;
    FFeature: TObject;
    FTabIndex: Integer;
    FImageIndex: Integer;
    FPadding: Integer;
    procedure SetChecked(Value: Boolean);
    procedure UpdateBorderSpacing;
    procedure SetTitle(const Value: String);
    procedure SetImageIndex(Index: Integer);
    procedure SetPadding(Value: Integer);
  protected
    procedure PaintBackground(var PaintRect: TRect); override;
    function DrawGlyph(ACanvas: TCanvas; const AClient: TRect; const AOffset: TPoint;
      AState: TButtonState; ATransparent: Boolean; BiDiFlags: Longint): TRect; override;
  public
    constructor Create(AOwner: TNotebookTabs); reintroduce;
    procedure Click; override;
    procedure GetPreferredSize(var PreferredWidth, PreferredHeight: integer;
      Raw: Boolean = False; WithThemeSpace: Boolean = True); override;
    property Checked: Boolean read FChecked write SetChecked;
    property Feature: TObject read FFeature;
    property TabIndex: Integer read FTabIndex;
    property Title: String read FTitle write SetTitle;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
    property Padding: Integer read FPadding write SetPadding;
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

  TToolPanelHeader = class(TCustomPanel)
  private const
    ImageMargin = 2;
    TextIndent = 2;
    ButtonMargin = 1;
  private
    FCloseButton: TSpeedButton;
    FImages: TCustomImageList;
    FImageIndex: Integer;
    FImageMargin: Integer;
    FButtonMargin: Integer;
    FTextIndent: Integer;
    procedure SetImages(Value: TCustomImageList);
    procedure SetImageIndex(Value: Integer);
  protected
    procedure Paint; override;
    procedure AdjustSize; override;
  public
    constructor Create(AOwner: TWinControl); reintroduce;
    destructor Destroy; override;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write SetImageIndex;
  end;

implementation

uses
  SysUtils, RtlConsts,
  OriStrings, OriGraphics,
  SpectrumStrings;

const
  SourceDPI = 96;

function CanPaintImage(AImages: TCustomImageList; AImageIndex: Integer): Boolean;
begin
  Result := Assigned(AImages) and (AImageIndex > -1) and (AImageIndex < AImages.Count)
end;

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

function TNotebookTabs.GetTab(Index: Integer): TNotebookTab;
begin
  if (Index < 0) or (Index >= FTabs.Count) then
    raise EListError.CreateFmt(SListIndexError, [Index]);
  Result := FTabs[Index];
end;
{%endregion}

{%region TNotebookTab}
constructor TNotebookTab.Create(AOwner: TNotebookTabs);
begin
  inherited Create(AOwner);

  FOwner := AOwner;
  FImageIndex := -1;
  FPadding := 12;

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

procedure TNotebookTab.GetPreferredSize(var PreferredWidth, PreferredHeight: integer;
  Raw: Boolean = False; WithThemeSpace: Boolean = True);
begin
  inherited GetPreferredSize(PreferredWidth, PreferredHeight, Raw, WithThemeSpace);
  Inc(PreferredWidth, FPadding + FPadding);
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

function TNotebookTab.DrawGlyph(ACanvas: TCanvas; const AClient: TRect; const AOffset: TPoint;
  AState: TButtonState; ATransparent: Boolean; BiDiFlags: Longint): TRect;
var
  Offset: TPoint;
begin
  if CanPaintImage(FOwner.Images, FImageIndex) then
  begin
    Offset := AOffset;
    if FTitle = '' then
    begin
      Offset.X := (AClient.Right - AClient.Left - FOwner.Images.Width) div 2;
    end;
    Result := inherited DrawGlyph(ACanvas, AClient, Offset, AState, ATransparent, BiDiFlags);
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
  FTitle := Value;
  Caption := Value;
  InvalidatePreferredSize;
  AdjustSize;
end;

procedure TNotebookTab.SetPadding(Value: Integer);
begin
  if FPadding <> Value then
  begin
    FPadding := Value;
    InvalidatePreferredSize;
    AdjustSize;
    Invalidate;
  end;
end;

procedure TNotebookTab.SetImageIndex(Index: Integer);
begin
  FImageIndex := Index;
  if CanPaintImage(FOwner.Images, Index) then
    FOwner.Images.GetBitmap(Index, Glyph);
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

{%region TToolPanelHeader}
constructor TToolPanelHeader.Create(AOwner: TWinControl);
begin
  inherited Create(AOwner);

  FImageMargin := ScaleX(ImageMargin, SourceDPI);
  FButtonMargin := ScaleX(ButtonMargin, SourceDPI);
  FTextIndent := ScaleX(TextIndent, SourceDPI);

  Align := alTop;
  Parent := AOwner;
  BorderSpacing.Left := ScaleX(3, SourceDPI);
  BorderSpacing.Right := BorderSpacing.Left;
  Alignment := taLeftJustify;
  BevelInner := bvNone;
  BevelOuter := bvNone;

  FCloseButton := TSpeedButton.Create(Self);
  FCloseButton.Parent := Self;
  FCloseButton.Align := alRight;
  FCloseButton.Flat := True;
  FCloseButton.Glyph.LoadFromLazarusResource('close');
  FCloseButton.BorderSpacing.Around := 1;

  AutoSize := True;
end;

destructor TToolPanelHeader.Destroy;
begin
  FCloseButton.Free;
  inherited;
end;

procedure TToolPanelHeader.Paint;
var
  R: TRect;
  TS: TTextStyle;
begin
  R := ClientRect;

  Canvas.Pen.Color := cl3DShadow;
  Canvas.Brush.Color := clBtnFace;
  Canvas.Rectangle(R);

  if Assigned(FCloseButton) then
    Dec(R.Right, FCloseButton.Width + FButtonMargin*2);

  if CanPaintImage(FImages, FImageIndex) then
  begin
    FImages.Draw(Canvas, FImageMargin, FImageMargin, FImageIndex);
    Inc(R.Left, FImages.Width + FImageMargin);
  end;

  if Caption <> '' then
  begin
    Inc(R.Left, FTextIndent);

    TS := Canvas.TextStyle;
    TS.Alignment := BidiFlipAlignment(Self.Alignment, UseRightToLeftAlignment);
    if BiDiMode <> bdLeftToRight then TS.RightToLeft := True;
    TS.Layout := tlCenter;
    TS.Opaque := False;
    TS.Clipping := False;
    TS.SystemFont := Canvas.Font.IsDefault;
    Canvas.Font.Color := Font.Color;
    Canvas.TextRect(R, R.Left, R.Top, Caption, TS);
  end;
end;

procedure TToolPanelHeader.SetImages(Value: TCustomImageList);
begin
  FImages := Value;
  Constraints.MinHeight := FImages.Height + FImageMargin*2;
  AdjustSize;
  Invalidate;
end;

procedure TToolPanelHeader.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Invalidate;
  end;
end;

procedure TToolPanelHeader.AdjustSize;
begin
  inherited;
  if Assigned(FCloseButton) then
    FCloseButton.Width := FCloseButton.Height;
end;

end.

