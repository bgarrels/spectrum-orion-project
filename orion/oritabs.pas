unit OriTabs;

{$mode objfpc}{$H+}

interface

uses
  Classes, Controls, SysUtils, ImgList, Graphics, Messages, LMessages;

type
  TOriTabSet = class;

  TOriTab = class(TCollectionItem)
  private
    FCaption: String;
    FHint: String;
    FControl: TControl;
    FImageIndex: Integer;
    FTabWidth: Integer;
    FTag: Integer;
    procedure SetImageIndex(Value: Integer);
    procedure SetCaption(const Value: String);
  protected
    function GetDisplayName: String; override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    property TabWidth: Integer read FTabWidth;
  published
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Caption: String read FCaption write SetCaption;
    property Hint: String read FHint write FHint; // TODO this
    property Control: TControl read FControl write FControl;
    property Tag: Integer read FTag write FTag default 0;
  end;

  TOriTabs = class(TCollection)
  private
    FOwner: TOriTabSet;
    procedure SetItem(Index: Integer; Value: TOriTab);
    function GetItem(Index: Integer): TOriTab;
  protected
    procedure Update(Item: TCollectionItem); override;
    function GetOwner: TPersistent; override;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  public
    constructor Create(AOwner: TOriTabSet);
    function Add: TOriTab;
    function IndexOf(Item: TOriTab): Integer;
    function IndexOfControl(Control: TControl): Integer;
    property Items[index: Integer]: TOriTab read GetItem write SetItem; default;
  end;

  TOriTabSetOption = (tsoUseButtons, tsoUseMargins, tsoOwnControls,
    tsoFrameTabs, tsoDrawHover, tsoUseTabKeys, tsoNoSeparateTabs, tsoShowBorder,
    tsoNoRotateText, tsoSeparateTab);
  TOriTabSetOptions = set of TOriTabSetOption;
  TOriTabSetButton = (tsbNone, tsbUndef, tsbLeft, tsbRight);
  TOriTabVisibleState = (tvsFalse, tvsTrue, tvsPartial);
  TOriTabsPosition = (otpTop, otpBottom, otpLeft, otpRight);
  TOriTabSetLook = (otlDotNet, otlFlat);

  TOriTabEvent = procedure (Sender: TObject; Tab: TOriTab) of object;
  TTabChangingEvent = procedure (Sender: TObject; var AllowChange: Boolean) of object;

  TOriTabSet = class(TCustomControl)
  private
    FTabs: TOriTabs;
    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FTabIndex: Integer;
    FTabOffset: Integer;
    FButtonsVisible: Boolean;
    FLeftEnabled, FRightEnabled: Boolean;
    FButtonHover: TOriTabSetButton;
    FButtonDown: TOriTabSetButton;
    FOnTabChange: TNotifyEvent;
    FOnTabChanging: TTabChangingEvent;
    FMarginLeft, FMarginRight: Integer;
    FMarginTop, FMarginBottom: Integer;
    FTabsHeight, FTabsWidth: Integer;
    FTabWidth, FTabHeight: Integer;
    FOptions: TOriTabSetOptions;
    FTabsPosition: TOriTabsPosition;
    FUndockingTab: Integer;
    FColorTabsBack: TColor;
    FColorTabsBorder: TColor;
    FColorActiveTab: TColor;
    FColorHoverTab: TColor;
    FHoverTab: Integer;
    FOnTabAdded: TOriTabEvent;
    FLook: TOriTabSetLook;
    CTabMarginV, CTabMarginH: Byte;
    CTabIndentH, CTabIndentV: Byte;

    procedure UpdateTabControls(OldTab, NewTab: Integer);
    function FindNextTabIndex(CurIndex: Integer; GoForward: Boolean): Integer;
    function GetTabIndexFromDockClient(Client: TControl): Integer;
    function GetButtonFromPos(X, Y: Integer): TOriTabSetButton;

    procedure ImageListChange(Sender: TObject);

    function GetTabCount: Integer;
    function GetActiveTab: TOriTab;

    procedure SetImages(Value: TCustomImageList);
    procedure SetOptions(Value: TOriTabSetOptions);
    procedure SetMargin(Index: Integer; Value: Integer);
    procedure SetIndent(Index: Integer; Value: Byte);
    procedure SetActiveTab(Value: TOriTab);
    procedure SetActiveTabIndex(Value: Integer);
    procedure SetColors(Index: Integer; Value: TColor);
    procedure SetTabsPosition(const Value: TOriTabsPosition);
    procedure SetTabSize(Index: Integer; Value: Integer);
    procedure SetLook(Value: TOriTabSetLook);

    procedure InvalidateTab(Target: TCanvas; Index: Integer; const TabRect: TRect);
    procedure InvalidateButtons(Target: TCanvas);

    procedure EnableButtons;
    procedure ShiftLeft;
    procedure ShiftRight;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    //procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    //procedure CMDockClient(var Message: TCMDockClient); message CM_DOCKCLIENT;
    //procedure CMUnDockClient(var Message: TCMUnDockClient); message CM_UNDOCKCLIENT;
    //procedure CMDockNotification(var Message: TCMDockNotification); message CM_DOCKNOTIFICATION;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
    procedure WMLButtonDblClk(var Message: TLMMouse); message WM_LBUTTONDBLCLK;
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure DoTabAdded(Tab: TOriTab); dynamic;
    procedure DoChange; dynamic;
    function DoChanging: Boolean; dynamic;
    procedure MeasureTabs; virtual;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure DoRemoveDockClient(Client: TControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure UpdateView;
    function GetTabRect(Index: Integer): TRect;
    function GetTabFromPos(X, Y: Integer): Integer;
    function GetTabFromControl(Control: TControl): Integer;
    function TabIsVisible(Index: Integer): TOriTabVisibleState;
    function TabIndexAtCursor: Integer;
    procedure SelectNextTab; // for navigation by Ctrl+Tab
    procedure SelectPrevTab; // for navigation by Shift+Ctrl+Tab
    property TabCount: Integer read GetTabCount;
    property TabsHeight: Integer read FTabsHeight;
    property ActiveTab: TOriTab read GetActiveTab write SetActiveTab;
  published
    property Images: TCustomImageList read FImages write SetImages;
    property MarginLeft: Integer index 0 read FMarginLeft write SetMargin default 3;
    property MarginRight: Integer index 1 read FMarginRight write SetMargin default 3;
    property MarginTop: Integer index 2 read FMarginTop write SetMargin default 3;
    property MarginBottom: Integer index 3 read FMarginBottom write SetMargin default 3;
    property Tabs: TOriTabs read FTabs write FTabs; // must be before ActiveTabIndex
    property ActiveTabIndex: Integer read FTabIndex write SetActiveTabIndex default -1;
    property ColorTabsBack: TColor index 0 read FColorTabsBack write SetColors default clWindow;
    property ColorTabsBorder: TColor index 1 read FColorTabsBorder write SetColors default clBlack;
    property ColorActiveTab: TColor index 2 read FColorActiveTab write SetColors default clBtnFace;
    property ColorHoverTab: TColor index 3 read FColorHoverTab write SetColors default clBtnFace;
    property TabsPosition: TOriTabsPosition read FTabsPosition write SetTabsPosition default otpTop;
    property Options: TOriTabSetOptions read FOptions write SetOptions default [tsoUseButtons, tsoUseMargins];
    property OnChange: TNotifyEvent read FOnTabChange write FOnTabChange;
    property OnChanging: TTabChangingEvent read FOnTabChanging write FOnTabChanging;
    property OnTabAdded: TOriTabEvent read FOnTabAdded write FOnTabAdded;
    property TabWidth: Integer index 0 read FTabWidth write SetTabSize default 0;
    property TabHeight: Integer index 1 read FTabHeight write SetTabSize default 0;
    property TabMarginV: Byte index 0 read CTabMarginV write SetIndent default 3;
    property TabMarginH: Byte index 1 read CTabMarginH write SetIndent default 4;
    property TabIndentH: Byte index 2 read CTabIndentH write SetIndent default 5;
    property TabIndentV: Byte index 3 read CTabIndentV write SetIndent default 5;
    property Look: TOriTabSetLook read FLook write SetLook default otlDotNet;
    property Align;
    property Color default clBtnFace;
    property Font;
    property TabStop;
    property ParentFont;
    property PopupMenu;
    property OnDblClick;
    property OnMouseUp;
  end;

implementation

uses
  LCLIntf,
  OriGraphics;

{%region 'Constants'}
const
  //CTabMarginV = 3;
  //CTabMarginH = 4;
{
   tabs text margins - range between tab bound an text inside of any tab

   ------+ <- CTabMarginH ->      <- CTabMarginH -> +------
         |                     |  CTabMarginV       |
         |                   TEXT                   |
         |                     |  CTabMarginV       |
         +------------------------------------------+

   эти значения также задаются относительно направления текста, т.е. при
   расположении закладок слева или справа, если не стоит флаг tsoNoRotateText,
   вся картинка повернется на 90 градусов. Если флаг tsoNoRotateText стоит, то
   смысл "горизонтальности" и "вертикальности" полей сохраняется:

                                                |
     +------------------------------------------+
     |                     | CTabMarginV
     | <- CTabMarginH -> TEXT <- CTabMarginH ->
     |                     | CTabMarginV
     +------------------------------------------+
                                                |
}
  //CTabIndentH = 5;
  //CTabIndentV = 5;
{
   tabs indents - range between bounds of tab-set control and bound or tabs area

   |                                                                      |
   +-------------------+               +--------------+-------------------+
   | <- CTabIndentH -> |      TAB1     |    TAB2      | <- CTabIndentH -> |
   |                   +---------------+--------------+                   |
   |                                          | CTabIndentV               |
   +----------------------------------------------------------------------+

   отступы области закладок измеряются относительно стороны компонента, вдоль
   которой расположены закладки, независимо от флага tsoNoRotateText (т.к.
   текст тут не причем). Т.е. при положении закладок справа или слева картинки
   поворачиваются на 90 градусов. В этом смысле немного не логичны названия
   констант, указывающие на "горизонтальность" или "вертикальность" отступа
   (V или H), но умнее ничего не придумалось:

   +------------------------------+--------
   |          CTabIndentH |       |
   |                   +----------+
   |                   |
   | <- CTabIndentV -> |    TAB
   |                   |
   |                   +----------+
   |                              |
}
  CButtonIndentV = 2;
  CButtonIndentH = 2;
  CButtonW = 16;
{
    width and indents of navigate or close buttons

                                          Buttons "Tab"                |
  ---+               +---+ --------------------------------------------+
     |               |   |            CButtonW      | CButtonIndentV   |
     |               |   | <- CButton     +---+---+---+                |
     |      TAB      |   |     IndentH -> | < | > | X |    CButton ->  |
     |               |   |                +---+---+---+  <- IndentH    |
     |               |   |   CButtonIndentV |                          |
     +---------------+   +---------------------------------------------+
                                                                       |
  ---------------------------------------------------------------------+

   аналогично отступам области закладок, измеряются относительно стороны
   компонента, вдоль которой расположены закладки
}
{%endregion}

{%region 'TOriTab'}
constructor TOriTab.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FImageIndex := -1;
end;

procedure TOriTab.Assign(Source: TPersistent);
begin
  if Source is TOriTab then
  begin
    FCaption := TOriTab(Source).Caption;
    FHint := TOriTab(Source).Hint;
    FImageIndex := TOriTab(Source).ImageIndex;
    FControl := TOriTab(Source).Control;
    FTabWidth := TOriTab(Source).TabWidth;
  end;
  inherited Assign(Source);
  Changed(False);
end;

function TOriTab.GetDisplayName: String;
begin
  Result := Trim(FCaption);
  if Result = '' then Result := ClassName;
end;

procedure TOriTab.SetCaption(const Value: String);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TOriTab.SetImageIndex(Value: Integer);
begin
  if FImageIndex <> Value then
  begin
    FImageIndex := Value;
    Changed(False);
  end;
end;
{%endregion}

{%region 'TOriTabs'}
constructor TOriTabs.Create(AOwner: TOriTabSet);
begin
  inherited Create(TOriTab);
  FOwner := AOwner;
end;

function TOriTabs.Add: TOriTab;
begin
  Result := TOriTab(inherited Add);
  FOwner.DoTabAdded(Result);
end;

procedure TOriTabs.Update(Item: TCollectionItem);
begin
  if Count = 0 then FOwner.FTabIndex := -1;
  FOwner.UpdateView;
end;

function TOriTabs.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TOriTabs.SetItem(Index: Integer; Value: TOriTab);
begin
  inherited SetItem(Index, Value);
end;

function TOriTabs.GetItem(Index: Integer): TOriTab;
begin
  Result := TOriTab(inherited GetItem(Index));
end;

function TOriTabs.IndexOf(Item: TOriTab): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count-1 do
    if GetItem(I) = Item then
    begin
      Result := I;
      Exit;
    end;
end;

function TOriTabs.IndexOfControl(Control: TControl): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count-1 do
    if GetItem(I).Control = Control then
    begin
      Result := I;
      Exit;
    end;
end;

procedure TOriTabs.Notify(Item: TCollectionItem; Action: TCollectionNotification);
var Index: Integer;
begin
  if Action in [cnDeleting, cnExtracting] then
  begin
    if Assigned(TOriTab(Item).Control) and (tsoOwnControls in FOwner.Options) then
      FreeAndNil(TOriTab(Item).FControl);

    Index := IndexOf(TOriTab(Item));
    // если удалена активная страница, то индекс сбрасывается в -1
    // приложение само должно позаботиться о том, какую страницу
    // активизировать после удаления
    if Index = FOwner.FTabIndex then FOwner.FTabIndex := -1
    // если была удалена неактивная страница, то индекс активной странцы
    // уменьшается, если она находится после удаляемой. просто меняем индекс,
    // никаких событий при этом не генерится т.к. формально ничего не
    // изменилось - какая страница была активна, та и осталась. предполагается,
    // что если приложение добавляет/удаляет страницы, то оно не привязывается
    // жестко к их индексу
    else if Index < FOwner.FTabIndex then Dec(FOwner.FTabIndex);
  end;
  inherited;
end;
{%endregion}

{%region 'TOriTabSet'}
constructor TOriTabSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csSetCaption] +
    [csAcceptsControls, csOpaque, csDoubleClicks, csCaptureMouse] ;
  //DoubleBuffered := True; we do this by hands

  FButtonHover := tsbNone;
  FLeftEnabled := False;
  FRightEnabled := False;

  FMarginLeft := 3;
  FMarginTop := 3;
  FMarginRight := 3;
  FMarginBottom := 3;

  CTabMarginV := 3;
  CTabMarginH := 4;
  CTabIndentH := 5;
  CTabIndentV := 5;

  FTabs := TOriTabs.Create(Self);

  Color := clBtnFace;
  FColorTabsBack := clWindow;
  FColorTabsBorder := clBlack;
  FColorActiveTab := clBtnFace;
  FColorHoverTab := clBtnFace;
  FLook := otlDotNet;

  FOptions := [tsoUseButtons, tsoUseMargins];
  FTabOffset := 0;
  FTabIndex := -1;
  FHoverTab := -1;

  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := @ImageListChange;
end;

destructor TOriTabSet.Destroy;
begin
  FTabs.Free;
  inherited;
end;

procedure TOriTabSet.Loaded;
begin
  inherited;
end;

procedure TOriTabSet.UpdateView;
begin
  MeasureTabs;
  Realign;
  Invalidate;
end;

procedure TOriTabSet.AdjustClientRect(var Rect: TRect);
begin
  inherited;
  if tsoUseMargins in FOptions then
  begin
    Inc(Rect.Left, FMarginLeft);
    Dec(Rect.Right, FMarginRight);
    Inc(Rect.Top, FMarginTop);
    Dec(Rect.Bottom, FMarginBottom);
  end;
  case FTabsPosition of
    otpBottom:
    begin
      Dec(Rect.Bottom, FTabsHeight + CTabIndentV);
      if tsoShowBorder in FOptions then
      begin
        Inc(Rect.Top); Inc(Rect.Left); Dec(Rect.Right);
      end;
    end;
    otpTop:
    begin
      Inc(Rect.Top, FTabsHeight + CTabIndentV + 1);
      if tsoShowBorder in FOptions then
      begin
        Dec(Rect.Bottom); Inc(Rect.Left); Dec(Rect.Right);
      end;
    end;
    otpLeft:
    begin
      Inc(Rect.Left, FTabsHeight + CTabIndentV + 1);
      if tsoShowBorder in FOptions then
      begin
        Inc(Rect.Top); Dec(Rect.Bottom); Dec(Rect.Right);
      end;
    end;
    otpRight:
    begin
      Dec(Rect.Right, FTabsHeight + CTabIndentV);
      if tsoShowBorder in FOptions then
      begin
        Inc(Rect.Top); Dec(Rect.Bottom); Inc(Rect.Left);
      end;
    end;
  end;
end;

procedure TOriTabSet.MeasureTabs;
var
  I, Tmp: Integer;
  ImgExtraW, ImgExtraH: Integer;
begin
  if Assigned(FImages) then
    if (FTabsPosition in [otpTop, otpBottom]) or (tsoNoRotateText in FOptions) then
    begin
      ImgExtraW := FImages.Width;
      ImgExtraH := FImages.Height;
    end
    else begin
      ImgExtraH := FImages.Width;
      ImgExtraW := FImages.Height;
    end
  else begin
    ImgExtraW := 0;
    ImgExtraH := 0;
  end;

  Canvas.Font := Self.Font;
  if not (tsoNoRotateText in FOptions) then
    case FTabsPosition of
      otpLeft: Canvas.Font.Orientation := 900;
      otpRight: Canvas.Font.Orientation := 2700;
    end;
  if FTabWidth = 0 then
  begin
    FTabsWidth := 0;  // width of all tabs together
    for I := 0 to FTabs.Count-1 do
      with FTabs[I] do
      begin
        FTabWidth := Canvas.TextWidth(Caption) + CTabMarginH * 2;
        if Assigned(FImages) and (ImageIndex > -1) then
          Inc(FTabWidth, ImgExtraW + CTabMarginH);
        Inc(FTabsWidth, FTabWidth);
      end;
  end
  else
  begin
    for I := 0 to FTabs.Count-1 do
      FTabs[I].FTabWidth := FTabWidth;
    FTabsWidth := FTabWidth * FTabs.Count;
  end;

  // height of tabs (equal for all tabs)
  if FTabHeight = 0 then
  begin
    FTabsHeight := Canvas.TextHeight('Iy');
    if Assigned(FImages) and (ImgExtraH > FTabsHeight) then
      FTabsHeight := ImgExtraH;
    Inc(FTabsHeight, CTabMarginV * 2);
  end
  else FTabsHeight := FTabHeight;

  if (FTabsPosition in [otpLeft, otpRight]) and (tsoNoRotateText in FOptions) then
  begin // максимальная ширина закладки -> высота закладок и наоборот
    Tmp := 0;
    for I := 0 to FTabs.Count-1 do
      if FTabs[I].FTabWidth > Tmp then
        Tmp := FTabs[I].FTabWidth;
    I := FTabsHeight;
    FTabsHeight := Tmp;
    Tmp := I;
    FTabsWidth := FTabs.Count * Tmp;
    for I := 0 to FTabs.Count-1 do
      FTabs[I].FTabWidth := Tmp;
  end;

  Tmp := MaxInt; // чтобы гарантировано не выполнилось условие ниже
  case FTabsPosition of
    otpTop, otpBottom: if Width > 0 then Tmp := Width - CTabIndentH;
    otpLeft, otpRight: if Height > 0 then Tmp := Height - CTabIndentH;
  end;

  if FTabsWidth > Tmp then
    // recalc tab-widths to find room for all tabs
    if (FTabWidth = 0) and not (tsoUseButtons in FOptions) then
    begin
      FTabOffset := 0;
      for I := 0 to FTabs.Count-1 do
        FTabs[I].FTabWidth := Trunc(FTabs[I].FTabWidth / FTabsWidth * Tmp);
      FTabsWidth := 0;
      for I := 0 to FTabs.Count-1 do Inc(FTabsWidth, FTabs[I].FTabWidth);
      FButtonsVisible := False;
    end
    else
    begin
      FButtonsVisible := True;
      EnableButtons;
    end
  else
  begin
    FButtonsVisible := False;
    FTabOffset := 0; // usefull only if (tsoUseButtons in FOptions)
  end;
end;

procedure TOriTabSet.Paint;
var
  I, Bound, Separ: Integer;
  Pos1, Pos2, ActivePos: Integer;
  Target: TCanvas;
  Buffer: TBitmap;
begin
  Buffer := TBitmap.Create;
  try
    Buffer.Width := Width;
    Buffer.Height := Height;
    Target := Buffer.Canvas;
    with Target do
    begin
      Font := Self.Font;
      if not (tsoNoRotateText in FOptions) then
        case FTabsPosition of
          otpLeft: Font.Orientation := 900;
          otpRight: Font.Orientation := 2700;
        end;
      // fill background
      Brush.Color := Color;
      FillRect(0, 0, Self.Width, Self.Height);
      case FTabsPosition of
        otpTop:
        begin
          Bound := CTabIndentV;
          Separ := Bound + FTabsHeight;
          // fill tabs background
          Brush.Color := ColorTabsBack;
          FillRect(0, 0, Width, Separ+1);
          if tsoFrameTabs in Options then
          begin
            Pen.Color := ColorTabsBorder;
            Polyline([Point(0, Separ), Point(0, 0),
              Point(Width-1, 0), Point(Width-1, Separ)]);
          end;
          if tsoShowBorder in FOptions then
          begin
            Pen.Color := ColorTabsBorder;
            Polyline([Point(0, Separ), Point(0, Height-1),
              Point(Width-1, Height-1), Point(Width-1, Separ)]);
          end;
        end;
        otpBottom:
        begin
          Bound := Height - CTabIndentV;
          Separ := Bound - FTabsHeight;
          // fill tabs background
          Brush.Color := ColorTabsBack;
          FillRect(0, Separ, Width, Height);
          if tsoFrameTabs in Options then
          begin
            Pen.Color := ColorTabsBorder;
            Polyline([Point(0, Separ), Point(0, Height-1),
              Point(Width-1, Height-1), Point(Width-1, Separ)]);
          end;
          if tsoShowBorder in FOptions then
          begin
            Pen.Color := ColorTabsBorder;
            Polyline([Point(0, Separ), Point(0, 0),
              Point(Width-1, 0), Point(Width-1, Separ)]);
          end;
        end;
        otpLeft:
        begin
          Bound := CTabIndentV;
          Separ := Bound + FTabsHeight;
          // fill tabs background
          Brush.Color := ColorTabsBack;
          FillRect(0, 0, Separ+1, Height);
          if tsoFrameTabs in Options then
          begin
            Pen.Color := ColorTabsBorder;
            Polyline([Point(Separ, 0), Point(0, 0),
              Point(0, Height-1), Point(Separ, Height-1)]);
          end;
          if tsoShowBorder in FOptions then
          begin
            Pen.Color := ColorTabsBorder;
            Polyline([Point(Separ, 0), Point(Width-1, 0),
              Point(Width-1, Height-1), Point(Separ, Height-1)]);
          end;
        end;
        otpRight:
        begin
          Bound := Width - CTabIndentV;
          Separ := Bound - FTabsHeight;
          // fill tabs background
          Brush.Color := ColorTabsBack;
          FillRect(Separ, 0, Width, Height);
          if tsoFrameTabs in Options then
          begin
            Pen.Color := ColorTabsBorder;
            Polyline([Point(Separ, 0), Point(Width-1, 0),
              Point(Width-1, Height-1), Point(Separ, Height-1)]);
          end;
          if tsoShowBorder in FOptions then
          begin
            Pen.Color := ColorTabsBorder;
            Polyline([Point(Separ, 0), Point(0, 0),
              Point(0, Height-1), Point(Separ, Height-1)]);
          end;
        end;
      end;

      // tabs separator line
      if not (tsoNoSeparateTabs in Options) then
      begin
        Pen.Color := ColorTabsBorder;
        case TabsPosition of
          otpTop, otpBottom: begin MoveTo(0, Separ); LineTo(Width, Separ); end;
          otpLeft, otpRight: begin MoveTo(Separ, 0); LineTo(Separ, Height); end;
        end;
      end;
    end;

    // paint not active tabs
    Pos1 := CTabIndentH;
    for I := FTabOffset to FTabs.Count-1 do
    begin
      Pos2 := Pos1 + FTabs[I].TabWidth;
      if I <> FTabIndex then
        case FTabsPosition of
          otpTop: InvalidateTab(Target, I, Rect(Pos1, Bound, Pos2, Separ));
          otpLeft: InvalidateTab(Target, I, Rect(Bound, Pos1, Separ, Pos2));
          otpBottom: InvalidateTab(Target, I, Rect(Pos1, Separ, Pos2, Bound));
          otpRight: InvalidateTab(Target, I, Rect(Separ, Pos1, Bound, Pos2));
        end
      else ActivePos := Pos1;
      Pos1 := Pos2 - 1;
    end;

    // paint active tab
    if FTabIndex <> -1 then
    begin
      Pos1 := ActivePos;
      Pos2 := Pos1 + FTabs[FTabIndex].TabWidth;
      case FTabsPosition of
        otpTop: InvalidateTab(Target, FTabIndex, Rect(Pos1, Bound, Pos2, Separ));
        otpLeft: InvalidateTab(Target, FTabIndex, Rect(Bound, Pos1, Separ, Pos2));
        otpBottom: InvalidateTab(Target, FTabIndex, Rect(Pos1, Separ, Pos2, Bound));
        otpRight: InvalidateTab(Target, FTabIndex, Rect(Separ, Pos1, Bound, Pos2));
      end
    end;

    if FButtonsVisible then InvalidateButtons(Target);

    Canvas.Draw(0, 0, Buffer);
  finally
    Buffer.Free;
  end;
end;

function TOriTabSet.GetTabRect(Index: Integer): TRect;
var I, Pos1, Pos2: Integer;
begin
  Pos1 := CTabIndentH;
  for I := FTabOffset to FTabs.Count-1 do
  begin
    Pos2 := Pos1 + FTabs[I].TabWidth;
    if I = Index then
    begin
      case FTabsPosition of
        otpTop, otpBottom:
        begin
          Result.Left := Pos1;
          Result.Right := Pos2;
          case FTabsPosition of
            otpTop:
            begin
              Result.Top := CTabIndentV;
              Result.Bottom := Result.Top + FTabsHeight;
            end;
            otpBottom:
            begin
              Result.Bottom := Height - CTabIndentV;
              Result.Top := Result.Bottom - FTabsHeight;
            end;
          end;
        end;
        otpLeft, otpRight:
        begin
          Result.Top := Pos1;
          Result.Bottom := Pos2;
          case FTabsPosition of
            otpLeft:
            begin
              Result.Left := CTabIndentV;
              Result.Right := Result.Left + FTabsHeight;
            end;
            otpRight:
            begin
              Result.Right := Width - CTabIndentV;
              Result.Left := Result.Right - FTabsHeight;
            end;
          end;
        end;
      end;
      Break;
    end;
    Pos1 := Pos2 - 1;
  end;
end;

procedure TOriTabSet.InvalidateTab(Target: TCanvas; Index: Integer; const TabRect: TRect);
var
  Flags: Cardinal;
  TxtRect: TRect;
  Style: TTextStyle;
begin
  Text := FTabs[Index].Caption;
  with Target, TabRect do
  begin
    //case FLook of
      begin
        // отрисовка активной вкладки
        if FTabIndex = Index then
        begin
          // draw tab border
          Brush.Color := ColorActiveTab;
          Brush.Style := bsSolid;
          Pen.Color := ColorTabsBorder;
          case FTabsPosition of
            otpTop:
            begin
              MoveTo(Left, Bottom);
              LineTo(Left, Top);
              LineTo(Right-1, Top);
              LineTo(Right-1, Bottom+1);
              FillRect(Left+1, Top+1, Right-1, Bottom+1);
              if tsoSeparateTab in FOptions then
              begin
                MoveTo(Left, Bottom);
                LineTo(Right, Bottom);
              end;
            end;
            otpBottom:
            begin
              MoveTo(Left, Top);
              LineTo(Left, Bottom-1);
              LineTo(Right-1, Bottom-1);
              LineTo(Right-1, Top-1);
              FillRect(Left+1, Top, Right-1, Bottom-1);
              if tsoSeparateTab in FOptions then
              begin
                MoveTo(Left, Top);
                LineTo(Right, Top);
              end;
            end;
            otpLeft:
            begin
              MoveTo(Right, Top);
              LineTo(Left, Top);
              LineTo(Left, Bottom-1);
              LineTo(Right+1, Bottom-1);
              FillRect(Left+1, Top+1, Right+1, Bottom-1);
              if tsoSeparateTab in FOptions then
              begin
                MoveTo(Right, Top);
                LineTo(Right, Bottom);
              end;
            end;
            otpRight:
            begin
              MoveTo(Left, Top);
              LineTo(Right, Top);
              LineTo(Right, Bottom-1);
              LineTo(Left, Bottom-1);
              FillRect(Left, Top+1, Right, Bottom-1);
              if tsoSeparateTab in FOptions then
              begin
                MoveTo(Left, Top);
                LineTo(Left, Bottom);
              end;
            end;
          end;
        end
        // <-- конец отрисовки активной вкладки
        else // обычная (неактивная) закладка
        begin
          // фон закладки
          if tsoDrawHover in Options then
          begin
            Brush.Style := bsSolid;
            if Index = FHoverTab
              then Brush.Color := ColorHoverTab
              else Brush.Color := ColorTabsBack;
            case FTabsPosition of
              otpTop: FillRect(Left+1, Top+1, Right-1, Bottom);
              otpBottom: FillRect(Left+1, Top+1, Right-1, Bottom-1);
              otpLeft: FillRect(Left+1, Top+1, Right, Bottom-1);
              otpRight: FillRect(Left+1, Top+1, Right-1, Bottom-1);
            end;
          end;
          // рамка закладки
          case FLook of
            otlDotNet:
            begin
              // draw tab separator
              Pen.Color := Lighten(ColorTabsBack, -75);
              case FTabsPosition of
                otpTop, otpBottom:
                begin
                  MoveTo(Right-1, Top+3);
                  LineTo(Right-1, Bottom-3);
                end;
                otpLeft, otpRight:
                begin
                  MoveTo(Left+3, Bottom-1);
                  LineTo(Right-3, Bottom-1);
                end;
              end;
            end;

            otlFlat:
            begin
              Brush.Style := bsSolid;
              Brush.Color := ColorTabsBack;
              Pen.Color := ColorTabsBorder;
              case FTabsPosition of
                otpTop:
                begin
                  MoveTo(Left, Bottom);
                  LineTo(Left, Top);
                  LineTo(Right-1, Top);
                  LineTo(Right-1, Bottom+1);
                  FillRect(Left+1, Top+1, Right-1, Bottom);
                  if tsoSeparateTab in FOptions then
                  begin
                    MoveTo(Left, Bottom);
                    LineTo(Right, Bottom);
                  end;
                end;
                otpBottom:
                begin
                  MoveTo(Left, Top);
                  LineTo(Left, Bottom-1);
                  LineTo(Right-1, Bottom-1);
                  LineTo(Right-1, Top-1);
                  FillRect(Left+1, Top+1, Right-1, Bottom-1);
                  if tsoSeparateTab in FOptions then
                  begin
                    MoveTo(Left, Top);
                    LineTo(Right, Top);
                  end;
                end;
                otpLeft:
                begin
                  MoveTo(Right, Top);
                  LineTo(Left, Top);
                  LineTo(Left, Bottom-1);
                  LineTo(Right+1, Bottom-1);
                  FillRect(Left+1, Top+1, Right, Bottom-1);
                  if tsoSeparateTab in FOptions then
                  begin
                    MoveTo(Right, Top);
                    LineTo(Right, Bottom);
                  end;
                end;
                otpRight:
                begin
                  MoveTo(Left, Top);
                  LineTo(Right, Top);
                  LineTo(Right, Bottom-1);
                  LineTo(Left, Bottom-1);
                  FillRect(Left+1, Top+1, Right, Bottom-1);
                  if tsoSeparateTab in FOptions then
                  begin
                    MoveTo(Left, Top);
                    LineTo(Left, Bottom);
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    //end;
    //------------------------------------------------

    Style.Opaque := True; // warning suppress
    FillChar(Style, SizeOf(TTextStyle), 0);
    Style.SingleLine := True;
    Style.EndEllipsis := True;
    Style.Alignment := taCenter;
    Style.Layout := tlCenter;

    // вывод картинки и текста
    Brush.Style := bsClear;
    TxtRect := TabRect;
    case FTabsPosition of

      otpTop, otpBottom:
      begin
        Inc(TxtRect.Left, CTabMarginH);
        Dec(TxtRect.Right, CTabMarginH);
        // draw tab image
        if Assigned(FImages) and (FTabs[Index].ImageIndex > -1) and
          (FImages.Width <= (FTabs[Index].TabWidth - CTabMarginH * 2)) then
        begin
          Inc(TxtRect.Left, FImages.Width + CTabMarginH);
          FImages.Draw(Target, TabRect.Left + CTabMarginH,
            (TabRect.Top + TabRect.Bottom - FImages.Height) div 2,
            FTabs[Index].ImageIndex);
        end;
        // draw tab text
        TextRect(TxtRect, TxtRect.Left, TxtRect.Top, Caption, Style);
      end;

      // TODO: нужна процедура для вывода вертикального текста.
      // Выравнивание в Canvas.TextRect отличается от того, что делала
      // виндовая DrawText с вроде как аналогичными настройками.
      otpLeft:
        if tsoNoRotateText in FOptions then
        begin
          Inc(TxtRect.Left, CTabMarginH);
          Dec(TxtRect.Right, CTabMarginH);
          // draw tab image
          if Assigned(FImages) and (FTabs[Index].ImageIndex > -1) and
            (FImages.Width <= (FTabsHeight - CTabMarginH * 2)) then
          begin
            Inc(TxtRect.Left, FImages.Width + CTabMarginH);
            FImages.Draw(Target, TabRect.Left + CTabMarginH,
              (TabRect.Top + TabRect.Bottom - FImages.Height) div 2,
              FTabs[Index].ImageIndex);
          end;
          // draw tab text
          Style.Alignment := taLeftJustify;
          TextRect(TxtRect, TxtRect.Left, TxtRect.Top, Caption, Style);
        end
        else
        begin
          Inc(TxtRect.Top, CTabMarginH);
          Dec(TxtRect.Bottom, CTabMarginH);
          // draw tab image
          if Assigned(FImages) and (FTabs[Index].ImageIndex > -1) and
            (FImages.Height <= (FTabs[Index].TabWidth - CTabMarginH * 2)) then
          begin
            Dec(TxtRect.Bottom, FImages.Height + CTabMarginH);
            FImages.Draw(Target, (TabRect.Left + TabRect.Right - FImages.Width) div 2,
              TabRect.Bottom - FImages.Height - CTabMarginV, FTabs[Index].ImageIndex);
          end;
          // draw tab text
          Style.Alignment := taCenter;
          Style.Layout := tlCenter;
          TextRect(TxtRect, TxtRect.Left, TxtRect.Bottom, Caption, Style);
        end;

      otpRight:
        // если текст горизонтален, то код отрисовки слева и справа
        // совершенно одинаков. Нужно ли текст справа выравнивать по правому
        // краю или рисовать картинки справа от текста?
        if tsoNoRotateText in FOptions then
        begin
          Inc(TxtRect.Left, CTabMarginH);
          Dec(TxtRect.Right, CTabMarginH);
          // draw tab image
          if Assigned(FImages) and (FTabs[Index].ImageIndex > -1) and
            (FImages.Width <= (FTabsHeight - CTabMarginH * 2)) then
          begin
            Inc(TxtRect.Left, FImages.Width + CTabMarginH);
            FImages.Draw(Target, TabRect.Left + CTabMarginH,
              (TabRect.Top + TabRect.Bottom - FImages.Height) div 2,
              FTabs[Index].ImageIndex);
          end;
          // draw tab text
          Style.Alignment := taLeftJustify;
          TextRect(TxtRect, TxtRect.Left, TxtRect.Top, Caption, Style);
        end
        else
        begin
          Inc(TxtRect.Top, CTabMarginH);
          Dec(TxtRect.Bottom, CTabMarginH);
          // draw tab image
          if Assigned(FImages) and (FTabs[Index].ImageIndex > -1) and
            (FImages.Height <= (FTabs[Index].TabWidth - CTabMarginH * 2)) then
          begin
            Inc(TxtRect.Top, FImages.Height + CTabMarginH);
            FImages.Draw(Target, (TabRect.Left + TabRect.Right - FImages.Width) div 2,
              TabRect.Top + CTabMarginV, FTabs[Index].ImageIndex);
          end;
          Style.Alignment := taLeftJustify;
          TextRect(TxtRect, TxtRect.Left, TxtRect.Top, Caption, Style);
        end;
    end;
  end;
end;

procedure TOriTabSet.InvalidateButtons(Target: TCanvas);
var
  X1, Y1, X2, Y2: Integer;

  procedure DrawButton_Flat(Enabled, Hover, Pressed: Boolean);
  begin
    with Target do
    begin
      if Hover
        then Brush.Color := cl3DLight
        else Brush.Color := clBtnFace;
      if Enabled
        then Pen.Color := cl3DDkShadow
        else Pen.Color := clBtnShadow;
      if Pressed
        then Rectangle(X1+1, Y1+1, X2-1, Y2-1)
        else Rectangle(X1, Y1, X2, Y2);
    end;
  end;

const
  GlyphColors: array [Boolean] of TColor = (clBtnShadow, clBlack);
var
  butHover, butPressed: Boolean;
begin
  with Target do
  begin
    Brush.Color := ColorTabsBack;

    // fill buttons area to erase tabs under buttons
    // CButtonW*2 - because of two buttons: left shift and right
    // CButtonIndentH*2 - buttons placed within buttons-area with margins
    case FTabsPosition of
      otpTop, otpBottom:
      begin
        X1 := Width - CButtonW * 2 - CButtonIndentH * 2;
        if tsoFrameTabs in Options then X2 := Width-1 else X2 := Width;
        case FTabsPosition of
          otpTop:
          begin
            FillRect(X1, 0, X2, CTabIndentV + FTabsHeight);
            case FLook of
              otlDotNet:
              begin
                Pen.Color := FColorTabsBorder;
                MoveTo(X1, CTabIndentV + FTabsHeight);
                LineTo(X2, CTabIndentV + FTabsHeight);
              end;
            end;
            Y1 := CTabIndentV + CButtonIndentV - 1; // top
            Y2 := CTabIndentV + FTabsHeight - CButtonIndentV - 1; // bottom
          end;
          otpBottom:
          begin
            FillRect(X1, Height - CTabIndentV - FTabsHeight + 1, X2, Height);
            Pen.Color := FColorTabsBorder;
            MoveTo(X1, Height - CTabIndentV - FTabsHeight);
            LineTo(X2, Height - CTabIndentV - FTabsHeight);
            Y2 := Height - CTabIndentV - CButtonIndentV + 1; // bottom
            Y1 := Height - CTabIndentV - FTabsHeight + CButtonIndentV + 1; // top
          end;
        end;

        X2 := Width - CButtonIndentH;
        X1 := X2 - CButtonW;
        butPressed := FButtonDown = tsbRight;
        butHover := FButtonHover = tsbRight;
        DrawButton_Flat(FRightEnabled, butHover, butPressed);
        //DrawMonoGlyph(Target.Handle, (X1 + X2 - MonoGlyphW) div 2,
        //  (Y1 + Y2 - MonoGlyphH) div 2, bgTriangleRight, GlyphColors[FRightEnabled]);
        X2 := X1;
        X1 := X2 - CButtonW;
        butPressed := FButtonDown = tsbLeft;
        butHover := FButtonHover = tsbLeft;
        DrawButton_Flat(FLeftEnabled, butHover, butPressed);
        //DrawMonoGlyph(Target.Handle, (X1 + X2 - MonoGlyphW) div 2,
        //  (Y1 + Y2 - MonoGlyphH) div 2, bgTriangleLeft, GlyphColors[FLeftEnabled]);
      end;
      otpLeft, otpRight:
      begin
        Y1 := Height - CButtonW * 2 - CButtonIndentH * 2;
        if tsoFrameTabs in Options then Y2 := Height - 1 else Y2 := Height;
        case FTabsPosition of
          otpLeft:
          begin
            FillRect(0, Y1, CTabIndentV + FTabsHeight, Y2);
            Pen.Color := FColorTabsBorder;
            MoveTo(CTabIndentV + FTabsHeight, Y1);
            LineTo(CTabIndentV + FTabsHeight, Y2);
            X1 := CTabIndentV + CButtonIndentV - 1; // left
            X2 := CTabIndentV + FTabsHeight - CButtonIndentV - 1 // right
          end;
          otpRight:
          begin
            FillRect(Width - CTabIndentV - FTabsHeight + 1, Y1, Width, Y2);
            Pen.Color := FColorTabsBorder;
            MoveTo(Width - CTabIndentV - FTabsHeight, Y1);
            LineTo(Width - CTabIndentV - FTabsHeight, Y2);
            X2 := Width - CTabIndentV - CButtonIndentV + 1; // right
            X1 := Width - CTabIndentV - FTabsHeight + CButtonIndentV + 1; // left
          end;
        end;
        Y2 := Height - CButtonIndentH;
        Y1 := Y2 - CButtonW;
        butPressed := FButtonDown = tsbRight;
        butHover := FButtonHover = tsbRight;
        DrawButton_Flat(FRightEnabled, butHover, butPressed);
        //DrawMonoGlyph(Target.Handle, (X1 + X2 - MonoGlyphW) div 2,
        //  (Y1 + Y2 - MonoGlyphH) div 2, bgTriangleDown, GlyphColors[FRightEnabled]);
        Y2 := Y1;
        Y1 := Y2 - CButtonW;
        butPressed := FButtonDown = tsbLeft;
        butHover := FButtonHover = tsbLeft;
        DrawButton_Flat(FLeftEnabled, butHover, butPressed);
        //DrawMonoGlyph(Target.Handle, (X1 + X2 - MonoGlyphW) div 2,
        //  (Y1 + Y2 - MonoGlyphH) div 2, bgTriangleUp, GlyphColors[FLeftEnabled]);
      end;
    end;
  end;
end;

procedure TOriTabSet.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  TabIndex: Integer;
  Btn: TOriTabSetButton;
begin
  inherited;
  if Button = mbLeft then
  begin
    if FButtonsVisible then
    begin
      Btn := GetButtonFromPos(X, Y);
      if Btn <> tsbNone then
      begin
        FButtonHover := Btn;
        FButtonDown := Btn;
        case FButtonDown of
          tsbLeft: ShiftLeft;
          tsbRight: ShiftRight;
        end;
        Exit;
      end;
    end;
    TabIndex := GetTabFromPos(X, Y);
    if TabIndex > -1 then
    begin
      if FButtonsVisible and (TabIsVisible(TabIndex) = tvsPartial) then
      begin
        Inc(FTabOffset); // don't call ShiftRight to avoid unnecessary drawing
        EnableButtons;
      end;
      ActiveTabIndex := TabIndex
    end;
  end;
end;

procedure TOriTabSet.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FButtonsVisible and (Button = mbLeft) and (FButtonDown <> tsbNone) then
  begin
    FButtonDown := tsbNone; // release button
    InvalidateButtons(Canvas);
  end;
end;

procedure TOriTabSet.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Index, OldIndex: Integer;
  Button: TOriTabSetButton;
begin
  inherited MouseMove(Shift, X, Y);
  if FButtonsVisible then
  begin
    if (ssLeft in Shift) and (FButtonDown <> tsbNone) then Exit;
    Button := GetButtonFromPos(X, Y);
    if Button <> FButtonHover then
    begin
      FButtonHover := Button;
      FButtonDown := tsbNone;
      InvalidateButtons(Canvas);
    end;
  end
  else Button := tsbNone;

  if Button = tsbNone then
  begin
    Index := GetTabFromPos(X, Y);
    if (tsoDrawHover in Options) and (Index <> FHoverTab) then
    begin
      OldIndex := FHoverTab;
      FHoverTab := Index;
      if OldIndex <> -1 then InvalidateTab(Canvas, OldIndex, GetTabRect(OldIndex));
      if Index <> -1 then InvalidateTab(Canvas, Index, GetTabRect(Index));
      if FButtonsVisible and
        (TabIsVisible(Index) = tvsPartial) or
        (TabIsVisible(OldIndex) = tvsPartial) then InvalidateButtons(Canvas);
      if (FTabIndex <> -1) and (
        (Index = FTabIndex+1) or (Index = FTabIndex-1) or
        (OldIndex = FTabIndex+1) or (OldIndex = FTabIndex-1)) then
      begin
        InvalidateTab(Canvas, FTabIndex, GetTabRect(FTabIndex));
        if FButtonsVisible and (TabIsVisible(FTabIndex) = tvsPartial) then
          InvalidateButtons(Canvas);
      end;
    end;
    if (ssLeft in Shift) and (Index <> -1)
      and Assigned(FTabs[Index].Control)
      and (FTabs[Index].Control.HostDockSite = Self) then
        FTabs[Index].Control.BeginDrag(False, 10);
  end;
end;

procedure TOriTabSet.CMMouseLeave(var Message: TMessage);
var
  OldIndex: Integer;
begin
  inherited;
  if (tsoDrawHover in Options) and (FHoverTab <> -1) then
  begin
    OldIndex := FHoverTab;
    FHoverTab := -1;
    InvalidateTab(Canvas, OldIndex, GetTabRect(OldIndex));
  end;
  if FButtonsVisible and (FButtonHover <> tsbNone) then
  begin
    FButtonHover := tsbNone;
    FButtonDown := tsbNone;
    InvalidateButtons(Canvas);
  end;
end;

procedure TOriTabSet.CMFontChanged(var Message: TMessage);
begin
  UpdateView;
  inherited;
end;

procedure TOriTabSet.CMColorChanged(var Message: TMessage);
begin
  Invalidate;
  inherited;
end;

procedure TOriTabSet.WMEraseBkgnd(var Message: TMessage);
begin
  // do nothing
//  inherited;
  //inherited EraseBackground(DC);
end;

procedure TOriTabSet.WMSize(var Message: TWMSize);
begin
  inherited;
  MeasureTabs;
  Invalidate;
end;

function TOriTabSet.TabIndexAtCursor: Integer;
var
  pt: TPoint;
begin
  pt.x := 0;
  pt.y := 0;
  GetCursorPos(pt);
  pt := ScreenToClient(pt);
  Result := GetTabFromPos(pt.X, pt.Y);
end;

function TOriTabSet.GetTabFromPos(X, Y: Integer): Integer;
var
  I, Pos, TabPos: Integer;
begin
  Result := -1;
  TabPos := CTabIndentH;
  case FTabsPosition of
    otpTop: if (Y < CTabIndentV) or (Y > CTabIndentV + FTabsHeight) then Exit;
    otpBottom: if (Y < Height - CTabIndentV - FTabsHeight) or (Y > Height - CTabIndentV) then Exit;
    otpLeft: if (X < CTabIndentV) or (X > CTabIndentV + FTabsHeight) then Exit;
    otpRight: if (X < Width - CTabIndentV - FTabsHeight) or (X > Width - CTabIndentV) then Exit;
  end;
  case FTabsPosition of
    otpTop, otpBottom: Pos := X;
    else Pos := Y;
  end;
  for I := FTabOffset to FTabs.Count-1 do
    if (Pos >= TabPos) and (Pos < TabPos + FTabs[I].TabWidth) then
    begin
      Result := I;
      Exit;
    end
    else Inc(TabPos, FTabs[I].TabWidth);
end;

function TOriTabSet.GetButtonFromPos(X, Y: Integer): TOriTabSetButton;
begin
  Result := tsbNone;
  case TabsPosition of
    otpTop, otpBottom:
      if X > Width - CButtonW * 2 - CButtonIndentH * 2 then
      begin
        Result := tsbUndef;
        case TabsPosition of
          otpBottom:
            if (Y < Height - CTabIndentV - FTabsHeight + CButtonIndentV + 1)
              or (Y > Height - CTabIndentV - CButtonIndentV + 1) then Exit;
          otpTop:
            if (Y < CTabIndentV + CButtonIndentV)
              or (Y > CTabIndentV + FTabsHeight - CButtonIndentV) then Exit;
        end;
        if X > Width - CButtonW * 2 - CButtonIndentH then
          if X > Width - CButtonW - CButtonIndentH then
            if X > Width - CButtonIndentH
              then Result := tsbUndef
            else Result := tsbRight
          else Result := tsbLeft;
      end;
    otpLeft, otpRight:
      if Y > Height - CButtonW * 2 - CButtonIndentH * 2 then
      begin
        Result := tsbUndef;
        case TabsPosition of
          otpRight:
            if (X < Width - CTabIndentV - FTabsHeight + CButtonIndentV + 1)
              or (X > Width - CTabIndentV - CButtonIndentV + 1) then Exit;
          otpLeft:
            if (X < CTabIndentV + CButtonIndentV)
              or (X > CTabIndentV + FTabsHeight - CButtonIndentV) then Exit;
        end;
        if Y > Height - CButtonW * 2 - CButtonIndentH then
          if Y > Height - CButtonW - CButtonIndentH then
            if Y > Height - CButtonIndentH
              then Result := tsbUndef
            else Result := tsbRight
          else Result := tsbLeft;
      end;
  end;
end;

procedure TOriTabSet.EnableButtons;
var
  I, Pos1, Pos2, Limit: Integer;
begin
  FLeftEnabled := FTabOffset > 0;
  FRightEnabled := False;
  if FButtonsVisible then
  begin
    case FTabsPosition of
      otpTop, otpBottom:
           Limit := Width - CButtonW*2 - CButtonIndentH*2;
      else Limit := Height - CButtonW*2 - CButtonIndentH*2;
    end;
    Pos1 := CTabIndentH;
    for I := FTabOffset to FTabs.Count-1 do
    begin
      Pos2 := Pos1 + FTabs[I].TabWidth;
      if Pos2 > Limit then
      begin
        FRightEnabled := True;
        Break;
      end;
      Inc(Pos1, FTabs[I].TabWidth);
    end;
  end;
end;

procedure TOriTabSet.ShiftLeft;
begin
  if FLeftEnabled then
  begin
    Dec(FTabOffset);
    EnableButtons;
    Invalidate;
  end;
end;

procedure TOriTabSet.ShiftRight;
begin
  if FRightEnabled then
  begin
    Inc(FTabOffset);
    EnableButtons;
    Invalidate;
  end;
end;

function TOriTabSet.TabIsVisible(Index: Integer): TOriTabVisibleState;
var
  I, Pos, Limit: Integer;
begin
  Result := tvsTrue;
  if FButtonsVisible then
    if Index >= FTabOffset then
    begin
      Pos := CTabIndentH; // near limit
      case FTabsPosition of
        otpTop, otpBottom: Limit := Width - CButtonW*2 - CButtonIndentH*2; // far limit
        else Limit := Height - CButtonW*2 - CButtonIndentH*2; // far limit
      end;
      for I := FTabOffset to FTabs.Count-1 do
        if I = Index then
        begin
          if Pos >= Limit then Result := tvsFalse
          else if (Pos + FTabs[I].TabWidth) > Limit then Result := tvsPartial
          else Result := tvsTrue;
          Break;
        end
        else Inc(Pos, FTabs[I].TabWidth);
    end
    else Result := tvsFalse;
end;

function TOriTabSet.GetTabCount: Integer;
begin
  Result := FTabs.Count;
end;

function TOriTabSet.GetActiveTab: TOriTab;
begin
  if (FTabIndex > -1) and (FTabIndex < FTabs.Count)
    then Result := FTabs[FTabIndex]
    else Result := nil;
end;

procedure TOriTabSet.SetActiveTab(Value: TOriTab);
begin
  SetActiveTabIndex(FTabs.IndexOf(Value));
end;

procedure TOriTabSet.SetActiveTabIndex(Value: Integer);
begin
  if (Value > -1) and (Value < FTabs.Count) and (Value <> FTabIndex) then
  begin
    if not DoChanging then Exit;
    UpdateTabControls(FTabIndex, Value);
    FTabIndex := Value;
    Invalidate;
    DoChange;
  end;
end;

function TOriTabSet.GetTabFromControl(Control: TControl): Integer;
var I: Integer;
begin
  Result := -1;
  for I := 0 to FTabs.Count-1 do
    if FTabs[I].Control = Control then
    begin
      Result := I;
      Break;
    end;
end;

procedure TOriTabSet.UpdateTabControls(OldTab, NewTab: Integer);
var
  Control: TControl;
begin
  if (NewTab > -1) and (NewTab < FTabs.Count) then
  begin
    Control := FTabs[NewTab].Control;
    if Assigned(Control) then
    begin
      Control.Show;
      Control.BringToFront;
      if Control is TWinControl then
        with TWinControl(Control) do
        try
          if CanFocus then SetFocus;
        except // can't focus disable or invisible window
        end;   // и CanFocus в некоторых случаях не помогает
    end;
  end;
  if (OldTab > -1) and (OldTab < FTabs.Count) then
  begin
    Control := FTabs[OldTab].Control;
    if Assigned(Control) then Control.Hide;
  end;
end;

function TOriTabSet.DoChanging: Boolean;
begin
  Result := True;
  if Assigned(FOnTabChanging) then FOnTabChanging(Self, Result);
end;

procedure TOriTabSet.DoChange;
begin
  if Assigned(FOnTabChange) then FOnTabChange(Self);
end;

procedure TOriTabSet.DoTabAdded(Tab: TOriTab);
begin
  if Assigned(FOnTabAdded) then FOnTabAdded(Self, Tab);
end;

function TOriTabSet.FindNextTabIndex(CurIndex: Integer; GoForward: Boolean): Integer;
begin
  if FTabs.Count <> 0 then
  begin
    if (CurIndex < 0) or (CurIndex > FTabs.Count) then
      if GoForward then CurIndex := FTabs.Count - 1 else CurIndex := 0;
    Result := CurIndex;
    if GoForward then
    begin
      Inc(Result);
      if Result = FTabs.Count then Result := 0;
    end
    else
    begin
      if Result = 0 then Result := FTabs.Count;
      Dec(Result);
    end;
  end
  else Result := -1;
end;

procedure TOriTabSet.SelectNextTab;
begin
  ActiveTabIndex := FindNextTabIndex(FTabIndex, True);
end;

procedure TOriTabSet.SelectPrevTab;
begin
  ActiveTabIndex := FindNextTabIndex(FTabIndex, False);
end;

procedure TOriTabSet.SetOptions(Value: TOriTabSetOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    UpdateView;
  end;
end;

procedure TOriTabSet.SetMargin(Index: Integer; Value: Integer);
begin
  case Index of
    0: if FMarginLeft <> Value then begin FMarginLeft := Value; UpdateView; end;
    1: if FMarginRight <> Value then begin FMarginRight := Value; UpdateView; end;
    2: if FMarginTop <> Value then begin FMarginTop := Value; UpdateView; end;
    3: if FMarginBottom <> Value then begin FMarginBottom := Value; UpdateView; end;
  end;
end;

procedure TOriTabSet.SetIndent(Index: Integer; Value: Byte);
begin
  case Index of
    0: if Value <> CTabMarginV then begin CTabMarginV := Value; UpdateView; end;
    1: if Value <> CTabMarginH then begin CTabMarginH := Value; UpdateView; end;
    2: if Value <> CTabIndentH then begin CTabIndentH := Value; UpdateView; end;
    3: if Value <> CTabIndentV then begin CTabIndentV := Value; UpdateView; end;
  end;
end;

procedure TOriTabSet.SetTabSize(Index: Integer; Value: Integer);
begin
  case Index of
    0: if FTabWidth <> Value then begin FTabWidth := Value; UpdateView; end;
    1: if FTabHeight <> Value then begin FTabHeight := Value; UpdateView; end;
  end;
end;

procedure TOriTabSet.SetColors(Index: Integer; Value: TColor);
begin
  case Index of
    0: if FColorTabsBack <> Value then begin FColorTabsBack := Value; UpdateView; end;
    1: if FColorTabsBorder <> Value then begin FColorTabsBorder := Value; UpdateView; end;
    2: if FColorActiveTab <> Value then begin FColorActiveTab := Value; UpdateView; end;
    3: if FColorHoverTab <> Value then begin FColorHoverTab := Value; UpdateView; end;
  end;
end;

procedure TOriTabSet.SetImages(Value: TCustomImageList);
begin
  if Images <> nil then
    Images.UnRegisterChanges(FImageChangeLink);
  FImages := Value;
  if Images <> nil then
  begin
    Images.RegisterChanges(FImageChangeLink);
    Images.FreeNotification(Self);
  end;
  UpdateView;
end;

procedure TOriTabSet.SetTabsPosition(const Value: TOriTabsPosition);
begin
  if FTabsPosition <> Value then
  begin
    FTabsPosition := Value;
    UpdateView;
  end;
end;

procedure TOriTabSet.SetLook(Value: TOriTabSetLook);
begin
  if FLook <> Value then
  begin
    FLook := Value;
    UpdateView;
  end;
end;

procedure TOriTabSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then Images := nil;
end;

procedure TOriTabSet.ImageListChange(Sender: TObject);
begin
  UpdateView;
end;

procedure TOriTabSet.WMLButtonDblClk(var Message: TLMMouse);
var
  Index: Integer;
  Control: TControl;
begin
  inherited;
  if FButtonsVisible then
    if GetButtonFromPos(Message.Pos.x, Message.Pos.y) <> tsbNone then
      Exit;
  Index := GetTabFromPos(Message.Pos.x, Message.Pos.y);
  if (Index <> -1) and Assigned(FTabs[Index].Control) and
    (FTabs[Index].Control.HostDockSite = Self) then
  begin
    Control := FTabs[Index].Control;
    Control.ManualDock(nil, nil, alNone);
    if not Control.Visible then Control.Visible := True;
  end;
end;

function TOriTabSet.GetTabIndexFromDockClient(Client: TControl): Integer;
var
  I: Integer;
begin
  Result := -1;
  if Client.HostDockSite = Self then
    for I := 0 to FTabs.Count-1 do
      if FTabs[I].Control = Client then
      begin
        Result := I;
        Break;
      end;
end;

procedure TOriTabSet.DoRemoveDockClient(Client: TControl);
begin
  if (FUndockingTab <> -1) and not (csDestroying in ComponentState) then
  begin
    FTabs.Delete(FUndockingTab);
    FTabIndex := -1; // to prevent hiding just selected page
    ActiveTabIndex := FindNextTabIndex(FUndockingTab-1, True);
    FUndockingTab := -1;
    Client.Visible := True;
  end;
end;

{ not implemented in Lazarus 1.0.4
procedure TOriTabSet.CMDockClient(var Message: TCMDockClient);
var
  DockCtl: TControl;
  NewTab: TOriTab;
begin
  Message.Result := 0;
  NewTab := FTabs.Add;
  try
    DockCtl := Message.DockSource.Control;
    if DockCtl is TForm then
      NewTab.Caption := TForm(DockCtl).Caption;
    DockCtl.Dock(Self, Message.DockSource.DockRect);
  except
    NewTab.Free;
    raise;
  end;
  DockCtl.Align := alClient;
  NewTab.Control := DockCtl;
  ActiveTab := NewTab;
end;
}

{ not implemented in Lazarus 1.0.4
procedure TOriTabSet.CMUnDockClient(var Message: TCMUnDockClient);
begin
  Message.Result := 0;
  Message.Client.Align := alNone;
  Message.Client.Visible := True;

  FUndockingTab := GetTabIndexFromDockClient(Message.Client);
end;
}

{ not implemented in Lazarus 1.0.4
procedure TOriTabSet.CMDockNotification(var Message: TCMDockNotification);
var
  I: Integer;
  S: TOriString;
  Index: Integer;
begin
  Index := GetTabIndexFromDockClient(Message.Client);
  if Index <> -1 then
    case Message.NotifyRec.ClientMsg of
      WM_SETTEXT:
      begin
        S := POriChar(Message.NotifyRec.MsgLParam);
        // Search for first CR/LF and end string there
        for I := 1 to Length(S) do
          if (S[I] = #13) or (S[I] = #10) then
          begin
            SetLength(S, I - 1);
            Break;
          end;
        FTabs[Index].Caption := S;
      end;
    end;
  inherited;
end;
}

{ not implemented in Lazarus 1.0.4
procedure TOriTabSet.CMDialogKey(var Message: TCMDialogKey);
begin
  if (tsoUseTabKeys in Options) and (Focused or Windows.IsChild(Handle, Windows.GetFocus)) and
    (Message.CharCode = VK_TAB) and (GetKeyState(VK_CONTROL) < 0) then
  begin
    if GetKeyState(VK_SHIFT) >= 0
      then SelectNextTab
      else SelectPrevTab;
    Message.Result := 1;
  end
  else inherited;
end;
}
{%endregion}

end.

