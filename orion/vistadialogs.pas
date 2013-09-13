unit VistaDialogs;

{$mode objfpc}{$H+}

interface

{$ifdef WINDOWS}

uses
  SysUtils, Classes, Windows, ShlObj, Dialogs;

type
  TFileName = type string;
  TTaskWindowList = Pointer;

  TComdlgFilterSpec = COMDLG_FILTERSPEC;
  TComdlgFilterSpecArray = array of TComdlgFilterSpec;

  TFavoriteLinkItem = class(TCollectionItem)
  private
    FLocation: string;
  protected
    function GetDisplayName: string; override;
  published
    property Location: string read FLocation write FLocation;
  end;

  TFavoriteLinkItems = class;

  TFavoriteLinkItemsEnumerator = class
  private
    FIndex: Integer;
    FCollection: TFavoriteLinkItems;
  public
    constructor Create(ACollection: TFavoriteLinkItems);
    function GetCurrent: TFavoriteLinkItem;
    function MoveNext: Boolean;
    property Current: TFavoriteLinkItem read GetCurrent;
  end;

  TFavoriteLinkItems = class(TCollection)
  private
    function GetItem(Index: Integer): TFavoriteLinkItem;
    procedure SetItem(Index: Integer; const Value: TFavoriteLinkItem);
  public
    function Add: TFavoriteLinkItem;
    function GetEnumerator: TFavoriteLinkItemsEnumerator;
    property Items[Index: Integer]: TFavoriteLinkItem read GetItem write SetItem; default;
  end;

  TFileTypeItem = class(TCollectionItem)
  private
    FDisplayName: string;
    FDisplayNameWStr: LPCWSTR;
    FFileMask: string;
    FFileMaskWStr: LPCWSTR;
    function GetDisplayNameWStr: LPCWSTR;
    function GetFileMaskWStr: LPCWSTR;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property DisplayNameWStr: LPCWSTR read GetDisplayNameWStr;
    property FileMaskWStr: LPCWSTR read GetFileMaskWStr;
  published
    property DisplayName: string read FDisplayName write FDisplayName;
    property FileMask: string read FFileMask write FFileMask;
  end;

  TFileTypeItems = class(TCollection)
  private
    function GetItem(Index: Integer): TFileTypeItem;
    procedure SetItem(Index: Integer; const Value: TFileTypeItem);
  public
    function Add: TFileTypeItem;
    function FilterSpecArray: TComdlgFilterSpecArray;
    property Items[Index: Integer]: TFileTypeItem read GetItem write SetItem; default;
  end;

  EPlatformVersionException = class(Exception);

  TFileDialogOption = (fdoOverWritePrompt, fdoStrictFileTypes,
    fdoNoChangeDir, fdoPickFolders, fdoForceFileSystem,
    fdoAllNonStorageItems, fdoNoValidate, fdoAllowMultiSelect,
    fdoPathMustExist, fdoFileMustExist, fdoCreatePrompt,
    fdoShareAware, fdoNoReadOnlyReturn, fdoNoTestFileCreate,
    fdoHideMRUPlaces, fdoHidePinnedPlaces, fdoNoDereferenceLinks,
    fdoDontAddToRecent, fdoForceShowHidden, fdoDefaultNoMiniMode,
    fdoForcePreviewPaneOn);
  TFileDialogOptions = set of TFileDialogOption;

  TFileDialogOverwriteResponse = (forDefault = FDEOR_DEFAULT,
    forAccept = FDEOR_ACCEPT, forRefuse = FDEOR_REFUSE);
  TFileDialogShareViolationResponse = (fsrDefault = FDESVR_DEFAULT,
    fsrAccept = FDESVR_ACCEPT, fsrRefuse = FDESVR_REFUSE);

  TFileDialogCloseEvent = procedure(Sender: TObject; var CanClose: Boolean) of object;
  TFileDialogFolderChangingEvent = procedure(Sender: TObject; var CanChange: Boolean) of object;
  TFileDialogOverwriteEvent = procedure(Sender: TObject; var Response: TFileDialogOverwriteResponse) of object;
  TFileDialogShareViolationEvent = procedure(Sender: TObject; var Response: TFileDialogShareViolationResponse) of object;

  TCustomFileDialog = class(TComponent)
  private
    FClientGuid: string;
    FDefaultExtension: string;
    FDefaultFolder: string;
    FDialog: IFileDialog;
    FFavoriteLinks: TFavoriteLinkItems;
    FFileName: TFileName;
    FFileNameLabel: string;
    FFiles: TStrings;
    FFileTypeIndex: Cardinal;
    FFileTypes: TFileTypeItems;
    FHandle: HWnd;
    FOkButtonLabel: string;
    FOptions: TFileDialogOptions;
    FShellItem: IShellItem;
    FShellItems: IShellItemArray;
    FTitle: string;
    FOnExecute: TNotifyEvent;
    FOnFileOkClick: TFileDialogCloseEvent;
    FOnFolderChange: TNotifyEvent;
    FOnFolderChanging: TFileDialogFolderChangingEvent;
    FOnOverwrite: TFileDialogOverwriteEvent;
    FOnSelectionChange: TNotifyEvent;
    FOnShareViolation: TFileDialogShareViolationEvent;
    FOnTypeChange: TNotifyEvent;
    function GetDefaultFolder: string;
    function GetFileName: TFileName;
    function GetFiles: TStrings;
    procedure GetWindowHandle;
    procedure SetClientGuid(const Value: string);
    procedure SetDefaultFolder(const Value: string);
    procedure SetFavoriteLinks(const Value: TFavoriteLinkItems);
    procedure SetFileName(const Value: TFileName);
    procedure SetFileTypes(const Value: TFileTypeItems);
  strict protected
    function CreateFileDialog: IFileDialog; virtual; abstract;
    procedure DoOnExecute; dynamic;
    function DoOnFileOkClick: Boolean; dynamic;
    procedure DoOnFolderChange; dynamic;
    function DoOnFolderChanging: Boolean; dynamic;
    procedure DoOnOverwrite(var Response: TFileDialogOverwriteResponse); dynamic;
    procedure DoOnSelectionChange; dynamic;
    procedure DoOnShareViolation(var Response: TFileDialogShareViolationResponse); dynamic;
    procedure DoOnTypeChange; dynamic;
    function GetFileNames(Items: IShellItemArray): HResult; dynamic;
    function GetItemName(Item: IShellItem; var ItemName: TFileName): HResult; dynamic;
    function GetResults: HResult; virtual;
  protected
    function FileOkClick: HResult; dynamic;
    function FolderChange: HResult; dynamic;
    function FolderChanging(psiFolder: IShellItem): HResult; dynamic;
    function Overwrite(psiFile: IShellItem; var Response: LongInt): HResult; dynamic;
    function SelectionChange: HResult; dynamic;
    function ShareViolation(psiFile: IShellItem; var Response: LongInt): HResult; dynamic;
    function TypeChange: HResult; dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute: Boolean; overload; virtual;
    function Execute(ParentWnd: HWND): Boolean; overload; virtual;
    property ClientGuid: string read FClientGuid write SetClientGuid;
    property DefaultExtension: string read FDefaultExtension write FDefaultExtension;
    property DefaultFolder: string read GetDefaultFolder write SetDefaultFolder;
    property Dialog: IFileDialog read FDialog;
    property FavoriteLinks: TFavoriteLinkItems read FFavoriteLinks write SetFavoriteLinks;
    property FileName: TFileName read GetFileName write SetFileName;
    property FileNameLabel: string read FFileNameLabel write FFileNameLabel;
    property Files: TStrings read GetFiles;
    property FileTypes: TFileTypeItems read FFileTypes write SetFileTypes;
    property FileTypeIndex: Cardinal read FFileTypeIndex write FFileTypeIndex default 1;
    property Handle: HWnd read FHandle;
    property OkButtonLabel: string read FOkButtonLabel write FOkButtonLabel;
    property Options: TFileDialogOptions read FOptions write FOptions;
    property ShellItem: IShellItem read FShellItem;
    property ShellItems: IShellItemArray read FShellItems;
    property Title: string read FTitle write FTitle;
    property OnExecute: TNotifyEvent read FOnExecute write FOnExecute;
    property OnFileOkClick: TFileDialogCloseEvent read FOnFileOkClick write FOnFileOkClick;
    property OnFolderChange: TNotifyEvent read FOnFolderChange write FOnFolderChange;
    property OnFolderChanging: TFileDialogFolderChangingEvent read FOnFolderChanging write FOnFolderChanging;
    property OnOverwrite: TFileDialogOverwriteEvent read FOnOverwrite write FOnOverwrite;
    property OnSelectionChange: TNotifyEvent read FOnSelectionChange write FOnSelectionChange;
    property OnShareViolation: TFileDialogShareViolationEvent read FOnShareViolation write FOnShareViolation;
    property OnTypeChange: TNotifyEvent read FOnTypeChange write FOnTypeChange;
  end;

  TCustomFileOpenDialog = class(TCustomFileDialog)
  strict protected
    function CreateFileDialog: IFileDialog; override;
    function GetResults: HResult; override;
  protected
    function SelectionChange: HResult; override;
  end;

  TCustomFileSaveDialog = class(TCustomFileDialog)
  strict protected
    function CreateFileDialog: IFileDialog; override;
  end;

  TFileDialogWrapper = class(TObject)
  private
    procedure AssignFileTypes;
    procedure AssignOptions;
    function GetFileName: TFileName;
    function GetHandle: HWND;
    procedure HandleShareViolation(Sender: TObject;
      var Response: TFileDialogShareViolationResponse);
    procedure OnFileOkEvent(Sender: TObject; var CanClose: Boolean);
    procedure OnFolderChangeEvent(Sender: TObject);
    procedure OnSelectionChangeEvent(Sender: TObject);
    procedure OnTypeChangeEvent(Sender: TObject);
  protected
    FFileDialog: TCustomFileDialog;
    FOpenDialog: TOpenDialog;
    function CreateFileDialog: TCustomFileDialog; virtual; abstract;
  public
    constructor Create(OpenDialog: TOpenDialog);
    destructor Destroy; override;
    function Execute(ParentWnd: HWND): Boolean;
    property FileName: TFileName read GetFileName;
    property Handle: HWND read GetHandle;
  end;

  TFileOpenDialogWrapper = class(TFileDialogWrapper)
  private
    procedure OnExecuteEvent(Sender: TObject);
  protected
    function CreateFileDialog: TCustomFileDialog; override;
  end;

  TFileSaveDialogWrapper = class(TFileDialogWrapper)
  protected
    function CreateFileDialog: TCustomFileDialog; override;
  end;

{$endif WINDOWS}

implementation

{$ifdef WINDOWS}

uses
  Forms, ActiveX, StrUtils;

resourcestring
  SWindowsVistaRequired = '%s requires Windows Vista or later';

const
  shell32 = 'shell32.dll';
  SID_IShellItem = '{43826D1E-E718-42EE-BC55-A1E261C37BFE}';
  SID_IFileSaveDialog = '{84BCCD23-5FDE-4CDB-AEA4-AF64B83D78AB}';
  SID_IFileOpenDialog = '{D57C7288-D4AD-4768-BE02-9D969532D960}';

{%region Helpers}
function WStrLCopy(Dest: PWideChar; const Source: PWideChar; MaxLen: Cardinal): PWideChar;
var
  Src : PWideChar;
begin
  Result := Dest;
  Src := Source;
  while (Src^ <> #$00) and (MaxLen > 0) do
  begin
    Dest^ := Src^;
    Inc(Src);
    Inc(Dest);
    Dec(MaxLen);
  end;
  Dest^ := #$00;
end;

function WStrPLCopy(Dest: PWideChar; const Source: WideString; MaxLen: Cardinal): PWideChar;
begin
  Result := WStrLCopy(Dest, PWideChar(Source), MaxLen);
end;

procedure FreeCoTaskMemStr(const S: LPCWSTR); inline;
begin
  CoTaskMemFree(S);
end;

function AllocCoTaskMemStr(const S: string): LPCWSTR;
var
  Len: Integer;
begin
  Len := Length(S);
  Result := CoTaskMemAlloc((Len * SizeOf(WideChar)) + SizeOf(WideChar));
  Result := WStrPLCopy(Result, WideString(S), Len);
end;

function WStrPtrToString(const S: LPCWSTR): string; inline;
begin
  Result := S;
end;

type
  PTaskWindow = ^TTaskWindow;
  TTaskWindow = record
    Next: PTaskWindow;
    Window: HWnd;
  end;
  TTaskWindowType = PTaskWindow;

var
  TaskActiveWindow: HWnd = 0;
  {%H-}DisablingWindows: Boolean = False;
  TaskWindowList: TTaskWindowType = nil;

function DoDisableWindow(Window: HWnd; {%H-}Data: LPARAM): Bool; stdcall;
var
  P: TTaskWindowType;
begin
  if (Window <> TaskActiveWindow) and IsWindowVisible(Window) and
    IsWindowEnabled(Window) then
  begin
    New(P);
    P^.Next := TaskWindowList;
    P^.Window := Window;
    TaskWindowList := P;
    EnableWindow(Window, False);
  end;
  Result := True;
end;

procedure EnableTaskWindows(WindowList: TTaskWindowList);
var
  P: TTaskWindowType;
begin
  while WindowList <> nil do
  begin
    P := WindowList;
    if IsWindow(P^.Window) then EnableWindow(P^.Window, True);
    WindowList := P^.Next;
    Dispose(P);
  end;
end;

function DisableTaskWindows(ActiveWindow: HWnd): TTaskWindowList;
var
  SaveActiveWindow: HWND;
  SaveWindowList: TTaskWindowType;
begin
  Result := nil;
  SaveActiveWindow := TaskActiveWindow;
  SaveWindowList := TaskWindowList;
  TaskActiveWindow := ActiveWindow;
  TaskWindowList := nil;
  try
    DisablingWindows := True;
    try
      EnumThreadWindows(GetCurrentThreadID, @DoDisableWindow, 0);
      Result := TaskWindowList;
    except
      EnableTaskWindows(TaskWindowList);
      raise;
    end;
  finally
    DisablingWindows := False;
    TaskWindowList := SaveWindowList;
    TaskActiveWindow := SaveActiveWindow;
  end;
end;

function SHCreateItemFromParsingName(
  pszPath: LPCWSTR; const pbc: IBindCtx; const riid: TIID; out ppv): HResult; stdcall;
  external shell32 name 'SHCreateItemFromParsingName';

{%endregion}

{%region TFavoriteLinkItem}
function TFavoriteLinkItem.GetDisplayName: string;
begin
  if FLocation <> '' then
    Result := FLocation
  else
    Result := inherited GetDisplayName;
end;
{%endregion}

{%region TFavoriteLinkItemsEnumerator}
constructor TFavoriteLinkItemsEnumerator.Create(ACollection: TFavoriteLinkItems);
begin
  inherited Create;
  FIndex := -1;
  FCollection := ACollection;
end;

function TFavoriteLinkItemsEnumerator.GetCurrent: TFavoriteLinkItem;
begin
  Result := FCollection[FIndex];
end;

function TFavoriteLinkItemsEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FCollection.Count - 1;
  if Result then
    Inc(FIndex);
end;
{%endregion}

{%region TFavoriteLinkItems}
function TFavoriteLinkItems.Add: TFavoriteLinkItem;
begin
  Result := TFavoriteLinkItem(inherited Add);
end;

function TFavoriteLinkItems.GetEnumerator: TFavoriteLinkItemsEnumerator;
begin
  Result := TFavoriteLinkItemsEnumerator.Create(Self);
end;

function TFavoriteLinkItems.GetItem(Index: Integer): TFavoriteLinkItem;
begin
  Result := TFavoriteLinkItem(inherited GetItem(Index));
end;

procedure TFavoriteLinkItems.SetItem(Index: Integer; const Value: TFavoriteLinkItem);
begin
  inherited SetItem(Index, Value);
end;
{%endregion}

{%region TFileTypeItem}
constructor TFileTypeItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FDisplayNameWStr := nil;
  FFileMaskWStr := nil;
end;

destructor TFileTypeItem.Destroy;
begin
  if FDisplayNameWStr <> nil then
    FreeCoTaskMemStr(FDisplayNameWStr);
  if FFileMaskWStr <> nil then
    FreeCoTaskMemStr(FFileMaskWStr);
  inherited;
end;

function TFileTypeItem.GetDisplayNameWStr: LPCWSTR;
begin
  if FDisplayNameWStr <> nil then
    FreeCoTaskMemStr(FDisplayNameWStr);
  FDisplayNameWStr := AllocCoTaskMemStr(FDisplayName);
  Result := FDisplayNameWStr;
end;

function TFileTypeItem.GetDisplayName: string;
begin
  if FDisplayName <> '' then
    Result := FDisplayName
  else
    Result := inherited GetDisplayName;
end;

function TFileTypeItem.GetFileMaskWStr: LPCWSTR;
begin
  if FFileMaskWStr <> nil then
    FreeCoTaskMemStr(FFileMaskWStr);
  FFileMaskWStr := AllocCoTaskMemStr(FFileMask);
  Result := FFileMaskWStr;
end;
{%endregion}

{%region TFileTypeItems}
function TFileTypeItems.Add: TFileTypeItem;
begin
  Result := TFileTypeItem(inherited Add);
end;

function TFileTypeItems.FilterSpecArray: TComdlgFilterSpecArray;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
  begin
    Result[I].pszName := Items[I].DisplayNameWStr;
    Result[I].pszSpec := Items[I].FileMaskWStr;
  end;
end;

function TFileTypeItems.GetItem(Index: Integer): TFileTypeItem;
begin
  Result := TFileTypeItem(inherited GetItem(Index));
end;

procedure TFileTypeItems.SetItem(Index: Integer; const Value: TFileTypeItem);
begin
  inherited SetItem(Index, Value);
end;
{%endregion}

{%region TFileDialogEvents}
type
  TFileDialogEvents = class(TInterfacedObject, IFileDialogEvents)
  private
    FFileDialog: TCustomFileDialog;
    FRetrieveHandle: Boolean;
  public
    constructor Create(AFileDialog: TCustomFileDialog);
    { IFileDialogEvents }
    function OnFileOk({%H-}pfd: IFileDialog): HResult; stdcall;
    function OnFolderChanging({%H-}pfd: IFileDialog; psiFolder: IShellItem): HResult; stdcall;
    function OnFolderChange({%H-}pfd: IFileDialog): HResult; stdcall;
    function OnSelectionChange({%H-}pfd: IFileDialog): HResult; stdcall;
    function OnShareViolation({%H-}pfd: IFileDialog; psi: IShellItem;
      pResponse: pFDE_SHAREVIOLATION_RESPONSE): HResult; stdcall;
    function OnTypeChange({%H-}pfd: IFileDialog): HResult; stdcall;
    function OnOverwrite({%H-}pfd: IFileDialog; psi: IShellItem;
      pResponse: pFDE_OVERWRITE_RESPONSE): HResult; stdcall;
  end;

constructor TFileDialogEvents.Create(AFileDialog: TCustomFileDialog);
begin
  inherited Create;
  FFileDialog := AFileDialog;
  FRetrieveHandle := True;
end;

function TFileDialogEvents.OnFileOk(pfd: IFileDialog): HResult; stdcall;
begin
  if Assigned(FFileDialog.OnFileOkClick) then
    Result := FFileDialog.FileOkClick
  else
    Result := E_NOTIMPL;
end;

function TFileDialogEvents.OnFolderChange(pfd: IFileDialog): HResult; stdcall;
begin
  if Assigned(FFileDialog.OnFolderChange) then
    Result := FFileDialog.FolderChange
  else
    Result := E_NOTIMPL;
end;

function TFileDialogEvents.OnFolderChanging(pfd: IFileDialog; psiFolder: IShellItem): HResult; stdcall;
begin
  if Assigned(FFileDialog.OnFolderChanging) then
    Result := FFileDialog.FolderChanging(psiFolder)
  else
    Result := E_NOTIMPL;
end;

function TFileDialogEvents.OnOverwrite(pfd: IFileDialog; psi: IShellItem;
  pResponse: pFDE_OVERWRITE_RESPONSE): HResult; stdcall;
begin
  if Assigned(FFileDialog.OnOverwrite) then
    Result := FFileDialog.Overwrite(psi, pResponse^)
  else
    Result := E_NOTIMPL;
end;

function TFileDialogEvents.OnSelectionChange(pfd: IFileDialog): HResult; stdcall;
begin
  // OnSelectionChange is called when the dialog is opened, use this
  // to retrieve the window handle if OnTypeChange wasn't triggered.
  if FRetrieveHandle then
  begin
    FFileDialog.GetWindowHandle;
    FRetrieveHandle := False;
  end;

  if Assigned(FFileDialog.OnSelectionChange) then
    Result := FFileDialog.SelectionChange
  else
    Result := E_NOTIMPL;
end;

function TFileDialogEvents.OnShareViolation(pfd: IFileDialog; psi: IShellItem;
  pResponse: pFDE_SHAREVIOLATION_RESPONSE): HResult; stdcall;
begin
  if Assigned(FFileDialog.OnShareViolation) then
    Result := FFileDialog.ShareViolation(psi, pResponse^)
  else
    Result := E_NOTIMPL;
end;

function TFileDialogEvents.OnTypeChange(pfd: IFileDialog): HResult; stdcall;
begin
  // OnTypeChange is supposed to always be called when the dialog is
  // opened. In reality it isn't called if you don't assign any FileTypes.
  // Use this to retrieve the window handle, if it's called.
  if FRetrieveHandle then
  begin
    FFileDialog.GetWindowHandle;
    FRetrieveHandle := False;
  end;

  Result := FFileDialog.TypeChange
end;
{%endregion}

{%region TCustomFileDialog}
constructor TCustomFileDialog.Create(AOwner: TComponent);
begin
  inherited;
  FFiles := TStringList.Create;
  FFileTypeIndex := 1;
  FFileTypes := TFileTypeItems.Create(TFileTypeItem);
  FHandle := 0;
  FOptions := [];
  FFavoriteLinks := TFavoriteLinkItems.Create(TFavoriteLinkItem);
  FShellItem := nil;
  FShellItems := nil;
end;

destructor TCustomFileDialog.Destroy;
begin
  FFiles.Free;
  FFileTypes.Free;
  FFavoriteLinks.Free;
  FShellItem := nil;
  FShellItems := nil;
  inherited;
end;

procedure TCustomFileDialog.DoOnExecute;
begin
  if Assigned(FOnExecute) then
    FOnExecute(Self);
end;

function TCustomFileDialog.DoOnFileOkClick: Boolean;
begin
  Result := True;
  if Assigned(FOnFileOkClick) then
    FOnFileOkClick(Self, Result);
end;

procedure TCustomFileDialog.DoOnFolderChange;
begin
  if Assigned(FOnFolderChange) then
    FOnFolderChange(Self);
end;

function TCustomFileDialog.DoOnFolderChanging: Boolean;
begin
  Result := True;
  if Assigned(FOnFolderChanging) then
    FOnFolderChanging(Self, Result);
end;

procedure TCustomFileDialog.DoOnOverwrite(var Response: TFileDialogOverwriteResponse);
begin
  if Assigned(FOnOverwrite) then
    FOnOverwrite(Self, Response);
end;

procedure TCustomFileDialog.DoOnSelectionChange;
begin
  if Assigned(FOnSelectionChange) then
    FOnSelectionChange(Self);
end;

procedure TCustomFileDialog.DoOnShareViolation(var Response: TFileDialogShareViolationResponse);
begin
  if Assigned(FOnShareViolation) then
    FOnShareViolation(Self, Response);
end;

procedure TCustomFileDialog.DoOnTypeChange;
begin
  if Assigned(FOnTypeChange) then
    FOnTypeChange(Self);
end;

function TCustomFileDialog.Execute: Boolean;
begin
  Result := Execute(Application.MainFormHandle);
end;

function TCustomFileDialog.Execute(ParentWnd: HWND): Boolean;
const
  CDialogOptions: array[TFileDialogOption] of DWORD = (
    FOS_OVERWRITEPROMPT, FOS_STRICTFILETYPES, FOS_NOCHANGEDIR,
    FOS_PICKFOLDERS, FOS_FORCEFILESYSTEM, FOS_ALLNONSTORAGEITEMS,
    FOS_NOVALIDATE, FOS_ALLOWMULTISELECT, FOS_PATHMUSTEXIST,
    FOS_FILEMUSTEXIST, FOS_CREATEPROMPT, FOS_SHAREAWARE,
    FOS_NOREADONLYRETURN, FOS_NOTESTFILECREATE, FOS_HIDEMRUPLACES,
    FOS_HIDEPINNEDPLACES, FOS_NODEREFERENCELINKS, FOS_DONTADDTORECENT,
    FOS_FORCESHOWHIDDEN, FOS_DEFAULTNOMINIMODE, FOS_FORCEPREVIEWPANEON);
var
  LWindowList: TTaskWindowList;
  LFocusState: TFocusState;
  LPlace: TFavoriteLinkItem;
  LShellItem: IShellItem;
  LAdviseCookie: DWORD;
  LDialogOptions: Cardinal;
  LDialogEvents: TFileDialogEvents;
  LDialogOption: TFileDialogOption;
  Guid: TGuid;
  Filters: TComdlgFilterSpecArray;
begin
  if Win32MajorVersion < 6 then
    raise EPlatformVersionException.CreateResFmt(@SWindowsVistaRequired, [ClassName]);

  Result := False;
  FDialog := CreateFileDialog;
  if FDialog <> nil then
    try
      with FDialog do
      begin
        // ClientGuid, DefaultExt, FileName, Title, OkButtonLabel, FileNameLabel
        if FClientGuid <> '' then
        begin
          Guid := StringToGUID(FClientGuid);
          SetClientGuid(@Guid);
        end;
        if FDefaultExtension <> '' then
          SetDefaultExtension(PWideChar(WideString(FDefaultExtension)));
        if FFileName <> '' then
          SetFileName(PWideChar(WideString(FFileName)));
        if FFileNameLabel <> '' then
          SetFileNameLabel(PWideChar(WideString(FFileNameLabel)));
        if FOkButtonLabel <> '' then
          SetOkButtonLabel(PWideChar(WideString(FOkButtonLabel)));
        if FTitle <> '' then
          SetTitle(PWideChar(WideString(FTitle)));

        // DefaultFolder
        if FDefaultFolder <> '' then
        begin
          if Succeeded(SHCreateItemFromParsingName(PWideChar(WideString(FDefaultFolder)),
             nil, StringToGUID(SID_IShellItem), LShellItem)) then
            SetFolder(LShellItem);
        end;

        // FileTypes, FileTypeIndex
        if FFileTypes.Count > 0 then
        begin
          Filters := FFileTypes.FilterSpecArray;
          FDialog.SetFileTypes(FFileTypes.Count, @Filters);
          SetFileTypeIndex(FFileTypeIndex);
        end;

        // Options
        LDialogOptions := 0;
        for LDialogOption in Options do
          LDialogOptions := LDialogOptions or CDialogOptions[LDialogOption];
        SetOptions(LDialogOptions);

        // Additional Places
        for LPlace in FFavoriteLinks do
          if Succeeded(SHCreateItemFromParsingName(PWideChar(WideString(LPlace.Location)),
             nil, StringToGUID(SID_IShellItem), LShellItem)) then
            AddPlace(LShellItem, FDAP_BOTTOM);

        // Show dialog and get results
        DoOnExecute;
        LWindowList := DisableTaskWindows(ParentWnd);
        LFocusState := SaveFocusState;
        try
          LDialogEvents := TFileDialogEvents.Create(Self);
          Advise(LDialogEvents, @LAdviseCookie);
          try
            Result := Succeeded(Show(ParentWnd));
            if Result then
              Result := Succeeded(GetResults);
          finally
            Unadvise(LAdviseCookie);
          end;
        finally
          EnableTaskWindows(LWindowList);
          SetActiveWindow(ParentWnd);
          RestoreFocusState(LFocusState);
        end;
      end;
    finally
      FDialog := nil;
    end;
end;

function TCustomFileDialog.FileOkClick: HResult;
const
  CResults: array[Boolean] of HResult = (S_FALSE, S_OK);
begin
  Result := GetResults;
  if Succeeded(Result) then
    Result := CResults[DoOnFileOkClick];
  Files.Clear;
end;

function TCustomFileDialog.FolderChange: HResult;
begin
  FFileName := '';
  Result := FDialog.GetFolder(@FShellItem);
  if Succeeded(Result) then
  begin
    Result := GetItemName(FShellItem, FFileName);
    if Succeeded(Result) then
      DoOnFolderChange;
  end;
  FShellItem := nil;
end;

function TCustomFileDialog.FolderChanging(psiFolder: IShellItem): HResult;
const
  CResults: array[Boolean] of HResult = (S_FALSE, S_OK);
begin
  FFileName := '';
  FShellItem := psiFolder;
  Result := GetItemName(FShellItem, FFileName);
  if Succeeded(Result) then
    Result := CResults[DoOnFolderChanging];
  FShellItem := nil;
end;

function TCustomFileDialog.GetDefaultFolder: string;
begin
  Result := FDefaultFolder;
end;

function TCustomFileDialog.GetFileName: TFileName;
var
  LFileName: TFileName = '';
  pszFileName: LPWSTR;
begin
  if (FDialog <> nil) and
     Succeeded(FDialog.GetFolder(@FShellItem)) and
     Succeeded(GetItemName(FShellItem, LFileName)) and
     Succeeded(FDialog.GetFileName(@pszFileName)) then
  try
    FFileName := IncludeTrailingPathDelimiter(LFileName) + WStrPtrToString(pszFileName);
  finally
    FreeCoTaskMemStr(pszFileName);
  end;
  Result := FFileName;
end;

function TCustomFileDialog.GetFileNames(Items: IShellItemArray): HResult;
var
  I: Integer;
  Count: Cardinal = 0;
  ItemCount: Cardinal = 0;
  LEnumerator: IEnumShellItems = nil;
  LShellItems: array of IShellItem;
begin
  Files.Clear;
  Result := Items.EnumItems(LEnumerator);
  if Succeeded(Result) then
  begin
    Result := Items.GetCount(ItemCount);
    if Succeeded(Result) then
    begin
      SetLength(LShellItems, ItemCount);
      Result := LEnumerator.Next(ItemCount, LShellItems[0], Count);
      if Succeeded(Result) then
        for I := 0 to Count - 1 do
        begin
          GetItemName(LShellItems[I], FFileName);
          Files.Add(FFileName);
        end;
    end;
    if Files.Count > 0 then
      FFileName := Files[0];
  end;
end;

function TCustomFileDialog.GetFiles: TStrings;
begin
  Result := FFiles;
end;

function TCustomFileDialog.GetItemName(Item: IShellItem; var ItemName: TFileName): HResult;
var
  pszItemName: LPCWSTR = nil;
begin
  Result := Item.GetDisplayName(SIGDN_FILESYSPATH{%H-}, pszItemName);
  if Failed(Result) then
    Result := Item.GetDisplayName(SIGDN_NORMALDISPLAY, pszItemName);
  if Succeeded(Result) then
  try
    ItemName := WStrPtrToString(pszItemName);
  finally
    FreeCoTaskMemStr(pszItemName);
  end;
end;

function TCustomFileDialog.GetResults: HResult;
begin
  Result := FDialog.GetResult(@FShellItem);
  if Succeeded(Result) then
  begin
    Result := GetItemName(FShellItem, FFileName);
    FFiles.Clear;
    FFiles.Add(FFileName);
  end;
end;

procedure TCustomFileDialog.GetWindowHandle;
var
  LOleWindow: IOleWindow;
begin
  if Supports(FDialog, IOleWindow, LOleWindow) then
    LOleWindow.GetWindow(FHandle);
end;

function TCustomFileDialog.Overwrite(psiFile: IShellItem; var Response: Longint): HResult;
var
  LResponse: TFileDialogOverwriteResponse;
begin
  FFileName := '';
  LResponse := forAccept;
  FShellItem := psiFile;
  Result := GetItemName(FShellItem, FFileName);
  if Succeeded(Result) then
    DoOnOverwrite(LResponse);
  Response := Cardinal(LResponse);
  FShellItem := nil;
end;

function TCustomFileDialog.SelectionChange: HResult;
begin
  FFileName := '';
  Result := FDialog.GetCurrentSelection(@FShellItem);
  if Succeeded(Result) then
  begin
    Result := GetItemName(FShellItem, FFileName);
    if Succeeded(Result) then
      DoOnSelectionChange;
  end;
  FShellItem := nil;
end;

procedure TCustomFileDialog.SetClientGuid(const Value: string);
begin
  if Value <> FClientGuid then
  begin
    if Value <> '' then
      StringToGUID(Value);
    FClientGuid := Value;
  end;
end;

procedure TCustomFileDialog.SetDefaultFolder(const Value: string);
begin
  if FDefaultFolder <> Value then
    FDefaultFolder := Value;
end;

procedure TCustomFileDialog.SetFileName(const Value: TFileName);
begin
  if Value <> FFileName then
    FFileName := Value;
end;

procedure TCustomFileDialog.SetFileTypes(const Value: TFileTypeItems);
begin
  if Value <> nil then
    FFileTypes.Assign(Value);
end;

procedure TCustomFileDialog.SetFavoriteLinks(const Value: TFavoriteLinkItems);
begin
  if Value <> nil then
    FFavoriteLinks.Assign(Value);
end;

function TCustomFileDialog.ShareViolation(psiFile: IShellItem; var Response: LongInt): HResult;
var
  LResponse: TFileDialogShareViolationResponse;
begin
  FFileName := '';
  LResponse := fsrAccept;
  FShellItem := psiFile;
  Result := GetItemName(FShellItem, FFileName);
  if Succeeded(Result) then
    DoOnShareViolation(LResponse);
  Response := Cardinal(LResponse);
  FShellItem := nil;
end;

function TCustomFileDialog.TypeChange: HResult;
begin
  Result := FDialog.GetFileTypeIndex(@FFileTypeIndex);
  if Assigned(FOnTypeChange) and Succeeded(Result) then
    DoOnTypeChange;
end;
{%endregion}

{%region TCustomFileOpenDialog}
function TCustomFileOpenDialog.CreateFileDialog: IFileDialog;
var
  LGuid: TGUID;
begin
  LGuid := CLSID_FileOpenDialog;
  CoCreateInstance(LGuid, nil, CLSCTX_INPROC_SERVER,
    StringToGUID(SID_IFileOpenDialog), Result);
end;

function TCustomFileOpenDialog.GetResults: HResult;
begin
  if not (fdoAllowMultiSelect in Options) then
    Result := inherited GetResults
  else
  begin
    Result := (Dialog as IFileOpenDialog).GetResults(FShellItems);
    if Succeeded(Result) then
      Result := GetFileNames(FShellItems);
  end;
end;

function TCustomFileOpenDialog.SelectionChange: HResult;
begin
  if not (fdoAllowMultiSelect in Options) then
    Result := inherited SelectionChange
  else
  begin
    Result := (Dialog as IFileOpenDialog).GetSelectedItems(FShellItems);
    if Succeeded(Result) then
    begin
      Result := GetFileNames(FShellItems);
      if Succeeded(Result) then
      begin
        Dialog.GetCurrentSelection(@FShellItem);
        DoOnSelectionChange;
      end;
      FShellItems := nil;
    end;
  end;
end;
{%endregion}

{%region TCustomFileSaveDialog}
function TCustomFileSaveDialog.CreateFileDialog: IFileDialog;
var
  LGuid: TGUID;
begin
  LGuid := CLSID_FileSaveDialog;
  CoCreateInstance(LGuid, nil, CLSCTX_INPROC_SERVER,
    StringToGUID(SID_IFileSaveDialog), Result);
end;
{%endregion}

{%region TFileDialogWrapper}
constructor TFileDialogWrapper.Create(OpenDialog: TOpenDialog);
begin
  inherited Create;
  FOpenDialog := OpenDialog;
  FFileDialog := CreateFileDialog;
end;

destructor TFileDialogWrapper.Destroy;
begin
  FFileDialog.Free;
  inherited;
end;

procedure TFileDialogWrapper.AssignFileTypes;
var
  I, J: Integer;
  FilterStr: string;
begin
  FilterStr := FOpenDialog.Filter;
  J := 1;
  I := AnsiPos('|', FilterStr);
  while I <> 0 do
    with FFileDialog.Filetypes.Add do
    begin
      DisplayName := Copy(FilterStr, J, I - J);
      if not SysLocale.FarEast then
        J := PosEx('|', FilterStr, I + 1)
      else
      begin
        J := AnsiPos('|', Copy(FilterStr, I + 1, MAXINT));
        if J <> 0 then
          J := J + (I + 1) - 1;
      end;
      if J = 0 then
        J := Length(FilterStr) + 1;
      FileMask := Copy(FilterStr, I + 1, J - I - 1);
      Inc(J);

      if not SysLocale.FarEast then
        I := PosEx('|', FilterStr, J)
      else
      begin
        I := AnsiPos('|', Copy(FilterStr, J, MAXINT));
        if I <> 0 then
          I := I + J - 1
        else if J < Length(FilterStr) then
          I := Length(FilterStr) + 1;
      end;
    end;
end;

procedure TFileDialogWrapper.AssignOptions;
const
  CDialogOptionsMap: array[TOpenOption] of TFileDialogOptions = (
    [] {ofReadOnly}, [fdoOverWritePrompt], [] {ofHideReadOnly},
    [fdoNoChangeDir], [] {ofShowHelp}, [fdoNoValidate], [fdoAllowMultiSelect],
    [fdoStrictFileTypes], [fdoPathMustExist], [fdoFileMustExist],
    [fdoCreatePrompt], [fdoShareAware], [fdoNoReadOnlyReturn],
    [fdoNoTestFileCreate], [] {ofNoNetworkButton}, [] {ofNoLongNames},
    [] {ofOldStyleDialog}, [fdoNoDereferenceLinks], [] {ofEnableIncludeNotify},
    [] {ofEnableSizing}, [fdoDontAddToRecent], [fdoForceShowHidden],
    [] {ofViewDetail}, [] {ofAutoPreview});
var
  LOption: TOpenOption;
begin
  for LOption := Low(LOption) to High(LOption) do
    if LOption in FOpenDialog.Options then
      FFileDialog.Options := FFileDialog.Options + CDialogOptionsMap[LOption];
  {if ofExNoPlacesBar in FOpenDialog.OptionsEx then
    FFileDialog.Options := FFileDialog.Options + [fdoHidePinnedPlaces];}
end;

function TFileDialogWrapper.Execute(ParentWnd: HWND): Boolean;
begin
  FFileDialog.DefaultExtension := FOpenDialog.DefaultExt;
  FFileDialog.DefaultFolder := FOpenDialog.InitialDir;
  FFileDialog.FileName := FOpenDialog.FileName;
  FFileDialog.FileTypeIndex := FOpenDialog.FilterIndex;
  FFileDialog.Title := FOpenDialog.Title;
  if Assigned(FOpenDialog.OnCanClose) then
    FFileDialog.OnFileOkClick := @OnFileOkEvent;
  if Assigned(FOpenDialog.OnFolderChange) then
    FFileDialog.OnFolderChange := @OnFolderChangeEvent;
  if Assigned(FOpenDialog.OnSelectionChange) then
    FFileDialog.OnSelectionChange := @OnSelectionChangeEvent;
  if Assigned(FOpenDialog.OnTypeChange) then
    FFileDialog.OnTypeChange := @OnTypeChangeEvent;
  // TOpenDialog/TSaveDialog ignore sharing violations when ofShareAware
  // is set. Assign an event handler to mimic that behavior.
  if ofShareAware in FOpenDialog.Options then
    FFileDialog.OnShareViolation := @HandleShareViolation;
  AssignFileTypes;
  AssignOptions;
  Result := FFileDialog.Execute(ParentWnd);
  if Result then
  begin
    FOpenDialog.FileName := FFileDialog.FileName;
    FOpenDialog.Files.Assign(FFileDialog.Files);
    FOpenDialog.FilterIndex := FFileDialog.FileTypeIndex;
  end;
end;

function TFileDialogWrapper.GetFileName: TFileName;
begin
  Result := FFileDialog.FileName;
end;

function TFileDialogWrapper.GetHandle: HWND;
begin
  Result := FFileDialog.Handle;
end;

procedure TFileDialogWrapper.HandleShareViolation(Sender: TObject;
  var Response: TFileDialogShareViolationResponse);
begin
  Response := fsrAccept;
end;

procedure TFileDialogWrapper.OnFileOkEvent(Sender: TObject; var CanClose: Boolean);
begin
  with FOpenDialog do
  begin
    FileName := FFileDialog.FileName;
    Files.Assign(FFileDialog.Files);
  end;
  FOpenDialog.OnCanClose(FOpenDialog, CanClose);
end;

procedure TFileDialogWrapper.OnFolderChangeEvent(Sender: TObject);
begin
  with FOpenDialog do
  begin
    FileName := FFileDialog.FileName;
    OnFolderChange(FOpenDialog);
  end;
end;

procedure TFileDialogWrapper.OnSelectionChangeEvent(Sender: TObject);
begin
  with FOpenDialog do
  begin
    FileName := FFileDialog.FileName;
    Files.Assign(FFileDialog.Files);
    OnSelectionChange(FOpenDialog);
  end;
end;

procedure TFileDialogWrapper.OnTypeChangeEvent(Sender: TObject);
begin
  with FOpenDialog do
  begin
    FilterIndex := FFileDialog.FileTypeIndex;
    OnTypeChange(FOpenDialog);
  end;
end;
{%endregion}

{%region TFileOpenDialogWrapper}
function TFileOpenDialogWrapper.CreateFileDialog: TCustomFileDialog;
begin
  Result := TCustomFileOpenDialog.Create(nil);
  Result.OnExecute := @OnExecuteEvent;
end;

procedure TFileOpenDialogWrapper.OnExecuteEvent(Sender: TObject);
var
  LOptions: Cardinal;
begin
  if FOpenDialog.ClassName = 'TOpenPictureDialog' then // do not localize
  begin
    FFileDialog.Dialog.GetOptions(@LOptions);
    LOptions := LOptions or FOS_FORCEPREVIEWPANEON;
    FFileDialog.Dialog.SetOptions(LOptions);
  end;
end;
{%endregion}

{%region TFileSaveDialogWrapper}
function TFileSaveDialogWrapper.CreateFileDialog: TCustomFileDialog;
begin
  Result := TCustomFileSaveDialog.Create(nil);
end;
{%endregion}

{$endif WINDOWS}

end.

