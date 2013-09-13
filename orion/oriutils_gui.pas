unit OriUtils_Gui;

{$mode objfpc}{$H+}

interface

uses
  Classes, Graphics, Controls, ExtCtrls, Forms;

procedure SetDefaultColor(AControl: TWinControl; AColor: TColor = clNone);
procedure ScaleDPI(AControl: TControl; FromDPI: Integer);

{%region Message Dialogs}
procedure MessageDlg(const S: String);
procedure ErrorDlg(const S: String);
procedure ErrorDlg(const S: String; Args: array of const);
function ConfirmDlg(const S: String): Boolean;
function ConfirmDlg(const S: String; Args: array of const): Boolean;
{%endregion}

{%region Form Helpers}
procedure SaveFormPos(AForm: TForm; var ASavedPos: Longword);
procedure SaveFormSize(AForm: TForm; var ASavedSize: Longword);
procedure SaveFormSizePos(AForm: TForm; var ASavedSize, ASavedPos: Longword);
procedure RestoreFormPos(AForm: TForm; ASavedPos: Longword);
procedure RestoreFormSize(AForm: TForm; ASavedSize: Longword);
procedure RestoreFormSizePos(AForm: TForm; ASavedSize, ASavedPos: Longword);
procedure SetPosNoOverlap(AForm: TForm; X, Y: Integer; Position: TPosition = poDesigned); overload;
procedure SetPosNoOverlap(AForm: TForm; X, Y, W, H: Integer; Position: TPosition = poDesigned); overload;
{%endregion}

{%region File Dialogs}
type
  TFileDialogParams = record
    Title: String;
    Filters: String;
    FilterIndex: Integer;
    FileName: String;
    FileNames: TStrings;
    InitialDir: String;
    MultiSelect: Boolean;
    DefaultExt: String;
  end;

function OpenFileDialog(var Params: TFileDialogParams): Boolean;
{%endregion}

implementation

uses
  {$ifdef WINDOWS}
  Windows, CommDlg,
  {$endif}
  SysUtils, Dialogs, LCLIntf, LazUTF8,
  OriStrings;

{%region Controls}
// Sets a default theme-aware color for TPanels. On some environments
// (for example on Ubuntu Unity) the default window color is not clBtnFace.
// But TPanel has clBtnFace color when we set clDefault for it.
procedure SetDefaultColor(AControl: TWinControl; AColor: TColor = clNone);
var
  I: Integer;
  Control: TControl;
begin
  if AColor = clNone then
    AColor := AControl.GetDefaultColor(dctBrush);
  if AControl is TCustomPanel then
     AControl.Color := AColor;
  for I := 0 to AControl.ControlCount-1 do
  begin
    Control := AControl.Controls[I];
    if Control is TWinControl then
        SetDefaultColor(TWinControl(Control), AColor);
  end;
end;

// Scales a control and its children according to current screen dpi.
// From here: http://wiki.lazarus.freepascal.org/High_DPI
procedure ScaleDPI(AControl: TControl; FromDPI: Integer);
var
  I: Integer;
  Control: TWinControl;
begin
  if Screen.PixelsPerInch = FromDPI then
    Exit;

  with AControl do
  begin
    Left := ScaleX(Left, FromDPI);
    Top := ScaleY(Top, FromDPI);
    Width := ScaleX(Width, FromDPI);
    Height := ScaleY(Height, FromDPI);
  end;

  if AControl is TWinControl then
  begin
    Control := TWinControl(AControl);
    if Control.ControlCount = 0 then
      Exit;

    with Control.ChildSizing do
    begin
      HorizontalSpacing := ScaleX(HorizontalSpacing, FromDPI);
      LeftRightSpacing := ScaleX(LeftRightSpacing, FromDPI);
      TopBottomSpacing := ScaleY(TopBottomSpacing, FromDPI);
      VerticalSpacing := ScaleY(VerticalSpacing, FromDPI);
    end;

    for I := 0 to Control.ControlCount - 1 do
      ScaleDPI(Control.Controls[I], FromDPI);
  end;
end;
{%endregion}

{%region Message Dialogs}
procedure MessageDlg(const S: String);
begin
  Dialogs.MessageDlg(Application.Title, S, mtInformation, [mbOK], '');
end;

procedure ErrorDlg(const S: String);
begin
  Dialogs.MessageDlg(Application.Title, S, mtError, [mbOK], '');
end;

procedure ErrorDlg(const S: String; Args: array of const);
begin
  ErrorDlg(Format(S, Args));
end;

function ConfirmDlg(const S: String): Boolean;
begin
  Result := Dialogs.MessageDlg(Application.Title, S, mtConfirmation, mbYesNo, '') = mrYes;
end;

function ConfirmDlg(const S: String; Args: array of const): Boolean;
begin
  Result := ConfirmDlg(Format(S, Args));
end;
{%endregion}

{%region Form Helpers}
procedure SaveFormPos(AForm: TForm; var ASavedPos: Longword);
begin
  with AForm do ASavedPos := MakeLong(Left, Top);
end;

procedure SaveFormSize(AForm: TForm; var ASavedSize: Longword);
begin
  with AForm do ASavedSize := MakeLong(Width, Height);
end;

procedure SaveFormSizePos(AForm: TForm; var ASavedSize, ASavedPos: Longword);
begin
  SaveFormPos(AForm, ASavedPos);
  SaveFormSize(AForm, ASavedSize);
end;

procedure RestoreFormPos(AForm: TForm; ASavedPos: Longword);
begin
  with AForm do
    if ASavedPos <> 0 then
    begin
      Position := poDesigned;
      Left := Lo(ASavedPos);
      Top := Hi(ASavedPos);
    end
    else Position := poScreenCenter
end;

procedure RestoreFormSize(AForm: TForm; ASavedSize: Longword);
begin
  with AForm do
    if ASavedSize <> 0 then
    begin
      Width := Lo(ASavedSize);
      Height := Hi(ASavedSize);
    end;
end;

procedure RestoreFormSizePos(AForm: TForm; ASavedSize, ASavedPos: Longword);
begin
  RestoreFormSize(AForm, ASavedSize);
  RestoreFormPos(AForm, ASavedPos);
end;

procedure SetPosNoOverlap(AForm: TForm; X, Y: Integer; Position: TPosition = poDesigned);
begin
  SetPosNoOverlap(AForm, X, Y, AForm.Width, AForm.Height, Position)
end;

procedure SetPosNoOverlap(AForm: TForm; X, Y, W, H: Integer; Position: TPosition = poDesigned);
const
  SafeCounter = 100;
var
  FormClass: TClass;
  I, J, DW, DH: Integer;
begin
  DW := Screen.DesktopWidth;
  DH := Screen.DesktopHeight;
  if Position = poDesktopCenter then
  begin
    X := (DW - W) div 2;
    Y := (DH - H) div 2;
  end
  else if Position = poScreenCenter then
    with Screen.WorkAreaRect do
    begin
      X := (Right - Left - W) div 2;
      Y := (Bottom - Top - H) div 2;
    end;
  I := 0; J := 0;
  FormClass := AForm.ClassType;
  while (I < Screen.FormCount-1) and (J < SafeCounter) do
  begin
    if (Screen.Forms[I] is FormClass) and (Screen.Forms[I] <> AForm) then
      if (X = Screen.Forms[I].Left) and (Y = Screen.Forms[I].Top) then
      begin
        Inc(X, 40); if X >= DW then X := 0;
        Inc(Y, 40); if Y >= DH then Y := 0;
        I := -1;
      end;
    Inc(I);
    Inc(J);
  end;
  AForm.SetBounds(X, Y, W, H);
end;
{%endregion}

{%region File Dialogs}
{$ifdef WINDOWS}
type TWinApiFileDlgType = (wadtOpen, wadtSave);

function WinApiFileDialog(var Params: TFileDialogParams; DlgType: TWinApiFileDlgType): Boolean;
var
  fnStruct: TOpenFileName;
  szFile: array[0..MAX_PATH] of Char;

  procedure ParseMultipleFileNames;
  var
    I: Integer;
    Index: Integer = 0;
    CurDir: String = '';
    FileName: String;
  begin
    for I := 0 to MAX_PATH do
      if szFile[I] = #0 then
      begin
        if (I > 0) and (szFile[I-1] = #0) then // #0#0 ends the string
        begin
          if (Params.FileNames.Count = 0) and (CurDir <> '') then // single file is selected
            if FileExists(CurDir) then
               Params.FileNames.Add(SysToUTF8(CurDir));
          exit;
        end;
        if CurDir <> '' then
        begin
          FileName := PChar(@(szFile[Index]));
          FileName := IncludeTrailingPathDelimiter(CurDir) + FileName;
          if FileExists(FileName) then
            Params.FileNames.Add(SysToUTF8(FileName));
        end
        else CurDir := szFile;
        Index := I+1;
      end;
  end;

begin
  Result := False;
  FillChar(fnStruct{%H-}, SizeOf(TOpenFileName), 0);
  with fnStruct do
  begin
    hwndOwner := Application.MainFormHandle;
    lStructSize := SizeOf(TOpenFileName);
    lpstrFile := {%H-}szFile;
    StrPCopy(lpstrFile, UTF8ToSys(Params.FileName));
    nMaxFile := Length(szFile);
    lpstrFilter := PChar(ReplaceChar(Params.Filters, '|', #0) + #0#0);
    nFilterIndex := Params.FilterIndex;
    if Params.Title <> '' then
      lpstrTitle := PChar(Params.Title);
    if Params.InitialDir <> '' then
      lpstrInitialDir := PChar(Params.InitialDir);
    if Params.DefaultExt <> '' then
      lpstrDefExt := PChar(Params.DefaultExt);
    if Params.MultiSelect then
      Flags := Flags or OFN_ALLOWMULTISELECT;
    Flags := Flags or OFN_ENABLESIZING or OFN_EXPLORER;
  end;
  case DlgType of
    wadtOpen:
    begin
      with fnStruct do
        Flags := Flags or OFN_FILEMUSTEXIST or OFN_PATHMUSTEXIST;
      Result := GetOpenFileName(@fnStruct);
      if Result then
      begin
        if Params.MultiSelect and Assigned(Params.FileNames) then
        begin
          ParseMultipleFileNames;
          if Params.FileNames.Count > 0 then
            Params.FileName := Params.FileNames[0];
        end
        else Params.FileName := SysToUTF8(StrPas(szFile));
        Params.FilterIndex := fnStruct.nFilterIndex;
      end;
    end;
    wadtSave:
    begin
      Result := GetSaveFileName(@fnStruct);
      if Result then
      begin
        Params.FileName := SysToUTF8(StrPas(szFile));
        Params.FilterIndex := fnStruct.nFilterIndex;
      end;
    end;
  end;
end;
{$endif}

function OpenFileDialog(var Params: TFileDialogParams): Boolean;
begin
  if Assigned(Params.FileNames) then
    Params.FileNames.Clear;
{$ifdef WINDOWS}
  Result := WinApiFileDialog(Params, wadtOpen);
{$else}
  with TOpenDialog.Create(nil) do
  try
    if Params.Title <> '' then
      Title := Params.Title;
    Filter := Params.Filters;
    FileName := Params.FileName;
    if Params.MultiSelect then
      Options := Options + [ofAllowMultiSelect];
    Options := Options + [ofFileMustExist, ofPathMustExist];
    FilterIndex := Params.FilterIndex;
    if Params.InitialDir <> '' then
      InitialDir := Params.InitialDir;
    if Params.DefaultExt <> '' then
      DefaultExt := PChar(Params.DefaultExt);
    Result := Execute;
    if Result then
    begin
      Params.FilterIndex := FilterIndex;
      Params.FileName := FileName;
      if Assigned(Params.FileNames) then
      begin
        if Params.MultiSelect
          then Params.FileNames.Assign(Files)
          else Params.FileNames.Add(FileName);
      end;
    end;
  finally
    Free;
  end;
{$endif}
end;


{%endregion}
end.

