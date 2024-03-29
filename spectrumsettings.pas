unit SpectrumSettings;

interface

uses
//  Windows, Classes, Controls, Graphics, Forms, Dialogs, ActnList, StdCtrls,
//  Grids,
//  TB2Toolbar,
//  OriUtils,
//  PropSaver, OriCombos, OriUtilsTChart
  Classes,
  OriIniFile,
  SpectrumTypes;

type

//------------------------------------------------------------------------------
  // ��� ���������� ������ ����� ������������ ���������
  // ����� ������������ ��� ������������� �������� �� �����
  // (��������, ������� ��� �������� �����)
  TPreferences = class
  public
    LoadDefChartSetting: Boolean; // ��������� ��������� �������� �� ���������
    AnimatedChartZoom: Boolean;   // ������������� ��������������� ��������
    AxisDblClickLimits: Boolean;  // ������� ���� �� ��� - �������, ����� ��������
    ScrollAxesByMouse: Boolean;   // ������������ ��� �����
    NoConfirmDelete: Boolean;     // �� ������������ �������� ��������
    SelectJustAdded: Boolean;     // �������� ����������� ������� c���������

    // PlotReaders settings
    ValueSeparators: String;
    DecSeparator: TDecimalSeparator;
    SkipFirstLines: Integer;
    OneColumnFirst: TValue;
    OneColumnInc: TValue;
    TablePreviewLines: Integer;
    RSReadLowValues: Boolean;
    RSFileVersion: Byte;

    // ������� ������� ������ ���������, ���������������� � ������� ����
    // ������������ ���������� ������, ����������� ������������ ���������,
    // ����� ��������� ������ � ������ ���������� ���� ���� �����������
    //scSelectAll: TShortCut;
    //scSelectNone: TShortCut;
    //scSelectInv: TShortCut;
    //scEditCopy: TShortCut;

    // ��� �������� ��� �����������
    ExportParams: record
    //  ChartCopyFormat: TCopyImageFormat;
      DecSeparator: TDecimalSeparator;
      LineDelimiter: TLineDelimiter;
      ValueDelimiter: TValueDelimiter;
      LineDelimSpec: String;
      ValueDelimSpec: String;
    end;

    // States
    ProjectOpenCurDir: String;
    ProjectOpenFilter: Integer;

    GraphsOpenCurDir: String;
    GraphsOpenFilter: Integer;     
    GraphsOpenFilterTable: Integer;

    TitlePresets: TStringList; // Templates of axes and chart titles

    constructor Create;
    destructor Destroy; override;
    procedure Load(Ini: TOriIniFile);
    procedure Save(Ini: TOriIniFile);
    procedure LoadStates(Ini: TOriIniFile);
    procedure SaveStates(Ini: TOriIniFile);
  end;

{%region Program States}
type
  TSaveStateProcedure = procedure (Ini: TOriIniFile);
  TSaveStateProcedures = array of TSaveStateProcedure;

procedure RegisterSaveStateProc(AProc: TSaveStateProcedure);
procedure SaveAllStates(Ini: TOriIniFile);
{%endregion}

var
  Preferences: TPreferences;

{
//------------------------------------------------------------------------------
// ������� ����������
type
  TTemplateKind = (tkNone, tkAxesNames, tkGraphTitles);

//------------------------------------------------------------------------------
// ������������ ����������
type
  TKeyboardShortCut = record
    Action: TAction;
    ShortCut: TShortCut;
  end;
  TKeyboardShortCutArray = array of TKeyboardShortCut;

procedure AssignShortCutsToMenus(var ShortCuts: TKeyboardShortCutArray; MenuBar: TTBCustomToolbar);
procedure ResetFactoryDefaultShortCuts(var ShortCuts: TKeyboardShortCutArray; ActionList: TActionList);
procedure WriteShortCuts(ASaver: TPropSaver; const ASection: String; var ShortCuts: TKeyboardShortCutArray);
procedure ReadShortCuts(ASaver: TPropSaver; const ASection: String; var ShortCuts: TKeyboardShortCutArray; ActionList: TActionList);

var
  KeyboardShortCuts: TKeyboardShortCutArray;
  KeyboardShortCutsReserved: TKeyboardShortCutArray;

//------------------------------------------------------------------------------
// ���� �����/������ ������
type
  TMultiLineOpt = (mloNone, mloNl, mloNlCr);
  TTextFileViewOption = (
    tfvFileNameInCaption,      // ���������� ��� ����� � ��������� ����
    tfvFileNamePanel,          // ���������� ��� ����� � ����������� ����
    tfvCaptionContainsFileName // �������� Caption �������� ��� �����
  );
  TTextFileViewOptions = set of TTextFileViewOption;
function EnterText(const ACaption: String; var Value: String;
  Options: TMultiLineOpt = mloNlCr; Templates: TTemplateKind = tkNone): Boolean; overload;
function EnterText(const ACaption: String; Value: TStrings; Templates: TTemplateKind = tkNone): Boolean; overload;
//function EnterString(const ACaption: String; var Value: String): Boolean;
//function EnterStringDK(const ACaptionKey: String; var Value: String): Boolean;
procedure TextView(const ACaption, AFileName: String; AOptions: TTextFileViewOptions = []); overload;
procedure TextView(const ACaption: String; AStream: TStream; AOptions: TTextFileViewOptions = []); overload;

//------------------------------------------------------------------------------
// �������� �������

function OpenGraphsDialog(AFileNames: TStrings; var AFilterIndex: Integer): Boolean;
function OpenTableDialog(var AFileName: String): Boolean;
function OpenFolderDialog: Boolean;

//------------------------------------------------------------------------------

procedure SaveFilePacked(const Data, FileName: String);

// ���������� ������ ����� ��� ������ ��������.
function GetScriptPaths: TStringArray;
}
//------------------------------------------------------------------------------
const
  CSomeStates = 'STT_SOMESTATES';
  //APP_MAIL = 'spectrum@orion-project.org';
  //APP_HOME = 'www.spectrum.orion-project.org';
  //PATH_SCRIPTS = 'scripts';
  //ACTN_HIDDEN_CTGR = 'Reserved';
  //ACTN_DEBUG_CMD = 999;

implementation

uses
//  SysUtils, Menus, StrUtils, Clipbrd, ComCtrls, ZLib, WinInet,
  OriUtils;
//  WinTextView, WinEnterText, PlotReaders;

{%region TPreferences}
{����������� ��������� ��������� ���� �������� �� ���������.
 �������: ��������������� ����� Preferences.Load ���������
 ���������� � Load �� ����������� (����� ����, ����� DKLang
 �� ����� ���������) � ����� Preferences �������� ������,
 � �� ����������� ����������. ����� ������ �� ���� ��� �������������
 ������ ��������� ���� �� ������� (�� ����, ������� �� �������
 ������� ��� �� �������, ��� ��� ����� ������, ���� ����� �� �������� )}
constructor TPreferences.Create;
begin
  TitlePresets := TStringList.Create;
  OneColumnFirst := 0;
  OneColumnInc := 1;
end;

destructor TPreferences.Destroy;
begin
  TitlePresets.Free;
end;

procedure TPreferences.Load(Ini: TOriIniFile);
begin
  with Ini do
  begin
    Section             := CMainSection;
    LoadDefChartSetting := ReadBool('LoadDefChartSetting', False);
    AxisDblClickLimits  := ReadBool('AxisDblClickShowLimits', False);
    ScrollAxesByMouse   := ReadBool('ScrollAxesByMouse', False);
    AnimatedChartZoom   := ReadBool('AnimatedChartZoom', True);
    NoConfirmDelete     := ReadBool('NoConfirmDelete', False);
    SelectJustAdded     := ReadBool('SelectJustAdded', True);

    Section                         := 'EXPORT_PARAMS';
    //ExportParams.ChartCopyFormat    := TCopyImageFormat(ReadInteger('ChartCopyFormat', Ord(cifWmf)));
    ExportParams.DecSeparator       := TDecimalSeparator(ReadInteger('DecimalSeparator', Ord(dsSystem)));
    ExportParams.LineDelimiter      := TLineDelimiter(ReadInteger('LineDelimiter', Ord(ldWindows)));
    ExportParams.ValueDelimiter     := TValueDelimiter(ReadInteger('ValueDelimiter', Ord(vdTab)));
    ExportParams.LineDelimSpec      := ReadString('LineDelimiterSpec', '');
    ExportParams.ValueDelimSpec     := ReadString('ValueDelimiterSpec', '');

    // PlotReader settings
    Section           := 'DATA_READERS';
    ValueSeparators   := ReadString('ValueSeparators', '');
    DecSeparator      := TDecimalSeparator(ReadInteger('DecimalSeparator', Ord(dsAuto)));
    SkipFirstLines    := ReadInteger('SkipFirstLines', 0);
    OneColumnFirst    := ReadFloat('OneColumnFirst', 1);
    OneColumnInc      := ReadFloat('OneColumnInc', 1);
    TablePreviewLines := ReadInteger('PreviewLineCountTable', 25);
    RSReadLowValues   := ReadBool('RSReadLowValues', False);
    RSFileVersion     := ReadInteger('RSFileVersion', 0);
  end;
end;

procedure TPreferences.Save(Ini: TOriIniFile);
begin
  with Ini do
  begin
    Section := CMainSection;
    WriteBool('LoadDefChartSetting', LoadDefChartSetting);
    WriteBool('AxisDblClickShowLimits', AxisDblClickLimits);
    WriteBool('ScrollAxesByMouse', ScrollAxesByMouse);
    WriteBool('AnimatedChartZoom', AnimatedChartZoom);
    WriteBool('NoConfirmDelete', NoConfirmDelete);
    WriteBool('SelectJustAdded', SelectJustAdded);

    // export
    Section := 'EXPORT_PARAMS';
    //WriteInteger('ChartCopyFormat', Ord(ExportParams.ChartCopyFormat));
    WriteInteger('DecimalSeparator', Ord(ExportParams.DecSeparator));
    WriteInteger('LineDelimiter', Ord(ExportParams.LineDelimiter));
    WriteInteger('ValueDelimiter', Ord(ExportParams.ValueDelimiter));
    WriteString('LineDelimiterSpec', ExportParams.LineDelimSpec);
    WriteString('ValueDelimiterSpec', ExportParams.ValueDelimSpec);

    // PlotReader settings
    Section := 'DATA_READERS';
    WriteString('ValueSeparators', ValueSeparators);
    WriteInteger('DecimalSeparator', Ord(DecSeparator));
    WriteInteger('SkipFirstLines', SkipFirstLines);
    WriteFloat('OneColumnFirst', OneColumnFirst);
    WriteFloat('OneColumnInc', OneColumnInc);
    WriteInteger('PreviewLineCountTable', TablePreviewLines);
  end;
end;

procedure TPreferences.LoadStates(Ini: TOriIniFile);
begin
  with Ini do
  begin
    Section                 := CSomeStates;
    ProjectOpenCurDir       := EnsurePath(ReadString('ProjectOpenCurDir', ''));
    ProjectOpenFilter       := ReadInteger('ProjectOpenFilter', 1);
    GraphsOpenCurDir        := EnsurePath(ReadString('GraphsOpenCurDir', ''));
    GraphsOpenFilter        := ReadInteger('GraphsOpenFilter', 1);
    GraphsOpenFilterTable   := ReadInteger('GraphsOpenFilterTable', 1);

    //ReadTextSection('TITLE_PRESETS', TitlePresets);
  end;
end;

procedure TPreferences.SaveStates(Ini: TOriIniFile);
begin
  with Ini do
  begin
    Section := CSomeStates;
    WriteString('ProjectOpenCurDir', ProjectOpenCurDir);
    WriteInteger('ProjectOpenFilter', ProjectOpenFilter);
    WriteString('GraphsOpenCurDir', GraphsOpenCurDir);
    WriteInteger('GraphsOpenFilter', GraphsOpenFilter);
    WriteInteger('GraphsOpenFilterTable', GraphsOpenFilterTable);

    //WriteTextSection('TITLE_PRESETS', TitlePresets);
  end;
end;
{%endregion TPreferences}

{%region Program States}
var
  SaveStateProcs: TSaveStateProcedures;

procedure RegisterSaveStateProc(AProc: TSaveStateProcedure);
var L: Integer;
begin
  for L := 0 to Length(SaveStateProcs)-1 do
    if Addr(AProc) = Addr(SaveStateProcs[L]) then Exit;
  L := Length(SaveStateProcs);
  SetLength(SaveStateProcs, L+1);
  SaveStateProcs[L] := AProc;
end;

procedure SaveAllStates(Ini: TOriIniFile);
var I: Integer;
begin
  for I := 0 to Length(SaveStateProcs)-1 do SaveStateProcs[I](Ini);
end;
{%endregion}


//{$region 'Shortcuts'}
//procedure WriteShortCuts(ASaver: TPropSaver; const ASection: String;
//  var ShortCuts: TKeyboardShortCutArray);
//var
//  I: Integer;
//  Strs: TStringList;
//begin
//  ASaver.DeleteSection(ASection);
//  ASaver.Section := ASection;
//  Strs := TStringList.Create;
//  try
//    // ������ ������������ WriteInteger(Action, ShortCut) �.�.
//    // ���� ������� ����� ����� ��������� ��������� ������,
//    for I := 0 to Length(ShortCuts)-1 do
//      with ShortCuts[I] do
//        if Assigned(Action) and (ShortCut <> 0) then
//          Strs.Add(Action.Name + '=' + IntToStr(ShortCut));
//    ASaver.SetStrings(Strs);
//  finally
//    Strs.Free;
//  end;
//end;
//
//procedure ReadShortCuts(ASaver: TPropSaver; const ASection: String;
//  var ShortCuts: TKeyboardShortCutArray; ActionList: TActionList);
//var
//  Strs: TStringList;
//  I, J, L, P: Integer;
//  Str, Name, Value: String;
//begin
//  ShortCuts := nil;
//  Strs := TStringList.Create;
//  try
//    ASaver.GetSectionValues(ASection, Strs);
//    for I := 0 to Strs.Count-1 do
//    begin
//      // ������ ������������ ShortCut := ReadInteger(Action) �.�.
//      // ���� ������� ����� ����� ��������� ��������� ������,
//      Str := Strs[I];
//      P := CharPos(Str, '=');
//      if P < 2 then Continue;
//      Name := Copy(Str, 1, P-1);
//      Value := Copy(Str, P+1, Length(Str)-P);
//      for J := 0 to ActionList.ActionCount-1 do
//        if ActionList.Actions[J].Name = Name then
//        begin
//          L := Length(ShortCuts);
//          SetLength(ShortCuts, L+1);
//          ShortCuts[L].Action := TAction(ActionList.Actions[J]);
//          ShortCuts[L].ShortCut := TShortCut(StrToIntDef(Value, 0));
//          Break;
//        end;
//    end;
//  finally
//    Strs.Free;
//  end;
//end;
//
//procedure ResetFactoryDefaultShortCuts(var ShortCuts: TKeyboardShortCutArray; ActionList: TActionList);
//var
//  Strs: TStringList;
//  Stream: TResourceStream;
//  Saver: TPropSaver;
//begin
//  Saver := nil;
//  Stream := TResourceStream.Create(HInstance, 'DefaultShortCuts', RT_RCDATA);
//  Strs := TStringList.Create;
//  try
//    Strs.LoadFromStream(Stream);
//    Saver := TPropSaver.CreateInMemory;
//    Saver.SetStrings(Strs);
//    ReadShortCuts(Saver, 'SHORTCUTS', ShortCuts, ActionList);
//  finally
//    Strs.Free;
//    Saver.Free;
//    Stream.Free;
//  end;
//end;
//
//procedure AssignShortCutsToMenus(var ShortCuts: TKeyboardShortCutArray; MenuBar: TTBCustomToolbar);
//var
//  L: Integer;
//
//  procedure GetUserShortCut(Item: TTBCustomItem);
//  var
//    I: Integer;
//  begin
//    for I := 0 to L do
//      if Item.Action = ShortCuts[I].Action then
//      begin
//        Item.ShortCut := ShortCuts[I].ShortCut;
//        Exit;
//      end;
//    Item.ShortCut := 0;
//  end;
//
//  procedure AssignShortCutsToBranch(Item: TTBCustomItem);
//  var
//    I: Integer;
//    S: String;
//  begin
//    for I := 0 to Item.Count-1 do
//    begin
//      S := Item.Items[I].Caption;
//      if Item.Items[I].Count <> 0
//        then AssignShortCutsToBranch(Item.Items[I])
//        else GetUserShortCut(Item.Items[I]);
//    end;
//  end;
//
//var
//  I: Integer;
//begin
//  L := Length(ShortCuts)-1;
//  with MenuBar do
//    for I := 0 to Items.Count-1 do
//      if Items[I].Count <> 0 then
//        AssignShortCutsToBranch(Items[I]);
//end;
//{$endregion}

//{$region 'EnterText / EnterString / TextView'}
//function EnterText(const ACaption: String; var Value: String;
//  Options: TMultiLineOpt = mloNlCr; Templates: TTemplateKind = tkNone): Boolean;
//begin
//  with TwndEnterText.Create(Application) do
//  try
//    if ACaption <> '' then
//      Caption := ACaption;
//    LoadTemplates(Templates);
//    Memo1.Lines.Text := Value;
//    Result := ShowModal = mrOk;
//    if Result then
//      case Options of
//        mloNone: Value := AnsiReplaceStr(Memo1.Lines.Text, #$D#$A, ' ');
//        mloNl:   Value := AnsiReplaceStr(Memo1.Lines.Text, #$A, '');
//        mloNlCr: Value := Memo1.Lines.Text;
//      end;
//  finally
//    Free;
//  end;
//end;
//
//function EnterText(const ACaption: String; Value: TStrings; Templates: TTemplateKind = tkNone): Boolean;
//begin
//  with TwndEnterText.Create(Application) do
//  try
//    if ACaption <> '' then
//      Caption := ACaption;
//    LoadTemplates(Templates);
//    Memo1.Lines.Assign(Value);
//    Result := ShowModal = mrOk;
//    if Result then
//      Value.Assign(Memo1.Lines);
//  finally
//    Free;
//  end;
//end;
//
//procedure TextView(const ACaption, AFileName: String; AOptions: TTextFileViewOptions = []);
//begin
//  TwndTextView.Create(ACaption, AFileName, AOptions).Show;
//end;
//
//// ���� ���������� ����� tfvCaptionContainsFileName, �� ��������� ����� ��������
//// ��� ����� ����� �������� ������, ��������: ACaption := "��������� ����"#13"��� �����"
//// ��� ���� ������� ����� tfvFileNameInCaption, tfvFileNamePanel �������� ������
//// ������ � tfvCaptionContainsFileName
//procedure TextView(const ACaption: String; AStream: TStream; AOptions: TTextFileViewOptions = []);
//begin
//  TwndTextView.Create(ACaption, AStream, AOptions).Show;
//end;
//{$endregion}

//procedure SaveFilePacked(const Data, FileName: String);
//var
////  OutBuf: Pointer;
////  OutSize: Integer;
//  Buf: Pointer;
//  OutFile: TFileStream;
//  Zip: TCompressionStream;
//begin
//  OutFile := TFileStream.Create(FileName, fmCreate, fmShareExclusive);
//  Zip := TCompressionStream.Create(clMax, outFile);
//  try
//    Buf := PAnsiChar(Data);
//    Zip.Write(Buf, Length(Data));
////    CompressBuf(PAnsiChar(Data), Length(Data), OutBuf, OutSize);
////    OutFile.Write(OutBuf, OutSize);
//  finally
//    Zip.Free;
//    OutFile.Free;
////    if Assigned(OutBuf) then FreeMem(OutBuf);
//  end
//end;
//
//// ��� ��������� OriUtils.ShowHelp
//function GetHelpFileName: String;
//var
//  HelpFile: String;
//begin
//  case LangManager.LanguageID of
//    1049: HelpFile := 'Spectrum.ru.chm';
//    else  HelpFile := 'Spectrum.en.chm';
//  end;
//  Result := ExtractFilePath(Application.ExeName) + HelpFile;
//end;
//
//function GetScriptPaths: TStringArray;
//begin
//  SetLength(Result, 1);
//  Result[0] := JoinPaths(ExtractFilePath(ParamStr(0)), PATH_SCRIPTS);
//end;

initialization
  Preferences := TPreferences.Create;

  //OriUtils.GetHelpFileName := GetHelpFileName;

  Randomize;

finalization
  Preferences.Free;

end.
