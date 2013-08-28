unit SpectrumSettings;

interface

uses
//  Windows, Classes, Controls, Graphics, Forms, Dialogs, ActnList, StdCtrls,
//  Grids,
//  TB2Toolbar,
//  OriUtils,
//  PropSaver, OriCombos, OriUtilsTChart
  OriIniFile,
  SpectrumTypes;

type

//------------------------------------------------------------------------------
  // тут содержатся только часто используемые настройки
  // редко используемые при необходимости читаются из файла
  // (например, фильтры для открытия папки)
  TPreferences = class
  {
    LoadDefChartSetting: Boolean; // Загружать настройки графиков по умолчанию
    AnimatedChartZoom: Boolean;   // Анимированное масштабирование графиков
    AxisDblClickLimits: Boolean;  // двойной клик по оси - пределы, иначе свойства
    ScrollAxesByMouse: Boolean;   // Прокручивать оси мышью
    NoConfirmDelete: Boolean;     // Не подтверждать удаление графиков
    SelectJustAdded: Boolean;     // выделять добавленные графики cелектором

    // горячие клавиши команд выделения, инициализируются в главном окне
    // используются некоторыми окнами, обладающими аналогичными командами,
    // чтобы сочетания клавиш в каждом конкретном окне были одинаковыми
    scSelectAll: TShortCut;
    scSelectNone: TShortCut;
    scSelectInv: TShortCut;
    scEditCopy: TShortCut;

    // для экспорта или копирования
    ExportParams: record
      ChartCopyFormat: TCopyImageFormat;
      DecSeparator: TDecimalSeparator;
      LineDelimiter: TLineDelimiter;
      ValueDelimiter: TValueDelimiter;
      LineDelimSpec: String;
      ValueDelimSpec: String;
    end;

    // состояния
    ProjectOpenCurDir: String;
    ProjectOpenFilter: Integer;

    GraphsOpenCurDir: String;
    GraphsOpenFilter: Integer;     
    GraphsOpenFilterTable: Integer;

    TitlePresets: TStringList; // шаблоны заголовков осей и диаграммы

    // процедуры применения/использования некоторых состояний или настроек

    constructor Create;
    destructor Destroy; override;
    procedure Load(ASaver: TPropSaver);
    procedure Save(ASaver: TPropSaver);
    procedure LoadStates(ASaver: TPropSaver);
    procedure SaveStates(ASaver: TPropSaver);
    }
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
// шаблоны заголовков
type
  TTemplateKind = (tkNone, tkAxesNames, tkGraphTitles);

//------------------------------------------------------------------------------
// клавиатурные сокращения
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
// окна ввода/вывода текста
type
  TMultiLineOpt = (mloNone, mloNl, mloNlCr);
  TTextFileViewOption = (
    tfvFileNameInCaption,      // отображать имя файла в заголовке окна
    tfvFileNamePanel,          // отображать имя файла в специальном поле
    tfvCaptionContainsFileName // параметр Caption содержит имя файла
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
// Файловые диалоги

function OpenGraphsDialog(AFileNames: TStrings; var AFilterIndex: Integer): Boolean;
function OpenTableDialog(var AFileName: String): Boolean;
function OpenFolderDialog: Boolean;

//------------------------------------------------------------------------------

procedure SaveFilePacked(const Data, FileName: String);

// Возвращает список путей для поиска скриптов.
function GetScriptPaths: TStringArray;

//------------------------------------------------------------------------------
const
  CSomeStates = 'STT_SOMESTATES';
  APP_MAIL = 'spectrum@orion-project.org';
  APP_HOME = 'www.spectrum.orion-project.org';
  PATH_SCRIPTS = 'scripts';
  ACTN_HIDDEN_CTGR = 'Reserved';
  ACTN_DEBUG_CMD = 999;

var
  SMultSign: WideChar;
}
implementation

//uses
//  SysUtils, Menus, StrUtils, Clipbrd, ComCtrls, ZLib, WinInet,
//  TB2Item,
//  DKLang,
//  WinTextView, WinEnterText, PlotReaders;

{%region TPreferences}
(*
{конструктор заполняет некоторые поля настроек по умолчанию.
 Стуация: непосредственно перед Preferences.Load возникает
 исключение и Load не выполняется (такое было, когда DKLang
 не нашел константу) в итоге Preferences заполнен нулями,
 в не дефолтовыми значениями. Чтобы такого не было при инициализации
 модуля заполняем поля по дефолту (те поля, которые по дефолту
 нулевые или не страшно, что они будут нулями, сюда можно не включать )}
constructor TPreferences.Create;
begin
  TitlePresets := TStringList.Create;
end;

destructor TPreferences.Destroy;
begin
  TitlePresets.Free;
end;

procedure TPreferences.Load(ASaver: TPropSaver);
begin
  with ASaver do
  begin
    Section             := CMainSection;
    LoadDefChartSetting := ReadBool('LoadDefChartSetting', False);
    AxisDblClickLimits  := ReadBool('AxisDblClickShowLimits', False);
    ScrollAxesByMouse   := ReadBool('ScrollAxesByMouse', False);
    AnimatedChartZoom   := ReadBool('AnimatedChartZoom', True);
    NoConfirmDelete     := ReadBool('NoConfirmDelete', False);
    SelectJustAdded     := ReadBool('SelectJustAdded', True);

    Section                         := 'EXPORT_PARAMS';
    ExportParams.ChartCopyFormat    := TCopyImageFormat(ReadInteger('ChartCopyFormat', Ord(cifWmf)));
    ExportParams.DecSeparator       := TDecimalSeparator(ReadInteger('DecimalSeparator', Ord(dseSystem)));
    ExportParams.LineDelimiter      := TLineDelimiter(ReadInteger('LineDelimiter', Ord(ldWindows)));
    ExportParams.ValueDelimiter     := TValueDelimiter(ReadInteger('ValueDelimiter', Ord(vdTab)));
    ExportParams.LineDelimSpec      := ReadString('LineDelimiterSpec', '');
    ExportParams.ValueDelimSpec     := ReadString('ValueDelimiterSpec', '');
  end;
end;

procedure TPreferences.Save(ASaver: TPropSaver);
begin
  with ASaver do
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
    WriteInteger('ChartCopyFormat', Ord(ExportParams.ChartCopyFormat));
    WriteInteger('DecimalSeparator', Ord(ExportParams.DecSeparator));
    WriteInteger('LineDelimiter', Ord(ExportParams.LineDelimiter));
    WriteInteger('ValueDelimiter', Ord(ExportParams.ValueDelimiter));
    WriteString('LineDelimiterSpec', ExportParams.LineDelimSpec);
    WriteString('ValueDelimiterSpec', ExportParams.ValueDelimSpec);
  end;
end;

procedure TPreferences.LoadStates(ASaver: TPropSaver);
begin
  with ASaver do
  begin
    Section                 := CSomeStates;
    ProjectOpenCurDir       := EnsurePath(ReadString('ProjectOpenCurDir', ''));
    ProjectOpenFilter       := ReadInteger('ProjectOpenFilter', 1);
    GraphsOpenCurDir        := EnsurePath(ReadString('GraphsOpenCurDir', ''));
    GraphsOpenFilter        := ReadInteger('GraphsOpenFilter', 1);
    GraphsOpenFilterTable   := ReadInteger('GraphsOpenFilterTable', 1);

    ReadTextSection('TITLE_PRESETS', TitlePresets);
  end;
end;

procedure TPreferences.SaveStates(ASaver: TPropSaver);
begin
  with ASaver do
  begin
    Section := CSomeStates;
    WriteString('ProjectOpenCurDir', ProjectOpenCurDir);
    WriteInteger('ProjectOpenFilter', ProjectOpenFilter);
    WriteString('GraphsOpenCurDir', GraphsOpenCurDir);
    WriteInteger('GraphsOpenFilter', GraphsOpenFilter);
    WriteInteger('GraphsOpenFilterTable', GraphsOpenFilterTable);

    WriteTextSection('TITLE_PRESETS', TitlePresets);
  end;
end;
 *)
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
//    // нельзя пользоваться WriteInteger(Action, ShortCut) т.к.
//    // одна команда может иметь несколько сочетаний клавиш,
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
//      // нельзя пользоваться ShortCut := ReadInteger(Action) т.к.
//      // одна команда может иметь несколько сочетаний клавиш,
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
//// Если выставлена опция tfvCaptionContainsFileName, то заголовок может включать
//// имя файла после переноса строки, например: ACaption := "Заголовок окна"#13"Имя файла"
//// Для этой функции опции tfvFileNameInCaption, tfvFileNamePanel работают только
//// вместе с tfvCaptionContainsFileName
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
//// для процедуры OriUtils.ShowHelp
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

//{%region File Dialogs}
//function OpenDataFilters: String;
//begin
//  Result :=
//    LangManager.ConstantValue['Filter_TXT'] + ' (*.TXT)|*.txt|' +
//    LangManager.ConstantValue['Filter_CSV'] + ' (*.CSV)|*.csv|' +
//    LangManager.ConstantValue['Filter_DAT'] + ' (*.DAT)|*.dat|' +
//    LangManager.ConstantValue['Filter_RS'] + ' (*.TR1,2,3,4)|*.tr1;*.tr2;*.tr3;*.tr4|' +
//    LangManager.ConstantValue['Filter_OO'] + ' (*.Scope)|*.Scope|' +
//    LangManager.ConstantValue['Filter_RAR'] + ' (*.RAR)|*.rar|' +
//    LangManager.ConstantValue['Filter_ZIP'] + ' (*.ZIP)|*.zip|' +
//    LangManager.ConstantValue['Filter_All'] + '|*.*|';
//end;
//
//function OpenTableFilters: String;
//begin
//  Result :=
//    LangManager.ConstantValue['Filter_TXT'] + ' (*.TXT)|*.txt|' +
//    LangManager.ConstantValue['Filter_CSV'] + ' (*.CSV)|*.csv|' +
//    LangManager.ConstantValue['Filter_DAT'] + ' (*.DAT)|*.dat|' +
//    LangManager.ConstantValue['Filter_All'] + '|*.*|';
//end;
//
//function OpenGraphsDialog(AFileNames: TStrings; var AFilterIndex: Integer): Boolean;
//begin
//  AFileNames.Clear;
//  with TOpenDialog.Create(nil) do
//  try
//    Title := Constant('Dlg_OpenGraphs');
////    Filter := OpenDataFilters;
//    Filter := PlotReaders.TFileReaders.FileFilters;
//    DefaultExt := 'txt';
//    FileName := '';
//    Options := Options + [ofPathMustExist, ofFileMustExist,
//      ofAllowMultiSelect, ofDontAddToRecent];
//    FilterIndex := Preferences.GraphsOpenFilter;
//    InitialDir := Preferences.GraphsOpenCurDir;
//    Result := Execute;
//    if Result then
//    begin
//      Preferences.GraphsOpenFilter := FilterIndex;
//      Preferences.GraphsOpenCurDir := ExtractFilePath(FileName);
//      AFilterIndex := FilterIndex;
//      AFileNames.Assign(Files);
//    end;
//  finally
//    Free;
//  end;
//end;
//
//function OpenTableDialog(var AFileName: String): Boolean;
//begin
//  with TOpenDialog.Create(nil) do
//  try
//    Title := Constant('Dlg_OpenTable');
//    Filter := OpenTableFilters;
//    DefaultExt := 'txt';
//    FileName := '';
//    Options := Options + [ofPathMustExist, ofFileMustExist, ofDontAddToRecent];
//    FilterIndex := Preferences.GraphsOpenFilterTable;
//    InitialDir := Preferences.GraphsOpenCurDir;
//    Result := Execute;
//    if Result then
//    begin
//      Preferences.GraphsOpenFilterTable := FilterIndex;
//      Preferences.GraphsOpenCurDir := ExtractFilePath(FileName);
//      AFileName := FileName;
//    end;
//  finally
//    Free;
//  end;
//end;
//
//function OpenFolderDialog: Boolean;
//var
//  CurDir: String;
//begin
//  CurDir := Preferences.GraphsOpenCurDir;
//  // TODO: свой диалог выбора папки, в котором сразу же можно будет задать
//  // фильтр и, возможно, посмотреть какие файлы будут выбраны.
//  Result := OriUtils.OpenFolderDialog(Constant('Dlg_OpenFolder'), CurDir);
//  if Result then Preferences.GraphsOpenCurDir := CurDir;
//end;
//{%endregion}

initialization
  //Preferences := TPreferences.Create;

  //OriUtils.GetHelpFileName := GetHelpFileName;

  Randomize;

finalization
//  Preferences.Free;

end.