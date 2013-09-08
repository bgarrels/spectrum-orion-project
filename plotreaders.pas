unit PlotReaders;

interface

uses
  Classes, SysUtils,
  SpectrumTypes;

type
  TByteSet = set of Byte;
  TDecimalSeparator = (dsAuto, dsSystem, dsPoint, dsComma);
  TDecimalSeparator1 = (ds1Point, ds1Comma);
  TOpenDialogKing = (odkSimple, odkTable);

  TDataReader = class;
  TDataReaderClass = class of TDataReader;

  {
    Параметры добавления графика на диаграмму.
    Объект может создаваться где угодно (обработчик какого-либо экшена и т.п.),
    затем передается в процедуру TPlot.AddGraph, которая является процедурой
    верхнего уровня для создания графиков. Там параметры уточняются, передаются
    в TGraph.Load(TGraphAppendParams), где и происходит реальное чтение данных
    в соответствии с заданными параметрами. Затем объект-параметр уничтожается
    (т.е. в том месте, где его создали, не нужно заботиться о его уничтожении).
    При загрузке данных создается ридер, читающий данные, тип которого указан в
    поле Reader, объект-график дополнительно уточняет параметры и предает их
    созданному ридеру. Если же тип ридера не задан, то предполагается что данные
    кем-то откуда-то уже прочитаны, указатели на них содержатся в полях ValuesX/Y
    и никакой ридер использовать не нужно.
  }              (*
  TGraphAppendParams = class
  public
    // Диаграмма, на которую добавляется график.
    // В частности, может использоваться ридерами, создающими несколько графиков
    // из одного источника (как TTableDataReader), для вызова TPlot.AddGraph
    Plot: TObject;

    // Способ чтения данных. Может не указываться, если заданы готовые
    // массивы данных (указатели ValuesX/Y).
    Reader: TDataReaderClass;

    // Источник данных. Если задан поток, то это просто информация о том, из
    // чего этот поток был создан. Иначе предполагается, что это имя файла, из
    // которого нужно создать поток (в общем случае данные читаются из потока).
    // В зависимости от типа ридера, может вообще не использоваться.
    Source: String;

    // Заголовок графика. Если не задано, то используется Source (см. TGraph.Load)
    Title: String;

    // Поток, из которого читаются данные. Передается графику, а потом и ридеру
    // откуда-то извне или создается самим ридером (из файла указанного в Source
    // или откуда угодно, в зависимости от типа ридера).
    Stream: TStream;

    // Указывыют ридеру, куда складывать считанные данные.
    // Это либо указатели на аналогичные поля объекта-графика (TGraph), либо
    // указатели на массивы, находящиеся в другом месте (см. TTableDataReader),
    // которые после чтения (или вместо чтения) копируются в объект-график.
    ValuesX: PValueArray;
    ValuesY: PValueArray;

    // Дополнтельный параметр, интерпретируется конкретным ридером.
    // Например, при открытии RS-файла из архива, это - указатель на поток
    // дополнительно открытого файла параметров спектра.
    AuxParam: TObject;

    // Если не задано ни одного параметра, то в процедуру создания графиков
    // передаваются имена файлов и тип ридера определяется то типу файлов.
    constructor Create(AReader: TDataReaderClass = nil;
      const ASource: String = ''; AStream: TStream = nil); overload;
    // Данные уже загружены и передаются графику в готовом виде.
    constructor Create(AValuesX, AValuesY: PValueArray;
      const ASource: String = ''; const ATitle: String = ''); overload;
    // Способ создания данных определяется самим графиком по типу объекта.
    constructor Create(AAuxParam: TObject); overload;
  end;       *)

  TDataReader = class
  const
    ValueInc = 1000;
  protected
    FValueIndex: Integer;
    FValueCount: Integer;
    FValuesX: TValueArray;
    FValuesY: TValueArray;
    FResults: TGraphRecs;
    procedure CheckValuesSize; inline;
    function GetResultCount: Integer;
    function GetResult(Index: Integer): TGraphRec;
  public
    function Configure: Boolean; virtual;
    procedure Read(const AFileName: String); virtual;
    procedure Read(AStream: TStream); virtual; abstract;
    property ResultCount: Integer read GetResultCount;
    property Result[Index: Integer]: TGraphRec read GetResult;
    class function IsMulti: Boolean; virtual;
    class procedure FileFilters(Strings: TStrings); virtual;
    class procedure FileExts(Strings: TStrings); virtual;
  end;

  // It's a holder for all TDataReaders that can read data from file.
  // Use RegisterReader in initialization section to add a new reader
  // to known file-readers list. Each file reader will be presented in
  // its own line in filter list of 'Add Graph' open file dialog.
  TFileReaders = class
  private class var
    FReaders: TList;
    FFilters: TStringList;
  private
    class function GetReadersCount: Integer; static;
    class function GetReader(Index: Integer): TDataReaderClass; static;
  public
    class function FileFilters: String;
    class function ReaderByFilter(FilterIndex: Integer): TDataReaderClass;
    class function ReaderByExt(const FileName: String): TDataReaderClass;
    class procedure RegisterReader(AClass: TDataReaderClass);
    class property ReadersCount: Integer read GetReadersCount;
    class property Readers[Index: Integer]: TDataReaderClass read GetReader;
  end;

  TCSVDataReader = class(TDataReader)
  const
    StrInc = 256;
    BufSize = 4096;
  protected
    FLinesRead: Integer;
    FValDelimiters: TByteSet;
    FFloatFmt: TFormatSettings;
    FOneColumnX: TValue;
  protected
    procedure ProcessString(const Str: String); virtual;
    procedure InitValueDelimiters; virtual;
    procedure InitFloatFormat; virtual;
  public
    class var ValueSeparators: String;          // параметры "открывателя"
    class var DecSeparator: TDecimalSeparator;  // задаются в окне настроек
    class var SkipFirstLines: Integer;          // программы, там же сохраняются,
    class var OneColumnFirst: TValue;           // а загружаются в главном FormCreate
    class var OneColumnInc: TValue;             // с помощью LoadFileOpeners
  public
    procedure Read(AStream: TStream); override;
  end;

  TCSVFileReader = class(TCSVDataReader)
  public
    class procedure FileFilters(Strings: TStrings); override;
    class procedure FileExts(Strings: TStrings); override;
  end;
(*
  TTableGraph = record
    ColumnX: Byte;
    ColumnY: Byte;
    Title: String;
    OneColumnX: TValue;
    ValueCount: Integer;
    ValueIndex: Integer;
    ValuesX: TValueArray;
    ValuesY: TValueArray;
  end;
  TTableGraphs = array of TTableGraph;

  TTableDataReader = class(TCSVDataReader)
  protected
    procedure ProcessString(const Str: String); override;
    procedure InitValueDelimiters; override;
    procedure InitFloatFormat; override;
  public
    //class var SkipFirstLines: Integer;
    //class var DecSeparator: TDecimalSeparator1;
    //class var ValueSeparators: String;
    class var PreviewLineCount: Integer;
  public
    Graphs: TTableGraphs;
    function Configure: Boolean; override;
    procedure ReadValues; override;
    class function IsMulti: Boolean; override;
  end;

  TTableFileReader = class(TTableDataReader)
  public
    //constructor Create(Params: TGraphAppendParams); override;
  end;

  TClipbrdDataReader = class(TCSVDataReader)
  private
    ValuesX: TValueArray;
    ValuesY: TValueArray;
    Props: TStringList;
    ProcessingProps: Boolean;
    procedure ProcessData;
  protected
    procedure ProcessString(const Str: String); override;
  public
    //constructor Create(Params: TGraphAppendParams); override;
    destructor Destroy; override;
    procedure ReadValues; override;
    class function IsMulti: Boolean; override;
  end;

  TClipbrdTableReader = class(TTableDataReader)
  public
    //constructor Create(Params: TGraphAppendParams); override;
  end;

//procedure LoadReadersSettings(ASaver: TPropSaver);
//procedure SaveReadersSettings(ASaver: TPropSaver);

function RefineOpener(AOpener: TDataReaderClass; const AFileName: String = ''): TDataReaderClass;
function FileIsRS(const AFileName: String): Boolean;
function FileIsArchive(const AFileName: String): Boolean;

const
  CDefaultFolderFilter = '*.txt;*.dat';
*)
implementation

uses
  {Controls,} Clipbrd,
  OriStrings, OriUtils,
  PlotReadersAux, SpectrumStrings{, WinOpenTable};

{%region Saving/Loading of settings and states of openers}
//procedure LoadReadersSettings(ASaver: TPropSaver);
//begin
//  with ASaver do
//  begin
//    Section                          := CMainSection;
//    TCSVDataReader.ValueSeparators   := ReadString('ValueSeparators', '');
//    TCSVDataReader.DecSeparator      := TDecimalSeparator(ReadInteger('DecimalSeparator', Ord(dsAuto)));
//    TCSVDataReader.SkipFirstLines    := ReadInteger('SkipFirstLines', 0);
//    TCSVDataReader.OneColumnFirst    := ReadFloat('OneColumnFirst', 1);
//    TCSVDataReader.OneColumnInc      := ReadFloat('OneColumnInc', 1);
//
//    TTableDataReader.PreviewLineCount  := ReadInteger('PreviewLineCountTable', 25);
//    // The rest settings of TFileOpenerTable are read in WinOpenTable.LoadState
//
//    TRSFileReader.ReadLowValues       := ReadBool('RSReadLowValues', False);
//    TRSFileReader.RSFileVersion       := ReadInteger('RSFileVersion', 0);
//  end;
//end;
//
//procedure SaveReadersSettings(ASaver: TPropSaver);
//begin
//  with ASaver do
//  begin
//    Section := CMainSection;
//    WriteString('ValueSeparators', TCSVDataReader.ValueSeparators);
//    WriteInteger('DecimalSeparator', Ord(TCSVDataReader.DecSeparator));
//    WriteInteger('SkipFirstLines', TCSVDataReader.SkipFirstLines);
//    WriteFloat('OneColumnFirst', TCSVDataReader.OneColumnFirst);
//    WriteFloat('OneColumnInc', TCSVDataReader.OneColumnInc);
//
//    WriteInteger('PreviewLineCountTable', TTableDataReader.PreviewLineCount);
//  end;
//end;
{%endregion}
(*
function RefineOpener(AOpener: TDataReaderClass; const AFileName: String = ''): TDataReaderClass;
begin
  if FileIsRS(AFileName)
    then Result := TRSFileReader
    else Result := AOpener;
end;

function FileIsRS(const AFileName: String): Boolean;
var L: Integer;
begin
  L := Length(AFileName);
  Result := (L > 4) and (AFileName[L] in ['1','2','3','4']) and
    (AFileName[L-1] in ['r','R']) and (AFileName[L-2] in ['t','T']) and (AFileName[L-3] = '.');
end;

function FileIsArchive(const AFileName: String): Boolean;
var
  Ext: String;
begin
  Ext := ExtractFileExt(AFileName);
  Result := SameText(Ext, '.rar') or SameText(Ext, '.zip');
end;

{%region TGraphAppendParams}
constructor TGraphAppendParams.Create(AReader: TDataReaderClass; const ASource: String; AStream: TStream);
begin
  Reader := AReader;
  Source := ASource;
  Stream := AStream;
end;

constructor TGraphAppendParams.Create(AValuesX, AValuesY: PValueArray; const ASource, ATitle: String);
begin
  Title := ATitle;
  Source := ASource;
  ValuesX := AValuesX;
  ValuesY := AValuesY;
end;

constructor TGraphAppendParams.Create(AAuxParam: TObject);
begin
  AuxParam := AAuxParam;
end;
{%endregion}
*)
{%region TDataReader}
procedure TDataReader.Read(const AFileName: String);
var
  S: TStream;
begin
  S := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Read(S);
  finally
    S.Free;
  end;
end;

function TDataReader.Configure: Boolean;
begin
  Result := True;
end;

procedure TDataReader.CheckValuesSize;
begin
  if FValueIndex = FValueCount then
  begin
    Inc(FValueCount, ValueInc);
    SetLength(FValuesX, FValueCount);
    SetLength(FValuesY, FValueCount);
  end;
end;

class function TDataReader.IsMulti: Boolean;
begin
  Result := False;
end;

function TDataReader.GetResultCount: Integer;
begin
  Result := Length(FResults);
end;

function TDataReader.GetResult(Index: Integer): TGraphRec;
begin
  Result := FResults[Index];
end;

class procedure TDataReader.FileFilters(Strings: TStrings);
begin
  // do nothing
end;

class procedure TDataReader.FileExts(Strings: TStrings);
begin
  // do nothing
end;
{%endregion}

{%region TCSVDataReader}
{function TCSVDataReader.ReadString: String;
var
  I, J, L: Integer;
  P: Int64;
  Buf: array[0..255] of Char;
begin
  I := 0;
  L := 0;
  P := Stream.Position; // backup pos
  while Stream.Position < Stream.Size do
    for J := 0 to Stream.Read(Buf, 255)-1 do
      case Buf[J] of
// Вроде как ноль - стандартный детектор бинарного фйла; например, так
// определяет "бинарность" AkelPad. Но нулевые байты встречаются, например,
// в файлах samples\climate\*.per, а в остальном это нормальные текстовые файлы
// и как-то странно их не читать из-за этого нуля. Поэтому нуль-байт трактуем
// как конец строки, а бинарность определяем по наличию "малых" значений (< #9).
//        #$0:
//          raise ESpectrumError.CreateDK('Err_IllegalChar');
        #$A, #$0:
          begin
            Stream.Position := P + Int64(I) + 1;
            SetLength(Result, I);
            Exit;
          end;
        #$D:
          begin
            Inc(P);
            Continue;
          end
        else
          begin
            if Ord(Buf[J]) < 9 then
              raise ESpectrumError.Create(Err_IllegalChar);
            Inc(I);
            if I > L then
            begin
              Inc(L, 256);
              SetLength(Result, L);
            end;
            Result[I] := Buf[J];
          end;
      end;
  SetLength(Result, I);
end;   }

//procedure TCSVDataReader.SkipLines(Amount: Integer);
//var
//  I: Integer;
//begin
//  I := 0;
//  while (I < Amount) and (Stream.Position < Stream.Size) do
//  begin
//    ReadString;
//    Inc(I);
//  end;
//end;

procedure TCSVDataReader.InitValueDelimiters;
var I: Integer;
begin
  FValDelimiters := [];
  for I := 1 to Length(ValueSeparators) do
    Include(FValDelimiters, Ord(ValueSeparators[I]));
  Include(FValDelimiters, $9);
  Include(FValDelimiters, $20);
end;

procedure TCSVDataReader.InitFloatFormat;
begin
  case DecSeparator of
    dsPoint: FFloatFmt.DecimalSeparator := '.';
    dsComma: FFloatFmt.DecimalSeparator := ',';
    else     FFloatFmt.DecimalSeparator := DecimalSeparator;
  end;
  FFloatFmt.ThousandSeparator := #0;
end;

procedure TCSVDataReader.Read(AStream: TStream);
var
  I: Integer;
  Size, Pos: Int64;
  BufRead: Longint;
  InBuffer: array [0..BufSize-1] of Byte;
  InByte: Byte;
  InStr: String;
  PrevSeparator: Boolean;
  StrIndex, StrLen: Integer;
  Stream: TStream;
begin
  FOneColumnX := OneColumnFirst;

  Stream := AStream;
  FValueCount := 0;
  FValueIndex := 0;
  FLinesRead := 0;
  FValuesX := nil;
  FValuesY := nil;

  InitFloatFormat;
  InitValueDelimiters;

  // working line
  FLinesRead := 0;
  StrIndex := 0;
  StrLen := StrInc;
  SetLength(InStr, StrLen);

  PrevSeparator := False;

  Pos := 0;
  Stream.Position := Pos;
  Size := Stream.Size;
  while Pos < Size do
  begin
    Stream.Position := Pos;
    // Read next buffer
    BufRead := Stream.Read(InBuffer, BufSize);
    Inc(Pos, BufRead);
    // Process buffer
    for I := 0 to BufRead-1 do
    begin
      InByte := InBuffer[I];
      // Process next byte
      case InByte of
        $A, $D, $0: // EOL founded
          begin
            // Working line is empty yet
            if StrIndex = 0 then Continue;
            // Part of working line (InStr) containing current line of file
            // ends with double #0. Single #0 is used as values delimiter.
            Inc(StrIndex);
            InStr[StrIndex] := #0;
            if StrIndex = StrLen then
            begin
              Inc(StrLen, StrInc);
              SetLength(InStr, StrLen);
            end;
            InStr[StrIndex+1] := #0;
            Inc(FLinesRead);
            ProcessString(InStr);
            PrevSeparator := False;
            StrIndex := 0;
            Continue;
          end;

        else // Meaning char - add to working line
        begin
            if Ord(InByte) < 9 then // May be file is binary
              raise ESpectrumError.Create(Err_IllegalChar);

          if InByte in FValDelimiters then
            if not PrevSeparator then //  Doubled delimiters is ignored
            begin
              if StrIndex > 0 then // Delimiter at the begining of line - throw it away
              begin // All delimiters are replaced with zero to speed-up ProcessString
                Inc(StrIndex);
                InStr[StrIndex] := #0;
              end;
              PrevSeparator := True;
            end
            else Continue
          else
          begin
            PrevSeparator := False;
            Inc(StrIndex);
            InStr[StrIndex] := Chr(InByte);
          end;
        end;
      end; // Case InByte of

      if StrIndex = StrLen then
      begin
        Inc(StrLen, StrInc);
        SetLength(InStr, StrLen);
      end;

    end; // Process buffer
  end; // Process stream

  // Process the last line in file
  if StrIndex > 0 then
  begin
    Inc(StrIndex);
    InStr[StrIndex] := #0;
    if StrIndex = StrLen then
    begin
      Inc(StrLen, StrInc);
      SetLength(InStr, StrLen);
    end;
    InStr[StrIndex+1] := #0;
    Inc(FLinesRead);
    ProcessString(InStr);
  end;

  if FValuesX <> nil then SetLength(FValuesX, FValueIndex);
  if FValuesY <> nil then SetLength(FValuesY, FValueIndex);

  SetLength(FResults, 1);
  FResults[0].X := FValuesX;
  FResults[0].Y := FValuesY;
end;

procedure TCSVDataReader.ProcessString(const Str: String);
var
  P, P1: PChar;
  Value: Extended;
  Value1, Value2: TValue;
  ValueCount: Byte;
begin
  if FLinesRead > SkipFirstLines then
  begin
    ValueCount := 0;
    P := PChar(Str);
    P1 := P;
    repeat
      Inc(P);
      if P^ = #0 then
      begin
        if not TextToFloat(P1, Value, fvExtended, FFloatFmt) then
          if DecSeparator = dsAuto then
          begin // Toggle decimal separator
            if FFloatFmt.DecimalSeparator = '.'
              then FFloatFmt.DecimalSeparator := ','
              else FFloatFmt.DecimalSeparator := '.';
            if not TextToFloat(P1, Value, fvExtended, FFloatFmt)
              then Exit; // Skip bad line
          end else Exit; // Skip bad line
        case ValueCount of
          0:
          begin
            Value1 := Value;
            Inc(ValueCount);
          end;
          1:
          begin
            Value2 := Value;
            Inc(ValueCount);
            Break;
          end;
        end;
        P1 := P + 1;
      end;
    until P1^ = #0;

    CheckValuesSize;
    case ValueCount of
      1:
        begin
          FValuesX[FValueIndex] := FOneColumnX;
          FValuesY[FValueIndex] := Value1;
          FOneColumnX := FOneColumnX + OneColumnInc;
          Inc(FValueIndex);
        end;
      2:
        begin
          FValuesX[FValueIndex] := Value1;
          FValuesY[FValueIndex] := Value2;
          Inc(FValueIndex);
        end;
    end;
  end;
end;
{%endregion}

{%region TCSVFileReader}
class procedure TCSVFileReader.FileFilters(Strings: TStrings);
begin
  Strings.Add(Filter_TXT);
  Strings.Add(Filter_CSV);
  Strings.Add(Filter_DAT);
  Strings.Add(Filter_AllCSV);
end;

class procedure TCSVFileReader.FileExts(Strings: TStrings);
begin
  Strings.Add('txt');
  Strings.Add('dat');
  Strings.Add('csv');
end;
{%endregion}
(*
{%region 'TTableDataReader'}
class function TTableDataReader.IsMulti: Boolean;
begin
  Result := True;
end;

procedure TTableDataReader.InitValueDelimiters;
var I: Integer;
begin
  FValDelimiters := [];
  for I := 1 to Length(ValueSeparators) do
    Include(FValDelimiters, Ord(ValueSeparators[I]));
  Include(FValDelimiters, $9);
  Include(FValDelimiters, $20);
end;

procedure TTableDataReader.InitFloatFormat;
begin
  case DecSeparator of
    dsComma: FFloatFmt.DecimalSeparator := ',';
    else FFloatFmt.DecimalSeparator := '.';
  end;
  FFloatFmt.ThousandSeparator := #0;
end;

procedure TTableDataReader.ReadValues;
var i: Integer;
begin
  for i := 0 to Length(Graphs)-1 do
    Graphs[i].OneColumnX := OneColumnFirst;

  inherited;

  // TODO
  //for i := 0 to Length(Graphs)-1 do
  //  with Graphs[i] do
  //  begin
  //    SetLength(ValuesX, ValueIndex);
  //    SetLength(ValuesY, ValueIndex);
  //    TPlot(FPlot).AddGraph(TGraphAppendParams.Create(
  //      @ValuesX, @ValuesY, FSource, Title));
  //  end;
end;

procedure TTableDataReader.ProcessString(const Str: String);
const
  ValInc = 25;
var
  i: Integer;
  P, P1: PChar;
  Parts: array of PChar;
  ValueY, ValueX: Extended;
  Index, Len: Byte;
begin
  if FLinesRead > SkipFirstLines then
  begin
    Index := 0;
    Len := ValInc;
    SetLength(Parts, Len);
    P := PChar(Str);
    P1 := P;
    repeat
      Inc(P);
      if P^ = #0 then
      begin
        Parts[Index] := P1;
        Inc(Index);
        if Index = Len then
        begin
          Inc(Len, ValInc);
          SetLength(Parts, Len);
        end;
        P1 := P + 1;
      end;
    until P1^ = #0;
    for i := 0 to Length(Graphs)-1 do
      with Graphs[i] do
      begin
        if ValueIndex = ValueCount then
        begin
          Inc(ValueCount, ValueInc);
          SetLength(ValuesX, ValueCount);
          SetLength(ValuesY, ValueCount);
        end;
        if (ColumnX > Len) or (ColumnY > Len) then Continue;
        if ColumnX = 0 then // не указана колонка для X
        begin
          ValueX := OneColumnX;
          OneColumnX := OneColumnX + OneColumnInc;
        end
        else
        begin
          if ColumnX > Index then Continue;
          if not TextToFloat(Parts[ColumnX-1], ValueX, fvExtended, FFloatFmt) then Continue;
        end;
        if (ColumnY < 1) or (ColumnY > Index) then Continue;
        if not TextToFloat(Parts[ColumnY-1], ValueY, fvExtended, FFloatFmt) then Continue;
        ValuesX[ValueIndex] := ValueX;
        ValuesY[ValueIndex] := ValueY;
        Inc(ValueIndex);
      end;
  end;
end;

function TTableDataReader.Configure: Boolean;
begin
  Result := False; // TODO TwndOpenTable.Create(Self).ShowModal = mrOk;
end;
{%endregion}

{%region TTableFileReader}
//constructor TTableFileReader.Create(Params: TGraphAppendParams);
//begin
//  inherited;
//  OpenSourceAsFile;
//end;
{%endregion}

{%region TClipbrdDataReader}
//constructor TClipbrdDataReader.Create(Params: TGraphAppendParams);
//begin
//  inherited;
//
//  FValuesX := @ValuesX;
//  FValuesY := @ValuesY;
//
//  FStream := TStringStream.Create(Clipboard.AsText);
//  FOwnedStream := True;
//end;

destructor TClipbrdDataReader.Destroy;
begin
  Props.Free;
  inherited;
end;

class function TClipbrdDataReader.IsMulti: Boolean;
begin
  Result := True;
end;

procedure TClipbrdDataReader.ReadValues;
begin
  inherited;
  ProcessData;
end;

procedure TClipbrdDataReader.ProcessData;
//var
//  Graph: TGraph;
begin
  //if Length(ValuesX) > 0 then
  //begin
  //  SetLength(ValuesX, FValueIndex);
  //  SetLength(ValuesY, FValueIndex);
  //  Graph := TPlot(FPlot).AddGraph(
  //    TGraphAppendParams.Create(FValuesX, FValuesY, FSource));
  //  if Assigned(Graph) and Assigned(Props) then
  //  begin
  //    with TPropSaver.CreateInMemory do
  //    try
  //      SetStrings(Props, True);
  //      ReadLineSeries('PROPERTIES', Graph.Series);
  //    finally
  //      Free;
  //    end;
  //    FreeAndNil(Props);
  //  end;
  //  FValueIndex := 0;
  //  FValueCount := 0;
  //  ValuesX := nil;
  //  ValuesY := nil;
  //end;
end;

procedure TClipbrdDataReader.ProcessString(const Str: String);
begin
  if Str[1] = '[' then
  begin
    if SameText(Copy(Str, 1, 12), '[PROPERTIES]') then
    begin
      // Построить график из предыдущей DATA-секции, если она была
      ProcessData;

      Props := TStringList.Create;
      Props.Add('[PROPERTIES]');
      ProcessingProps := True;
    end
    else if SameText(Copy(Str, 1, 6), '[DATA]') then
    begin
      // Построить график из предыдущей DATA-секции, если она была.
      // (Случай, когда в буфере несколько графиков без свойств, только данные)
      ProcessData;

      // Заканчиваем собирать строки формата.
      // (Если до этой DATA-секции была PROPERTIES-секция)
      ProcessingProps := False;
    end;
  end
  else if ProcessingProps then
  begin
    Props.Add(LeftTill(Str, #0));
  end
  else inherited;
end;
{%endregion}

{%region TClipbrdTableReader}
//constructor TClipbrdTableReader.Create(Params: TGraphAppendParams);
//begin
//  inherited;
//
//  FStream := TStringStream.Create(Clipboard.AsText);
//  FOwnedStream := True;
//end;
{%endregion}
*)
{%region TFileReaders}
class function TFileReaders.GetReadersCount: Integer;
begin
  Result := FReaders.Count;
end;

class function TFileReaders.GetReader(Index: Integer): TDataReaderClass;
begin
  Result := TDataReaderClass(FReaders[Index]);
end;

class procedure TFileReaders.RegisterReader(AClass: TDataReaderClass);
begin
  if FReaders = nil then FReaders := TList.Create;
  FReaders.Add(Pointer(AClass));
end;

class function TFileReaders.ReaderByFilter(FilterIndex: Integer): TDataReaderClass;
begin
  Result := TDataReaderClass(FFilters.Objects[FilterIndex-1]);
end;

class function TFileReaders.ReaderByExt(const FileName: String): TDataReaderClass;
var
  I, J: Integer;
  Ext: String;
  Strs: TStringList;
begin
  Ext := ExtractFileExtNoDot(FileName);
  Strs := TStringList.Create;
  try
    for I := 0 to FReaders.Count-1 do
    begin
      Strs.Clear;
      Readers[I].FileExts(Strs);
      for J := 0 to Strs.Count-1 do
        if SameText(Ext, Strs[J]) then
        begin
          Result := Readers[I];
          Exit;
        end;
    end;
  finally
    Strs.Free;
  end;
  Result := TCSVFileReader; // default reader
end;

class function TFileReaders.FileFilters: String;
var
  I, J: Integer;
  R: TDataReaderClass;
begin
  if FFilters = nil then
    FFilters := TStringList.Create;
  FFilters.Clear;
  for I := 0 to FReaders.Count-1 do
  begin
    R := Readers[I];
    J := FFilters.Count;
    R.FileFilters(FFilters);
    for J := J to FFilters.Count-1 do
      FFilters.Objects[J] := TObject(R);
  end;
  FFilters.Add(Filter_All);
  Result := '';
  for I := 0 to FFilters.Count-1 do
    Result := Result + FFilters[I];
end;
{%endregion}

initialization
  TFileReaders.RegisterReader(TCSVFileReader);
  //TFileReaders.RegisterReader(TRSFileReader);
  //TFileReaders.RegisterReader(TOOFileReader);
  //TFileReaders.RegisterReader(TDagatronFileReaders);

finalization
  TFileReaders.FReaders.Free;
  TFileReaders.FFilters.Free;

end.
