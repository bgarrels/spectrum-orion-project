unit PlotReadersAux;

interface

uses
  Classes,
  PlotReaders;

type

  TRSFileReader = class(TDataReader)
  private
    FSettings: TStream;
  public
    procedure Read(const AFileName: String); override;
    procedure Read(AStream: TStream); override;
    class procedure FileFilters(Strings: TStrings); override;
    class procedure FileExts(Strings: TStrings); override;
  end;

  TOOFileReader = class(TCSVFileReader)
    class procedure FileFilters(Strings: TStrings); override;
    class procedure FileExts(Strings: TStrings); override;
  end;

  TDagatronFileReaders = class(TCSVFileReader)
  protected
    procedure InitFloatFormat; override;
    procedure ProcessString(const Str: String); override;
  public
    class procedure FileFilters(Strings: TStrings); override;
  end;

implementation

uses
  SysUtils,
  OriStrings, SpectrumStrings, SpectrumSettings, SpectrumTypes;

{%region TRSFileReader}
procedure TRSFileReader.Read(const AFileName: String);
var
  DataStream: TStream;
  SetFile: String;
begin
  SetFile := ChangeFileExt(AFileName, '.set');
  if not FileExists(SetFile) then
    raise ESpectrumError.CreateFmt(Err_RSNoSetFile, [SetFile]);

  FSettings := TFileStream.Create(SetFile, fmOpenRead or fmShareDenyWrite);
  try
    DataStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      Read(DataStream);
    finally
      DataStream.Free;
    end;
  finally
    FSettings.Free;
  end;
end;

procedure TRSFileReader.Read(AStream: TStream);
var
  Vers: Byte;
  Buf: array [0..3] of Char;
  dwAddrStart, dwAddrStop, dwAddrData: Cardinal;
  Xmin, Xmax, Xstep: Double;
  X: Double;
  Y: Single;
  I, IncValue: Integer;
begin
  Buf[0] := #0; // warning suppress
  Xmin := 0; // warning suppress
  Xmax := 0; // warning suppress
  Y := 0; // warning suppress

  ResetResults;
  ResetValues;

  Vers := Preferences.RSFileVersion;
  if Vers = 0 then
  begin // try autodetect
    AStream.Seek($0029, soFromBeginning);
    AStream.Read(Buf, 4);
    if (Buf[0] = '1') and (Buf[1] = '.') and (Buf[3] = '0') then
      case Buf[2] of
        '8': Vers := 1; // Version 1.80 - FSE 40GHz
        '6': Vers := 2; // Version 1.60 - FSE 3GHz
      end;
  end;
  case Vers of
    1: {FSE 40Ghz}
    begin
      dwAddrStart := $21DB;
      dwAddrStop := $21E3;
      dwAddrData := $03B7;
    end;
    2: {FSE 3Ghz}
    begin
      dwAddrStart := $1E67;
      dwAddrStop := $1EB8;
      dwAddrData := $0847;
    end;
    else
      raise ESpectrumError.Create(Err_RSUnknownVersion);
  end;
  FSettings.Seek(dwAddrStart, soFromBeginning);
  FSettings.Read(Xmin, 8);
  FSettings.Seek(dwAddrStop, soFromBeginning);
  FSettings.Read(Xmax, 8);
  Xstep := (Xmax - Xmin) / 499.0;
  X := Xmin;
  IncValue := 1 + Ord(Preferences.RSReadLowValues);
  SetLength(FValuesX, 500 * IncValue);
  SetLength(FValuesY, 500 * IncValue);
  AStream.Seek(dwAddrData, soFromBeginning);
  FValueIndex := 0;
  I := 0;
  while I < 500 do
  begin
    AStream.Read(Y, 4);
    FValuesX[FValueIndex] := X;
    FValuesY[FValueIndex] := Y;
    Inc(FValueIndex, IncValue);
    X := X + Xstep;
    Inc(I);
  end;
  if Preferences.RSReadLowValues then
  begin
    X := Xmin;
    FValueIndex := 1;
    I := 0;
    while I < 500 do
    begin
      AStream.Read(Y, 4);
      FValuesX[FValueIndex] := X;
      FValuesY[FValueIndex] := Y;
      Inc(FValueIndex, IncValue);
      X := X + Xstep;
      Inc(I);
    end;
  end;

  AddResult(FValuesX, FValuesY);
end;

class procedure TRSFileReader.FileFilters(Strings: TStrings);
begin
  Strings.Add(Filter_RS);
end;

class procedure TRSFileReader.FileExts(Strings: TStrings);
begin
  Strings.Add('tr1');
  Strings.Add('tr2');
  Strings.Add('tr3');
  Strings.Add('tr4');
end;
{%endregion}

{%region TOOFileReader}
class procedure TOOFileReader.FileFilters(Strings: TStrings);
begin
  Strings.Add(Filter_OO);
end;

class procedure TOOFileReader.FileExts(Strings: TStrings);
begin
  Strings.Add('scope');
end;
{%endregion}

{%region TDagatronFileReaders}
procedure TDagatronFileReaders.InitFloatFormat;
begin
  FFloatFmt.DecimalSeparator := '.';
end;

procedure TDagatronFileReaders.ProcessString(const Str: String);
var
  S: String;
  I: Integer;
  P, P1: PChar;
  Value: Double;
begin
  S := '';
  P := PChar(Str);
  P1 := P;
  repeat
    Inc(P);
    if P^ = #0 then
    begin
      // First column is timestamp, skip it.
      // The rest columns are sticked together.
      if CharPos(P1, ':') = 0 then
      begin
        S := S + P1;
      end;
      P1 := P + 1;
    end;
  until P1^ = #0;
  // Remove all non digit chars from the end
  for I := Length(S) downto 1 do
    if S[I] in ['0'..'9'] then
    begin
      SetLength(S, I);
      Break;
    end;
  // Skip some scrap (lines like "14:12:53  ð         0")
  // and zero values (lines like  "17:10:48            0")
  // (samples are taken from a real file written by Dagatron counter)
  if TryStrToFloat(S, Value, FFloatFmt) and (Value > 0) then
  begin
    CheckValuesSize;
    if FValueIndex > 0 then
      // Skip several consecutive identical values
      if FValuesY[FValueIndex-1] = Value then Exit;
    FValuesX[FValueIndex] := FOneColumnX;
    FValuesY[FValueIndex] := Value;
    FOneColumnX := FOneColumnX + Preferences.OneColumnInc;
    Inc(FValueIndex);
  end;
end;

class procedure TDagatronFileReaders.FileFilters(Strings: TStrings);
begin
  Strings.Add(Filter_Dag);
end;
{%endregion}

end.
