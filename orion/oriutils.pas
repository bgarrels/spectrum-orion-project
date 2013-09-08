unit OriUtils;

interface

uses
  FGL;

type
  TIntegerList = specialize TFPGList<Integer>;

{%region Paths and FileNames}
function EnsurePath(const APath: String): String;
function ExtractFileExtNoDot(const FileName: String): String;
{%endregion}

{%region Log}
procedure WriteLogString(const LogString: String; Params: array of const); overload;
procedure WriteLogString(const FileName: String; const LogString: String; Params: array of const); overload;
procedure WriteLogString(const FileName: String; const LogString: String); overload;
{%endregion}

implementation

uses
  SysUtils, FileUtil;

{%region Log}
procedure WriteLogString(const LogString: String; Params: array of const);
{$ifndef ORI_DISABLE_LOG}
var
  FileName: String;
{$endif}
begin
{$ifndef ORI_DISABLE_LOG}
  FileName := ChangeFileExt(ParamStrUTF8(0), '.log');
  WriteLogString(FileName, Format(LogString, Params));
{$endif}
end;

procedure WriteLogString(const FileName: String; const LogString: String; Params: array of const);
begin
{$ifndef ORI_DISABLE_LOG}
  WriteLogString(FileName, Format(LogString, Params));
{$endif}
end;

procedure WriteLogString(const FileName: String; const LogString: String);
{$ifndef ORI_DISABLE_LOG}
var
  FileOut: TextFile;
{$endif}
begin
{$ifndef ORI_DISABLE_LOG}
  if not FileExistsUTF8(FileName) then
    FileClose(FileCreateUTF8(FileName));
  AssignFile(FileOut, UTF8ToSys(FileName));
  Append(FileOut);
  WriteLn(FileOut, Format('%s : %s', [
    FormatDateTime('yyyy/mm/dd hh:nn:ss.zzz', Now), LogString]));
  Flush(FileOut);
  CloseFile(FileOut);
{$endif}
end;
{%endregion}

{%region Paths and FileNames}
// Procedure checks if a path exists and substitutes an existed if not.
function EnsurePath(const APath: String): String;
begin
  if (APath = '') or not (DirectoryExistsUTF8(APath))
    then Result := ExtractFilePath(ParamStrUTF8(0))
    else Result := APath;
end;

function ExtractFileExtNoDot(const FileName: String): String;
begin
  Result := Copy(ExtractFileExt(FileName), 2, MaxInt);
end;
{%endregion}

end.
