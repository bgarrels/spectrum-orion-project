unit OriUtils;

interface

uses
  FGL;

type
  TIntegerList = specialize TFPGList<Integer>;

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

end.