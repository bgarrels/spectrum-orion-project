unit OriIniFile;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, IniFiles;

type
  TOriIniFile = class(TIniFile)
  private
    FSection: String;
    FFormat: TFormatSettings;
  public
    constructor Create; reintroduce;
    property Section: String read FSection write FSection;
    procedure WriteString(const Key, Value: String); overload;
    procedure WriteInteger(const Key: string; Value: Longint); overload;
    procedure WriteBool(const Key: string; Value: Boolean); overload;
    procedure WriteFloat(const Key: string; Value: Extended); overload;
    procedure WriteDate(const Key: string; Value: TDateTime); overload;
    procedure WriteTime(const Key: string; Value: TDateTime); overload;
    procedure WriteDateTime(const Key: string; Value: TDateTime); overload;
    function ReadString(const Key, Value: String): String; overload;
    function ReadInteger(const Key: string; Value: Longint): Longint; overload;
    function ReadBool(const Key: string; Value: Boolean): Boolean; overload;
    function ReadFloat(const Key: string; Value: Extended): Extended; overload;
    function ReadDate(const Key: string; Value: TDateTime): TDateTime; overload;
    function ReadTime(const Key: string; Value: TDateTime): TDateTime; overload;
    function ReadDateTime(const Key: string; Value: TDateTime): TDateTime; overload;
    procedure Init;
    procedure UpdateFile; override;
  end;

{ Pointer to a function returning path to application ini-file.
  It is used by TOriIniFile when its constructor is called without parameters.
  The GetIniFileName function is used by default.
}
var GetIniName: function(): String;

{ The flag defines if application is portable. Portable application stores all
  its settings, templates and various user-specific infomation near the program
  executable file. So application must have write access to a folder where it
  is located. Non-portable (normal) application stores its settings in user
  profile catalog (~/.config/appname/ on Linux, $(APPDATA)\appname\ on Windows).
}
var IsPortable: Boolean;

const CMainSection = 'PREFERENCES';

implementation

uses
  FileUtil, Math;

{%region Ini File Location}
var IniFileName: String;

function GetLocalIniFileName: String;
begin
  Result := ChangeFileExt(ParamStrUTF8(0), '.cfg');
end;

function GetIniFileName: String;
begin
  if IniFileName = '' then
  begin
    if IsPortable
      then IniFileName := GetLocalIniFileName
      else IniFileName := GetAppConfigFileUTF8(False, True);
  end;
  Result := IniFileName;
end;
{%endregion}


{%region TOriIniFile}
constructor TOriIniFile.Create;
begin
  FSection := CMainSection;

  if Assigned(GetIniName)
    then inherited Create(GetIniName())
    else inherited Create(GetIniFileName);

  Init;
end;

procedure TOriIniFile.Init;
begin
  FFormat.DecimalSeparator := '.';
  FFormat.DateSeparator := '.';
  FFormat.TimeSeparator := ':';
  FFormat.ShortDateFormat := 'dd.mm.yyyy';
  FFormat.ShortTimeFormat := 'hh:nn';
  FFormat.LongDateFormat := 'dd mmmm yyyy';
  FFormat.LongTimeFormat := 'hh:nn:ss.zzzz';

  CaseSensitive := True;
  CacheUpdates := True;
end;

procedure TOriIniFile.UpdateFile;
var
  Dir: String;
begin
  if not Dirty then Exit;
  Dir := TrimFilename(ExtractFilePath(FileName));
  if not DirectoryExistsUTF8(Dir) then
    if not CreateDirUTF8(Dir) then
      raise Exception.CreateFmt('Unable to create directory "%s"', [Dir]);
  inherited;
end;

{%region Common Types}
procedure TOriIniFile.WriteString(const Key, Value: String);
begin
  WriteString(FSection, Key, Value);
end;

function TOriIniFile.ReadString(const Key, Value: String): String;
begin
  Result := ReadString(FSection, Key, Value);
end;

procedure TOriIniFile.WriteInteger(const Key: string; Value: Longint);
begin
  WriteInteger(FSection, Key, Value);
end;

function TOriIniFile.ReadInteger(const Key: string; Value: Longint): Longint;
begin
  Result := ReadInteger(FSection, Key, Value);
end;

procedure TOriIniFile.WriteBool(const Key: string; Value: Boolean);
begin
  WriteBool(FSection, Key, Value);
end;

function TOriIniFile.ReadBool(const Key: string; Value: Boolean): Boolean;
begin
  Result := ReadBool(FSection, Key, Value);
end;

procedure TOriIniFile.WriteFloat(const Key: string; Value: Extended);
begin
  WriteString(FSection, Key, FloatToStr(Value, FFormat));
end;

function TOriIniFile.ReadFloat(const Key: string; Value: Extended): Extended;
var S: String;
begin
  S := UpperCase(Trim(ReadString(FSection, Key, '')));
  if S = 'INF' then
    Result := Infinity
  else if S = '-INF' then
    Result := -Infinity
  else
    Result := StrToFloatDef(S, Value, FFormat);
end;
{%endregion}

{%region Date and Time}
procedure TOriIniFile.WriteDate(const Key: string; Value: TDateTime);
begin
  WriteString(FSection, Key, DateToStr(Value, FFormat));
end;

procedure TOriIniFile.WriteTime(const Key: string; Value: TDateTime);
begin
  WriteString(FSection, Key, TimeToStr(Value, FFormat));
end;

procedure TOriIniFile.WriteDateTime(const Key: string; Value: TDateTime);
begin
  WriteString(FSection, Key, DateTimeToStr(Value, FFormat));
end;

function TOriIniFile.ReadDate(const Key: string; Value: TDateTime): TDateTime;
begin
  if not TryStrToDate(ReadString(FSection, Key, ''), Result, FFormat) then Result := Value;
end;

function TOriIniFile.ReadTime(const Key: string; Value: TDateTime): TDateTime;
begin
  if not TryStrToTime(ReadString(FSection, Key, ''), Result, FFormat) then Result := Value;
end;

function TOriIniFile.ReadDateTime(const Key: string; Value: TDateTime): TDateTime;
begin
  if not TryStrToDateTime(ReadString(FSection, Key, ''), Result, FFormat) then Result := Value;
end;
{%endregion}

{%endregion}

initialization
  GetIniName := @GetIniFileName;
  IsPortable := FileExistsUTF8(GetLocalIniFileName);

end.

