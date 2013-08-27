unit OriIniFile;

{$mode objfpc}{$H+}

interface

uses
  Classes, IniFiles;

type
  TOriIniFile = class(TIniFile)
  public
    constructor Create; reintroduce;
    procedure Save;
  end;

{
  Pointer to a function returning path to application ini-file.
  It is used by TOriIniFile when its constructor is called without parameters.

}
var GetIniName: function(): String;

var IsPortable: Boolean;

implementation

uses
  SysUtils, FileUtil;

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

constructor TOriIniFile.Create;
begin
  if Assigned(GetIniName)
    then inherited Create(GetIniName())
    else inherited Create(GetIniFileName);
end;

procedure TOriIniFile.Save;
begin
  UpdateFile;
end;

initialization
  GetIniName := @GetIniFileName;
  IsPortable := FileExistsUTF8(GetLocalIniFileName);

end.

