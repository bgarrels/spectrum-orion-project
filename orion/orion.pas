{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit Orion;

interface

uses
  OriTabs, DesignTime, OriEditors, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DesignTime', @DesignTime.Register);
end;

initialization
  RegisterPackage('Orion', @Register);
end.
