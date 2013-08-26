unit DesignTime;

{$mode objfpc}{$H+}

interface

uses
  Classes, LResources,
  OriTabs, OriEditors;

procedure Register;

implementation

const
  ORION_COMPONENTS_PAGE = 'Orion';

procedure Register;
begin
  RegisterComponents(ORION_COMPONENTS_PAGE, [TOriTabSet, TOriFloatEdit]);
end;

initialization
  {$I icons.lrs}
end.

