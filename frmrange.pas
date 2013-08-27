unit FrmRange;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, OriEditors;

type
  TRangeFrm = class(TFrame)
    fedMax: TOriFloatEdit;
    fedMin: TOriFloatEdit;
    labMin: TLabel;
    labMax: TLabel;
  private
    procedure SetMin(const Value: Double);
    procedure SetMax(const Value: Double);
    function GetMin: Double;
    function GetMax: Double;
  public
    property Min: Double read GetMin write SetMin;
    property Max: Double read GetMax write SetMax;
  end;

implementation

{$R *.lfm}

procedure TRangeFrm.SetMin(const Value: Double);
begin
  fedMin.Value := Value;
end;

procedure TRangeFrm.SetMax(const Value: Double);
begin
  fedMax.Value := Value;
end;

function TRangeFrm.GetMin: Double;
begin
  Result := fedMin.Value;
end;

function TRangeFrm.GetMax: Double;
begin
  Result := fedMax.Value;
end;

end.
