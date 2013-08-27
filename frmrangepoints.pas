unit FrmRangePoints;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, Spin, OriEditors;

type
  TRangePointsFrm = class(TFrame)
    fedStep: TOriFloatEdit;
    rbPoints: TRadioButton;
    rbStep: TRadioButton;
    sePoints: TSpinEdit;
    procedure fedStepChange(Sender: TObject);
    procedure rbPointsChange(Sender: TObject);
    procedure rbStepChange(Sender: TObject);
    procedure sePointsChange(Sender: TObject);
  private
    procedure SetStep(const Value: Double);
    procedure SetPoints(Value: Integer);
    procedure SetUseStep(Value: Boolean);
    function GetStep: Double;
    function GetPoints: Integer;
    function GetUseStep: Boolean;
  public
    property Step: Double read GetStep write SetStep;
    property Points: Integer read  GetPoints write SetPoints;
    property UseStep: Boolean read GetUseStep write SetUseStep;
  end;

implementation

{$R *.lfm}

procedure TRangePointsFrm.rbStepChange(Sender: TObject);
begin
  if rbStep.Checked then fedStep.SetFocus;
end;

procedure TRangePointsFrm.fedStepChange(Sender: TObject);
begin
  rbStep.Checked := True;
end;

procedure TRangePointsFrm.rbPointsChange(Sender: TObject);
begin
  if rbPoints.Checked then sePoints.SetFocus;
end;

procedure TRangePointsFrm.sePointsChange(Sender: TObject);
begin
  rbPoints.Checked := True;
end;

procedure TRangePointsFrm.SetStep(const Value: Double);
begin
  fedStep.Value := Value;
end;

procedure TRangePointsFrm.SetPoints(Value: Integer);
begin
  sePoints.Value := Value;
end;

procedure TRangePointsFrm.SetUseStep(Value: Boolean);
begin
  rbStep.Checked := Value;
  rbPoints.Checked := not Value;
end;

function TRangePointsFrm.GetStep: Double;
begin
  Result := fedStep.Value;
end;

function TRangePointsFrm.GetPoints: Integer;
begin
  Result := sePoints.Value;
end;

function TRangePointsFrm.GetUseStep: Boolean;
begin
  Result := rbStep.Checked;
end;

end.
