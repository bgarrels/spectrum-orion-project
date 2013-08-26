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

end.

