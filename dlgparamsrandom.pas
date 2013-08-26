unit DlgParamsRandom;

{$mode objfpc}{$H+}

interface

uses
  Forms, ButtonPanel, StdCtrls,
  FrmRange, FrmRangePoints;

type

  { TRandomParamsDlg }

  TRandomParamsDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    GroupBoxY: TGroupBox;
    GroupBoxX: TGroupBox;
    RangeX: TRangeFrm;
    RangeY: TRangeFrm;
    PointsX: TRangePointsFrm;
    procedure FormCreate(Sender: TObject);
  end;

implementation

uses
  OriUtils_Gui;

{$R *.lfm}

procedure TRandomParamsDlg.FormCreate(Sender: TObject);
begin
  SetDefaultColor(Self);
  ScaleDPI(Self, 96);
end;

end.

