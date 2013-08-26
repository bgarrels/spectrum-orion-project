unit DlgFormulaEditor;

{$mode objfpc}{$H+}

interface

uses
  Forms, ButtonPanel, StdCtrls, FrmRange,
  FrmRangePoints;

type
  TFormulaEditorDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    CodeEditor: TMemo;
    RangeX: TRangeFrm;
    PointsX: TRangePointsFrm;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  end;


implementation

uses
  OriUtils_Gui;

{$R *.lfm}

{ TFormulaEditorDlg }

procedure TFormulaEditorDlg.FormCreate(Sender: TObject);
begin
  SetDefaultColor(Self);
  ScaleDPI(Self, 96);
end;

procedure TFormulaEditorDlg.FormResize(Sender: TObject);
begin
  RangeX.BorderSpacing.Top := PointsX.Height div 4;
end;

end.

