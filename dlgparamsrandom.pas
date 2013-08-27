unit DlgParamsRandom;

{$mode objfpc}{$H+}

interface

uses
  Forms, ButtonPanel, StdCtrls,
  FrmRange, FrmRangePoints;

type
  TRandomParamsDlg = class(TForm)
    ButtonPanel1: TButtonPanel;
    GroupBoxY: TGroupBox;
    GroupBoxX: TGroupBox;
    RangeX: TRangeFrm;
    RangeY: TRangeFrm;
    PointsX: TRangePointsFrm;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
  end;

implementation

uses
  Controls,
  OriMath, OriUtils_Gui,
  SpectrumTypes, SpectrumStrings;

{$R *.lfm}

var
  StateLoaded: Boolean = False;
  State: TRandomSampleParams;
{
procedure SaveState(ASaver: TPropSaver);
begin
  with ASaver, State do
  begin
    Section := 'PARAMS_RANDOM_SAMPLE';
    WriteFloat('MinY', MinY);
    WriteFloat('MaxY', MaxY);
    X.Save('X', ASaver);
  end;
end;

procedure LoadState;
var
  Ini: TPropSaver;
begin
  Ini := TPropSaver.Create(GetIniName);
  with Ini, State do
  try
    Section := 'PARAMS_RANDOM_SAMPLE';
    MinY := ReadFloat('MinY', 0);
    MaxY := ReadFloat('MaxY', 100);
    X.Load('X', Ini);
  finally
    Free;
  end;
end;}

procedure TRandomParamsDlg.FormCreate(Sender: TObject);
begin
  SetDefaultColor(Self);
  ScaleDPI(Self, 96);

  //if not StateLoaded then LoadState;

  RangeX.Min := State.X.Min;
  RangeX.Max := State.X.Max;
  PointsX.Step := State.X.Step;
  PointsX.Points := State.X.Points;
  PointsX.UseStep := State.X.UseStep;
  RangeY.Min := State.MinY;
  RangeY.Max := State.MaxY;
end;

procedure TRandomParamsDlg.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  MinValue, MaxValue, Value: TValue;
begin
  if ModalResult = mrOk then
  begin
    CanClose := False;

    MinValue := RangeX.Min;
    MaxValue := RangeX.Max;
    if MinValue > MaxValue then
    begin
      RangeX.Max := MinValue;
      RangeX.Min := MaxValue;
      Swap(MinValue, MaxValue);
    end
    else if MinValue = MaxValue then
      ErrorDlg(ParamErr_ZeroRange);

    if PointsX.UseStep then
    begin
      Value := PointsX.Step;
      if Value < 0 then
      begin
        Value := -Value;
        PointsX.Step := Value;
      end;
      if (Value = 0) or (Value >= MaxValue - MinValue) then
        ErrorDlg(ParamErr_WrongStep, [Value, MaxValue-MinValue]);
    end;

    CanClose := True;
  end;
end;

procedure TRandomParamsDlg.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  State.X.Min := RangeX.Min;
  State.X.Max := RangeX.Max;
  State.X.Step := PointsX.Step;
  State.X.Points := PointsX.Points;
  State.X.UseStep := PointsX.UseStep;
  State.MinY := RangeY.Min;
  State.MaxY := RangeY.Max;

  //CopyMemory(FParams, @State, SizeOf(TRandomSampleParams));
  //
  //if (ModalResult = mrOk) and not StateLoaded then
  //begin
  //  StateLoaded := True;
  //  RegisterSaveStateProc(SaveState);
  //end;

  CloseAction := caFree;
end;

end.

