unit DlgParamsRandom;

{$mode objfpc}{$H+}

interface

uses
  Forms, ButtonPanel, StdCtrls,
  SpectrumTypes, FrmRange, FrmRangePoints;

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
  private
    FParams: PRandomSampleParams;
  public
    constructor Create(var AParams: TRandomSampleParams); reintroduce;
  end;

implementation

uses
  Controls,
  OriMath, OriIniFile, OriUtils_Gui,
  SpectrumStrings, SpectrumSettings;

{$R *.lfm}

var
  StateLoaded: Boolean = False;
  State: TRandomSampleParams;

procedure SaveState(Ini: TOriIniFile);
begin
  with Ini, State do
  begin
    Section := 'PARAMS_RANDOM_SAMPLE';
    WriteFloat('MinY', MinY);
    WriteFloat('MaxY', MaxY);
    SpectrumTypes.Save(X, 'X', Ini);
  end;
end;

procedure LoadState;
var
  Ini: TOriIniFile;
begin
  Ini := TOriIniFile.Create;
  with Ini, State do
  try
    Section := 'PARAMS_RANDOM_SAMPLE';
    MinY := ReadFloat('MinY', 0);
    MaxY := ReadFloat('MaxY', 100);
    SpectrumTypes.Load(X, 'X', Ini);
  finally
    Free;
  end;
end;

constructor TRandomParamsDlg.Create(var AParams: TRandomSampleParams);
begin
  inherited Create(Application.MainForm);

  FParams := @AParams;
end;

procedure TRandomParamsDlg.FormCreate(Sender: TObject);
begin
  SetDefaultColor(Self);
  ScaleDPI(Self, 96);

  if not StateLoaded then LoadState;

  RangeX.Min := State.X.Min;
  RangeX.Max := State.X.Max;
  PointsX.Step := State.X.Step;
  PointsX.Points := State.X.Points;
  PointsX.UseStep := State.X.UseStep;
  RangeY.Min := State.MinY;
  RangeY.Max := State.MaxY;

  ActiveControl := RangeX.ActiveControl;
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
    begin
      ErrorDlg(ParamErr_ZeroRange);
      Exit;
    end;

    if PointsX.UseStep then
    begin
      Value := PointsX.Step;
      if Value < 0 then
      begin
        Value := -Value;
        PointsX.Step := Value;
      end;
      if (Value = 0) or (Value >= MaxValue - MinValue) then
      begin
        ErrorDlg(ParamErr_WrongStep, [Value, MaxValue-MinValue]);
        Exit;
      end;
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

  if (ModalResult = mrOk) and not StateLoaded then
  begin
    StateLoaded := True;
    RegisterSaveStateProc(@SaveState);
  end;

  FParams^ := State;
  CloseAction := caFree;
end;

end.

