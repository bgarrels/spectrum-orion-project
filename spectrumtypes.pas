unit SpectrumTypes;

interface

uses
  SysUtils, Math;
//  PropSaver;

type
  TValue = Double;
  TValueArray = array of TValue;
  PValueArray = ^TValueArray;

const
  CMinValue = -MaxDouble;
  CMaxValue = MaxDouble;
  VALUE_MIN = -MaxDouble;
  VALUE_MAX = MaxDouble;

type
  TGraphRec = record
    X, Y: TValueArray;
  end;
  PGraphRec = ^TGraphRec;

  TGraphRec2 = record
    X, Y1, Y2: TValueArray;
  end;

  TAxisDirection = (adirX, adirY);
  TGroupOperation = (gropSum, gropDiff, gropProd, gropQuot);

  TScaleCenterKind = (sckNone, sckMax, sckMin, sckAvg, sckValue);
  TScaleParams = packed record
    Direction: TAxisDirection;
    CenterKind: TScaleCenterKind;
    CenterValue: TValue;
    Value: TValue;
  end;
  PScaleParams = ^TScaleParams;

  TNormalizeParams = packed record
    Direction: TAxisDirection;
    PerMaximum: Boolean;
    Value: TValue;
  end;
  PNormalizeParams = ^TNormalizeParams;

  TOffsetKind = (ofkMax, ofkMin, ofkAvg, ofkValue);
  TOffsetParams = packed record
    Direction: TAxisDirection;
    Kind: TOffsetKind;
    Value: TValue;
  end;
  POffsetParams = ^TOffsetParams;

  TDespikeRange = (drPercent, drAbsolute);
  TDespikeParams = packed record
    Kind: TDespikeRange;
    Min: TValue;
    Max: TValue;
  end;
  PDespikeParams = ^TDespikeParams;

  TMiscParams = packed record
    Direction: TAxisDirection;
    Value: TValue;
  end;
  PMiscParams = ^TMiscParams;

  TRangeParams = packed record
    Min, Max, Step: TValue;
    Points: Integer;
    UseStep: Boolean;
  end;
  PRangeParams = ^TRangeParams;

  procedure CalcStep(var AParams: TRangeParams; out ACount: Integer; out AStep: TValue);
  //procedure Save(var AParams: TRangeParams; const Key: String; Ini: TPropSaver);
  //procedure Load(var AParams: TRangeParams; const Key: String; Ini: TPropSaver);

type
  TRandomSampleParams = packed record
    X: TRangeParams;
    MinY, MaxY: TValue;
  end;
  PRandomSampleParams = ^TRandomSampleParams;

  TFormulaParams = class
  public
    Formula: String;
    Range: TRangeParams;
  end;

  TTrimParams = packed record
    TrimLeft, TrimRight: Boolean;
    VisibleLeft, VisibleRight: Boolean;
    RefineLeft, RefineRight: Boolean;
    ValueLeft, ValueRight: TValue;
  end;
  PTrimParams = ^TTrimParams;

  TVarianceKind = (varStandard, varAllan);
  TVarianceParams = record
    Kind: TVarianceKind;
  end;
  PVarianceParams = ^TVarianceParams;

const
  VarianceNames: array[TVarianceKind] of String = ('SDEV', 'ADEV');

type
  ESpectrumError = class(Exception);

implementation

{%region 'TRangeParams'}
procedure CalcStep(var AParams: TRangeParams; out ACount: Integer; out AStep: TValue);
begin
  with AParams do
    if UseStep then
    begin
      ACount := Trunc((Max - Min) / Step) + 1;
      AStep := Step;
    end
    else
    begin
      ACount := Points;
      AStep := (Max - Min) / (Points - 1);
    end;
end;

//procedure Save(var AParams: TRangeParams; const Key: String; Ini: TPropSaver);
//begin
//  with AParams, Ini do
//  begin
//    WriteFloat(Key + '.Min', Min);
//    WriteFloat(Key + '.Max', Max);
//    WriteFloat(Key + '.Step', Step);
//    WriteInteger(Key + '.Points', Points);
//    WriteBool(Key + '.UseStep', UseStep);
//  end
//end;
//
//procedure Load(var AParams: TRangeParams; const Key: String; Ini: TPropSaver);
//begin
//  with AParams, Ini do
//  begin
//    Min := ReadFloat(Key + '.Min', 0);
//    Max := ReadFloat(Key + '.Max', 100);
//    Step := ReadFloat(Key + '.Step', 1);
//    Points := ReadInteger(Key + '.Points', 101);
//    UseStep := ReadBool(Key + '.UseStep', True);
//  end;
//end;
{%endregion}

end.
