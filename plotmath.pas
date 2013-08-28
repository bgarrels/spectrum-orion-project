unit PlotMath;

interface

uses
//  Classes,
//  TeEngine,
//  StringCalc,
  SpectrumTypes{, Plots};

type
  TSelectGraphsDlgOption = (sgdoAllowReorder);
  TSelectGraphsDlgOptions = set of TSelectGraphsDlgOption;
//function SelectGraphs(const ACaption: String; APlot: TPlot;
//  var Selects: TGraphArray; Selected: TCustomSeriesList = nil;
//  Options: TSelectGraphsDlgOptions = []): Boolean; overload;
//function SelectGraph(): TGraph; overload;

//function AskTrimParams(var AParams: TTrimParams): Boolean;
function AskRandomSampleParams(var AParams: TRandomSampleParams): Boolean;
//function AskOffsetParams(var AParams: TOffsetParams): Boolean;
//function AskNormalizeParams(var AParams: TNormalizeParams): Boolean;
//function AskScaleParams(var AParams: TScaleParams): Boolean;
//function AskFlipParams(var AParams: TMiscParams): Boolean;
//function AskInverseParams(var AParams: TMiscParams): Boolean;
//function AskVarianceParams(var AParams: TVarianceParams): Boolean;
//function AskDespikeParams(var AParams: TDespikeParams): Boolean;
//function AskFormulaParams(AParams: TFormulaParams): Boolean;
//function EditFormulaParams(AParams: TFormulaParams): Boolean;

function GetSampleGraph(var AParams: TRandomSampleParams): TGraphRec;
procedure FillSampleValues(var ValuesX: TValueArray; var ValuesY: TValueArray;
  Count: Integer); overload;
procedure FillSampleValues(var ValuesX: TValueArray; var ValuesY: TValueArray;
  var AParams: TRandomSampleParams); overload;

function TrimValuesLeft(var ValuesX: TValueArray; var ValuesY: TValueArray;
  const Value: TValue; Refine: Boolean): Boolean;
function TrimValuesRight(var ValuesX: TValueArray; var ValuesY: TValueArray;
  const Value: TValue; Refine: Boolean): Boolean;
procedure NormalizeValues(var Values: TValueArray; const Value: TValue; PerMax: Boolean);
procedure ScaleValues(var Values: TValueArray;
  CenterKind: TScaleCenterKind; const CenterValue, Value: TValue);
procedure OffsetValues(var Values: TValueArray; Kind: TOffsetKind; const Value: TValue); overload;
procedure OffsetValues(var Values: TValueArray; const Value: TValue); overload;
procedure InverseValues(var Values, Values1: TValueArray; const Value: TValue);
procedure FlipValues(var Values: TValueArray; const Value: TValue);
procedure DespikePer(var Values: TValueArray; RangeMin, RangeMax: TValue);
procedure DespikeAbs(var Values: TValueArray; RangeMin, RangeMax: TValue);
procedure ReorderValues(var ValuesX, ValuesY: TValueArray);
procedure Sort(var ValuesX, ValuesY: TValueArray); overload;
procedure Sort(var G: TGraphRec2); overload;

type
  TPartialFunc =  function (const Values: TValueArray; Index1, Index2: Integer): TValue;

function CallPartialFunc(const ValuesX, ValuesY: TValueArray;
  const Limit1, Limit2: TValue; Func: TPartialFunc): TValue;
procedure GetPartialBounds(const Values: TValueArray;
  const Limit1, Limit2: TValue; out Index1, Index2: Integer);

function GetPointCount(const Values: TValueArray; Min, Max: TValue): Integer;
//procedure GetOverlapLimits(const Graphs: TGraphArray; out Min, Max: TValue);
function CalcOverlapGraph(const Gr1, Gr2: TGraphRec; AllPoints: Boolean = False;
  Min: TValue = VALUE_MIN; Max: TValue = VALUE_MAX): TGraphRec2;

function MeanAM(const Values: TValueArray): TValue; overload;
function MeanAM(const ValuesX, ValuesY: TValueArray; const Limit1, Limit2: TValue): TValue; overload;
function MeanAMPart(const Values: TValueArray; Index1, Index2: Integer): TValue;
function MaxValue(const Values: TValueArray): TValue;
function MinValue(const Values: TValueArray): TValue;
function MaxValueIndex(const Values: TValueArray): Integer;
function MinValueIndex(const Values: TValueArray): Integer;
procedure GetMinMax(const Values: TValueArray; out MinValue, MaxValue: TValue);
function AverageValue(const Values: TValueArray): Extended; // тоже что MeanAM

procedure Differentiate(const X, Y: TValueArray; var DY: TValueArray);
procedure Differentiate2(const X, Y: TValueArray; var DY: TValueArray);
procedure Variance(const X, Y: TValueArray; var SX, SY: TValueArray);
procedure Regularity(const Values: TValueArray; var X, Y: TValueArray);
//procedure SumGraphs(var Selects: TGraphArray);

//function CheckFormula(const Formula: String): Boolean;
//procedure ParseFormula(const Formula: String; Calc: TStringCalc);
//procedure PlotFormula(Params: TFormulaParams; var ValuesX, ValuesY: TValueArray);
function FormulaExpression(const Formula: String): String;

//function GraphToRec(Graph: TGraph): TGraphRec; inline;

implementation

uses
  SysUtils, Controls, Classes, Math,
  OriUtils, OriStrings,
  DlgParamsRandom;
//  WinGraphsSelect, WinParamsTrim, WinParamsRandomSample, WinParamsOffset,
//  WinParamsNorm, WinParamsScale, WinParamsMisc, WinParamsAllan, WinParamsDespike,
//  WinFormula;

{%region 'Ask params routines'}
{
function AskTrimParams(var AParams: TTrimParams): Boolean;
begin
  Result := TwndParamsTrim.Create(@AParams).ShowModal = mrOk;
end;
}
function AskRandomSampleParams(var AParams: TRandomSampleParams): Boolean;
begin
  Result := TRandomParamsDlg.Create(AParams).ShowModal = mrOK;
end;
{
function AskOffsetParams(var AParams: TOffsetParams): Boolean;
begin
  Result := TwndParamsOffset.Create(@AParams).ShowModal = mrOK;
end;

function AskNormalizeParams(var AParams: TNormalizeParams): Boolean;
begin
  Result := TwndParamsNorm.Create(@AParams).ShowModal = mrOK;
end;

function AskScaleParams(var AParams: TScaleParams): Boolean;
begin
  Result := TwndParamsScale.Create(@AParams).ShowModal = mrOK;
end;

function AskFlipParams(var AParams: TMiscParams): Boolean;
begin
  Result := TwndParamsMisc.CreateFlip(@AParams).ShowModal = mrOK;
end;

function AskInverseParams(var AParams: TMiscParams): Boolean;
begin
  Result := TwndParamsMisc.CreateInverse(@AParams).ShowModal = mrOK;
end;

function AskVarianceParams(var AParams: TVarianceParams): Boolean;
begin
  Result := TwndParamsAllan.Create(@AParams).ShowModal = mrOK;
end;

function AskDespikeParams(var AParams: TDespikeParams): Boolean;
begin
  Result := TwndParamsDespike.Create(@AParams).ShowModal = mrOK;
end;

function AskFormulaParams(AParams: TFormulaParams): Boolean;
begin
  Result := TwndFormula.Create(AParams).ShowModal = mrOK;
end;

function EditFormulaParams(AParams: TFormulaParams): Boolean;
begin
  Result := TwndFormula.CreateEdit(AParams).ShowModal = mrOK;
end;
}
{%endregion}

//function SelectGraphs(const ACaption: String; APlot: TPlot;
//  var Selects: TGraphArray; Selected: TCustomSeriesList;
//  Options: TSelectGraphsDlgOptions): Boolean; overload;
//begin
//  Result := False;
//  if APlot.Count > 0 then
//    with TwndGraphsSelect.Create(ACaption, APlot) do
//    try
//      SetOrderBtnsVisible(sgdoAllowReorder in Options);
//      if Assigned(Selected)
//        then SetSelected(Selected)
//        else SetSelected(Selects);
//      Result := ShowModal = mrOk;
//      if Result then
//        Selects := GetSelected;
//    finally
//      Free;
//    end;
//end;

{ Уточнение границы:
  F0 = F1 + (F2 - F1)(X0 - X1)/(X2 - X1)
  где
  X0 - положение границы (точка обрезки)
  F0 - значение графика в точке обрезки
  X1, X2 - ближайшие точки слева и справа от X0
  F1, F2 - значения графика в точках X1, X2
}
function TrimValuesLeft(var ValuesX: TValueArray; var ValuesY: TValueArray;
  const Value: TValue; Refine: Boolean): Boolean;
var
  I: Integer;
  TrimIndex: Integer; // точка, начиная с которой нужно оставить
begin
  TrimIndex := 0;
  for I := 0 to Length(ValuesX)-1 do
    if ValuesX[I] > Value then
    begin
      if Refine and (I > 0) then
      begin
        TrimIndex := I-1;
        ValuesY[TrimIndex] := ValuesY[I-1] + (ValuesY[I] - ValuesY[I-1])*
          (Value - ValuesX[I-1]) / (ValuesX[I] - ValuesX[I-1]);
        ValuesX[TrimIndex] := Value;
      end
      else // оставляем ближайшую к заданной точку
        if (I > 0) and (ValuesX[I]-Value > Value-ValuesX[I-1])
          then TrimIndex := I-1
          else TrimIndex := I;
      Break;
    end;
  // (Length(ValuesX)-2) т.к. нужно чтобы в графике осталось хотя бы две точки.
  // В противном случае считаем, что линии обрезки лежит за пределами
  // графика и никакой обрезки не присходит.
  Result := ((TrimIndex > 0) or Refine) and (TrimIndex < Length(ValuesX)-2);
  if Result then
  begin
    ValuesX := Copy(ValuesX, TrimIndex, MaxInt);
    ValuesY := Copy(ValuesY, TrimIndex, MaxInt);
  end;
end;

function TrimValuesRight(var ValuesX: TValueArray; var ValuesY: TValueArray;
  const Value: TValue; Refine: Boolean): Boolean;
var
  I: Integer;
  TrimIndex: Integer; // точка, начиная с которой нужно удалить
begin
  TrimIndex := 0;
  for I := 0 to Length(ValuesX)-1 do
    if ValuesX[I] > Value then
    begin
      if Refine and (I > 0) then
      begin
        TrimIndex := I;
        ValuesY[TrimIndex] := ValuesY[I-1] + (ValuesY[I] - ValuesY[I-1])*
          (Value - ValuesX[I-1]) / (ValuesX[I] - ValuesX[I-1]);
        ValuesX[TrimIndex] := Value;
      end
      else // Оставляем ближайшую к заданной точку.
        if (I > 0) and (ValuesX[I]-Value > Value-ValuesX[I-1])
          then TrimIndex := I-1
          else TrimIndex := I;
      Break;
    end;
  // (TrimIndex > 1) т.к. нужно чтобы в графике осталось хотя бы две точки.
  // В противном случае считаем, что линии обрезки лежит за пределами
  // графика и никакой обрезки не присходит.
  Result := (TrimIndex > 1) and ((TrimIndex < Length(ValuesX)-1) or Refine);
  if Result then
  begin
    SetLength(ValuesX, TrimIndex+1);
    SetLength(ValuesY, TrimIndex+1);
  end;
end;

function GetSampleGraph(var AParams: TRandomSampleParams): TGraphRec;
begin
  FillSampleValues(Result.X, Result.Y, AParams);
end;

procedure FillSampleValues(var ValuesX: TValueArray; var ValuesY: TValueArray; Count: Integer);
const
  H = 25.0;
var
  i: Integer;
  Y: TValue;
begin
  SetLength(ValuesX, Count);
  SetLength(ValuesY, Count);
  Y := Random(100)*H*0.01;
  for i := 0 to Count-1 do
  begin
    Y := Abs(Y + Random(100)*H*0.01 - H*0.5);
    ValuesX[i] := i;
    ValuesY[i] := Y;
  end;
end;

procedure FillSampleValues(var ValuesX: TValueArray; var ValuesY: TValueArray;
  var AParams: TRandomSampleParams);
var
  I, Count: Integer;
  Y, X, StepX, H: TValue;
begin
  CalcStep(AParams.X, Count, StepX);
  SetLength(ValuesX, Count);
  SetLength(ValuesY, Count);
  Randomize;
  H := (AParams.MaxY - AParams.MinY) / Count * 25;
  X := AParams.X.Min;
  Y := Random*H;
  for I := 0 to Count-1 do
  begin
    ValuesX[I] := X;
    ValuesY[I] := Y;

    X := X + StepX;
    Y := Y + Random*H - H*0.5;

    if Y > AParams.MaxY then  Y := 2* AParams.X.Max - Y
    else if Y < AParams.MinY then Y := 2* AParams.X.Min - Y;
  end;
end;

procedure NormalizeValues(var Values: TValueArray; const Value: TValue; PerMax: Boolean);
var
  I: Integer;
  Fact: Extended;
begin
  if PerMax then
  begin
    Fact := -MaxExtended;
    for I := 0 to Length(Values)-1 do
      if Values[I] > Fact then Fact := Values[I];
  end
  else
    Fact := Value;
  for I := 0 to Length(Values)-1 do
    Values[I] := Values[I] / Fact;
end;

procedure ScaleValues(var Values: TValueArray;
  CenterKind: TScaleCenterKind; const CenterValue, Value: TValue);
var
  I: Integer;
  OffsetValue: TValue;
begin
  case CenterKind of
    sckNone: OffsetValue := 0;
    sckMax: OffsetValue := MaxValue(Values);
    sckMin: OffsetValue := MinValue(Values);
    sckAvg: OffsetValue := AverageValue(Values);
    else OffsetValue := CenterValue;
  end;
  for I := 0 to Length(Values)-1 do
    Values[I] := (Values[I] - OffsetValue) * Value + OffsetValue;
end;

procedure OffsetValues(var Values: TValueArray;
  Kind: TOffsetKind; const Value: TValue);
var
  OffsetValue: TValue;
begin
  case Kind of
    ofkMax: OffsetValue := -MaxValue(Values);
    ofkMin: OffsetValue := -MinValue(Values);
    ofkAvg: OffsetValue := -AverageValue(Values);
    else OffsetValue := Value;
  end;
  OffsetValues(Values, OffsetValue);
end;

procedure OffsetValues(var Values: TValueArray; const Value: TValue);
var I: Integer;
begin
  for I := 0 to Length(Values)-1 do Values[I] := Values[I] + Value;
end;

procedure FlipValues(var Values: TValueArray; const Value: TValue);
var I: Integer;
begin
  for I := 0 to Length(Values)-1 do Values[I] := Value - Values[I];
end;

// Чтобы избежать деления на ноль выбрасываем нулевые точки из обрабатываеммого
// массива. Но, т.к. массив часть графика, то нужно выбросить и соответствующую
// точку и из парного массива.
procedure InverseValues(var Values, Values1: TValueArray; const Value: TValue);
var
  I, J: Integer;
begin
  J := 0;
  for I := 0 to Length(Values)-1 do
    if Values[I] <> 0 then
    begin
      Values[J] := Value / Values[I];
      Values1[J] := Values1[I];
      Inc(J);
    end;
  SetLength(Values, J);
  SetLength(Values1, J);
end;

procedure Differentiate(const X, Y: TValueArray; var DY: TValueArray);
var I: Integer;
begin
  SetLength(DY, Length(X));
  DY[0] := (Y[1] - Y[0]) / (X[1] - X[0]);
  for I := 1 to Length(X)-2 do
    DY[I] := ((Y[I+1] - Y[I]) / (X[I+1] - X[I]) +
      (Y[I] - Y[I-1]) / (X[I] - X[I-1])) / 2;
  I := Length(X)-1;
  DY[I] := (Y[I] - Y[I-1]) / (X[I] - X[I-1]);
end;

procedure Differentiate2(const X, Y: TValueArray; var DY: TValueArray);
var
  I: Integer;
begin
  SetLength(DY, Length(X)-2);
  for I := 1 to Length(X)-2 do
    DY[I-1] := (Y[I+1] + Y[I-1] - Y[I] - Y[I]) / (X[I+1] - X[I]) / (X[I] - X[I-1]);
end;

function MaxValue(const Values: TValueArray): TValue;
var I: Integer;
begin
  Result := -MaxDouble;
  for I := 0 to Length(Values)-1 do
    if Values[I] > Result then Result := Values[I];
end;

function MinValue(const Values: TValueArray): TValue;
var I: Integer;
begin
  Result := MaxDouble;
  for I := 0 to Length(Values)-1 do
    if Values[I] < Result then Result := Values[I];
end;

function MaxValueIndex(const Values: TValueArray): Integer;
var
  I: Integer;
  Tmp: TValue;
begin
  Result := -1;
  Tmp := -MaxDouble;
  for I := 0 to Length(Values)-1 do
    if Values[I] > Tmp then
    begin
      Tmp := Values[I];
      Result := I;
    end;
end;

function MinValueIndex(const Values: TValueArray): Integer;
var
  I: Integer;
  Tmp: TValue;
begin
  Result := -1;
  Tmp := MaxDouble;
  for I := 0 to Length(Values)-1 do
    if Values[I] < Tmp then
    begin
      Tmp := Values[I];
      Result := I;
    end;
end;

procedure GetMinMax(const Values: TValueArray; out MinValue, MaxValue: TValue);
var I: Integer;
begin
  MinValue := CMaxValue;
  MaxValue := CMinValue;
  for I := 0 to Length(Values)-1 do
  begin
    if Values[I] > MaxValue then MaxValue := Values[I];
    if Values[I] < MinValue then MinValue := Values[I];
  end;
end;

function AverageValue(const Values: TValueArray): Extended;
var I: Integer;
begin
  Result := 0;
  for I := 0 to Length(Values)-1 do Result := Result + Values[I];
  Result := Result / Length(Values);
end;

procedure Variance(const X, Y: TValueArray; var SX, SY: TValueArray);
var
  I: Integer;
  MinS, MaxS, AvgS, S: Extended;
begin
  SX := X;
  SY := Y;
  MinS := MaxExtended;
  MaxS := -MinS;
  AvgS := 0;
  for I := 0 to Length(Y)-2 do
  begin
    S := Sqrt(Sqr(Y[I+1] - Y[I]) / 2);
    if S < MinS then MinS := S;
    if S > MaxS then MaxS := S;
    AvgS := AvgS + S;
  end;
  AvgS := AvgS / (Length(Y) - 1);
  //DebugPrint('MinS = %g; S = %g; MaxS = %g', [MinS, AvgS, MaxS]);
end;

procedure GetPartialBounds(const Values: TValueArray;
  const Limit1, Limit2: TValue; out Index1, Index2: Integer);
var I: Integer;
begin
  if Limit1 <> Limit2 then
    for I := 0 to Length(Values)-1 do
    begin
      if Values[I] < Limit1 then
      begin
        Index1 := I+1;
        Continue;
      end;
      if Values[I] > Limit2 then
      begin
        Index2 := I-1;
        Break;
      end;
    end
  else
  begin
    Index1 := 0;
    Index2 := Length(Values)-1;
  end;
end;

function CallPartialFunc(const ValuesX, ValuesY: TValueArray;
  const Limit1, Limit2: TValue; Func: TPartialFunc): TValue;
var
  Index1, Index2: Integer;
begin
  GetPartialBounds(ValuesX, Limit1, Limit2, Index1, Index2);
  Result := Func(ValuesY, Index1+1, Index2-1);
end;

function MeanAM(const Values: TValueArray): TValue;
begin
  Result := MeanAMPart(Values, 0, Length(Values)-1);
end;

function MeanAM(const ValuesX, ValuesY: TValueArray; const Limit1, Limit2: TValue): TValue;
begin
  Result := CallPartialFunc(ValuesX, ValuesY, Limit1, Limit2, @MeanAMPart);
end;

function MeanAMPart(const Values: TValueArray; Index1, Index2: Integer): TValue;
var I: Integer;
begin
  Result := 0;
  for I := Index1 to Index2 do Result := Result + Values[I];
  Result := Result / (Index2 - Index1 + 1);
end;

procedure DespikePer(var Values: TValueArray; RangeMin, RangeMax: TValue);
var
  I: Integer;
  V, Avg: Extended;
  BoundMin, BoundMax: Extended;
  Indexes: TIntegerList;
begin
  BoundMin := 1 - RangeMin/100;
  BoundMax := 1 + RangeMax/100;
  Indexes := TIntegerList.Create;
  try
    for I := 0 to Length(Values)-1 do
      Indexes.Add(I);
    I := Random(Indexes.Count);
    Avg := Values[Indexes[I]];
    Indexes.Delete(I);
    while Indexes.Count > 0 do
    begin
      I := Random(Indexes.Count);
      V := Values[Indexes[I]];
      if (V < Avg * BoundMin) or (V > Avg * BoundMax) then
        Values[Indexes[I]] := Avg
      else
        Avg := (Avg + V) / 2;
      Indexes.Delete(I);
    end;
  finally
    Indexes.Free;
  end;
end;

procedure DespikeAbs(var Values: TValueArray; RangeMin, RangeMax: TValue);
var
  I: Cardinal;
  V, Avg: Extended;
  Indexes: TIntegerList;
begin
  Indexes := TIntegerList.Create;
  try
    for I := 0 to Length(Values)-1 do
      Indexes.Add(I);
    I := Random(Indexes.Count);
    Avg := Values[Indexes[I]];
    Indexes.Delete(I);
    while Indexes.Count > 0 do
    begin
      I := Random(Indexes.Count);
      // We have to analize something only if average value is suited inside
      // the specified range, to avoid such erroneous situation when the range
      // is set to be laying lower or upper of graph's trend.
      if (Avg >= RangeMin) and (Avg <= RangeMax) then
      begin
        V := Values[Indexes[I]];
        if (V < RangeMin) or (V > RangeMax) then
          Values[Indexes[I]] := Avg
        else
          Avg := (Avg + V) / 2;
      end;
      Indexes.Delete(I);
    end;
  finally
    Indexes.Free;
  end;
end;

procedure ReorderValues(var ValuesX, ValuesY: TValueArray);
var
  I, L: Integer;
  Tmp: TValue;
begin
  L := Length(ValuesX);
  for I := 0 to L div 2 do
  begin
    Tmp := ValuesX[I];
    ValuesX[I] := ValuesX[L-I-1];
    ValuesX[L-I-1] := Tmp;

    Tmp := ValuesY[I];
    ValuesY[I] := ValuesY[L-I-1];
    ValuesY[L-I-1] := Tmp;
  end;
end;

function FormulaExpression(const Formula: String): String;
var
  Code, Line: String;
  P, Index: Integer;
begin
  Code := Formula + ';';
  Index := CharPos(Code, ';');
  while Index > 0 do
  begin
    Line := Copy(Code, 1, Index-1);
    P := CharPos(Line, '=');
    if (P > 0) and (Trim(Copy(Line, 1, P-1)) = 'Y') then
    begin
      Result := Trim(Copy(Line, P+1, MaxInt));
      Exit;
    end;
    Code := Copy(Code, Index+1, MaxInt);
    Index := CharPos(Code, ';');
  end;
  Result := Formula;
end;

{procedure ParseFormula(const Formula: String; Calc: TStringCalc);
var
  Code, Line, Name, S: String;
  Index, P: Integer;
  Value: Extended;
  fmt: TFormatSettings;
begin
  fmt.DecimalSeparator := '.';
  Code := Formula + ';';
  Index := CharPos(Code, ';');
  while Index > 0 do
  begin
    Line := Copy(Code, 1, Index-1);
    P := CharPos(Line, '=');
    if P > 0 then
    begin
      Name := Trim(Copy(Line, 1, P-1));
      S := Trim(Copy(Line, P+1, MaxInt));
      if Name <> 'Y' then
      begin
        if not TryStrToFloat(S, Value, fmt) then
          raise Exception.CreateFmt(Constant('ParamErr_WrongFloat'), [S]);
        Calc.SetConstant(Name, Value);
      end
      else Calc.Expression := S;
    end;
    Code := Copy(Code, Index+1, MaxInt);
    Index := CharPos(Code, ';');
  end;
end;}

//function CheckFormula(const Formula: String): Boolean;
//var
//  Calc: TStringCalc;
//begin
//  Calc := TStringCalc.Create;
//  try
//    ParseFormula(Formula, Calc);
//    if Calc.Expression = '' then
//      raise Exception.Create(Constant('FormulaErr_EmptyY'));
//    Calc.AddVariable('X');
//    Calc.Compile;
//  finally
//    Calc.Free;
//  end;
//  Result := True;
//end;

//procedure PlotFormula(Params: TFormulaParams; var ValuesX, ValuesY: TValueArray);
//var
//  I, Count: Integer;
//  Y, X, Step: TValue;
//  Calc: TStringCalc;
//begin
//  Calc := TStringCalc.Create;
//  try
//    ParseFormula(Params.Formula, Calc);
//    Calc.AddVariable('X');
//    Calc.Compile;
//
//    Params.Range.CalcStep(Count, Step);
//    SetLength(ValuesX, Count);
//    SetLength(ValuesY, Count);
//    X := Params.Range.Min;
//    for I := 0 to Count-1 do
//    begin
//      Calc.SetVariable('X', X);
//      Y := Calc.Calculate;
//
//      ValuesX[I] := X;
//      ValuesY[I] := Y;
//
//      X := X + Step;
//    end;
//  finally
//    Calc.Free;
//  end;
//end;

function CrossValues(const G1, G2: TGraphRec): TGraphRec2;
var
  I, J, RI, L1, L2: Integer;
  V1, V2: TGraphRec;
  X, Y1, Y2: TValue;
begin
  // первым графиком будет тот, который начинается "позже"
  if G1.X[0] >= G2.X[0] then
  begin
    V1 := G1;
    V2 := G2;
  end
  else
  begin
    V1 := G2;
    V2 := G1;
  end;
  L1 := Length(G1.X);
  L2 := Length(G2.X);
  SetLength(Result.X, L1+L2);
  SetLength(Result.Y1, L1+L2);
  SetLength(Result.Y2, L1+L2);
  // находим индекс, начиная с которого нужно учитывать второй график
  for I := 0 to Length(V2.X)-1 do
    if V2.X[I] > V1.X[0] then
    begin
      J := I;
      Break;
    end;
  I := 0;
  RI := 0;
  repeat
    X := V1.X[I];
    Y1 := V1.Y[I];
    Y2 := (V2.Y[J-1]*(V2.X[J] - X) + V2.Y[J]*(X - V2.X[J-1]))/(V2.X[J] - V2.X[J-1]);
    Result.X[RI] := X;
    Result.Y1[RI] := Y1;
    Result.Y2[RI] := Y2;
    Inc(RI);
    if I+1 < L1 then
    begin
      X := V2.X[J];
      Y1 := (V1.Y[I]*(V1.X[I+1] - X) + V1.Y[I+1]*(X - V1.X[I]))/(V1.X[I+1] - V1.X[I]);
      Y2 := V2.Y[J];
      Result.X[RI] := X;
      Result.Y1[RI] := Y1;
      Result.Y2[RI] := Y2;
      Inc(RI);
    end;
    Inc(I);
    Inc(J);
  until (J > L2-1) and (I > L1-1);
  SetLength(Result.X, RI);
  SetLength(Result.Y1, RI);
  SetLength(Result.Y2, RI);
end;

function GetPointCount(const Values: TValueArray; Min, Max: TValue): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Length(Values)-1 do
  begin
    if Values[I] >= Min then Inc(Result);
    if Values[I] >= Max then Break;
  end;
end;

//procedure GetOverlapLimits(const Graphs: TGraphArray; out Min, Max: TValue);
//var
//  I: Integer;
//begin
//  Min := CMinValue;
//  Max := CMaxValue;
//  for I := 0 to Length(Graphs)-1 do
//    with Graphs[I] do
//    begin
//      if ValueX[0] > Min then Min := ValueX[0];
//      if ValuesX[ValueCount-1] < Max then Max := ValuesX[ValueCount-1];
//    end;
//end;

// В качестве первого (опорного) берется тот график, который плотнее (имеет
// больше точек) на заданном промежутке. При использовании процедуры, этот график
// следует рассматривать как "модифицируемый", т.е. его точки модифицуруются
// соответствующими им значениями второго графика.
//
// Например используем так: имеем спектр содержащий много точек, и монотонно
// возрастающую кривую с малым кол-вом точек (например, это результат формулы).
// Нужно умножить спектр на кривую, чтобы "усилить" его правую часть.
// При помощи CalcOverlapGraph находим "перекрытие" спектра и кривой.
// В TGraphRec2.X и Y1 будут содержаться копии точек опорного графика (спектра)
// из заданного промежутка (за исключением крайних точек, там будут интерополированные
// значения, соответствующие пределам (Min и Max), если они не совпадают с точками
// первого графика). TGraphRec2.Y2 будут значения Y второго графика, полученные
// в результате интерполяции двух его точек, ближайших к соответствующей
// точке первого графика. Т.е. точек второго графика как таковых в "перекрытии"
// может и не содержаться, и форма кривой (X;Y2) будет отличаться от исходной
// формы второго графика. Теперь для выполнени "усиления" достаточно пробежаться
// по всем точкам "перекрытия" (TGraphRec2), и умножить Y1 на Y2.
//
// Все это имеет смысл только если графики имеют разную плотность на заданном
// промежутке. Если же они построенны на одних и тех же значениях X, то "перекрытием"
// будет просто хитро полученные копии участков каждого графика. Если известно,
// что значения X графиков совпадают, то следует применить более простую процедуру.
//
// Подобным образом можно выполнить разные "модифицирующие" процедуры - произведение,
// сумма, и т.п. Однако для таких процедур как "средняя линия" или "огибающая"
// уже нельзя рассматривать только точки одного из графиков. В этих случаях
// используется флаг AllPoint. Если он выставлен, то в "перекрытии" появляются
// дополнительные точки - это точки второго графика, и соответствующие им
// интерполированные значения первого графика. Теперь формы кривых (X;Y1) и (X;Y2)
// будут совпадать с формами первого и второго графика, но в них будут дополнительные
// точки. Их X-значения для первого графика соответствуют X-ам точек второго и наооборот.
// Все это опять же имеет смысл только если исходные графики построены по разным наборам X.
//
function CalcOverlapGraph(const Gr1, Gr2: TGraphRec; AllPoints: Boolean; Min, Max: TValue): TGraphRec2;
var
  I, J, I1, I2, RI, L1, L2: Integer;
  G1, G2: TGraphRec;

  function Interpolate(G: TGraphRec; X: TValue; I: Integer): TValue; inline;
  begin
    Result := (G.Y[I-1]*(G.X[I] - X) + G.Y[I]*(X - G.X[I-1]))/(G.X[I] - G.X[I-1]);
  end;

begin
  RI := 0;
  L1 := Length(Gr1.X);
  L2 := Length(Gr2.X);

  // Если заданы слишком "обширные" пределы, то диапазон обработки будет
  // ограничиваться "концами" графиков - наибольшим минимумом и наименьшим максимумом.
  if (Min < Gr1.X[0]) or (Min < Gr2.X[0]) then Min := Math.Max(Gr1.X[0], Gr2.X[0]);
  if (Max > Gr1.X[L1-1]) or (Max > Gr2.X[L2-1]) then Max := Math.Min(Gr1.X[L1-1], Gr2.X[L2-1]);
  if Min > Max then Exit; // далее будет ошибка "слишком мало точек для построения"

  if GetPointCount(Gr1.X, Min, Max) >= GetPointCount(Gr2.X, Min, Max) then
  begin G1 := Gr1; G2 := Gr2; end else begin G1 := Gr2; G2 := Gr1; end;

  SetLength(Result.X, L1+L2);
  SetLength(Result.Y1, L1+L2);
  SetLength(Result.Y2, L1+L2);

  // Первый график - опорный, он содержит больше точек чем второй (или столько же).
  // Найдем индекс, начиная с которого нужно обрабатывать график.
  // Как минимум это будет вторая точка графика, I1 = 1
  for I := 0 to L1-1 do
    if G1.X[I] > Min then
    begin
      I1 := I;
      Break;
    end;

  // Найдем точку на втором графике, перед которой находится текущий X (Min, в
  // данном случае). Она и ей предшествующая точка (I2-1), будут использоваться
  // для интерполяции значений второго графика.
  // Как минимум это будет вторая точка графика, I2 = 1
  for I := 0 to L2-1 do
    if G2.X[I] > Min then
    begin
      I2 := I;

      // Если нужно, ищем интерполированную точку на первом графике,
      // соответствующую найденной точке на втором графике. N1) Точка I2
      // может отстоять как угодно далеко от текущего X, т.е. перед ней
      // может находиться несколько непройденных точек опорного графика. Тогда
      // последовательность Resul.X может оказатья неупорядоченной и потом нужно
      // будет ее отсортировать. N2) Точка I2 может находиться в том числе и за Max,
      // если он задан явно в параметре процедуры, тогда ее добавлять не надо.
      // N3) Если I2 совпадает по X с одной из точек первого графика, то ее добавлять
      // не надо, т.к. это будет сделано в основном цикле.
      if AllPoints and (G2.X[I2] < Max) then
        for J := I1 to L1-1 do
          if G1.X[J] > G2.X[I2] then
          begin
            if G1.X[J-1] < G2.X[I2] then // N3)
            begin
              Result.X[RI] := G2.X[I2];
              Result.Y2[RI] := G2.Y[I2];
              Result.Y1[RI] := Interpolate(G1, G2.X[I2], J);
              Inc(RI);
            end;
            Break;
          end;

      Break;
    end;

  Result.X[RI] := Min;
  // Интерполированная точка на первом графике, которая соответствует минимальному X
  // Если граница точно совпадает с точкой графика, то интерполировать не нужно.
  if G1.X[I1-1] < Min
    then Result.Y1[RI] := Interpolate(G1, Min, I1)
    else Result.Y1[RI] := G1.Y[I1-1];
  // Интерполированная точка на втором графике, которая соответствует минимальному X
  // Если граница точно совпадает с точкой графика, то интерполировать не нужно.
  if G2.X[I2-1] < Min
    then Result.Y2[RI] := Interpolate(G2, Min, I2)
    else Result.Y2[RI] := G2.Y[I2-1];
  Inc(RI);

  // Обрабатываем все точки первого графика
  repeat

    // Если очередная точка первого графика оказалась дальше точки I2 второго
    // графика, то нужно найти новую точку I2, перед которой находится текущий X.
    if G1.X[I1] > G2.X[I2] then
      for I := I2 to L2-1 do
        if G2.X[I] > G1.X[I1] then
        begin
          I2 := I;

          if AllPoints and (G2.X[I2] < Max) then
            for J := I1 to L1-1 do
              if G1.X[J] > G2.X[I2] then
              begin
                if G1.X[J-1] < G2.X[I2] then // N3)
                begin
                  Result.X[RI] := G2.X[I2];
                  Result.Y2[RI] := G2.Y[I2];
                  Result.Y1[RI] := Interpolate(G1, G2.X[I2], J);
                  Inc(RI);
                end;
                Break;
              end;

          Break;
        end;

    Result.X[RI] := G1.X[I1];
    Result.Y1[RI] := G1.Y[I1];
    // Интерполированная точка на втором графике, которая соответствует заданному X
    // Если X точно совпадает с точкой графика, то интерполировать не нужно.
    if G2.X[I2-1] < G1.X[I1]
      then Result.Y2[RI] := Interpolate(G2, G1.X[I1], I2)
      else Result.Y2[RI] := G2.Y[I2-1];
    Inc(I1);
    Inc(RI);
  until (I1 = L1) or (G1.X[I1] > Max);
  // Интерполированная точка на первом графике, которая соответствует максимальному X
  if (G1.X[I1] > Max) and (G1.X[I1-1] < Max) then
  begin
    Result.X[RI] := Max;
    Result.Y1[RI] := Interpolate(G1, Max, I1);
    // Интерполированная точка на втором графике, которая соответствует максимальному X
    // Если граница точно совпадает с точкой графика, то интерполировать не нужно.
    if G2.X[I2] > Max
      then Result.Y2[RI] := Interpolate(G2, Max, I2)
      else Result.Y2[RI] := G2.Y[I2];
    Inc(RI);
  end;
  SetLength(Result.X, RI);
  SetLength(Result.Y1, RI);
  SetLength(Result.Y2, RI);
  if AllPoints then Sort(Result);
end;

procedure Regularity(const Values: TValueArray; var X, Y: TValueArray);
var
  I: Integer;
begin
  SetLength(X, Length(Values)-1);
  SetLength(Y, Length(Values)-1);
  for I := 0 to Length(Values)-2 do
  begin
    X[I] := I;
    Y[I] := Values[I+1] - Values[I];
  end;
end;

procedure Sort(var ValuesX, ValuesY: TValueArray);
var
  I, J: Integer;
  Tmp: TValue;
begin
  for I := 0 to Length(ValuesX)-1 do
    for J := 0 to I-1 do
      if ValuesX[J] > ValuesX[J+1] then
      begin
        Tmp := ValuesX[J+1]; ValuesX[J+1] := ValuesX[J]; ValuesX[J] := Tmp;
        Tmp := ValuesY[J+1]; ValuesY[J+1] := ValuesY[J]; ValuesY[J] := Tmp;
      end;
end;

procedure Sort(var G: TGraphRec2);
var
  I, J: Integer;
  Tmp: TValue;
begin
  for I := 0 to Length(G.X)-1 do
    for J := 0 to I-1 do
      if G.X[J] > G.X[J+1] then
      begin
        Tmp := G.X[J+1];  G.X[J+1] := G.X[J];   G.X[J] := Tmp;
        Tmp := G.Y1[J+1]; G.Y1[J+1] := G.Y1[J]; G.Y1[J] := Tmp;
        Tmp := G.Y2[J+1]; G.Y2[J+1] := G.Y2[J]; G.Y2[J] := Tmp;
      end;
end;

//function GraphToRec(Graph: TGraph): TGraphRec; inline;
//begin
//  Result.X := Graph.ValuesX;
//  Result.Y := Graph.ValuesY;
//end;

end.