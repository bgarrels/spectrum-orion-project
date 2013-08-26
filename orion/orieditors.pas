unit OriEditors;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, StdCtrls, LMessages,
  OriMath;

{%region TOriFloatEdit}
type
  TFloatEditOption = (
    feoAdvFormatting, // Use Mathcad-like formatting (see OriMath)
    feoUseInfinity,   // Ctrl+0 inserts Infinity
    feoDisablePaste,  // Disable pasting from Clipboard
    feoZeroFill       // Paremeter for feoAdvFormatting
  );
  TFloatEditOptions = set of TFloatEditOption;

  TOriFloatEdit = class(TCustomEdit)
  private
    FMinValue: Extended;
    FMaxValue: Extended;
    FFormatString: String;
    FFloatFormat: TFloatFormat;
    FPrecision: Byte; // for AdvFormatting it is ExpThreshold
    FDigits: Byte;
    FSavedText: String;
    FOptions: TFloatEditOptions;
    procedure SetValue(AValue: Extended);
    procedure SetMinValue(AValue: Extended);
    procedure SetMaxValue(AValue: Extended);
    procedure SetFormatString(AValue: String);
    procedure SetFloatFormat(AValue: TFloatFormat);
    procedure SetDigits(AValue: Byte);
    procedure SetPrecision(AValue: Byte);
    procedure SetOptions(AValue: TFloatEditOptions);
    function GetValue: Extended;
    function GetText: string;
    function CheckValue(NewValue: Extended): Extended;
    function FormatText(Value: Extended): String;
    procedure LMPasteFromClip(var Message: TLMessage); message LM_PASTE;
  protected
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure DoExit; override;
    procedure DoEnter; override;
  public
    constructor Create(AOwner: TComponent); override;
    property Text: string read GetText; // disable SetTest
    procedure SetFormatting(APrecision, ADigits: Byte; AZerofill: Boolean);
  published
    property MinValue: Extended read FMinValue write SetMinValue;
    property MaxValue: Extended read FMaxValue write SetMaxValue;
    property FormatString: String read FFormatString write SetFormatString;
    property FloatFormat: TFloatFormat read FFloatFormat write SetFloatFormat default ffGeneral;
    property Digits: Byte read FDigits write SetDigits default 9;
    property Precision: Byte read FPrecision write SetPrecision default 18;
    property Value: Extended read GetValue write SetValue;
    property Options: TFloatEditOptions read FOptions write SetOptions default [];
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property AutoSelect;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentBidiMode;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabStop;
    property TabOrder;
    property Visible;
  end;

procedure PrepareFloatEditsFormat(AParent: TWinControl;
  AExpThreshold, ADigits: Byte; AZerofill, Recursive: Boolean); overload;
procedure PrepareFloatEditsFormat(AParent: TWinControl;
  const AFormat: TOriFloatFormatParams; Recursive: Boolean); overload;
{%endregion}

implementation

uses
  Math, Graphics,
  OriStrings;

{%region TOriFloatEdit}
procedure PrepareFloatEditsFormat(AParent: TWinControl;
  AExpThreshold, ADigits: Byte; AZerofill, Recursive: Boolean);
var
  I: Integer;
begin
  with AParent do
    for I := 0 to ControlCount-1 do
      if Controls[I] is TOriFloatEdit then
        with TOriFloatEdit(Controls[I]) do
        begin
          Options := Options + [feoAdvFormatting];
          SetFormatting(AExpThreshold, ADigits, AZerofill);
        end
      else if Recursive and (Controls[I] is TWinControl) then
        PrepareFloatEditsFormat(TWinControl(Controls[I]),
          AExpThreshold, ADigits, AZerofill, True);
end;

procedure PrepareFloatEditsFormat(AParent: TWinControl;
  const AFormat: TOriFloatFormatParams; Recursive: Boolean);
begin
  with AFormat do
    PrepareFloatEditsFormat(AParent, Exponent, Digits, Zerofill, Recursive);
end;


constructor TOriFloatEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMinValue := 0;
  FMaxValue := 0;
  FFormatString := '';
  FFloatFormat := ffGeneral;
  FDigits := 9;
  FPrecision := 18;
  inherited Text := FormatText(FMinValue);
end;

function TOriFloatEdit.GetText: string;
begin
  Result := inherited Text;
end;

procedure TOriFloatEdit.CalculatePreferredSize(var PreferredWidth, PreferredHeight: integer; WithThemeSpace: Boolean);
begin
  inherited CalculatePreferredSize(PreferredWidth, PreferredHeight, WithThemeSpace);
  PreferredWidth := ScaleX(100 , 96);
end;

procedure TOriFloatEdit.SetMinValue(AValue: Extended);
begin
  if FMinValue <> AValue then
  begin
    FMinValue := AValue;
    Value := Value;
  end;
end;

procedure TOriFloatEdit.SetMaxValue(AValue: Extended);
begin
  if FMaxValue <> AValue then
  begin
    FMaxValue := AValue;
    Value := Value;
  end;
end;

procedure TOriFloatEdit.SetFloatFormat(AValue: TFloatFormat);
begin
  if AValue <> FFloatFormat then
  begin
    FFloatFormat := AValue;
    Value := Value;
  end;
end;

procedure TOriFloatEdit.SetPrecision(AValue: Byte);
begin
  if AValue > 18 then AValue := 18;
  if AValue <> FPrecision then
  begin
    FPrecision := AValue;
    Value := Value;
  end;
end;

procedure TOriFloatEdit.SetDigits(AValue: Byte);
begin
  if AValue > 18 then AValue := 18;
  if FDigits <> AValue then
  begin
    FDigits := AValue;
    Value := Value;
  end;
end;

procedure TOriFloatEdit.SetFormatString(AValue: String);
begin
  if AValue <> FFormatString then
  begin
    FFormatString := AValue;
    Value := Value;
  end;
end;

procedure TOriFloatEdit.SetFormatting(APrecision, ADigits: Byte; AZerofill: Boolean);
begin
  if APrecision > 18
    then FPrecision := 18
    else FPrecision := APrecision;
  if FDigits > 18
    then FDigits := 18
    else FDigits := ADigits;
  if AZerofill
    then Include(FOptions, feoZeroFill)
    else Exclude(FOptions, feoZeroFill);
  Value := Value;
end;

procedure TOriFloatEdit.SetValue(AValue: Extended);
begin
  inherited Text := FormatText(CheckValue(AValue));
end;

function TOriFloatEdit.GetValue: Extended;
var S: String;
begin
  S := inherited Text;
  case Length(S) of
    3:
      if CharInSet(S[1], ['N','n'])
        and CharInSet(S[2], ['A','a'])
        and CharInSet(S[3], ['N','n']) then Result := NaN
      else if CharInSet(S[1], ['I','i'])
        and CharInSet(S[2], ['N','n'])
        and CharInSet(S[3], ['F','f']) then Result := Infinity
      else Result := CheckValue(StrToFloatDef(S, 0));
    4:
      if (S[1] = '-')
        and CharInSet(S[2], ['I','i'])
        and CharInSet(S[3], ['N','n'])
        and CharInSet(S[4], ['F','f']) then Result := NegInfinity
      else Result := CheckValue(StrToFloatDef(S, 0));
    else Result := CheckValue(StrToFloatDef(S, 0));
  end;
end;

function TOriFloatEdit.FormatText(Value: Extended): String;
begin
  if not (feoAdvFormatting in Options) then
  begin
    if FormatString <> '' then
      Result := FormatFloat(FormatString, Value)
    else if Precision > 0 then
      Result := FloatToStrF(Value, FloatFormat, Precision, Digits)
    else
      Result := FloatToStr(Value);
  end
  else
    Result := StringFromFloat(Value, Precision, FDigits, feoZeroFill in Options);

  case CharCase of
    ecLowerCase: Result := ReplaceChar(Result, 'E', 'e');
    ecUppercase: Result := ReplaceChar(Result, 'e', 'E');
  end;
end;

procedure TOriFloatEdit.DoEnter;
begin
  if AutoSelect and not (csLButtonDown in ControlState) then SelectAll;
  inherited;
end;

procedure TOriFloatEdit.DoExit;
var
  Tmp: Extended;
begin
  Tmp := Value;
  Value := Tmp; // do formatting
  inherited;
end;

function TOriFloatEdit.CheckValue(NewValue: Extended): Extended;
begin
  Result := NewValue;
  // unable to compare with Infinity and NaN
  if IsNaN(NewValue) or IsInfinite(NewValue) then Exit;
  if (FMaxValue <> FMinValue) then
  begin
    if (FMaxValue > FMinValue) then
    begin
      if NewValue < FMinValue then Result := FMinValue
      else if NewValue > FMaxValue then Result := FMaxValue;
    end
    else
    begin
      if FMaxValue = 0 then
      begin
        if NewValue < FMinValue then Result := FMinValue;
      end
      else if FMinValue = 0 then
      begin
        if NewValue > FMaxValue then Result := FMaxValue;
      end;
    end;
  end;
end;

procedure TOriFloatEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    Ord('A'): SelectAll;
    Ord('0'):
      if (feoUseInfinity in Options) and (Shift = [ssCtrl]) then
      begin
        Key := 0;
        Value := Infinity;
        Changed;
      end;
  end;
  inherited;
end;

procedure TOriFloatEdit.KeyPress(var Key: Char);
var
  ts: string;
  flTemp: Extended;
  tmp, tmp1: Integer;
begin
  FSavedText := inherited Text;

  case Key of
    '.', ',': Key := DefaultFormatSettings.DecimalSeparator;
    '*', 'E': Key := 'e'; // let only lowercase 'E' allowed
    '-': // invert value sign or exponent sign by pressing '-'
      // If all the text is selected then do not change sign.
      // If all text is selected then it is most probably that user
      // is trying to replace all the text with a new one.
      if SelLength < Length(inherited Text) then
      begin
        ts := inherited Text;
        if (ts <> '') and (ts <> '-') then
        begin
          tmp := Pos('e', ts);
          if tmp = 0 then
            tmp := MaxInt;
          // invert value sign
          if SelStart < tmp then
          begin
            tmp := SelStart;
            if ts[1] = '-' then
            begin
              inherited Text := Copy(ts, 2, Length(ts)-1);
              SelStart := tmp - 1;
            end
            else
            begin
              inherited Text := '-' + ts;
              SelStart := tmp + 1;
            end;
            inherited;
            Key := #0;
            Exit;
          end
          else // invert exponent sign
            if tmp < Length(ts) then
            begin
              tmp1 := SelStart;
              case ts[tmp+1] of
                '-': inherited Text := Copy(ts, 1, tmp) + '+' + Copy(ts, tmp+2, MaxInt);
                '+': inherited Text := Copy(ts, 1, tmp) + '-' + Copy(ts, tmp+2, MaxInt);
                else inherited Text := Copy(ts, 1, tmp) + '-' + Copy(ts, tmp+1, MaxInt);
              end;
              SelStart := tmp1;
              inherited;
              Key := #0;
              Exit;
            end;
        end;
      end;
  end;

  case Key of
    #10, #13:
      begin
        Value := Value; // formatting on Enter
        Key := #0;
      end;

    #32..#255:
      begin
        if not CharInSet(Key, ['0'..'9', DefaultFormatSettings.DecimalSeparator, '-', 'e']) then
        begin
          inherited;
          Key := #0;
          Exit;
        end;

        // guessed new text
        ts := Copy(Text, 1, SelStart) + Key + Copy(Text, SelStart + SelLength + 1, MaxInt);

        // select all and press 'minus'
        if ts = '-' then
        begin
          inherited;
          Key := #0;
          inherited Text := '-0';
          SelStart := 1;
          SelLength := 1;
          Exit;
        end;

        // select all and press decimal separator
        if ts = DefaultFormatSettings.DecimalSeparator then
        begin
          inherited;
          Key := #0;
          inherited Text := '0' + DefaultFormatSettings.DecimalSeparator + '0';
          SelStart := 2;
          SelLength := 1;
          Exit;
        end;

        // select all and delete
        if ts = '' then
        begin
          inherited;
          Key := #0;
          inherited Text := FormatText(FMinValue);
          SelectAll;
          Exit;
        end;

        // other cases
        if not TryStrToFloat(ts, flTemp) then
        begin
          inherited;
          Key := #0;
          Exit;
        end;

        // not allow type more than FDigits after decimal separator -
        // remove unnecessary digits from the end (or before E)
        tmp1 := CharPos(ts, 'e')-1;
        if tmp1 = -1 then tmp1 := Length(ts);
        tmp := CharPos(ts, DefaultFormatSettings.DecimalSeparator);
        if (tmp > 0) and (tmp1 - tmp > Digits) then
        begin
          inherited;
          Key := #0;
          tmp := SelStart;
          inherited Text := Copy(ts, 1, tmp1-1) + Copy(ts, tmp1+1, MaxInt);
          SelStart := tmp;
          Exit;
        end;

        inherited;
      end;

  else
    inherited;
  end;
end;

procedure TOriFloatEdit.LMPasteFromClip(var Message: TLMessage);
var
  S: string;
  Temp: Extended;
begin
  inherited;

  if feoDisablePaste in Options then
  begin
    inherited Text := FSavedText;
    Exit;
  end;

  S := Trim(inherited Text);
  S := ReplaceChar(S, '.', DefaultFormatSettings.DecimalSeparator);
  S := ReplaceChar(S, ',', DefaultFormatSettings.DecimalSeparator);
  if not TryStrToFloat(S, Temp) then
  begin
    // restore original text
    inherited Text := FSavedText;
    SelStart := 1;
    SelLength := 0;
  end
  else Value := CheckValue(Temp);
end;

procedure TOriFloatEdit.SetOptions(AValue: TFloatEditOptions);
begin
  if FOptions <> AValue then
  begin
    FOptions := AValue;
    Value := Value; // re-format value
  end;
end;
{%endregion}

end.

