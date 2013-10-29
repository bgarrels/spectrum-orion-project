unit WinTestColors;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, Graphics, ColorBox, StdCtrls, ExtCtrls, FpcUnit, TestRegistry;

type

  TTest_Colors = class(TTestCase)
  published
    procedure TestColors;
  end;

  { TWndTestColors }

  TWndTestColors = class(TForm)
    BlendTestButton: TButton;
    BlendColor1: TColorBox;
    BlendColor2: TColorBox;
    BlendValue: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelBlended: TLabel;
    LabelColor2: TLabel;
    LabelColor1: TLabel;
    PanelBlend: TPanel;
    procedure BlendTestButtonClick(Sender: TObject);
  end;

implementation

{$R *.lfm}

uses
  OriGraphics;

procedure TTest_Colors.TestColors;
begin
  with TWndTestColors.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TWndTestColors.BlendTestButtonClick(Sender: TObject);
var
  Blended: TColor;
  Color1: TColor;
  Color2: TColor;
  Value: Integer;
begin
  Color1 := BlendColor1.Selected;
  Color2 := BlendColor2.Selected;
  Value := StrToInt(BlendValue.Text);
  Blended := Blend(Color1, Color2, Value);
  PanelBlend.Color := Blended;
  LabelColor1.Caption := IntToHex(Color1, 8) + #13 + IntToStr(Color1);
  LabelColor2.Caption := IntToHex(Color2, 8) + #13 + IntToStr(Color2);
  LabelBlended.Caption := IntToHex(Blended, 8) + #13 + IntToStr(Blended);
end;

initialization
  RegisterTest(TTest_Colors);
end.

