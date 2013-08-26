unit OriGraphics;

{$mode objfpc}{$H+}

interface

uses
  Graphics;

function Lighten(C: TColor; Amount: Integer): TColor;
function Blend(C1, C2: TColor; W1: Integer): TColor;
function MixColors(C1, C2: TColor; W1: Integer): TColor;

const
{%region 'Web colors'}
  // Reds
  clIndianRed             = TColor($5C5CCD);
  clLightCoral            = TColor($8080F0);
  clSalmon                = TColor($7280FA);
  clDarkSalmon            = TColor($7A96E9);
  clLightSalmon           = TColor($7AA0FF);
  clCrimson               = TColor($3C14DC);
  clFireBrick             = TColor($2222B2);
  clDarkRed               = TColor($00008B);
  // Pinks
  clPink                  = TColor($CBC0FF);
  clLightPink             = TColor($C1B6FF);
  clHotPink               = TColor($B469FF);
  clDeepPink              = TColor($9314FF);
  clMediumVioletRed       = TColor($8515C7);
  clPaleVioletRed         = TColor($9370DB);
  // Oranges
  clCoral                 = TColor($507FFF);
  clTomato                = TColor($4763FF);
  clOrangeRed             = TColor($0045FF);
  clDarkOrange            = TColor($008CFF);
  clOrange                = TColor($00A5FF);
  // Yellows
  clGold                  = TColor($00D7FF);
  clYellow                = TColor($00FFFF);
  clLightYellow           = TColor($E0FFFF);
  clLemonChiffon          = TColor($CDFAFF);
  clLightGoldenrodYellow  = TColor($D2FAFA);
  clPapayaWhip            = TColor($D5EFFF);
  clMoccasin              = TColor($B5E4FF);
  clPeachPuff             = TColor($B9DAFF);
  clPaleGoldenrod         = TColor($AAE8EE);
  clKhaki                 = TColor($8CE6F0);
  clDarkKhaki             = TColor($6BB7BD);
  // Violets
  clLavender              = TColor($FAE6E6);
  clThistle               = TColor($D8BFD8);
  clPlum                  = TColor($DDA0DD);
  clViolet                = TColor($EE82EE);
  clOrchid                = TColor($D670DA);
  clMagenta               = TColor($FF00FF);
  clMediumOrchid          = TColor($D355BA);
  clMediumPurple          = TColor($DB7093);
  clBlueViolet            = TColor($E22B8A);
  clDarkViolet            = TColor($D30094);
  clDarkOrchid            = TColor($CC3299);
  clDarkMagenta           = TColor($8B008B);
  clPurple                = TColor($800080);
  clIndigo                = TColor($82004B);
  clSlateBlue             = TColor($CD5A6A);
  clDarkSlateBlue         = TColor($8B3D48);
  // Greens
  clGreenYellow           = TColor($2FFFAD);
  clChartreuse            = TColor($00FF7F);
  clLawnGreen             = TColor($00FC7C);
  clLimeGreen             = TColor($32CD32);
  clPaleGreen             = TColor($98FB98);
  clLightGreen            = TColor($90EE90);
  clMediumSpringGreen     = TColor($9AFA00);
  clSpringGreen           = TColor($7FFF00);
  clMediumSeaGreen        = TColor($71B33C);
  clSeaGreen              = TColor($578B2E);
  clForestGreen           = TColor($228B22);
  clDarkGreen             = TColor($006400);
  clYellowGreen           = TColor($32CD9A);
  clOliveDrab             = TColor($238E6B);
  clDarkOliveGreen        = TColor($2F6B55);
  clMediumAquamarine      = TColor($AACD66);
  clDarkSeaGreen          = TColor($8FBC8F);
  clLightSeaGreen         = TColor($AAB220);
  clDarkCyan              = TColor($8B8B00);
  // Bules
  clLightCyan             = TColor($FFFFE0);
  clPaleTurquoise         = TColor($EEEEAF);
  clAquamarine            = TColor($D4FF7F);
  clTurquoise             = TColor($D0E040);
  clMediumTurquoise       = TColor($CCD148);
  clDarkTurquoise         = TColor($D1CE00);
  clCadetBlue             = TColor($A09E5F);
  clSteelBlue             = TColor($B48246);
  clLightSteelBlue        = TColor($DEC4B0);
  clPowderBlue            = TColor($E6E0B0);
  clLightBlue             = TColor($E6D8AD);
  clSkyBlue               = TColor($EBCE87);
  clLightSkyBlue          = TColor($FACE87);
  clDeepSkyBlue           = TColor($FFBF00);
  clDodgerBlue            = TColor($FF901E);
  clCornflowerBlue        = TColor($ED9564);
  clMediumSlateBlue       = TColor($EE687B);
  clRoyalBlue             = TColor($E16941);
  clMediumBlue            = TColor($CD0000);
  clDarkBlue              = TColor($8B0000);
  clMidnightBlue          = TColor($701919);
  // Browns
  clCornsilk              = TColor($DCF8FF);
  clBlanchedAlmond        = TColor($CDEBFF);
  clBisque                = TColor($C4E4FF);
  clNavajoWhite           = TColor($ADDEFF);
  clWheat                 = TColor($B3DEF5);
  clBurlyWood             = TColor($87B8DE);
  clTan                   = TColor($8CB4D2);
  clRosyBrown             = TColor($8F8FBC);
  clSandyBrown            = TColor($60A4F4);
  clGoldenrod             = TColor($20A5DA);
  clDarkGoldenrod         = TColor($0B86B8);
  clPeru                  = TColor($3F85CD);
  clChocolate             = TColor($1E69D2);
  clSaddleBrown           = TColor($13458B);
  clSienna                = TColor($2D52A0);
  clBrown                 = TColor($2A2AA5);
  // Whites
  clSnow                  = TColor($FAFAFF);
  clHoneydew              = TColor($F0FFF0);
  clMintCream             = TColor($FAFFF5);
  clAzure                 = TColor($FFFFF0);
  clAliceBlue             = TColor($FFF8F0);
  clGhostWhite            = TColor($FFF8F8);
  clWhiteSmoke            = TColor($F5F5F5);
  clSeashell              = TColor($EEF5FF);
  clBeige                 = TColor($DCF5F5);
  clOldLace               = TColor($E6F5FD);
  clFloralWhite           = TColor($F0FAFF);
  clIvory                 = TColor($F0FFFF);
  clAntiqueWhite          = TColor($D7EBFA);
  clLinen                 = TColor($E6F0FA);
  clLavenderBlush         = TColor($F5F0FF);
  clMistyRose             = TColor($E1E4FF);
  // Grays
  clGainsboro             = TColor($DCDCDC);
  clLightGray             = TColor($D3D3D3);
  clDarkGray              = TColor($A9A9A9);
  clDimGray               = TColor($696969);
  clLightSlateGray        = TColor($998877);
  clSlateGray             = TColor($908070);
  clDarkSlateGray         = TColor($4F4F2F);
{%endregion}

implementation

function Lighten(C: TColor; Amount: Integer): TColor;
var
  R, G, B: Integer;
begin
  //if C < 0 then C := GetSysColor(C and $000000FF);
  R := C and $FF + Amount;
  G := C shr 8 and $FF + Amount;
  B := C shr 16 and $FF + Amount;
  if R < 0 then R := 0 else if R > 255 then R := 255;
  if G < 0 then G := 0 else if G > 255 then G := 255;
  if B < 0 then B := 0 else if B > 255 then B := 255;
  Result := R or (G shl 8) or (B shl 16);
end;

function Blend(C1, C2: TColor; W1: Integer): TColor;
var
  W2, A1, A2, D, F, G: Integer;
begin
  //if C1 < 0 then C1 := GetSysColor(C1 and $FF);
  //if C2 < 0 then C2 := GetSysColor(C2 and $FF);

  if W1 >= 100 then D := 1000
  else D := 100;

  W2 := D - W1;
  F := D div 2;

  A2 := C2 shr 16 * W2;
  A1 := C1 shr 16 * W1;
  G := (A1 + A2 + F) div D and $FF;
  Result := G shl 16;

  A2 := (C2 shr 8 and $FF) * W2;
  A1 := (C1 shr 8 and $FF) * W1;
  G := (A1 + A2 + F) div D and $FF;
  Result := Result or G shl 8;

  A2 := (C2 and $FF) * W2;
  A1 := (C1 and $FF) * W1;
  G := (A1 + A2 + F) div D and $FF;
  Result := Result or G;
end;

function MixColors(C1, C2: TColor; W1: Integer): TColor;
var
  W2: Cardinal;
begin
  Assert(W1 in [0..255]);
  W2 := W1 xor 255;
  //if Integer(C1) < 0 then C1 := GetSysColor(C1 and $000000FF);
  //if Integer(C2) < 0 then C2 := GetSysColor(C2 and $000000FF);
  Result := Integer(
    ((Cardinal(C1) and $FF00FF) * Cardinal(W1) +
    (Cardinal(C2) and $FF00FF) * W2) and $FF00FF00 +
    ((Cardinal(C1) and $00FF00) * Cardinal(W1) +
    (Cardinal(C2) and $00FF00) * W2) and $00FF0000) shr 8;
end;

end.

