unit FrmRange;

{$mode objfpc}{$H+}

interface

uses
  Forms, StdCtrls, OriEditors;

type
  TRangeFrm = class(TFrame)
    fedMax: TOriFloatEdit;
    fedMin: TOriFloatEdit;
    labMin: TLabel;
    labMax: TLabel;
  end;

implementation

{$R *.lfm}

end.

