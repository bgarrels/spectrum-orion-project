unit OriDebugConsole;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Forms, StdCtrls, ComCtrls;

type
  TWndDebugConsole = class(TForm)
    MemoLog: TMemo;
    Toolbar: TToolBar;
    ButtonClear: TToolButton;
    procedure ButtonClearClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  end;

procedure DebugPrint(const Fmt: String; Args: array of const); overload;
procedure DebugPrint(const Msg: String); overload;
procedure DebugPrint(const Value: Double);

implementation

uses
  OriUtils_GUI;

{$R *.lfm}

var
  WndDebugConsole: TWndDebugConsole;
  SavedPos, SavedSize: Longword;

procedure DebugPrint(const Value: Double);
begin
  DebugPrint(FloatToStr(Value));
end;

procedure DebugPrint(const Fmt: String; Args: array of const);
begin
  DebugPrint(Format(Fmt, Args));
end;

procedure DebugPrint(const Msg: String);
begin
  if not Assigned(WndDebugConsole) then
    WndDebugConsole := TWndDebugConsole.Create(Application.MainForm);

  WndDebugConsole.MemoLog.Lines.Add(Msg);
end;

{%region TWndDebugConsole}
procedure TWndDebugConsole.ButtonClearClick(Sender: TObject);
begin
  MemoLog.Clear;
end;

procedure TWndDebugConsole.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  WndDebugConsole := nil;
  SaveFormSizePos(Self, SavedPos, SavedSize);
end;

procedure TWndDebugConsole.FormCreate(Sender: TObject);
begin
  WndDebugConsole := Self;
  RestoreFormSizePos(Self, SavedSize, SavedPos);
end;
{%endregion}

end.

