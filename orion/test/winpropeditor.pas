unit WinPropEditor;

{$mode objfpc}{$H+}

interface

uses
  Forms, Controls, PropEdits, ObjectInspector;

type

  { TWndPropEditor }

  TWndPropEditor = class(TForm)
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FObjectInspector: TObjectInspectorDlg;
  public
    procedure SetSelection(AControl: TControl);
  end;

function ShowPropEditor: TWndPropEditor; overload;
procedure ShowPropEditor(AControl: TControl); overload;

implementation

{$R *.lfm}

function ShowPropEditor: TWndPropEditor;
begin
  Result := TWndPropEditor.Create(Application.MainForm);
  Result.Show;
end;

procedure ShowPropEditor(AControl: TControl);
var
  Wnd: TWndPropEditor;
begin
  Wnd := ShowPropEditor;
  AControl.Parent := Wnd;
  AControl.Align := alClient;
  Wnd.SetSelection(AControl);
end;

{ TWndPropEditor }

procedure TWndPropEditor.FormCreate(Sender: TObject);
begin
  // create the PropertyEditorHook (the interface to the properties)
  if not Assigned(GlobalDesignHook) then
    GlobalDesignHook := TPropertyEditorHook.Create;

  // create the ObjectInspector
  FObjectInspector := TObjectInspectorDlg.Create(Application);
  FObjectInspector.PropertyEditorHook := GlobalDesignHook;
  FObjectInspector.Show;
end;

procedure TWndPropEditor.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FObjectInspector.Close;
  CloseAction := caFree;
end;

procedure TWndPropEditor.SetSelection(AControl: TControl);
var
  Selection: TPersistentSelectionList;
begin
  GlobalDesignHook.LookupRoot := AControl;
  Selection := TPersistentSelectionList.Create;
  try
    Selection.Add(AControl);
    FObjectInspector.Selection := Selection;
  finally
    Selection.Free;
  end;
end;

end.

