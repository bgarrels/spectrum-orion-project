unit OriUndo;

interface

uses
  SysUtils, Classes;

type
  TOriUndoCommand = class;
  TOriCommandGroup = class;

  TUndoRedoEvent = procedure (Sender: TObject; CmdUndo, CmdRedo: TOriUndoCommand) of object;
  TUndoneEvent = procedure (Sender: TObject; Cmd: TOriUndoCommand) of object;

  // Command history
  TOriHistory = class
  private
    FLockCount: Integer;
    FItems: TList;              // command stack
    FUndoIndex: Integer;        // top command to Undo, all upped - Redo
    FLimit: Integer;            // maximal count of command to undo
    FOnChanged: TUndoRedoEvent; // event on command stack was changed
    FOnUndone: TUndoneEvent;    // event after undo
    FOnRedone: TUndoneEvent;    // event after redo
    FGroup: TOriCommandGroup;   // temporary pointer to a group action

    function GetGroupped: Boolean;
    function GetUndoCmd: TOriUndoCommand;
    function GetRedoCmd: TOriUndoCommand;
    procedure ClearTo(Index: Integer);
    procedure ClearFirst;
    procedure DoChanged;
    procedure DoUndone(Cmd: TOriUndoCommand);
    procedure DoRedone(Cmd: TOriUndoCommand);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Undo;
    procedure Redo;
    function RedoCount: Integer;
    function UndoCount: Integer;
    property UndoCmd: TOriUndoCommand read GetUndoCmd;
    property RedoCmd: TOriUndoCommand read GetRedoCmd;

    procedure Append(Command: TOriUndoCommand);
    procedure Clear;

    procedure Lock;
    procedure Unlock;

    procedure BeginGroup(const ATitle: String);
    procedure EndGroup;
    property Groupped: Boolean read GetGroupped;

    property Limit: Integer read FLimit write FLimit;

    property OnChanged: TUndoRedoEvent read FOnChanged write FOnChanged;
    property OnUndone: TUndoneEvent read FOnUndone write FOnUndone;
    property OnRedone: TUndoneEvent read FOnRedone write FOnRedone;
  end;

  // Undoable command is an item of command history. Command must contain
  // all the information required to do an action and do it back.
  TOriUndoCommand = class
  protected
    FTitle: String;
  protected
    procedure Swap; virtual;
  public
    procedure Undo; virtual;
    procedure Redo; virtual;
    property Title: String read FTitle;
  end;

  // List of commands to undo is presented in history as single command.
  // It can be useful for group operations, for example deletion of some elements.
  // It is convenient if we already have a command to undo deletion of single element
  // and we are too lazy to implement a new command to undo deletion of several.
  TOriCommandGroup = class(TOriUndoCommand)
  private
    FItems: TList;                  // commands stack
  public
    constructor Create(const ATitle: String);
    destructor Destroy; override;
    procedure Append(Command: TOriUndoCommand);
    procedure Undo; override;
    procedure Redo; override;
  end;

// Global command history.
// It must be initialized (HistoryInit) anywhere in program before first usage.
var History: TOriHistory;

// Procedures to working with global command history.
procedure HistoryAppend(Command: TOriUndoCommand);
procedure HistoryUndo;
procedure HistoryRedo;
procedure HistoryInit;

implementation

const
  HistoryLimit = 100;

{%region Global History}
procedure HistoryInit;
begin
  History := TOriHistory.Create;
end;

procedure HistoryAppend(Command: TOriUndoCommand);
begin
  if Assigned(History) then History.Append(Command);
end;

procedure HistoryUndo;
begin
  if Assigned(History) then History.Undo;
end;

procedure HistoryRedo;
begin
  if Assigned(History) then History.Redo;
end;
{%endregion}


{%region TOriHistory}
constructor TOriHistory.Create;
begin
  FItems := TList.Create;
  FLimit := HistoryLimit;
  FLockCount := 0;
  FUndoIndex := -1;
end;

destructor TOriHistory.Destroy;
begin
  ClearTo(0);
  FItems.Free;
  inherited;
end;

procedure TOriHistory.Clear;
begin
  ClearTo(0);
  FUndoIndex := -1;
  DoChanged;
end;

function TOriHistory.RedoCount: Integer;
begin
  Result := FItems.Count - FUndoIndex - 1;
end;

function TOriHistory.UndoCount: Integer;
begin
  Result := FUndoIndex+1;
end;

procedure TOriHistory.Lock;
begin
  Inc(FLockCount);
end;

procedure TOriHistory.Unlock;
begin
  if FLockCount > 0 then Dec(FLockCount);
end;

procedure TOriHistory.Append(Command: TOriUndoCommand);
begin
  if FLockCount = 0 then
    if FGroup = nil then
    begin
      ClearTo(FUndoIndex+1);
      FItems.Add(Command);
      if FItems.Count = FLimit
        then ClearFirst
        else Inc(FUndoIndex);
      DoChanged;
    end
    else FGroup.Append(Command);
end;

procedure TOriHistory.ClearTo(Index: Integer);
var
  I: Integer;
begin
  for I := FItems.Count-1 downto Index do
  begin
    TOriUndoCommand(FItems[I]).Free;
    FItems.Delete(I);
  end;
end;

procedure TOriHistory.ClearFirst;
begin
  if FItems.Count = 0 then Exit;
  TOriUndoCommand(FItems[0]).Free;
  FItems.Delete(0);
end;

procedure TOriHistory.DoChanged;
begin
  if Assigned(FOnChanged) then FOnChanged(Self, UndoCmd, RedoCmd);
end;

procedure TOriHistory.DoUndone(Cmd: TOriUndoCommand);
begin
  if Assigned(FOnUndone) then FOnUndone(Self, Cmd);
end;

procedure TOriHistory.DoRedone(Cmd: TOriUndoCommand);
begin
  if Assigned(FOnRedone) then FOnRedone(Self, Cmd);
end;

function TOriHistory.GetUndoCmd: TOriUndoCommand;
begin
  if FUndoIndex > -1
    then Result := TOriUndoCommand(FItems[FUndoIndex])
    else Result := nil;
end;

function TOriHistory.GetRedoCmd: TOriUndoCommand;
begin
  if FUndoIndex+1 < FItems.Count
    then Result := TOriUndoCommand(FItems[FUndoIndex+1])
    else Result := nil;
end;

procedure TOriHistory.Undo;
var
  Cmd: TOriUndoCommand;
begin
  if FUndoIndex > -1 then
  begin
    Cmd := TOriUndoCommand(FItems[FUndoIndex]);
    Cmd.Undo;
    Dec(FUndoIndex);
    DoUndone(Cmd);
    DoChanged;
  end;
end;

procedure TOriHistory.Redo;
var
  Cmd: TOriUndoCommand;
begin
  if FUndoIndex+1 < FItems.Count then
  begin
    Cmd := TOriUndoCommand(FItems[FUndoIndex+1]);
    Cmd.Redo;
    Inc(FUndoIndex);
    DoRedone(Cmd);
    DoChanged;
  end;
end;

procedure TOriHistory.BeginGroup(const ATitle: String);
begin
  if FLockCount > 0 then exit;

  // If FGroup is assigned, it may be there was an exception between
  // BeginGroup and EndGroup. This group is not needed anymore.
  if Assigned(FGroup) and (FItems.IndexOf(FGroup) = -1) then FreeAndNil(FGroup);

  FGroup := TOriCommandGroup.Create(ATitle);
end;

procedure TOriHistory.EndGroup;
var
  Tmp: TOriCommandGroup;
begin
  if Assigned(FGroup) then
  begin
    Tmp := FGroup;
    FGroup := nil;
    Append(Tmp);
  end;
end;

function TOriHistory.GetGroupped: Boolean;
begin
  Result := Assigned(FGroup);
end;
{%endregion TOriHistory}


{%region TOriCommandGroup}
constructor TOriCommandGroup.Create(const ATitle: String);
begin
  FTitle := ATitle;
  FItems := TList.Create;
end;

destructor TOriCommandGroup.Destroy;
var i: Integer;
begin
  for i := FItems.Count-1 downto 0 do
    TOriUndoCommand(FItems[i]).Free;
  FItems.Free;
  inherited;
end;

procedure TOriCommandGroup.Append(Command: TOriUndoCommand);
begin
  FItems.Add(Command);
end;

procedure TOriCommandGroup.Undo;
var i: Integer;
begin
  for i := FItems.Count-1 downto 0 do
    TOriUndoCommand(FItems[i]).Undo;
end;

procedure TOriCommandGroup.Redo;
var i: Integer;
begin
  for i := 0 to FItems.Count-1 do
    TOriUndoCommand(FItems[i]).Redo;
end;
{%endregion TOriCommandGroup}


{%region TOriUndoCommand}
procedure TOriUndoCommand.Swap;
begin
  // do nothing
end;

procedure TOriUndoCommand.Undo;
begin
  Swap;
end;

procedure TOriUndoCommand.Redo;
begin
  Swap;
end;
{%endregion}

initialization

finalization
  History.Free;

end.