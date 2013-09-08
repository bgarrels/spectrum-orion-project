unit PlotReader;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  Plots;

type
  // Intermediator between TPlot, TDataReaders and user.
  //  GUI (User)     TDataReader
  //         \         /
  //        TPlotReader
  //             |
  //          TPlotSet
  TPlotReader = class
  private
    FPlot: TPlot;
  public
    constructor Create(APlot: TPlot);
    destructor Destroy; override;
    procedure AddFileDialog;
    class procedure AddFileDialog(APlot: TPlot);
  end;

implementation

uses
  SysUtils, Controls, Dialogs, Forms,
  OriUtils_Gui,
  SpectrumTypes, SpectrumSettings, SpectrumStrings, PlotReaders;

{%region Dialog Invokers}
function OpenGraphsDialog(AFileNames: TStrings; var AFilterIndex: Integer): Boolean;
begin
  AFileNames.Clear;
  with TOpenDialog.Create(nil) do
  try
    Title := Dlg_OpenGraphs;
    Filter := TFileReaders.FileFilters;
    FileName := '';
    Options := Options + [ofPathMustExist, ofFileMustExist,
      ofAllowMultiSelect, ofDontAddToRecent];
    FilterIndex := Preferences.GraphsOpenFilter;
    InitialDir := Preferences.GraphsOpenCurDir;
    Result := Execute;
    if Result then
    begin
      Preferences.GraphsOpenFilter := FilterIndex;
      Preferences.GraphsOpenCurDir := ExtractFilePath(FileName);
      AFilterIndex := FilterIndex;
      AFileNames.Assign(Files);
    end;
  finally
    Free;
  end;
end;

{function OpenTableDialog(var AFileName: String): Boolean;
begin
  with TOpenDialog.Create(nil) do
  try
    Title := Constant('Dlg_OpenTable');
    Filter := OpenTableFilters;
    DefaultExt := 'txt';
    FileName := '';
    Options := Options + [ofPathMustExist, ofFileMustExist, ofDontAddToRecent];
    FilterIndex := Preferences.GraphsOpenFilterTable;
    InitialDir := Preferences.GraphsOpenCurDir;
    Result := Execute;
    if Result then
    begin
      Preferences.GraphsOpenFilterTable := FilterIndex;
      Preferences.GraphsOpenCurDir := ExtractFilePath(FileName);
      AFileName := FileName;
    end;
  finally
    Free;
  end;
end;

function OpenFolderDialog: Boolean;
var
  CurDir: String;
begin
  CurDir := Preferences.GraphsOpenCurDir;
  // TODO: свой диалог выбора папки, в котором сразу же можно будет задать
  // фильтр и, возможно, посмотреть какие файлы будут выбраны.
  Result := OriUtils.OpenFolderDialog(Constant('Dlg_OpenFolder'), CurDir);
  if Result then Preferences.GraphsOpenCurDir := CurDir;
end;}
{%endregion Dialog Invokers}

{%region Static shortcuts to methods}
class procedure TPlotReader.AddFileDialog(APlot: TPlot);
begin
  with TPlotReader.Create(APlot) do
  try
    AddFileDialog
  finally
    Free
  end;
end;
{%endregion Static methods}

constructor TPlotReader.Create(APlot: TPlot);
begin
  FPlot := APlot;
end;

destructor TPlotReader.Destroy;
begin
  inherited;
end;

procedure TPlotReader.AddFileDialog;
var
  I, J: Integer;
  InputFiles: TStringList;
  InputFile: String;
  ErrorMsg: String;
  FilterIndex: Integer;
  ReaderClass: TDataReaderClass;
  PerFileReader: Boolean;
  Reader: TDataReader = nil;
  SavedCur: TCursor;
begin
  InputFiles := TStringList.Create;
  try
    if not OpenGraphsDialog(InputFiles, FilterIndex) then exit;

    ReaderClass := TFileReaders.ReaderByFilter(FilterIndex);
    if Assigned(ReaderClass) then
    begin
      Reader := ReaderClass.Create;
      if not Reader.Configure then exit;
      PerFileReader := False;
    end
    else
      PerFileReader := True;

    for I := 0 to InputFiles.Count-1 do
    try
      InputFile := InputFiles[I];
      if PerFileReader then
      begin
        ReaderClass := TFileReaders.ReaderByExt(InputFile);
        if not Assigned(ReaderClass) then
          raise ESpectrumError.CreateFmt(Err_UnsupportedFile, [InputFile]);

        Reader := ReaderClass.Create;
        if not Reader.Configure then
        begin
          Reader.Free;
          continue;
        end;
      end;
      SavedCur := Screen.Cursor;
      Screen.Cursor := crHourGlass;
      try
        Reader.Read(InputFile);
        for J := 0 to Reader.ResultCount-1 do
          FPlot.AddGraph(TGraph.Create(Reader.Result[J], InputFile));
      finally
        if PerFileReader then FreeAndNil(Reader);
        Screen.Cursor := SavedCur;
      end;
    except
      on e: Exception do
      begin
        ErrorMsg := Format(Err_ReadData, [InputFile]) + NL2 + e.Message;
        if I < InputFiles.Count-1 then
        begin
          // Some files are waiting to be opened. Should we continue with error?
          if not ConfirmDlg(ErrorMsg + NL2 + Err_FileTryNext) then break;
        end
        else ErrorDlg(ErrorMsg);
      end;
    end;
  finally
    FreeAndNil(Reader);
    InputFiles.Free;
  end;
end;

end.

