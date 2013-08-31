unit SpectrumStrings;

{$mode objfpc}{$H+}

interface

const
  NL = LineEnding;
  NL2 = LineEnding + LineEnding;

resourcestring
  Action_Undo = 'Undo';
  Action_Redo = 'Redo';

  Err_FileNotFound = 'File "%s" not found.';
  Err_FileTryNext = 'Try to open remaining files?';
  Err_IllegalChar = 'Illegal symbol in input data (the data may be binary).';
  Err_ReadData = 'Error while reading data from %s.';
  Err_TooFewPoints = '%s' + NL2 + 'Too few points for plotting.';
  Err_RSNoSetFile = 'Could not find the spectrum settings file (%s).';
  Err_RSUnknownVersion = 'Could not automatically determine the version of spectrum file. ' + NL +
    'Try to manually set on of predefined version numbers: 0 - Auto, 1 - FSE 40GHz, 2 - FSE 3GHz. ' + NL +
    '(Parameter RSFileVersion in main section of program configuration file).';
  Err_XYLengthsDiffer = 'TGraph.SetValuesXY: X and Y arrays must be equal length.';

  Filter_All = 'All files|*.*';
  Filter_AllCSV = 'All text data files (*.TXT;*.CSV;*.DAT)|*.TXT;*.CSV;*.DAT|';
  Filter_CSV = 'Data files with separators (*.CSV)|*.CSV|';
  Filter_Dag = 'Dagatron counter files (*.*)|*.*|';
  Filter_DAT = 'Data files (*.DAT)|*.DAT|';
  Filter_OO = 'Analyzer Ocean Optics HR2000 files (*.Scope)|*.scope|';
  Filter_RAR = 'RAR archives';
  Filter_RS = 'Analyzer Rohde&Schwarz files (*.TR1,2,3,4)|*.tr1;*.tr2;*.tr3;*.tr4|';
  Filter_TXT = 'Text files (*.TXT)|*.TXT|';
  Filter_ZIP = 'ZIP archives';

  Plot_DefTitle = 'Diagram';

  ParamErr_ZeroRange = 'Range can not be zero (Min = Max).';
  ParamErr_WrongFloat = '%s is not valid number.';
  ParamErr_WrongStep = 'Wrong step value: %g.' + NL2 +
    'Step must be greater than zero but less than range between minimum and maximum (%g).';

  Undo_AddGraph = 'Append Graph';
  Undo_DeleteGraph = 'Delete Graph';
  Undo_DeleteGraphs = 'Delete Graphs';
  Undo_Despike = 'Remove Spikes';
  Undo_Flip = 'Flip Graphs';
  Undo_FlipX = 'Flip Along X Axis';
  Undo_FlipY = 'Flip Along Y Axis';
  Undo_FormulaEdit = 'Edit Formula';
  Undo_GraphMod = 'Change Graphs';
  Undo_Inverse = 'Invert Graphs';
  Undo_Normalize = 'Normalize Graphs';
  Undo_Offset = 'Offset Graphs';
  Undo_Scale = 'Scale Graphs';
  Undo_SwapXY = 'Swap Axes';
  Undo_TitleGraph = 'Rename Graph';
  Undo_TitlePlot = 'Rename Diagram';
  Undo_Trim = 'Trim Graphs';

implementation

end.

