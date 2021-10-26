unit SudokuGUI;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SudokuSolver, Vcl.Grids, Vcl.StdCtrls, System.Diagnostics,
  System.TimeSpan;

type
  TForm2 = class(TForm)
    StringGrid: TStringGrid;
    BtnSolve: TButton;
    BtnClear: TButton;
    TimeElapsed: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure BtnSolveClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure DisplaySolution;
    procedure ClearGrid;
  private
    Stopwatch: TStopWatch;
    Elapsed: TTimeSpan;
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  Application.Title := 'Sudoku Solver';
  TimeElapsed.Caption := '';
end;

procedure TForm2.DisplaySolution;
begin
  for var I := 0 to 8 do begin
    for var J := 0 to 8 do begin
      StringGrid.Cells[J, I] := IntToStr(SudokuGrid[I+1][J+1]);
    end;
  end;
end;

procedure TForm2.ClearGrid;
begin
  for var I := 0 to 8 do begin
    for var J := 0 to 8 do begin
      StringGrid.Cells[J, I] := '';
    end;
  end;
  TimeElapsed.Caption := '';
end;

procedure TForm2.BtnSolveClick(Sender: TObject);
var RowArr: array[0..9] of Byte;
begin
  for var I := 0 to 8 do begin
    for var J := 0 to 8 do begin
      if StringGrid.Cells[J, I] = '' then RowArr[J] := 0
      else if StrToInt(StringGrid.Cells[J, I]) > 9 then begin
        ShowMessage('Given Sudoku is not valid.');
        Exit;
      end
      else RowArr[J] := StrToInt(StringGrid.Cells[J, I]);
    end;
    CreateSudoku(I+1, RowArr);
  end;

  if not IsValid then ShowMessage('Given Sudoku is not valid.')
  else begin
    Stopwatch := TStopWatch.StartNew;
    SolveSudoku;
    DisplaySolution;
    Elapsed := Stopwatch.Elapsed;
    TimeElapsed.Caption := 'Time Taken to solve: '+ sLineBreak + Elapsed.TotalSeconds.ToString + 's';
  end;
end;

procedure TForm2.BtnClearClick(Sender: TObject);
begin
  ClearGrid;
end;

end.
