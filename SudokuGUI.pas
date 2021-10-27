unit SudokuGUI;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SudokuSolver, Vcl.Grids, Vcl.StdCtrls, System.Diagnostics,
  System.TimeSpan, Vcl.ExtCtrls;

type
  TForm2 = class(TForm)
    StringGrid: TStringGrid;
    BtnSolve: TButton;
    BtnClear: TButton;
    TimeElapsed: TStaticText;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure BtnSolveClick(Sender: TObject);
    procedure BtnClearClick(Sender: TObject);
    procedure DisplaySolution;
    procedure ClearGrid;
  private
  public
    { Public declarations }
  end;

  MyThread = class(TThread)
    private
    protected
    public
      constructor Create;
      destructor Destroy; override;
      procedure Execute; override;
  end;

var
  Form2: TForm2;
  Stopwatch: TStopWatch;
  Elapsed: TTimeSpan;
  MT: MyThread;

implementation

{$R *.dfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  Application.Title := 'Sudoku Solver';
  TimeElapsed.Caption := '';
end;

procedure TForm2.DisplaySolution;
begin
  StringGrid.DefaultDrawing := True;
  for var I := 0 to 8 do begin
    StringGrid.DefaultDrawing := True;for var J := 0 to 8 do begin
      StringGrid.Cells[J, I] := IntToStr(SudokuGrid[I+1][J+1]);
    end;
  end;
  Panel1.Visible := False;
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
    Panel1.Visible := True;
    MT := MyThread.Create;
  end;
end;

procedure TForm2.BtnClearClick(Sender: TObject);
begin
  ClearGrid;
end;

{ MyThread }

constructor MyThread.Create;
begin
  inherited Create;
end;

destructor MyThread.Destroy;
begin
  inherited;
end;

procedure MyThread.Execute;
begin
  Stopwatch := TStopWatch.StartNew;
  Form2.StringGrid.DefaultDrawing := False;
  SolveSudoku;
  Form2.DisplaySolution;
  Elapsed := Stopwatch.Elapsed;
  Form2.TimeElapsed.Caption := 'Time Taken to solve: '+ sLineBreak + Elapsed.TotalSeconds.ToString + 's';
end;

end.
