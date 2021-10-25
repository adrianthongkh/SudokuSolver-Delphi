program SudokuGUIProj;

uses
  Vcl.Forms,
  SudokuGUI in 'SudokuGUI.pas' {Form2},
  SudokuSolver in 'SudokuSolver.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
