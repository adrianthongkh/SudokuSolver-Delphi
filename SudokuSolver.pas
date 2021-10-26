unit SudokuSolver;

interface
  function IsSafe(value, row, col: Byte): Boolean;
  function IsValid: Boolean;
  procedure createSudoku(row: Byte; const RowArr: array of Byte);
  procedure SolveSudokuCell(row, col: Byte);
  procedure SolveSudoku;

var
  SudokuGrid: array [1..9, 1..9] of Byte;
  complete: Boolean;

implementation

procedure createSudoku(row: Byte; const RowArr: array of Byte);
begin
  for var I := 1 to 9 do
    SudokuGrid[row][I] := RowArr[I-1];
end;

function IsSafe(value, row, col: Byte): Boolean;
var ptrRow, ptrCol: Byte;
begin
  // determine checking box
  if row mod 3 <> 0 then ptrRow := (row div 3) * 3 + 1
  else ptrRow := row - 2;
  if col mod 3 <> 0 then ptrCol := (col div 3) * 3 + 1
  else ptrCol := col - 2;

  Result := true;
  // Check box
  for var I := ptrRow to ptrRow + 2 do begin
    for var J := ptrCol to ptrCol + 2 do begin
      if (I <> row) and (J <> col) and (SudokuGrid[I][J] = value) then begin
        Result := false;
        Exit;
      end;
    end;
  end;

  // Check for row and column
  for var I := 1 to 9 do
  begin
    if value = SudokuGrid[row][I] then Result := false;
    if value = SudokuGrid[I][col] then Result := false;
    if not Result then Exit;
  end;
end;

procedure SolveSudokuCell(row, col: Byte);
begin
  // proceed to next row
  if col > 9 then
    SolveSudokuCell(row + 1, 1);

  // Filled completely
  if row > 9 then Exit;
  if col > 9 then Exit;

  // indicate that solution has been found
  if (col = 9) and (row = 9) then complete := True;

  // find suitable value incrementally
  if SudokuGrid[row][col] = 0 then begin
    for var I := 1 to 10 do begin
      if I <> 10 then begin
        if IsSafe(I, row, col) then begin
          SudokuGrid[row][col] := I;
          SolveSudokuCell(row, col+1);
        end;
      // Trigger Backtracking
      end else if (I = 10) and (not complete) then begin
        SudokuGrid[row][col] := 0;
        Exit;
      end;
    end;
  end else SolveSudokuCell(row, col+1);
end;

function IsValid: Boolean;
var temp : Byte;
begin
  Result := True;
  for var I := 1 to 9 do begin
    for var J := 1 to 9 do begin
      temp := SudokuGrid[I][J];
      SudokuGrid[I][J] := 0;
      if (temp <> 0) and (not IsSafe(temp, I, J)) then begin
        Result := False;
        Exit;
      end else SudokuGrid[I][J] := temp;
    end;
  end;

end;

procedure SolveSudoku;
begin
  complete := false;
  SolveSudokuCell(1, 1);
end;

end.
