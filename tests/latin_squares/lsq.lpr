program lsq;
uses math.latinsquare;
var
  LS : TLatinSquare;
begin
  repeat
    NewLatinSquare(5, LS);
    if IsLatinSquare(LS) then begin
      PrintLatinSquare(LS);
    end else begin
      PrintLatinSquare(LS);
      WriteLn('Failed.');
    end;
  until IsLatinSquare(LS);
  ReadLn;
end.

