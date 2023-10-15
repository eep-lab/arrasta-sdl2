unit session.strutils.mts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function GetWordValue(AParameters: TStringList; AIsSample : boolean;
  AIndex : integer = -1) : string;

implementation

uses session.constants.mts;

function GetWordValue(AParameters: TStringList; AIsSample: boolean;
  AIndex: integer): string;
begin
  if AIsSample then begin
    Result := AParameters.Values[MTSKeys.Word];
  end else begin
    Result := AParameters.Values[MTSKeys.Comparison+(AIndex+1).ToString];
  end;
end;

end.

