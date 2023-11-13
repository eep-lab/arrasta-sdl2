unit session.strutils.mts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;



function GetWordValue(const AParameters: TStringList; AIsSample : boolean;
  AIndex : integer = -1) : string;

function HasPrompt(const AParameters: TStringList) : Boolean;

implementation

uses session.constants.mts;

function GetWordValue(const AParameters: TStringList; AIsSample: boolean;
  AIndex: integer): string;
begin
  if AIsSample then begin
    Result := AParameters.Values[MTSKeys.WordKey];
  end else begin
    Result := AParameters.Values[MTSKeys.ComparisonKey+(AIndex+1).ToString];
  end;
end;

function HasPrompt(const AParameters: TStringList): Boolean;
begin
  StrToBooldef(AParameters.Values[MTSKeys.HasPromptKey], True);
end;

end.

