unit session.strutils.mts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;



function GetWordValue(const AParameters: TStringList; AIsSample : boolean;
  AIndex : integer = -1) : string;

function HasPrompt(const AParameters: TStringList) : Boolean;
function HasDAPAAPPrompt(const AParameters: TStringList) : Boolean;

function GetTotalLoopsValue(const AParameters: TStringList) : integer;

implementation

uses
  session.constants.mts,
  session.constants.trials.dapaap;

function GetWordValue(const AParameters: TStringList; AIsSample: boolean;
  AIndex: integer): string;
begin
  if AIsSample then begin
    Result := AParameters.Values[MTSKeys.SampleKey+(AIndex+1).ToString];
  end else begin
    Result := AParameters.Values[MTSKeys.ComparisonKey+(AIndex+1).ToString];
  end;
end;

function HasPrompt(const AParameters: TStringList): Boolean;
begin
  Result := StrToBooldef(AParameters.Values[MTSKeys.HasPromptKey], True);
end;

function HasDAPAAPPrompt(const AParameters: TStringList): Boolean;
begin
  Result := StrToBooldef(AParameters.Values[MTSKeys.HasPromptKey], False);
end;

function GetTotalLoopsValue(const AParameters: TStringList): integer;
begin
  Result := StrToIntDef(AParameters.Values[ParserTrialsDAPAAP.TotalLoopsKey], 1);
end;

end.

