unit session.strutils.mts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;



function GetWordValue(const AParameters: TStringList; AIsSample : boolean;
  AIndex : integer = -1) : string;

function GetFontName(const AParameters: TStringList) : string;
function GetAudioPromptForText(const AParameters: TStringList) : string;

function HasTextPrompt(const AParameters: TStringList) : Boolean;
function HasDAPAAPPrompt(const AParameters: TStringList) : Boolean;

function GetTotalLoopsValue(const AParameters: TStringList) : integer;

implementation

uses
  session.parameters.global,
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

function GetFontName(const AParameters: TStringList): string;
begin
  Result := AParameters.Values[MTSKeys.FontNameKey];
  if Result.IsEmpty then begin
    Result := GlobalTrialParameters.FontName;
  end;
end;

function GetAudioPromptForText(const AParameters: TStringList): string;
begin
  Result := AParameters.Values[MTSKeys.PromptKey];
  if Result.IsEmpty then begin
    Result := GlobalTrialParameters.AudioPromptForText;
  end;
end;

function HasTextPrompt(const AParameters: TStringList): Boolean;
begin
  Result := StrToBooldef(AParameters.Values[MTSKeys.HasTextPromptKey], True);
end;

function HasDAPAAPPrompt(const AParameters: TStringList): Boolean;
var
  LHasPrompt : string;
begin
  Result := StrToBooldef(AParameters.Values[MTSKeys.HasPromptKey], False);
end;

function GetTotalLoopsValue(const AParameters: TStringList): integer;
begin
  Result :=
    StrToIntDef(AParameters.Values[ParserTrialsDAPAAP.TotalLoopsKey],
      GlobalTrialParameters.DefaultAudioLoops);
end;

end.

