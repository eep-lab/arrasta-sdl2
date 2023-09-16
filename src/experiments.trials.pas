{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit experiments.trials;

{$mode objfpc}{$H+}

interface

uses session.constants.trials, session.constants.mts;

procedure WriteToConfigurationFile;

var
  GlobalTrialParameters : TTrialParameters;
  MTSParameters   : TMTSParameters;

implementation

uses
  Classes
  , SysUtils
  , LazFileUtils
  , StrUtils
  , session.csv
  , sdl.app.output
  , session.constants.dragdrop
  , session.fileutils
  , session.configurationfile
  , session.configurationfile.writer
  , sdl.app.trials.mts
  , picanco.experiments.words.types
  , picanco.experiments.words
  , picanco.experiments.images
  , picanco.experiments.audio
  , picanco.experiments.constants
  //, sdl.app.trials.dragdrop
  ;

var Writer : TConfigurationWriter;

procedure WriteTrials(AName: string; ARelation: string;
  AWord: TWord; AComparisons: integer; AHasConsequence : Boolean = True;
  ASamples: integer = 1; ARepeatTrials: integer = 1;
  AHasLimitedHold : Boolean = False);
var
  LWord : TWord;
  i: Integer;
begin
  with Writer.TrialConfig do begin
    with TrialKeys do begin
      Values[Name] := AName;
      Values[Cursor] := GlobalTrialParameters.Cursor.ToString;
      Values[Kind] := TMTS.ClassName;
      if AHasLimitedHold then
        Values[LimitedHold] := GlobalTrialParameters.LimitedHold.ToString;
      Values[InterTrialInterval] := GlobalTrialParameters.InterTrialInterval.ToString;
      Values[RepeatTrials] := ARepeatTrials.ToString;
      Values[HasConsequence] := AHasConsequence.ToString;
    end;

    with MTSKeys do begin
      Values[Relation] := ARelation;
      Values[Samples] := ASamples.ToString;
      Values[Comparisons] := AComparisons.ToString;
      Values[Word] := AWord.Caption;

      for i := Low(AWord.Comparisons) to High(AWord.Comparisons) do begin
        case AWord.Phase.CompModality of
          ModalityA : LWord := AWord.Comparisons[i].Audio^;
          ModalityB : LWord := AWord.Comparisons[i].Image^;
          ModalityC : LWord := AWord.Comparisons[i].Text^;
          ModalityD : LWord := AWord.Comparisons[i].Speech^;
          ModalityNone : LWord := EmptyWord; // should never occur
        end;
        if not LWord.IsEmpty then begin
          Values[Comparison+(i+1).ToString] := LWord.Caption;
        end;
      end;
    end;
  end;
  Writer.WriteTrial;
end;

procedure WriteToConfigurationFile;
var
  LID : integer;
  LCycle : integer;
  LBloc : integer = 0;
  LTrials : integer;
  LComparisons : integer;
  LCondition : integer;
  LName : string;
  LRelation : string;
  LCode : TAlphaNumericCode;
  LHasConsequence : Boolean;
  LWord : TWord;
  LPhase : TPhase;
  LStartAt : TStartAt;
  FCSVRows : TCSVRows;
  LTrialConfiguration : TStringList;

  function ToAlphaNumericCode(S : string) : TAlphaNumericCode;
  var
    LErrorCode : Word;
  begin
    Val(S, Result, LErrorCode);
    if LErrorCode <> 0 then
      Result := NA;
  end;

  function GetWord(APhase : TPhase; ACode : TAlphaNumericCode) : TWord;
  const
    LCodes = [Low(E1CyclesCodeRange)..High(E1CyclesCodeRange)];
  begin
    if ACode in LCodes then begin
      Result := HashWords[E1WordPerCycleCode[APhase.Cycle, ACode]]^;
      Result.Phase := APhase;
      SetComparisons(Result);
    end;
  end;
begin
  //Format('%.2d', [LCycle])
  FCSVRows := TCSVRows.Create('test.csv');

  Writer := TConfigurationWriter.Create(ConfigurationFile);
  try
    for LTrialConfiguration in FCSVRows do  begin
      with LTrialConfiguration do begin
        LID          := Values['ID'].ToInteger;
        LCycle       := Values['Cycle'].ToInteger;
        LCondition   := Values['Condition'].ToInteger;
        LBloc        := Values['Bloc'].ToInteger -1;
        LTrials      := Values['Trials'].ToInteger;
        LComparisons := Values['Comparisons'].ToInteger;
        LRelation    := Values['Relation'];
        LCode        := ToAlphaNumericCode(Values['Code']);
        LHasConsequence := True;
      end;
      LPhase := GetPhase(LCycle, LCondition, LRelation);
      LWord := GetWord(LPhase, LCode);
      LStartAt.Bloc := 1;
      LStartAt.Trial:= 1;
      Writer.StartAt := LStartAt;

      Writer.CurrentBloc := LBloc;
      with Writer.BlocConfig do begin
        Values['Name'] := 'Bloco ' + LBloc.ToString;
      end;
      Writer.WriteBloc;

      LName := LWord.Caption + #32 + LRelation + #32 + LComparisons.ToString + 'C';
      WriteTrials(LName, LRelation, LWord, LComparisons, LHasConsequence);
    end;
  finally
    Writer.Free;
    FCSVRows.Free;
  end;
end;

end.
