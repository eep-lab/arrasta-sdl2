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

procedure WriteToConfigurationFile(AFilename : string);

var
  GlobalTrialParameters : TTrialParameters;
  MTSParameters   : TMTSParameters;

implementation

uses
  Classes
  , SysUtils
  , LazFileUtils
  //, StrUtils
  , session.csv
  , session.constants.blocks
  , sdl.app.output
  , session.constants.dragdrop
  //, session.fileutils
  , session.configurationfile
  , session.configurationfile.writer
  , sdl.app.trials.mts
  , picanco.experiments.words.types
  , picanco.experiments.words
  , picanco.experiments.images
  //, picanco.experiments.audio
  , picanco.experiments.constants
  , picanco.experiments.constants.parser
  //, sdl.app.trials.dragdrop
  ;

var Writer : TConfigurationWriter;

procedure WriteInstruction(ABlock: integer; ATrial : integer; AName: string;
  AValue: string; ADoCalibration : Boolean);
begin
  with TrialKeys do begin
    Writer.WriteInstruction(ABlock, ATrial, AName, AValue);
    Writer.WriteInstruction(ABlock, ATrial, DoCalibrationKey,
     BoolToStr(ADoCalibration, True));
  end;
end;

procedure WriteMTSTrials(AName: string; AReferenceName : string;
  ARelation: string; AWord: TWord; AComparisons: integer;
  AHasConsequence : Boolean = True; ASamples: integer = 1;
  ARepeatTrials: integer = 1; AHasLimitedHold : Boolean = False);
var
  LWord : TWord;
  i: Integer;
begin
  with Writer.TrialConfig do begin
    with TrialKeys do begin
      Values[ReferenceNameKey] := AReferenceName;
      Values[NameKey] := AName;
      Values[CursorKey] := GlobalTrialParameters.Cursor.ToString;
      Values[KindKey] := TMTS.ClassName;
      if AHasLimitedHold then
        Values[LimitedHoldKey] := GlobalTrialParameters.LimitedHold.ToString;
      Values[InterTrialIntervalKey] := GlobalTrialParameters.InterTrialInterval.ToString;
      Values[RepeatTrialsKey] := ARepeatTrials.ToString;
      Values[HasConsequenceKey] := AHasConsequence.ToString;
    end;

    with MTSKeys do begin
      Values[RelationKey] := ARelation;
      Values[SamplesKey] := ASamples.ToString;
      Values[ComparisonsKey] := AComparisons.ToString;
      Values[WordKey] := AWord.Caption;

      for i := Low(AWord.Comparisons) to High(AWord.Comparisons) do begin
        case AWord.Phase.CompModality of
          ModalityA : LWord := AWord.Comparisons[i].Audio^;
          ModalityB : LWord := AWord.Comparisons[i].Image^;
          ModalityC : LWord := AWord.Comparisons[i].Text^;
          ModalityD : LWord := AWord.Comparisons[i].Speech^;
          ModalityNone : LWord := EmptyWord; // should never occur
        end;
        if not LWord.IsEmpty then begin
          Values[ComparisonKey+(i+1).ToString] := LWord.Caption;
        end;
      end;
    end;
  end;
  Writer.WriteTrial;
end;

procedure WriteToConfigurationFile(AFilename : string);
var
  LTrialID : integer;
  LBlockID : integer;
  LCycle : integer;
  LBackUpBlock : integer = 0;
  LTrials : integer;
  LHitCriterion : integer;
  LComparisons : integer;
  LCondition : integer;
  LBackUpBlockErrors : integer;
  LMaxBlockRepetition : integer;
  LMaxBlockRepetitionInSession, LNextBlockOnHitCriterion, i,
    LReinforcement: integer;
  LName : string;
  LRelation : string;
  LCode , LInstruction: string;
  LEndOnHitCriterion: Boolean;
  LWord : TWord;
  LPhase : TPhase;
  LStartAt : TStartAt;
  LParser : TCSVRows;
  LRow : TStringList;
  LHasBlocksFile , LDoCalibration: Boolean;

  function ToAlphaNumericCode(S : string) : TAlphaNumericCode;
  var
    LErrorCode : Word;
  begin
    Val(S, Result, LErrorCode);
    if LErrorCode <> 0 then
      Result := NA;
  end;

  function GetWord(APhase : TPhase; ACode : TAlphaNumericCode) : TWord;
  var
    LCode : string;
  begin
    case ACode of
      Low(E1PreTrainingRange)..High(E1PreTrainingRange): begin
        Result := HashPreTrainingWords[UniqueCodeToStr(ACode)]^;
      end;
      Low(E1CyclesCodeRange)..High(E1CyclesCodeRange): begin
       Result := HashWords[E1WordPerCycleCode[APhase.Cycle, ACode]]^;
      end;
      Low(E1WordsWithCodesRange)..High(E1WordsWithCodesRange): begin
       Result := HashWords[E1WordsWithCodes[ACode]]^;
      end;
     else begin
       WriteStr(LCode, ACode);
       raise Exception.Create('Unknown Word: '+ LCode);
     end;
    end;
    Result.Phase := APhase;
    SetComparisons(Result);
  end;

  function Validated : Boolean;
  begin
    Result := (LHitCriterion >= 0) and
              (LHitCriterion <= 100);
    if not Result then
      raise Exception.Create('Hit porcentage criterion is not valid: '+
        LHitCriterion.ToString);
  end;
  procedure PopulateBooleanStrings;
  begin
    SetLength(TrueBoolStrs, 1);
    TrueBoolStrs[0] := 'T';

    SetLength(FalseBoolStrs, 1);
    FalseBoolStrs[0] := 'F';
  end;

begin
  PopulateBooleanStrings;

  //Format('%.2d', [LCycle])
  Writer := TConfigurationWriter.Create(ConfigurationFile);
  LParser := TCSVRows.Create;
  try
    // parse blocks
    LHasBlocksFile := BlocksFileExists(AFilename);
    if LHasBlocksFile then begin
      LParser.Clear;
      LParser.LoadFromFile(InsideBlocksSubFolder(AFilename));
      for LRow in LParser do  begin
        with LRow, ParserBlockKeys do begin
          LBlockID := Values['ID'].Trim.ToInteger -1;
          LBackUpBlock :=
            Values[NextBlockOnNotCriterionKey].Trim.ToInteger -1;
          LBackUpBlockErrors :=
            Values[BackUpBlockErrorsKey].Trim.ToInteger;
          LMaxBlockRepetition :=
            Values[MaxBlockRepetitionKey].Trim.ToInteger;
          LMaxBlockRepetitionInSession :=
            Values[MaxBlockRepetitionInSessionKey].Trim.ToInteger;
          LEndOnHitCriterion :=
            StrToBool(Values[EndSessionOnHitCriterionKey].Trim);
          LNextBlockOnHitCriterion :=
            Values[NextBlockOnHitCriterionKey].Trim.ToInteger -1;
          LHitCriterion :=
            Values[CrtHitPorcentageKey].Trim.ToInteger;
          LReinforcement :=
            Values[ReinforcementKey].Trim.ToInteger;
        end;

        if not Validated then Exit;

        Writer.CurrentBlock := LBlockID;
        with Writer.BlockConfig, ParserBlockKeys do begin
          Values['Name'] :=
            'Block ' + (LBlockID+1).ToString;
          Values[NextBlockOnNotCriterionKey] :=
            LBackUpBlock.ToString;
          Values[BackUpBlockErrorsKey] :=
            LBackUpBlockErrors.ToString;
          Values[MaxBlockRepetitionKey] :=
            LMaxBlockRepetition.ToString;
          Values[MaxBlockRepetitionInSessionKey] :=
            LMaxBlockRepetitionInSession.ToString;
          Values[EndSessionOnHitCriterionKey] :=
            LEndOnHitCriterion.ToString;
          Values[NextBlockOnHitCriterionKey] :=
            LNextBlockOnHitCriterion.ToString;
          Values[CrtHitPorcentageKey] :=
            LHitCriterion.ToString;
          Values[ReinforcementKey] :=
            LReinforcement.ToString;
        end;
        Writer.WriteBlock;
      end;
    end;

    // parse trials
    if BaseFileExists(AFilename) then begin
      LParser.Clear;
      LParser.LoadFromFile(InsideBaseFolder(AFilename));
      for LRow in LParser do  begin
        with LRow, ParserMTSKeys  do begin
          LTrialID     := Values[IDKey].Trim.ToInteger;
          LCycle       := Values[CycleKey].Trim.ToInteger;
          LCondition   := Values[ConditionKey].Trim.ToInteger;
          LBlockID     := Values[BlockKey].Trim.ToInteger -1;
          LTrials      := Values[TrialsKey].Trim.ToInteger;
          LComparisons := Values[ComparisonsKey].Trim.ToInteger;
          LRelation    := Values[RelationKey].Trim;
          LCode        := Values[CodeKey].Trim;
        end;
        LPhase := GetPhase(LCycle, LCondition, LRelation);
        LWord := GetWord(LPhase, ToAlphaNumericCode(LCode));

        Writer.CurrentBlock := LBlockID;
        if LHasBlocksFile then begin
          { do nothing }
        end else begin
          with Writer.BlockConfig do begin
            Values['Name'] := 'Block ' + (LBlockID+1).ToString;
          end;
          Writer.WriteBlock;
        end;

        for i := 0 to LTrials -1 do begin
          LName := LTrialID.ToString + #32 + '(' + LWord.Caption + #32 +
            LRelation + #32 + LComparisons.ToString + 'C)';
          WriteMTSTrials(
            LName, LCode, LRelation, LWord, LComparisons);
        end;
      end;

      LStartAt.Block := 0;
      LStartAt.Trial:= 0;
      Writer.StartAt := LStartAt;
    end;

    // parse instructions to trials
    if InstructionsFileExist(AFilename) then begin
      LParser.Clear;
      LParser.LoadFromFile(InsideInstructionsSubFolder(AFilename));
       for LRow in LParser do  begin
        with LRow, TrialKeys do begin
          LBlockID     := Values['Block'].Trim.ToInteger-1;
          LTrialID     := Values['Trial'].Trim.ToInteger-1;
          LInstruction := Values[InstructionKey].Trim;
          LDoCalibration := StrToBool(Values[DoCalibrationKey].Trim);
          WriteInstruction(LBlockID, LTrialID,
            InstructionKey, LInstruction, LDoCalibration);
        end;
      end;
    end;
    Writer.Invalidate;
  finally
    Writer.Free;
    LParser.Free;
  end;
end;

end.
