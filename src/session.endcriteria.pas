{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.endcriteria;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math
  , session.configuration
  , session.configurationfile
  ;

type

  { TEndCriteria }

  TEndCriteria = class
  private
    //Pool.Block.ID : integer;
    //Pool.Trial.ID : integer;
    FCurrentBlock : TBlockConfiguration;
    FCurrentTrial : TTrialConfiguration;
    FOnHitCriteriaAtSessionEnd: TNotifyEvent;
    FOnNotHitCriteriaAtSessionEnd: TNotifyEvent;
    function NextTrial : SmallInt;
    function NextBlock : SmallInt;
    function HitPorcentageInBlock : Float;
    function IsEndBlock(ATrialID : Word) : Boolean;
    function IsEndSession(ABlockID : Word) : Boolean;
    procedure SetOnHitCriteriaAtSessionEnd(AValue: TNotifyEvent);
    procedure SetOnNotHitCriteriaAtSessionEnd(AValue: TNotifyEvent);
    function ShouldEndSession(var ABlockID : Word) : Boolean;
    function ShouldEndBlock(var ATrialID : Word) : Boolean;
    function HitPercentageCriterionAchieved : Boolean;
  public
    constructor Create;
    procedure InvalidateBlock;
    procedure InvalidateTrial(ATrialData : TTrialConfiguration);
    function OfSession : Boolean;
    function OfBlock : Boolean;
    function OfTrial : Boolean;
    property OnHitCriteriaAtSessionEnd : TNotifyEvent
      read FOnHitCriteriaAtSessionEnd write SetOnHitCriteriaAtSessionEnd;
    property OnNotHitCriteriaAtSessionEnd : TNotifyEvent
      read FOnNotHitCriteriaAtSessionEnd write SetOnNotHitCriteriaAtSessionEnd;
  end;

var
  EndCriteria : TEndCriteria;

implementation

uses
  session.pool
  , session.constants.trials
  , session.counters.all
  , session.loggers.writerow
  ;

{ TEndCriteria }

constructor TEndCriteria.Create;
begin
  inherited Create;

end;

procedure TEndCriteria.InvalidateBlock;
begin
  FCurrentBlock := ConfigurationFile.CurrentBlock;
  ConfigurationFile.NewOrdereringForTrialsInBlock(FCurrentBlock);
  BlockName := FCurrentBlock.Name;
  //Pool.Trial.ID := 0;
end;

procedure TEndCriteria.InvalidateTrial(ATrialData : TTrialConfiguration);
begin
  FCurrentTrial := ATrialData;
  TrialName := FCurrentTrial.Parameters.Values[TrialKeys.NameKey];
end;

function TEndCriteria.OfSession: Boolean;
begin
  // TEndCriteria.OfSession is called once every block end
  Result := IsEndSession(Pool.Block.ID);
end;

// TEndCriteria.OfBlock is called once every intertrial end
// after TEndCriteria.OfTrial
function TEndCriteria.OfBlock: Boolean;
var
  LNextBlock : SmallInt;
begin
  Result := ShouldEndBlock(Pool.Trial.ID);
  if Result then begin
    LNextBlock := NextBlock;
    Pool.Counters.BeforeEndBlock;
    Pool.Counters.EndBlock(LNextBlock);
  end;
end;

// TEndCriteria.OfTrial is called once every intertrial end
function TEndCriteria.OfTrial: Boolean;
var
  LNextTrial: Word;
begin
  LNextTrial := NextTrial;
  ShouldEndBlock(LNextTrial);
  Pool.Counters.BeforeEndTrial;
  Pool.Counters.EndTrial(LNextTrial);

  // if IsEndBlock reset NextTrial on next NextBlock call
  // this result does not have a function right now
  Result := True;
end;

function TEndCriteria.NextTrial: SmallInt;
var
  S1 : string = '';
  S2 : string = '';
  LRepeatValue: LongInt;
  LGoToTrial : SmallInt;
begin
  //LRepeatStyle := repsConsecutive;
  if Assigned(ConfigurationFile) then begin
    S1 := FCurrentTrial.Parameters.Values['RepeatTrial'];
    S2 := FCurrentTrial.Parameters.Values['GoToTrial'];   // TODO
  end;
  LRepeatValue := StrToIntDef(S1, 0) -1;
  LGoToTrial := StrToIntDef(S2, -1);

  if (LGoToTrial > -1) and (LGoToTrial < FCurrentBlock.TotalTrials) then begin
    Result := LGoToTrial;
  end else begin
    Result := Pool.Trial.ID+1;
  end;

  if LRepeatValue > 0 then begin
    if Pool.Session.Block.Trial.Consecutives < LRepeatValue then begin
      Result := Pool.Trial.ID;
    end;
  end;
end;

function TEndCriteria.NextBlock: SmallInt;
var
  LRepeatStyle: TRepeatStyle;
  LRepeatValue: Integer;
  i : integer;

  function NextBlockOnHitCriteria : SmallInt;
  begin
    if FCurrentBlock.NextBlockOnHitCriterion > -1 then begin
      Result := FCurrentBlock.NextBlockOnHitCriterion;
    end;

    if FCurrentBlock.EndSessionOnHitCriterion then begin
      Result := ConfigurationFile.TotalBlocks;
    end;
  end;

  procedure SetNextBlock(AValue : SmallInt);
  begin
    // decide where to go based on repeat style
    case LRepeatStyle of

        // if none, just go to the block, may generate infinite loops
        repsNone : begin
          Result := AValue;
        end;

        // if global, go to a different block
        repsGlobal: begin
          if LRepeatValue > 0 then begin
            if Pool.Session.Tree.Block[Pool.Block.ID].Count < LRepeatValue then begin
              Result := AValue;
            end;
          end;
        end;

        // if consecutive, "go to" same block
        repsConsecutive: begin
          if Pool.Session.Block.Consecutives < LRepeatValue then begin
            Result := Pool.Block.ID;
          end;
        end;
      end;
    //ShouldEndSession(Result);
  end;

  procedure NextBlockOnNotCriteria;
  begin
    if FCurrentBlock.NextBlockOnNotCriterion > -1 then begin
      SetNextBlock(FCurrentBlock.NextBlockOnNotCriterion);
    end;
  end;

begin
  LRepeatStyle := repsNone;
  if FCurrentBlock.MaxBlockRepetition > 0 then begin
    LRepeatValue := FCurrentBlock.MaxBlockRepetition -1;
    LRepeatStyle := repsConsecutive;
  end;

  if FCurrentBlock.MaxBlockRepetitionInSession > 0 then begin
    LRepeatValue := FCurrentBlock.MaxBlockRepetitionInSession -1;
    LRepeatStyle := repsGlobal;
  end;

  // go to next block by default
  Result := Pool.Block.ID+1;

  // todo: refactoring, use case statements of TCustomNextBlockType
  if (FCurrentBlock.BackUpBlock > -1) then begin
    if (FCurrentBlock.BackUpBlockErrors > 0) then begin
      i := Pool.Block.Events.Misses.Count;
      if i >= FCurrentBlock.BackUpBlockErrors then begin
        SetNextBlock(FCurrentBlock.BackUpBlock);
        Exit;
      end;
    end;
  end;

  if FCurrentBlock.CrtHitPorcentage > 0 then begin
    if HitPercentageCriterionAchieved then begin
      Result := NextBlockOnHitCriteria;
      if IsEndSession(Result) then begin
        OnHitCriteriaAtSessionEnd(Self);
      end;
    end else begin
      NextBlockOnNotCriteria;
      if IsEndSession(Result) then begin
        OnNotHitCriteriaAtSessionEnd(Self);
      end;
    end;
    Exit;
  end;

  if FCurrentBlock.CrtConsecutiveHit > 1 then begin
    Pool.Block.Events.Hits.FlushMaxConsecutives;
    i := Pool.Block.Events.Hits.MaxConsecutives;
    if i >= FCurrentBlock.CrtConsecutiveHit then begin
      //Result := NextBlockOnHitCriteria;
    end else begin
      NextBlockOnNotCriteria;
    end;
  end;
end;

function TEndCriteria.ShouldEndSession(var ABlockID: Word): Boolean;

  procedure ForceEndSession;
  begin
    ABlockID := ConfigurationFile.TotalBlocks;
  end;

  procedure EvaluateCriteriaToForceEndSession;
  begin
    if FCurrentBlock.MaxBlockRepetitionInSession > 0 then begin
      if Pool.Session.Tree.Block[Pool.Block.ID].Count >=
         FCurrentBlock.MaxBlockRepetitionInSession then begin
         ForceEndSession;
      end;
    end;
  end;

begin
  EvaluateCriteriaToForceEndSession;
  Result := IsEndSession(ABlockID);
end;

function TEndCriteria.HitPercentageCriterionAchieved: Boolean;
begin
  Result := HitPorcentageInBlock >= FCurrentBlock.CrtHitPorcentage;
end;

function TEndCriteria.HitPorcentageInBlock: Float;
var
  LHits : integer;
begin
  LHits := Pool.Block.Events.Hits.Count;
  Result := (LHits * 100)/FCurrentBlock.TotalTrials;
end;

function TEndCriteria.IsEndBlock(ATrialID : Word): Boolean;
begin
  Result := ATrialID >= FCurrentBlock.TotalTrials;
end;

function TEndCriteria.IsEndSession(ABlockID: Word): Boolean;
begin
  Result := ABlockID >= ConfigurationFile.TotalBlocks;
end;

procedure TEndCriteria.SetOnHitCriteriaAtSessionEnd(AValue: TNotifyEvent);
begin
  if FOnHitCriteriaAtSessionEnd = AValue then Exit;
  FOnHitCriteriaAtSessionEnd := AValue;
end;

procedure TEndCriteria.SetOnNotHitCriteriaAtSessionEnd(AValue: TNotifyEvent);
begin
  if FOnNotHitCriteriaAtSessionEnd = AValue then Exit;
  FOnNotHitCriteriaAtSessionEnd := AValue;
end;

function TEndCriteria.ShouldEndBlock(var ATrialID: Word): Boolean;

  procedure ForceEndBlock;
  begin
    ATrialID := FCurrentBlock.TotalTrials;
  end;

  procedure EvaluateCriteriaToForceEndBlock;
  var
    i : integer;
  begin
    if FCurrentBlock.CrtConsecutiveHit > 1 then begin
      Pool.Block.Events.Hits.FlushMaxConsecutives;
      i := Pool.Block.Events.Hits.MaxConsecutives;
      if i >= FCurrentBlock.CrtConsecutiveHit then begin
        ForceEndBlock;
      end;
    end;
  end;

begin
  EvaluateCriteriaToForceEndBlock;
  Result := IsEndBlock(ATrialID);
end;


end.

