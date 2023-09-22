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
  SysUtils
  , session.configuration
  , session.configurationfile
  ;

type

  { TEndCriteria }

  TEndCriteria = class
  private
    FCurrentBlock : TBlockData;
    //FTrial : TCfgTrial;
    procedure EndBlockOnEndTrial;
    procedure EndSessionOnEndBlock;
    function GetRunningAt: TStartAt;
    function HitPorcentageInBlock : real;
    procedure SetRunningAt(AValue: TStartAt);
  public
    constructor Create;
    procedure Invalidate;
    function OfSession : Boolean;
    function OfBlock : Boolean;
    function OfTrial : Boolean;
    function Running : Boolean;
    property RunningAt : TStartAt read GetRunningAt write SetRunningAt;
  end;

var
  EndCriteria : TEndCriteria;

implementation

uses
  session.pool
  , session.counters.all
  , session.loggers.writerow
  ;

{ TEndCriteria }

constructor TEndCriteria.Create;
begin
  // Pool.Counters.BeginSession;
end;

procedure TEndCriteria.Invalidate;
begin
  FCurrentBlock := ConfigurationFile.CurrentBlock;
  BlockName := FCurrentBlock.Name;
  //LCurrentTrial := Counters.CurrentTrial;
end;

function TEndCriteria.OfSession: Boolean;
begin
  EndSessionOnEndBlock;
  Result := Pool.Block.ID >= ConfigurationFile.Blocks;
end;

function TEndCriteria.OfBlock: Boolean;
begin
  EndBlockOnEndTrial;
  Result := Pool.Trial.ID >= FCurrentBlock.TotalTrials;
  if Result then begin
    Pool.Counters.EndBlock(0, 0);
  end;
end;

function TEndCriteria.OfTrial: Boolean;
var
  LRepeatTrial, LGoToTrial: integer;
  S1 : string = '';
  S2 : string = '';
begin
  if Assigned(ConfigurationFile) then begin
    S1 := ConfigurationFile.CurrentTrial.Parameters.Values['RepeatTrial'];
    S2 := ConfigurationFile.CurrentTrial.Parameters.Values['GoToTrial'];
  end;
  LRepeatTrial := StrToIntDef(S1, 0) -1;
  LGoToTrial := StrToIntDef(S2, 1);
  Result := Pool.Counters.EndTrial(LRepeatTrial, LGoToTrial);
end;

function TEndCriteria.Running: Boolean;
begin
  Result :=
    (Pool.Block.ID  >= ConfigurationFile.Blocks) and
    (Pool.Trial.ID >= FCurrentBlock.TotalTrials);
end;

procedure TEndCriteria.EndBlockOnEndTrial;
var
  LTrials : TTrialCounters;
  procedure EndBlock;
  begin
    Pool.Trial.ID := FCurrentBlock.TotalTrials;
  end;
begin
  LTrials := Pool.Block.Trial;
  if FCurrentBlock.CrtConsecutiveHit > 0 then begin
    if LTrials.Events.Hits.Consecutives >= FCurrentBlock.CrtConsecutiveHit then begin
      EndBlock;
      Exit;
    end;
  end;

  if FCurrentBlock.CrtMaxTrials > 0 then begin
    if LTrials.Count >= FCurrentBlock.CrtMaxTrials then begin
      EndBlock;
      Exit;
    end;
  end;
end;

procedure TEndCriteria.EndSessionOnEndBlock;
var
  LTrials : TTrialCounters;
  procedure EndSession;
  begin
    if FCurrentBlock.MaxBlockRepetition > 0 then begin
      if (Pool.Block.Consecutives < FCurrentBlock.MaxBlockRepetition) then begin
        Pool.Block.NextConsecutive;
        Exit;
      end;
    end;

    if FCurrentBlock.AutoEndSession then begin
      { End session }
    end else begin
      Exit;
    end;

    Pool.Block.ID := ConfigurationFile.Blocks;
  end;
  procedure NextBlockOnCriteria;
  begin
    if FCurrentBlock.NextBlockOnCriteria > 0 then begin
      Pool.Block.ID := FCurrentBlock.NextBlockOnCriteria-1;
    end;
  end;
begin
  LTrials := Pool.Block.Trial;
  if (FCurrentBlock.CrtHitValue > 0) then begin
    if (LTrials.Events.Hits.Count < FCurrentBlock.CrtHitValue) then begin
      EndSession;
    end else begin
      NextBlockOnCriteria;
    end;
  end;

  if (FCurrentBlock.CrtHitPorcentage > 0) and
     (FCurrentBlock.CrtHitPorcentage <= 100) then begin
    if (HitPorcentageInBlock < FCurrentBlock.CrtHitPorcentage) then begin
      EndSession;
    end else begin
      NextBlockOnCriteria;
    end;
  end;
end;

function TEndCriteria.GetRunningAt: TStartAt;
begin
  Result.Trial := Pool.Trial.ID;
  Result.Block := Pool.Block.ID;
end;

function TEndCriteria.HitPorcentageInBlock: real;
var
  LTrials: TTrialCounters;
begin
  LTrials := Pool.Block.Trial;
  Result := (LTrials.Events.Hits.Count * 100)/FCurrentBlock.TotalTrials;
end;

procedure TEndCriteria.SetRunningAt(AValue: TStartAt);
begin
  Pool.Trial.ID := AValue.Trial;
  Pool.Block.ID := AValue.Block;
end;


end.

