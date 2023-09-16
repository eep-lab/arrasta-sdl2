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
  , session.loggers.writerow
  ;

{ TEndCriteria }

constructor TEndCriteria.Create;
begin
  Pool.Counters.BeginSess;
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
  Result := Pool.Counters.CurrentBlock >= ConfigurationFile.BlockCount;
end;

function TEndCriteria.OfBlock: Boolean;
begin
  EndBlockOnEndTrial;
  Result := Pool.Counters.CurrentTrial >= FCurrentBlock.TotalTrials;
  if Result then begin
    Pool.Counters.EndBlock;
  end;
end;

function TEndCriteria.OfTrial: Boolean;
var
  RepeatTrial , LNextTrial: integer;
  S1 : string = '';
begin
  Result := True;
  if Assigned(ConfigurationFile) then begin
    S1 := ConfigurationFile.CurrentTrial.Parameters.Values['RepeatTrial'];
  end;
  RepeatTrial := StrToIntDef(S1, 0) -1;
  if RepeatTrial > 0 then begin
    if Pool.Counters.RepeatedTrials < RepeatTrial then begin
      Result := False;
      Pool.Counters.RepeatedTrials := Pool.Counters.RepeatedTrials +1;
    end else begin
      Pool.Counters.RepeatedTrials := 0;
    end;
  end;

  if Result then begin
    LNextTrial := 1;
  end else begin
    LNextTrial := 0;
  end;
  Pool.Counters.CurrentTrial := Pool.Counters.CurrentTrial+LNextTrial; // EndTrial
  if Pool.Counters.CurrentTrial < 0 then
    raise Exception.Create('CurrentTrial cannot be less than zero.');
end;

function TEndCriteria.Running: Boolean;
begin
  Result :=
    (Pool.Counters.CurrentBlock  >= ConfigurationFile.BlockCount) and
    (Pool.Counters.CurrentTrial >= FCurrentBlock.TotalTrials);
end;

procedure TEndCriteria.EndBlockOnEndTrial;
  procedure EndBlock;
  begin
    Pool.Counters.CurrentTrial := FCurrentBlock.TotalTrials;
  end;

begin
  if FCurrentBlock.CrtConsecutiveHit > 0 then begin
    if Pool.Counters.BlockCscHits >= FCurrentBlock.CrtConsecutiveHit then begin
      EndBlock;
      Exit;
    end;
  end;

  if FCurrentBlock.CrtMaxTrials > 0 then begin
    if Pool.Counters.BlockTrials >= FCurrentBlock.CrtMaxTrials then begin
      EndBlock;
      Exit;
    end;
  end;
end;

procedure TEndCriteria.EndSessionOnEndBlock;
  procedure EndSession;
  begin
    if FCurrentBlock.MaxBlockRepetition > 0 then begin
      if (Pool.Counters.RepeatedBlocks < FCurrentBlock.MaxBlockRepetition) then begin
        Inc(Pool.Counters.RepeatedBlocks);
        Exit;
      end;
    end;

    if FCurrentBlock.AutoEndSession then begin
      { End session }
    end else begin
      Exit;
    end;

    Pool.Counters.CurrentBlock := ConfigurationFile.BlockCount;
  end;
  procedure NextBlockOnCriteria;
  begin
    if FCurrentBlock.NextBlockOnCriteria > 0 then begin
      Pool.Counters.CurrentBlock := FCurrentBlock.NextBlockOnCriteria-1;
    end;
  end;
begin
  if (FCurrentBlock.CrtHitValue > 0) then begin
    if (Pool.Counters.BlockHits < FCurrentBlock.CrtHitValue) then begin
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
  Result.Trial := Pool.Counters.CurrentTrial;
  Result.Block := Pool.Counters.CurrentBlock;
end;

function TEndCriteria.HitPorcentageInBlock: real;
begin
  Result := (Pool.Counters.BlockHits * 100)/FCurrentBlock.TotalTrials;
end;

procedure TEndCriteria.SetRunningAt(AValue: TStartAt);
begin
  Pool.Counters.CurrentTrial := AValue.Trial;
  Pool.Counters.CurrentBlock := AValue.Block;
end;


end.

