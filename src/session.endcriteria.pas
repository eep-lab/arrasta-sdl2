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
  ;

type

  { TEndCriteria }

  TEndCriteria = class
  private
    FCurrentBloc : TBlocData;
    //FTrial : TCfgTrial;
    procedure EndBlocOnEndTrial;
    procedure EndSessionOnEndBloc;
    function HitPorcentageInBloc : real;
  public
    constructor Create;
    procedure Invalidate;
    function OfSession : Boolean;
    function OfBloc : Boolean;
    function OfTrial : Boolean;
  end;

var
  EndCriteria : TEndCriteria;

implementation

uses
  session.pool
  , session.loggers.writerow
  , session.configurationfile
  ;

{ TEndCriteria }

constructor TEndCriteria.Create;
begin
  Counters.BeginSess;
end;

procedure TEndCriteria.Invalidate;
begin
  FCurrentBloc := ConfigurationFile.CurrentBloc;
  BlocName := FCurrentBloc.Name;
  //LCurrentTrial := Counters.CurrentTrial;
end;

function TEndCriteria.OfSession: Boolean;
begin
  EndSessionOnEndBloc;
  Result := Counters.CurrentBloc >= ConfigurationFile.BlocCount;
end;

function TEndCriteria.OfBloc: Boolean;
begin
  EndBlocOnEndTrial;
  Result := Counters.CurrentTrial >= FCurrentBloc.TotalTrials;
  if Result then begin
    Counters.EndBlc;
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
    if Counters.RepeatedTrials < RepeatTrial then begin
      Result := False;
      Counters.RepeatedTrials := Counters.RepeatedTrials +1;
    end else begin
      Counters.RepeatedTrials := 0;
    end;
  end;

  if Result then begin
    LNextTrial := 1;
  end else begin
    LNextTrial := 0;
  end;
  Counters.CurrentTrial := Counters.CurrentTrial+LNextTrial; // EndTrial
  if Counters.CurrentTrial < 0 then
    raise Exception.Create('CurrentTrial cannot be less than zero.');
end;

procedure TEndCriteria.EndBlocOnEndTrial;
  procedure EndBloc;
  begin
    Counters.CurrentTrial := FCurrentBloc.TotalTrials;
  end;

begin
  if FCurrentBloc.CrtConsecutiveHit > 0 then begin
    if Counters.BlcCscHits >= FCurrentBloc.CrtConsecutiveHit then begin
      EndBloc;
      Exit;
    end;
  end;

  if FCurrentBloc.CrtMaxTrials > 0 then begin
    if Counters.BlcTrials >= FCurrentBloc.CrtMaxTrials then begin
      EndBloc;
      Exit;
    end;
  end;
end;

procedure TEndCriteria.EndSessionOnEndBloc;
  procedure EndSession;
  begin
    if FCurrentBloc.MaxBlcRepetition > 0 then begin
      if (Counters.RepeatedBlocs < FCurrentBloc.MaxBlcRepetition) then begin
        Inc(Counters.RepeatedBlocs);
        Exit;
      end;
    end;

    if FCurrentBloc.AutoEndSession then begin
      { End session }
    end else begin
      Exit;
    end;

    Counters.CurrentBloc := ConfigurationFile.BlocCount;
  end;
  procedure NextBlocOnCriteria;
  begin
    if FCurrentBloc.NextBlocOnCriteria > 0 then begin
      Counters.CurrentBloc := FCurrentBloc.NextBlocOnCriteria-1;
    end;
  end;
begin
  if (FCurrentBloc.CrtHitValue > 0) then begin
    if (Counters.BlcHits < FCurrentBloc.CrtHitValue) then begin
      EndSession;
    end else begin
      NextBlocOnCriteria;
    end;
  end;

  if (FCurrentBloc.CrtHitPorcentage > 0) and
     (FCurrentBloc.CrtHitPorcentage <= 100) then begin
    if (HitPorcentageInBloc < FCurrentBloc.CrtHitPorcentage) then begin
      EndSession;
    end else begin
      NextBlocOnCriteria;
    end;
  end;
end;


function TEndCriteria.HitPorcentageInBloc: real;
begin
  Result := (Counters.BlcHits * 100)/FCurrentBloc.TotalTrials;
end;


end.

