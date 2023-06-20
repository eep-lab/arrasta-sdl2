{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.blocs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TBloc }

  {
    Controls Trials Create/Destroy cycle
  }
  TBloc = class(TComponent)
  private
    FOnEndBloc: TNotifyEvent;
    FOnInterTrialEnd: TNotifyEvent;
    procedure SetOnEndBloc(AValue: TNotifyEvent);
    procedure InterTrialEventsEnd(Sender: TObject);
    procedure SetOnInterTrialEnd(AValue: TNotifyEvent);
  public
    constructor Create(AOwner : TComponent); override;
    procedure BeforePlay;
    procedure Play;
    property OnEndBloc : TNotifyEvent read FOnEndBloc write SetOnEndBloc;
    property OnInterTrialEnd : TNotifyEvent read FOnInterTrialEnd write SetOnInterTrialEnd;
  end;

implementation

uses sdl.app.trials.factory
   , sdl.app.trials.contract
   , session.intertrial
   , session.pool
   ;

{ TBloc }

procedure TBloc.SetOnEndBloc(AValue: TNotifyEvent);
begin
  if FOnEndBloc=AValue then Exit;
  FOnEndBloc:=AValue;
end;

procedure TBloc.InterTrialEventsEnd(Sender: TObject);
var
  LNextTrial: Integer;
begin
  if EndCriteria.OfTrial then begin
    LNextTrial := 1;
  end else begin
    LNextTrial := 0;
  end;
  Counters.CurrentTrial := Counters.CurrentTrial+LNextTrial;
  if Counters.CurrentTrial < 0 then
    Exception.Create('Exception. CurrentTrial cannot be less than zero.');
  Play;
end;

procedure TBloc.SetOnInterTrialEnd(AValue: TNotifyEvent);
begin
  if FOnInterTrialEnd=AValue then Exit;
  FOnInterTrialEnd:=AValue;
end;

constructor TBloc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InterTrial := TInterTrialEvents.Create(Self);
  InterTrial.OnEnd:=@InterTrialEventsEnd;
end;

procedure TBloc.BeforePlay;
begin

end;

procedure TBloc.Play;
begin
  EndCriteria.Invalidate;
  if EndCriteria.OfBloc then begin
    if Assigned(OnEndBloc) then
      OnEndBloc(Self);
  end else begin
    TTrialFactory.NextTrial;
  end;
end;

end.
