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
    procedure EndBloc;
  public
    constructor Create(AOwner : TComponent); override;
    procedure BeforePlay;
    procedure Play;
    property OnEndBloc : TNotifyEvent read FOnEndBloc write SetOnEndBloc;
    property OnInterTrialEnd : TNotifyEvent read FOnInterTrialEnd write SetOnInterTrialEnd;
  end;

implementation

uses sdl.app.trials.factory
   , session.intertrial
   , session.pool
   , session.loggers.writerow
   , session.configurationfile
   ;

{ TBloc }

procedure TBloc.SetOnEndBloc(AValue: TNotifyEvent);
begin
  if FOnEndBloc=AValue then Exit;
  FOnEndBloc:=AValue;
end;

procedure TBloc.InterTrialEventsEnd(Sender: TObject);
begin
  EndCriteria.OfTrial;
  Play;
end;

procedure TBloc.SetOnInterTrialEnd(AValue: TNotifyEvent);
begin
  if FOnInterTrialEnd=AValue then Exit;
  FOnInterTrialEnd:=AValue;
end;

procedure TBloc.EndBloc;
begin
  if Assigned(OnEndBloc) then
    OnEndBloc(Self);
end;

constructor TBloc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InterTrial := TInterTrialEvents.Create(Self);
  InterTrial.OnEnd:=@InterTrialEventsEnd;
end;

procedure TBloc.BeforePlay;
begin
  EndCriteria.Invalidate;
end;

procedure TBloc.Play;
begin
  if EndCriteria.OfBloc then begin
    EndBloc;
  end else begin
    TTrialFactory.Play;
  end;
end;

end.
