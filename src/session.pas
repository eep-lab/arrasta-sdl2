{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Session.Blocs, sdl.timer;

type

  { TSession }

  TSession = class(TComponent)
  private
    FTimer : TSDLTimer;
    FBloc : TBloc;
    FOnBeforeStart: TNotifyEvent;
    FOnEndSession: TNotifyEvent;
    procedure PlayBloc;
    procedure EndBloc(Sender : TObject);
    procedure SetOnBeforeStart(AValue: TNotifyEvent);
    procedure SetOnEndSession(AValue: TNotifyEvent);
    procedure TimerOnTimer(Sender: TObject);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure EndSession;
    procedure Play;
    property OnEndSession : TNotifyEvent read FOnEndSession write SetOnEndSession;
    property OnBeforeStart : TNotifyEvent read FOnBeforeStart write SetOnBeforeStart;
    property Timer : TSDLTimer read FTimer;
  end;

var
  SDLSession: TSession;

implementation

uses
  timestamps
  , session.configurationfile
  , session.pool
  , sdl.app.trials.factory
  ;

{ TSession }

procedure TSession.PlayBloc;
begin
  EndCriteria.Invalidate;
  if EndCriteria.OfSession then begin
    EndSession;
  end else begin
    FBloc.BeforePlay;
    FBloc.Play;
  end;
end;

procedure TSession.EndBloc(Sender: TObject);
begin
  Counters.EndBlc;
  PlayBloc;
end;

procedure TSession.EndSession;
begin
  TTrialFactory.GetLastTrial.Show;
  if Assigned(OnEndSession) then OnEndSession(Self);
end;

procedure TSession.SetOnBeforeStart(AValue: TNotifyEvent);
begin
  if FOnBeforeStart=AValue then Exit;
  FOnBeforeStart:=AValue;
end;

procedure TSession.SetOnEndSession(AValue: TNotifyEvent);
begin
  if FOnEndSession=AValue then Exit;
  FOnEndSession:=AValue;
end;

procedure TSession.TimerOnTimer(Sender: TObject);
begin
  FTimer.Stop;
  EndSession;
end;

constructor TSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TSDLTimer.Create;
  FTimer.OnTimer:=@TimerOnTimer;
  FTimer.Interval := 0;
  FBloc := TBloc.Create(Self);
  FBloc.OnEndBloc := @EndBloc;
end;

destructor TSession.Destroy;
begin
  FTimer.Free;
  ConfigurationFile.Free;
  inherited Destroy;
end;

procedure TSession.Play;
begin
  if Assigned(OnBeforeStart) then OnBeforeStart(Self);
  Pool.TimeStart := TickCount;
  {$IFDEF WINDOWS}
  StartEpikTimer;
  {$ENDIF}
  if FTimer.Interval > 0 then
    FTimer.Start;
  PlayBloc;
end;

end.

