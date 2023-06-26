{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.events.custom;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ctypes, sdl2, sdl.app.events.abstract;

type

  { audio events}
  TOnAudioChannelFinished = procedure(const AChannel : cint32) of object;

  { TCustomEventHandler }

  TCustomEventHandler = class sealed (TEventHandler)
    private
      FOnAudioChannelFinished: TOnAudioChannelFinished;
      procedure SetOnAudioChannelFinished(AValue: TOnAudioChannelFinished);
      procedure UserEvent(const event: TSDL_UserEvent);
    public
      constructor Create; reintroduce;
      procedure AssignEvents;
      property OnAudioChannelFinished : TOnAudioChannelFinished read FOnAudioChannelFinished write SetOnAudioChannelFinished;
    public
      property OnMouseMotion;
      property OnMouseButtonDown;
      property OnMouseButtonUp;
      property OnKeyDown;
      property OnUserEvent;
  end;

var
  EventHandler : TCustomEventHandler;

implementation

uses sdl.app.trials, sdl.timer, sdl.app.audio;

{ TCustomEventHandler }

procedure TCustomEventHandler.SetOnAudioChannelFinished(
  AValue: TOnAudioChannelFinished);
begin
  if FOnAudioChannelFinished=AValue then Exit;
  FOnAudioChannelFinished:=AValue;
end;

procedure TCustomEventHandler.UserEvent(const event: TSDL_UserEvent);

  procedure DoOnEndSound;
  begin
    if Assigned(OnAudioChannelFinished) then begin
      OnAudioChannelFinished(event.code);
    end;
  end;

  procedure DoOnEndTrial;
  var
    LOnTrialEnd : TNotifyEvent;
    LTrial: TTrial;
  begin
    LTrial := TTrial(event.data1);
    if Assigned(LTrial) then begin
      LOnTrialEnd := LTrial.OnTrialEnd;
      if Assigned(LOnTrialEnd) then begin
        LOnTrialEnd(LTrial);
      end;
    end;
  end;

  procedure DoOnTimer;
  var
    LTimer : TSDLTimer;
  begin
    LTimer := TSDLTimer(event.data1);
    if Assigned(LTimer) then begin
      if Assigned(LTimer.OnTimer) then begin
        LTimer.OnTimer(LTimer);
      end;
    end;
  end;

begin
  case event.type_ of
    SESSION_TRIALEND:
      DoOnEndTrial;

    SESSION_ONTIMER:
      DoOnTimer;

    SESSION_CHUNK_STOPPED:
      DoOnEndSound;
  end;
end;

constructor TCustomEventHandler.Create;
var
  Event : TSDL_EventType;
  SDLUserEvents : array [0..2] of TSDL_EventType =
    (SESSION_TRIALEND, SESSION_ONTIMER, SESSION_CHUNK_STOPPED);
begin
  inherited Create;
  EventHandler := Self;
  for Event in SDLUserEvents do
    if not UserEventRegistered(Event) then
      raise Exception.Create('Event not registered:'+IntToStr(Event));
  AssignEvents;
end;

procedure TCustomEventHandler.AssignEvents;
begin
  OnUserEvent := @UserEvent;
end;

end.

