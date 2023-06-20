{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.trials;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , SDL2
  , sdl.timer
  , sdl.app.renderer.custom
  , sdl.app.paintable.contract
  , sdl.app.clickable.contract
  , sdl.app.moveable.contract
  , sdl.app.trials.contract
  , sdl.app.stimuli.contract
  , sdl.app.events.abstract
  , sdl.app.events.custom
  , session.configuration
  ;

type

  { TTrial }

  TTrial = class(TCustomRenderer, ITrial)
    private
      FLimitedHoldTimer    : TSDLTimer;
      FVisible: Boolean;
    protected
      FOnTrialEnd : TNotifyEvent;
      FData : TTrialData;
      FIStimuli : IStimuli;
      procedure Paint; override;
      procedure MouseMove(Sender:TObject; Shift: TCustomShiftState; X, Y: Integer); override;
      procedure MouseDown(Sender:TObject; Shift: TCustomShiftState; X, Y: Integer); override;
      procedure MouseUp(Sender:TObject; Shift: TCustomShiftState; X, Y: Integer); override;
      procedure SetOnTrialEnd(ANotifyEvent: TNotifyEvent);
      procedure SetTrialData(ATrialData: TTrialData); virtual;
      function GetOnTrialEnd: TNotifyEvent;
      function GetTrialData: TTrialData;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function ConsequenceDelay: Cardinal; virtual;
      function ConsequenceInterval: Cardinal; virtual;
      function InterTrialInterval : Cardinal; virtual;
      function AsITrial : ITrial;
      procedure EndTrial; virtual;
      procedure Show; virtual;
      procedure Hide; virtual;
      property Visible : Boolean read FVisible;
      property Data : TTrialData read GetTrialData write SetTrialData;
      property OnTrialEnd : TNotifyEvent read GetOnTrialEnd write SetOnTrialEnd;
  end;

const
  SESSION_TRIALEND = SDL_USEREVENT+0;

implementation

uses session.constants.trials;

{ TTrial }

constructor TTrial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EventHandler.AssignEvents;
  EventHandler.OnMouseButtonDown := AsIClickable.GetSDLMouseButtonDown;
  EventHandler.OnMouseButtonUp := AsIMoveable.GetSDLMouseButtonUp;
  EventHandler.OnMouseMotion := AsIMoveable.GetSDLMouseMotion;
  FVisible := False;
  FLimitedHoldTimer := TSDLTimer.Create;
end;

destructor TTrial.Destroy;
begin
  EventHandler.OnMouseButtonDown := nil;
  EventHandler.OnMouseButtonUp := nil;
  EventHandler.OnMouseMotion := nil;
  EventHandler.OnUserEvent:=nil;
  FLimitedHoldTimer.Free;
  inherited Destroy;
end;

function TTrial.ConsequenceDelay: Cardinal;
begin
  Result := 0;
end;

function TTrial.ConsequenceInterval: Cardinal;
begin
  Result := 0;
end;

function TTrial.InterTrialInterval: Cardinal;
begin
  Result := 0;
end;

function TTrial.AsITrial: ITrial;
begin
  Result := Self as ITrial;
end;

procedure TTrial.MouseMove(Sender: TObject; Shift: TCustomShiftState; X,
  Y: Integer);
var
  Child : TComponent;
  SDLPoint : TSDL_Point;
  IChild : IMoveable;
begin
  if Visible then begin
    for Child in FChilds do begin
      SDLPoint.x := X;
      SDLPoint.y := Y;
      IChild := IMoveable(TCustomRenderer(Child));
      if IChild.PointInside(SDLPoint) then begin
        if not IChild.MouseInside then begin
          IChild.MouseInside:=True;
          IChild.MouseEnter(Sender);
        end;
        IChild.MouseMove(Sender, Shift, X, Y);
      end else begin
        if IChild.MouseInside then begin
          IChild.MouseInside:=False;
          IChild.MouseExit(Sender);
        end;
      end;
    end;
  end;
end;

procedure TTrial.MouseDown(Sender:TObject; Shift: TCustomShiftState; X, Y: Integer);
var
  Child : TComponent;
  SDLPoint : TSDL_Point;
  IChild : IClickable;
begin
  if Visible then begin
    for Child in FChilds do begin
      SDLPoint.x := X;
      SDLPoint.y := Y;
      IChild := IClickable(TCustomRenderer(Child));
      if IChild.PointInside(SDLPoint) then
        IChild.MouseDown(Sender, Shift, X, Y);
    end;
  end;
end;

procedure TTrial.MouseUp(Sender: TObject; Shift: TCustomShiftState; X,
  Y: Integer);
var
  Child : TComponent;
  SDLPoint : TSDL_Point;
  IChild   : IMoveable;
begin
  if Visible then begin
    for Child in FChilds do begin
      SDLPoint.x := X;
      SDLPoint.y := Y;
      IChild := IMoveable(TCustomRenderer(Child));
      if IChild.PointInside(SDLPoint) then
        IChild.MouseUp(Sender, Shift, X, Y);
    end;
  end;
end;

procedure DoEndTrial(Self: Pointer);
var
  event : TSDL_Event;
begin
  event.type_ := SDL_USEREVENT;
  event.user.code := SESSION_TRIALEND;
  event.user.data1 := Self;
  SDL_PushEvent(@event);
end;

procedure TTrial.EndTrial;
var
  SelfAsPointer : Pointer;
begin
  Hide;
  SelfAsPointer := Pointer(Self);
  DoEndTrial(SelfAsPointer);
end;

procedure TTrial.Paint;
var
  Child : TComponent;
begin
  if Visible then begin
    for Child in FChilds do
      IPaintable(TCustomRenderer(Child)).Paint;
  end;
end;

procedure TTrial.SetOnTrialEnd(ANotifyEvent: TNotifyEvent);
begin
  if FOnTrialEnd = ANotifyEvent then Exit;
  FOnTrialEnd := ANotifyEvent;
end;

procedure TTrial.SetTrialData(ATrialData: TTrialData);
var
  Parameters : TStringList;
begin
  FData := ATrialData;
  Parameters := FData.Parameters;
  if Assigned(Parameters) then begin
    with TrialKeys do begin
      with Parameters do begin
        FLimitedHoldTimer.Interval := Values[LimitedHold].ToInteger;
      end;
    end;
  end;
  if Assigned(FIStimuli) then begin
    FIStimuli.Load(FData.Parameters, Self);
  end;
end;

function TTrial.GetOnTrialEnd: TNotifyEvent;
begin
  Result := FOnTrialEnd;
end;

function TTrial.GetTrialData: TTrialData;
begin
  Result := FData;
end;

procedure TTrial.Show;
begin
  FVisible := True;
  if Assigned(FIStimuli) then FIStimuli.Start;
  FLimitedHoldTimer.Start;
end;

procedure TTrial.Hide;
begin
  FVisible := False;
  if Assigned(FIStimuli) then FIStimuli.Stop;
  FLimitedHoldTimer.Stop;
end;

end.

