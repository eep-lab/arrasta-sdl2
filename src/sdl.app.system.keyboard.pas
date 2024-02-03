unit sdl.app.system.keyboard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, SDL2,
  sdl.app.events.abstract;

type

  TKeyDownEvents = specialize TList<TOnKeyDownEvent>;

  { TSDLSystemKeyboard }

  TSDLSystemKeyboard = class
  private
    FOnKeyDown : TOnKeyDownEvent;
    FOnKeyDownEvents : TKeyDownEvents;
    procedure KeyDown(const event: TSDL_KeyboardEvent);
  public
    constructor Create;
    destructor Destroy; override;
    procedure RegisterOnKeyDown(AOnKeyDownEvent : TOnKeyDownEvent);
    property OnKeyDown : TOnKeyDownEvent read FOnKeyDown;
  end;

implementation

uses sdl.app, eye.tracker, ctypes, sdl.app.trials.factory;

{ TSDLKeyboard }

procedure TSDLSystemKeyboard.KeyDown(const event: TSDL_KeyboardEvent);
var
  LOnKeyDown : TOnKeyDownEvent;
  LKeyboardState: pcuint8 = nil;
begin

  case Event.keysym.sym of
    SDLK_ESCAPE: begin
      SDLApp.Terminate;
    end;

    SDLK_c: begin
      LKeyboardState := SDL_GetKeyboardState(nil);
      if GetKeyState(SDL_SCANCODE_LCTRL, LKeyboardState) then begin
        if EyeTracker <> nil then begin
          EyeTracker.StartCalibration;
        end;
      end;
    end;

    SDLK_s: begin
      LKeyboardState := SDL_GetKeyboardState(nil);
      if GetKeyState(SDL_SCANCODE_LCTRL, LKeyboardState) then begin
        if EyeTracker <> nil then begin
          EyeTracker.CalibrationSuccessful;
        end;
      end;
    end;

    SDLK_n: begin
      LKeyboardState := SDL_GetKeyboardState(nil);
      if GetKeyState(SDL_SCANCODE_LCTRL, LKeyboardState) then begin
        TTrialFactory.CurrentTrial.DoExpectedResponse;
      end;
    end;

    otherwise begin
      for LOnKeyDown in FOnKeyDownEvents do begin
        LOnKeyDown(event);
      end;
    end;
  end;
end;

constructor TSDLSystemKeyboard.Create;
begin
  FOnKeyDownEvents := TKeyDownEvents.Create;
  FOnKeyDown:=@KeyDown;
end;

destructor TSDLSystemKeyboard.Destroy;
begin
  FOnKeyDownEvents.Free;
  inherited Destroy;
end;

procedure TSDLSystemKeyboard.RegisterOnKeyDown(
  AOnKeyDownEvent: TOnKeyDownEvent);
begin
  if Assigned(AOnKeyDownEvent) then begin
    FOnKeyDownEvents.Add(AOnKeyDownEvent);
  end;
end;

end.

end.

