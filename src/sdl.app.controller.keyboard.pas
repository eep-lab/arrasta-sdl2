unit sdl.app.controller.keyboard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SDL2,
  sdl.app.events.abstract,
  sdl.app.navigator.contract,
  sdl.app.controller;

type

  { TSDLKeyboard }

  TSDLKeyboard = class(TController)
  private
    procedure KeyDown(const event: TSDL_KeyboardEvent);
  public
    constructor Create; override;
  end;

implementation

uses sdl.app.events.custom;

{ TSDLKeyboard }

procedure TSDLKeyboard.KeyDown(const event: TSDL_KeyboardEvent);
begin
  if FNavigator = nil then Exit;

  case Event.keysym.sym of
    SDLK_UP, SDLK_RIGHT : begin
      FNavigator.SelectNext;
    end;

    SDLK_DOWN, SDLK_LEFT: begin
      FNavigator.SelectPrevious;
    end;

    SDLK_RETURN, SDLK_RETURN2: begin
      FNavigator.ConfirmSelection;
    end;

    SDLK_BACKSPACE: begin
      FNavigator.Unselect;
    end;
  end;
end;

constructor TSDLKeyboard.Create;
begin
  inherited Create;
  SDLEvents.Keyboard.RegisterOnKeyDown(@KeyDown);
end;

end.

