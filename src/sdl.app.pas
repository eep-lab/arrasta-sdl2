{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , SDL2
  , ctypes
  , sdl.app.events.custom
  ;

type

  { TSDLApplication }

  TSDLApplication = class
    private
      //FKeyboardState : integer;
      FCurrentMonitorIndex : cint;
      FEvents : TCustomEventHandler;
      FRunning: Boolean;
      FSDLWindow: PSDL_Window;
      FSDLRenderer: PSDL_Renderer;
      FSDLSurface : PSDL_Surface;
      FMonitors : array of TSDL_Rect;
      function GetCurrentMonitor : TSDL_Rect;
      function GetMonitor(i : cint): TSDL_Rect;
      function GetMouse: TPoint;
      procedure SetCurrentMonitor(i : cint);
      procedure SetMouse(AValue: TPoint);
      procedure LoadMonitors;
      procedure KeyDown(const event: TSDL_KeyboardEvent);
    public
      constructor Create(ATitle : PAnsiChar = 'Stimulus Control';
        AMonitor : cint = 0); reintroduce;
      destructor Destroy; override;
      procedure Run;
      procedure SetupEvents;
      {$IFNDEF NO_LCL}
      procedure SetupAudio;
      procedure SetupText;
      {$ENDIF}
      property Running : Boolean read FRunning write FRunning;
      property Window  : PSDL_Window read FSDLWindow;
      property Monitor : TSDL_Rect read GetCurrentMonitor;
      property Mouse   : TPoint read GetMouse write SetMouse;
      property Events  : TCustomEventHandler read FEvents;
  end;

var
  SDLApp : TSDLApplication;

implementation

uses sdl.app.output
  , sdl.app.video.methods
{$IFDEF NO_LCL}
  , sdl.app.renderer.nolcl
{$ELSE}
  , sdl.app.text
  , sdl.app.audio
  , sdl.app.renderer
{$ENDIF}
  ;

{ TSDLApplication }

procedure TSDLApplication.LoadMonitors;
var
  i: Integer;
begin
  SetLength(FMonitors, SDL_GetNumVideoDisplays);
  for i := Low(FMonitors) to High(FMonitors) do begin
    SDL_GetDisplayBounds(i, @FMonitors[i]);
    Print('Display'.Join(#32,
      [i, FMonitors[i].x, FMonitors[i].y, FMonitors[i].w, FMonitors[i].h]));
  end;
end;

procedure TSDLApplication.KeyDown(const event: TSDL_KeyboardEvent);
begin
  case Event.keysym.sym of
    SDLK_ESCAPE: begin
      FRunning:=False;
      Print('SDLK_ESCAPE');
    end
  end;
end;

procedure TSDLApplication.SetMouse(AValue: TPoint);
begin
  SDL_WarpMouseInWindow(Window, AValue.X, AValue.Y)
end;

function TSDLApplication.GetMouse: TPoint;
var
  MouseX, MouseY : cint;
begin
  SDL_GetMouseState(@MouseX, @MouseY);
  Result.X := MouseX;
  Result.Y := MouseY;
end;

procedure TSDLApplication.SetCurrentMonitor(i: cint);
begin
  LoadMonitors;
  if i > SDL_GetNumVideoDisplays then Exit;
  with FMonitors[i] do begin
    SDL_SetWindowPosition(FSDLWindow, x, y);
    SDL_SetWindowSize(FSDLWindow, w, h);
  end;
end;

function TSDLApplication.GetCurrentMonitor: TSDL_Rect;
begin
  Result := FMonitors[FCurrentMonitorIndex];
end;

function TSDLApplication.GetMonitor(i: cint): TSDL_Rect;
begin
  LoadMonitors;
  if i > SDL_GetNumVideoDisplays then Exit;
  Result := FMonitors[i];
end;

constructor TSDLApplication.Create(ATitle: PAnsiChar; AMonitor: cint);
var
  LMonitor : TSDL_Rect;
begin
  FEvents := TCustomEventHandler.Create;
  if SDL_Init(TSDL_Init(SDL_INIT_VIDEO or SDL_INIT_TIMER or SDL_INIT_AUDIO)) < 0 then begin
    Print(SDL_GetError);
    Exit;
  end;
  Print(Self.ClassName+'.'+{$I %CURRENTROUTINE%}+#32+ATitle);
  if AMonitor > SDL_GetNumVideoDisplays then Exit;

  {$IFNDEF NO_LCL}
  // Audio Setup
  SDLAudio := TSDLAudio.Create;

  // text/font setup
  SDLText  := TSDLText.Create;
  {$ENDIF}

  // Monitor Setup
  LoadMonitors;
  FCurrentMonitorIndex := AMonitor;
  LMonitor := FMonitors[AMonitor];
  FSDLWindow := SDL_CreateWindow(ATitle, LMonitor.x, LMonitor.y,
    LMonitor.w, LMonitor.h, 0);

  FSDLRenderer := SDL_CreateRenderer(FSDLWindow, -1, SDL_RENDERER_ACCELERATED);
  //FSDLSurface  := SDL_CreateRGBSurface(0, LMonitor.w, LMonitor.h, 32, 128, 128, 128, 255);
  AssignVariables(FSDLWindow, FSDLRenderer, FSDLSurface);
end;

destructor TSDLApplication.Destroy;
begin
  {$IFNDEF NO_LCL}
  SDLText.Free;
  SDLAudio.Free;
  {$ENDIF}
  FEvents.Free;
  inherited Destroy;
end;

procedure TSDLApplication.Run;
begin
  FRunning:=True;
  while Running do begin
    FEvents.HandlePending;
    Render;
  end;
  SDL_DestroyRenderer(FSDLRenderer);
  SDL_DestroyWindow(FSDLWindow);
  SDL_Quit;
  Print('Good Bye');
end;

procedure TSDLApplication.SetupEvents;
begin
  FEvents.OnKeyDown:=@KeyDown;
end;

{$IFNDEF NO_LCL}
procedure TSDLApplication.SetupAudio;
begin
  AllocateAudioChannels;
end;

procedure TSDLApplication.SetupText;
begin
  SDLText.SetupFonts;
end;
{$ENDIF}

end.

