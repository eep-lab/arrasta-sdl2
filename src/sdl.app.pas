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
  , sdl.app.video.types
  {$IFDEF NO_LCL}
  , sdl.app.events.nolcl
  {$ELSE}
  , sdl.app.events.custom
  {$ENDIF}
  ;

type

  { TSDLApplication }

  TSDLApplication = class
    private
      //FKeyboardState : integer;
      FTitle : PAnsichar;
      FCurrentMonitorIndex : cint;
      FOnClose: TNotifyEvent;
      FRunning: Boolean;
      FSDLWindow: PSDL_Window;
      FSDLRenderer: PSDL_Renderer;
      FSDLSurface : PSDL_Surface;
      FMonitors : TMonitors;
      function GetCurrentMonitor : TSDL_Rect;
      function GetEvents: TCustomEventHandler;
      function GetMonitor(i : cint): TSDL_Rect;
      function GetMouse: TPoint;
      function GetShowMarkers: Boolean;
      procedure SetCurrentMonitor(i : cint);
      procedure SetMouse(AValue: TPoint);
      procedure KeyDown(const event: TSDL_KeyboardEvent);
      procedure SetOnClose(AValue: TNotifyEvent);
      procedure SetShowMarkers(AValue: Boolean);
    public
      class procedure LoadMonitors(var AMonitors: TMonitors);
      class procedure GetAvailableMonitors(AStrings: TStrings);
      constructor Create(ATitle : PAnsiChar = 'Stimulus Control'); reintroduce;
      destructor Destroy; override;
      procedure Run;
      procedure SetupVideo(AMonitor : cint = 0);
      procedure SetupEvents;
      {$IFNDEF NO_LCL}
      procedure SetupAudio;
      procedure SetupText;
      {$ENDIF}
      property Running : Boolean read FRunning write FRunning;
      property Window  : PSDL_Window read FSDLWindow;
      property Monitor : TSDL_Rect read GetCurrentMonitor;
      property Mouse   : TPoint read GetMouse write SetMouse;
      property Events  : TCustomEventHandler read GetEvents;
      property OnClose : TNotifyEvent read FOnClose write SetOnClose;
      property ShowMarkers : Boolean read GetShowMarkers write SetShowMarkers;
  end;

var
  SDLApp : TSDLApplication;

implementation

uses sdl2_image
  , sdl.app.output
  , sdl.app.video.methods
  , sdl.app.markers
{$IFDEF NO_LCL}
  , sdl.app.renderer.nolcl
{$ELSE}
  , sdl.app.text
  , sdl.app.audio
  , sdl.app.renderer
{$ENDIF}
  ;

{ TSDLApplication }

class procedure TSDLApplication.LoadMonitors(var AMonitors: TMonitors);
var
  i: Integer;
begin
  SetLength(AMonitors, SDL_GetNumVideoDisplays);
  for i := Low(AMonitors) to High(AMonitors) do begin
    SDL_GetDisplayBounds(i, @AMonitors[i]);
  end;
end;

procedure TSDLApplication.KeyDown(const event: TSDL_KeyboardEvent);
begin
  case Event.keysym.sym of
    SDLK_ESCAPE: begin
      FRunning:=False;
      Print('SDLK_ESCAPE');
    end;
  end;
end;

procedure TSDLApplication.SetOnClose(AValue: TNotifyEvent);
begin
  if FOnClose=AValue then Exit;
  FOnClose:=AValue;
end;

procedure TSDLApplication.SetShowMarkers(AValue: Boolean);
begin
  if ShowMarkers = AValue then Exit;
  if AValue then begin
    Markers := TMarkers.Create;
    Markers.LoadFromFile;
  end else begin
    FreeAndNil(Markers);
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

function TSDLApplication.GetShowMarkers: Boolean;
begin
  Result := Assigned(Markers);
end;

procedure TSDLApplication.SetCurrentMonitor(i: cint);
begin
  LoadMonitors(FMonitors);
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

function TSDLApplication.GetEvents: TCustomEventHandler;
begin
  Result := SDLEvents;
end;

function TSDLApplication.GetMonitor(i: cint): TSDL_Rect;
begin
  LoadMonitors(FMonitors);
  if i > SDL_GetNumVideoDisplays then Exit;
  Result := FMonitors[i];
end;

constructor TSDLApplication.Create(ATitle: PAnsiChar);
var
  LError : string;
begin
  Print(Self.ClassName+'.'+{$I %CURRENTROUTINE%}+#32+ATitle);
  FTitle := ATitle;
  SDLEvents := TCustomEventHandler.Create;

  // errors in the video subsystem may be related to
  // SDL_VIDEODRIVER system environment variable (particularly on windows)
  // https://wiki.libsdl.org/SDL2/FAQUsingSDL
  if SDL_InitSubSystem(SDL_INIT_VIDEO) < 0 then begin
    LError := SDL_GetError;
    Print(LError);
    raise Exception.Create(LError);
  end;
  LoadMonitors(FMonitors);

  if SDL_InitSubSystem(SDL_INIT_TIMER) < 0 then begin
    LError := SDL_GetError;
    Print(LError);
    raise Exception.Create(LError);
  end;

  IMG_Init(IMG_INIT_PNG);

  {$IFNDEF NO_LCL}
  if SDL_InitSubSystem(SDL_INIT_AUDIO) < 0 then begin
    LError := SDL_GetError;
    Print(LError);
    raise Exception.Create(LError);
  end;
  SDLAudio := TSDLAudio.Create;

  // text/font setup
  SDLText  := TSDLText.Create;
  {$ENDIF}
end;

destructor TSDLApplication.Destroy;
begin
  if Assigned(Markers) then begin
    Markers.Free;
  end;
  inherited Destroy;
end;

procedure TSDLApplication.Run;
begin
  FRunning:=True;
  try
    while FRunning do begin
      SDLEvents.HandlePending;
      Render;
    end;
  finally
    {$IFNDEF NO_LCL}
    if Assigned(SDLText) then
      SDLText.Free;

    if Assigned(SDLAudio) then
      SDLAudio.Free;
    {$ENDIF}
    if Assigned(SDLEvents) then
      SDLEvents.Free;

    SDL_DestroyRenderer(FSDLRenderer);
    SDL_DestroyWindow(FSDLWindow);
    SDL_Quit;
  end;

  Print('Good Bye');
  if Assigned(OnClose) then
    OnClose(Self);
end;

class procedure TSDLApplication.GetAvailableMonitors(AStrings: TStrings);
var
  i: Integer;
  LMonitors : array of TSDL_Rect;
  LError: PAnsiChar;
begin
  if SDL_InitSubSystem(SDL_INIT_VIDEO) < 0 then begin
    LError := SDL_GetError;
    Print(LError);
    raise Exception.Create(LError);
  end;
  LoadMonitors(LMonitors);

  AStrings.Clear;
  for i := Low(LMonitors) to High(LMonitors) do begin
    AStrings.Append('Monitor ' + String.Join(#32,
      [i, '-', LMonitors[i].x, LMonitors[i].y, LMonitors[i].w, LMonitors[i].h]));
  end;
  SDL_Quit;
end;

procedure TSDLApplication.SetupVideo(AMonitor: cint);
var
  LMonitor : TSDL_Rect;
begin
  if AMonitor > SDL_GetNumVideoDisplays then Exit;

  FCurrentMonitorIndex := AMonitor;

  LMonitor := FMonitors[AMonitor];
  FSDLWindow := SDL_CreateWindow(FTitle, LMonitor.x, LMonitor.y,
    LMonitor.w, LMonitor.h, 0);

  FSDLRenderer := SDL_CreateRenderer(FSDLWindow, -1, SDL_RENDERER_ACCELERATED);
  //FSDLSurface  := SDL_CreateRGBSurface(0, LMonitor.w, LMonitor.h, 32, 128, 128, 128, 255);
  AssignVariables(FSDLWindow, FSDLRenderer, FSDLSurface);

  {$IFDEF NO_LCL}
  sdl.app.renderer.nolcl.Monitor := Self.Monitor;
  {$ELSE}

  {$ENDIF}
end;

procedure TSDLApplication.SetupEvents;
begin
  SDLEvents.OnKeyDown:=@KeyDown;
end;

{$IFNDEF NO_LCL}
procedure TSDLApplication.SetupAudio;
begin
  AllocateDefaultAudioChannels;
end;

procedure TSDLApplication.SetupText;
begin
  SDLText.SetupFonts;
end;
{$ENDIF}

end.

