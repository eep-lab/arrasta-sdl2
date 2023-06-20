{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.video.methods;

{$mode ObjFPC}{$H+}

interface

uses SDL2;

procedure AssignVariables(APSDLWindow : PSDL_Window;
  APSDLRenderer : PSDL_Renderer; APSDLSurface : PSDL_Surface);
function MonitorFromWindow : TSDL_Rect;
procedure Invalidate;

var
  PSDLRenderer : PSDL_Renderer;
  PSDLSurface  : PSDL_Surface;

implementation

var
  PSDLWindow   : PSDL_Window;

procedure AssignVariables(APSDLWindow: PSDL_Window;
  APSDLRenderer: PSDL_Renderer; APSDLSurface: PSDL_Surface);
begin
  PSDLWindow := APSDLWindow;
  PSDLRenderer := APSDLRenderer;
  PSDLSurface  := APSDLSurface;
end;

function MonitorFromWindow: TSDL_Rect;
begin
  SDL_GetDisplayBounds(SDL_GetWindowDisplayIndex(PSDLWindow), @Result);
end;

procedure Invalidate;
begin
  SDL_UpdateWindowSurface(PSDLWindow);
end;


end.

