{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.renderer.nolcl;

{$mode ObjFPC}{$H+}


interface

uses sdl2;

procedure Render;

var
  Monitor : TSDL_Rect;

implementation

uses
  sdl.app.video.methods, sdl.colors;

var
  LRect : TSDL_Rect = (x: 0; y: 0; w: 200; h: 200);

procedure Render;
begin
  with clBlack do SDL_SetRenderDrawColor(PSDLRenderer, r, g, b, a);
  SDL_RenderClear(PSDLRenderer);

  LRect.x := Random(Monitor.w-LRect.w);
  LRect.y := Random(Monitor.h-LRect.h);

  with clGray do SDL_SetRenderDrawColor(PSDLRenderer, r, g, b, a);
  SDL_RenderFillRect(PSDLRenderer, @LRect);

  SDL_RenderPresent(PSDLRenderer);
  SDL_Delay(1000);
end;

end.



