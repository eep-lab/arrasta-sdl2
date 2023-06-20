{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.renderer;

{$mode ObjFPC}{$H+}

interface

uses sdl2;

procedure Render;

implementation

uses
  sdl.app.video.methods, sdl.app.trials.factory;

procedure Render;
begin
  SDL_SetRenderDrawColor(PSDLRenderer, 0, 0, 0, SDL_ALPHA_OPAQUE);
  SDL_RenderClear(PSDLRenderer);

  TTrialFactory.Paint;

  SDL_RenderPresent(PSDLRenderer);
  SDL_Delay(1000 div 50);
end;

end.



