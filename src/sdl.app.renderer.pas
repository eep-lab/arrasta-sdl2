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

//type
//
//  //Assigned to TTrial.Parent;
//  TSDLRenderer = class
//    class var Color : TSDL_Color;
//    class procedure Paint;
//  end;

procedure Render;

implementation

uses
  sdl.app.video.methods, sdl.app.trials.factory, sdl.colors, sdl.app.markers;

procedure Render;
begin
  SDL_SetRenderDrawColor(PSDLRenderer,
    clBackgroud.r, clBackgroud.g, clBackgroud.b, clBackgroud.a);
  SDL_RenderClear(PSDLRenderer);

  if Assigned(TTrialFactory.CurrentTrial) then begin
    TTrialFactory.CurrentTrial.AsIPaintable.Paint;
  end;

  if Assigned(Markers) then begin
    Markers.Paint;
  end;

  SDL_RenderPresent(PSDLRenderer);
  SDL_Delay(1000 div 50);
end;

end.



