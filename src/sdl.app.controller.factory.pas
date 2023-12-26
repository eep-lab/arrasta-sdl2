{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.controller.factory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  sdl.app.controller.types,
  sdl.app.controller;

type

  { TControllerFactory }

  TControllerFactory = class
    class function New(ACode : TControllerCode) : TController;
  end;

implementation

uses
  sdl.app.controller.mouse,
  sdl.app.controller.keyboard,
  sdl.app.controller.wii;

{ TControllerFactory }

class function TControllerFactory.New(ACode: TControllerCode): TController;
begin
  case ACode of
    gcMouse: begin
      Result := TSDLMouseController.Create;
    end;

    gcKeyboard: begin
      Result := TSDLKeyboard.Create;
    end;

    gcWii: begin
      Result := TSDLWiiRemote.Create;
    end;
  end;
end;

end.


