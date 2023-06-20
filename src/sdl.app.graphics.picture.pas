{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.graphics.picture;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , SDL2
  , sdl.app.choiceable.rectangule
  , sdl.app.paintable.contract
  , sdl.app.events.abstract
  ;

type

  { TPicture }

  TPicture = class(TChoiceableRect, IPaintable)
  private
    FTexture  : PSDL_Texture;
  protected
    procedure MouseMove(Sender: TObject; Shift: TCustomShiftState; X, Y: Integer); override;
    procedure MouseDown(Sender: TObject; Shift: TCustomShiftState; X, Y: Integer); override;
    procedure MouseEnter(Sender: TObject); override;
    procedure MouseExit(Sender: TObject); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename: string);
  end;


implementation

uses
  sdl2_image
  , sdl.app.video.methods
  , sdl.app.output
  , session.pool
  ;

{ TPicture }

constructor TPicture.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  IMG_Init(IMG_INIT_PNG);
end;

destructor TPicture.Destroy;
begin
  SDL_DestroyTexture(FTexture);
  inherited Destroy;
end;

procedure TPicture.MouseMove(Sender: TObject; Shift: TCustomShiftState; X,
  Y: Integer);
begin
  inherited MouseMove(Self, Shift, X, Y);
end;

procedure TPicture.MouseDown(Sender: TObject; Shift: TCustomShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Self, Shift, X, Y);
end;

procedure TPicture.MouseEnter(Sender: TObject);
begin
  inherited MouseEnter(Self);
end;

procedure TPicture.MouseExit(Sender: TObject);
begin
  inherited MouseExit(Self);
end;

procedure TPicture.Paint;
begin
  if Visible then begin
    SDL_RenderCopy(PSDLRenderer, FTexture, nil, @FRect);
  end else begin
    inherited Paint;
  end;
end;

procedure TPicture.LoadFromFile(AFilename: string);
var
  Media : PAnsiChar;
begin
  Media := PAnsiChar(Pool.RootMedia+AFilename);
  FTexture := IMG_LoadTexture(PSDLRenderer, Media);
end;

end.

