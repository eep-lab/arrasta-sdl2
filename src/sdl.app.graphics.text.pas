{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.graphics.text;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , sdl2
  , sdl2_ttf
  , sdl.app.graphics.rectangule
  ;

type

  { TText }

  TText = class(TRectangule)
    private
      FFont : PTTF_Font;
      FSDLTexture : PSDL_Texture;
      FVisible: Boolean;
    protected
      procedure Paint; override;
    public
      constructor Create(AOwner: TComponent); override;
      procedure Load(S : string;
        FontName : string = 'Picanco_et_al');
      procedure LoadFromFile(AFilename: string;
        FontName : string = 'Raleway-Regular');
      destructor Destroy; override;
      procedure Show;
      procedure Hide;
      property Visible : Boolean read FVisible write FVisible;
  end;

implementation

uses sdl.app.video.methods, sdl.app.text, sdl.colors, session.pool;

{ TText }

procedure TText.Paint;
begin
  if FVisible then begin
    SDL_RenderCopy(PSDLRenderer, FSDLTexture, nil, @FRect);
  end;
end;

constructor TText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFont := nil;
  FSDLTexture := nil;
  FVisible := False;
end;

procedure TText.Load(S: string; FontName: string);
//const
//  WrapOnNewLine : cuint32 = 0;
var
  PSDLSurface : PSDL_Surface;
  PText : PAnsiChar;
begin
  FFont := SDLText.Get(FontName).Font;
  PText := PAnsiChar(S);
  PSDLSurface := TTF_RenderUTF8_LCD(
    FFont, PText, clBlack, clWhite);
  FRect := PSDLSurface^.clip_rect;
  FSDLTexture := SDL_CreateTextureFromSurface(PSDLRenderer, PSDLSurface);
  SDL_FreeSurface(PSDLSurface);
end;

procedure TText.LoadFromFile(AFilename: string; FontName: string);
var
  LFile : TStringList;
  LText : string;
  LFontName : string;
begin
  LFile := TStringList.Create;
  try
    LFile.LoadFromFile(Pool.RootMedia+AFilename+'.txt');
    LText := LFile[0];
  finally
    LFile.Free;
  end;
  Load(LText, LFontName);
end;

destructor TText.Destroy;
begin
  SDL_DestroyTexture(FSDLTexture);
  inherited Destroy;
end;

procedure TText.Show;
begin
  FVisible := True;
end;

procedure TText.Hide;
begin
  FVisible := False;
end;

end.

