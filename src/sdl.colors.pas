unit sdl.colors;

{$mode ObjFPC}{$H+}

interface

uses SDL2;

const
  clWhite : TSDL_Color =
    (r: 255; g: 255; b: 255; a: SDL_ALPHA_OPAQUE);

  clBlack : TSDL_Color =
    (r: 0;   g: 0;   b: 0;   a: SDL_ALPHA_OPAQUE);

  clTransparent : TSDL_Color =
    (r: 0;   g: 0;   b: 0;   a: SDL_ALPHA_TRANSPARENT);

  clRed   : TSDL_Color =
    (r: 255; g: 0;   b: 0;   a: SDL_ALPHA_OPAQUE);

  clGray  : TSDL_Color =
    (r: 128; g: 128; b: 128; a: SDL_ALPHA_OPAQUE);

  clLightBlueShaded1 : TSDL_Color =
    (r: 134; g: 150; b: 254; a: 50);

  clLightBlueShaded2 : TSDL_Color =
    (r: 134; g: 150; b: 254; a: 150);

  clLightRedShaded1 : TSDL_Color =
    (r: 255; g: 57; b: 71; a: 25);

  clLightGrayShaded1 : TSDL_Color =
    (r: 128; g: 128; b: 128; a: 100);

var
  clBackgroud : TSDL_Color;

implementation

initialization
 clBackgroud := clWhite;

end.

