unit sdl.colors;

{$mode ObjFPC}{$H+}

interface

uses SDL2;

const
  clWhite : TSDL_Color = (r: 255; g: 255; b: 255; a: SDL_ALPHA_OPAQUE);
  clBlack : TSDL_Color = (r: 0;   g: 0;   b: 0;   a: SDL_ALPHA_OPAQUE);
  clRed   : TSDL_Color = (r: 255; g: 0;   b: 0;   a: SDL_ALPHA_OPAQUE);
  clGray  : TSDL_Color = (r: 128; g: 128; b: 128; a: SDL_ALPHA_OPAQUE);

implementation

end.

