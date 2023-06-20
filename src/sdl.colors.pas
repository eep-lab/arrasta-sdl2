unit sdl.colors;

{$mode ObjFPC}{$H+}

interface

uses SDL2;

const
  clWhite : TSDL_Color = (r:255;g:255;b:255;a:255);
  clBlack : TSDL_Color = (r:0;g:0;b:0;a:255);
  clRed   : TSDL_Color = (r:255;g:0;b:0;a:255);

  clInactiveCaption : TSDL_Color = (r:128;g:128;b:128;a:255);

implementation

end.

