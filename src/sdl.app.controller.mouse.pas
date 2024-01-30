unit sdl.app.controller.mouse;

{$mode ObjFPC}{$H+}

interface

uses sdl.app.controller;

type

  { TSDLMouseController }

  TSDLMouseController = class(TController)
    public
      procedure Hide; override;
      procedure Show; override;
  end;

implementation

uses sdl.app.mouse;

{ TSDLMouseController }

procedure TSDLMouseController.Hide;
begin
  Mouse.Hide;
end;

procedure TSDLMouseController.Show;
begin
  Mouse.Show;
end;

end.

