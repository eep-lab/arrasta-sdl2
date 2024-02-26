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

uses session.parameters.global, sdl.app.mouse;

{ TSDLMouseController }

procedure TSDLMouseController.Hide;
begin
  Mouse.Hide;
end;

procedure TSDLMouseController.Show;
begin
  if GlobalTrialParameters.HideMouse then begin
    { do not show }
  end else begin
    Mouse.Show;
  end;
end;

end.

