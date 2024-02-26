unit sdl.app.controller;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils,
  sdl.app.controller.contract,
  sdl.app.navigator.contract,
  sdl.app.navigator;

type

  { TController }

  TController = class(IController)
    protected
      FNavigator : TTableNavigator;
      function Navigator : ITableNavigator;
    public
      constructor Create; virtual;
      destructor Destroy; override;
      procedure Hide; virtual;
      procedure Show; virtual;
  end;

implementation

uses sdl.app.mouse;

{ TController }

function TController.Navigator: ITableNavigator;
begin
  Result := FNavigator as ITableNavigator;
end;

constructor TController.Create;
begin
  FNavigator := TTableNavigator.Create;
  Mouse.Hide;
end;

destructor TController.Destroy;
begin
  FNavigator.Free;
end;

procedure TController.Hide;
begin
  { do nothing }
end;

procedure TController.Show;
begin
  { do nothing }
end;

end.

