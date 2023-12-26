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
      FNavigator : TNavigator;
      function Navigator : INavigator;
    public
      constructor Create; virtual;
      destructor Destroy; override;
  end;

implementation

{ TController }

function TController.Navigator: INavigator;
begin
  Result := FNavigator as INavigator;
end;

constructor TController.Create;
begin
  FNavigator := TNavigator.Create;
end;

destructor TController.Destroy;
begin
  FNavigator.Free;
end;

end.

