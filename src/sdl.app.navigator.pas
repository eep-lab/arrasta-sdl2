unit sdl.app.navigator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  sdl.app.navigator.contract,
  sdl.app.selectable.contract,
  sdl.app.selectable.list,
  sdl.app.navigator.tableiterator;

type

  { TTableNavigator }

  TTableNavigator = class(ITableNavigator)
  private
    FCurrentControl : ISelectable;
    FControls : TPossibleSelections;
    procedure SetBaseControl(ABaseControl : ISelectable);
    procedure UpdateNavigationControls(AControls: TSelectables);
    procedure SelectControl(AControl : ISelectable);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Select;
    procedure GoTop;
    procedure GoBottom;
    procedure GoLeft;
    procedure GoRight;
    procedure GoTopRight;
    procedure GoBottomLeft;
    procedure GoTopLeft;
    procedure GoBottomRight;
    procedure GoBaseControl;
    procedure ConfirmSelection;
  end;

implementation

{ TTableNavigator }

procedure TTableNavigator.UpdateNavigationControls(
  AControls: TSelectables);
begin
  FControls.Update(AControls);
  Select;
end;

procedure TTableNavigator.SelectControl(AControl: ISelectable);
begin
  if AControl <> nil then begin
    FCurrentControl := AControl;
    FCurrentControl.Select;
  end;
end;

procedure TTableNavigator.SetBaseControl(ABaseControl: ISelectable);
begin
  FControls.SetBaseControl(ABaseControl);
end;

constructor TTableNavigator.Create;
begin
  inherited Create;
  FControls := TPossibleSelections.Create;
end;

destructor TTableNavigator.Destroy;
begin
  FControls.Free;
end;

procedure TTableNavigator.Select;
begin
  SelectControl(FControls.Select);
end;

procedure TTableNavigator.GoTop;
begin
  SelectControl(FControls.GoTop);
end;

procedure TTableNavigator.GoBottom;
begin
  SelectControl(FControls.GoBottom);
end;

procedure TTableNavigator.GoLeft;
begin
  SelectControl(FControls.GoLeft);
end;

procedure TTableNavigator.GoRight;
begin
  SelectControl(FControls.GoRight);
end;

procedure TTableNavigator.GoTopRight;
begin
  SelectControl(FControls.GoTopRight);
end;

procedure TTableNavigator.GoBottomLeft;
begin
  SelectControl(FControls.GoBottomLeft);
end;

procedure TTableNavigator.GoTopLeft;
begin
  SelectControl(FControls.GoTopLeft);
end;

procedure TTableNavigator.GoBottomRight;
begin
  SelectControl(FControls.GoBottomRight);
end;

procedure TTableNavigator.GoBaseControl;
begin
  SelectControl(FControls.GoBaseControl);
end;

procedure TTableNavigator.ConfirmSelection;
begin
  if FCurrentControl <> nil then
    FCurrentControl.Confirm;
end;

end.

