unit sdl.app.navigator;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  sdl.app.navigator.contract,
  sdl.app.selectable.contract,
  sdl.app.selectable.list,
  sdl.app.navigator.iterator;

type

  { TNavigator }

  TNavigator = class(INavigator)
  private
    FControls : TPossibleSelections;
    FCurrentControl : ISelectable;
    procedure SetBaseControl(ABaseControl : ISelectable);
    procedure UpdateNavigationControls(AControls: TSelectables);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Unselect;
    procedure Select(AIndex : integer);
    procedure SelectNext;
    procedure SelectPrevious;
    procedure ConfirmSelection;
  end;

implementation

{ TNavigator }

procedure TNavigator.UpdateNavigationControls(
  AControls: TSelectables);
begin
  FControls.Update(AControls);
  FCurrentControl := FControls.Next;
end;

procedure TNavigator.SetBaseControl(ABaseControl: ISelectable);
begin
  FControls.SetBaseControl(ABaseControl);
end;

constructor TNavigator.Create;
begin
  inherited Create;
  FControls := TPossibleSelections.Create;
end;

destructor TNavigator.Destroy;
begin
  FControls.Free;
end;

procedure TNavigator.Unselect;
begin
  FCurrentControl.Unselect;
end;

procedure TNavigator.SelectNext;
begin
  FCurrentControl := FControls.Next;
  FCurrentControl.Select;
end;

procedure TNavigator.SelectPrevious;
begin
  FCurrentControl := FControls.Previous;
  FCurrentControl.Select;
end;

procedure TNavigator.ConfirmSelection;
begin
  FCurrentControl.Confirm;
end;

procedure TNavigator.Select(AIndex: integer);
begin
  raise ENotImplemented.Create('TNavigator.Select');
end;

end.

