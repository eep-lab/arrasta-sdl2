unit sdl.app.navigator.contract;

{$mode ObjFPC}{$H+}

{$INTERFACES CORBA}

interface

uses sdl.app.selectable.contract, sdl.app.selectable.list;

type

  { INavigator }

  INavigator = interface
    ['{DEBD72A2-BC97-4D1C-80AB-A0CAB06BAEFC}']
    procedure Unselect;
    procedure SelectNext;
    procedure SelectPrevious;
    procedure ConfirmSelection;
    procedure SetBaseControl(AControl : ISelectable);
    procedure UpdateNavigationControls(AControls : TSelectables);
  end;

implementation

end.
