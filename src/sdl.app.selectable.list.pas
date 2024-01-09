unit sdl.app.selectable.list;

{$mode ObjFPC}{$H+}

interface

uses Generics.Collections, Generics.Defaults, sdl.app.selectable.contract;

type

  { TSelectableByOriginComparer }

  TSelectableByOriginComparer = class (specialize TComparer<ISelectable>)
    function Compare(const AItem1, AItem2: ISelectable): Integer; override;
  end;

  { TSelectables }

  TSelectables = class(specialize TList<ISelectable>)
    class function ByOrigin : specialize IComparer<ISelectable>;
  end;

implementation

uses SDL2, Math;

{ TSelectableComparer }

function TSelectableByOriginComparer.Compare(
  const AItem1, AItem2: ISelectable): Integer;
var
  A : TSDL_Point;
  B : TSDL_Point;
begin
  A := AItem1.Origen;
  B := AItem2.Origen;

  if A.x = B.x then begin
    Result := CompareValue(A.y, B.y);
  end else begin
    Result := CompareValue(A.x, B.x);
  end;
end;

class function TSelectables.ByOrigin : specialize IComparer<ISelectable>;
begin
  // TSelectableByOriginComparer is not reference counted
  Result := TSelectableByOriginComparer.Default;
end;

end.

