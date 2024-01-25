unit integers.list;

{$mode ObjFPC}{$H+}

interface

uses Generics.Collections, Generics.Defaults;

type

  IValueComparer = specialize IComparer<UInt32>;

  { TValueComparer }

  TValueComparer = class (specialize TComparer<UInt32>)
    function Compare(const AItem1, AItem2: UInt32): Integer; override;
  end;

  { TIntegers }

  TIntegers = class(specialize TList<UInt32>)
    class function ByValue : IValueComparer;
  end;

implementation

uses Math;

{ TSelectableComparer }

function TValueComparer.Compare(
  const AItem1, AItem2: UInt32): Integer;
begin
  Result := CompareValue(AItem1, AItem2);
end;

class function TIntegers.ByValue : IValueComparer;
begin
  Result := TValueComparer.Default;
end;

end.

