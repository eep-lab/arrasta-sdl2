unit session.shuffler.types;

{$mode ObjFPC}{$H+}
{$modeswitch AdvancedRecords}

interface

uses Generics.Collections;

type

  TIntArray = array of integer;

  TPositions = array of TIntArray;

  { TItem }

  TItem = record
    ID : integer;
    ReferenceName : string;
    class operator = (A, B: TItem): Boolean;
  end;

  TReferenceList = specialize TList<TItem>;

implementation

{ TItem }

class operator TItem. = (A, B: TItem): Boolean;
begin
  Result := A.ReferenceName = B.ReferenceName;
end;

end.

