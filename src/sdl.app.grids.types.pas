unit sdl.app.grids.types;

{$mode ObjFPC}{$H+}

interface

uses SDL2, Generics.Collections, Math.LatinSquares;

type
  TGridStyle =
     (gtCircle, gtSquare, gtDistributed);

   TGridOrientation = (goNone,
     goLeftToRight, goRightToLeft, goTopToBottom, goBottomToTop,
     goCustom);

   TCell = array [0..1] of Integer;

   TGridList = specialize TList<Integer>;
   TGridItem = record
     Index : integer;
     Position : integer;
     Rect : TSDL_Rect;
     Item : TObject;
   end;

   TGridItems = array of TGridItem;

   TMatrix = array of array of TGridItem;

   { TRandomPositions }

   TRandomPositions = record
     Samples: TGridItems;
     SamplesRows : TLatinSquare;
     Comparisons : TGridItems;
     ComparisonsRows : TLatinSquare;
   end;

   TBorder = record
     Top : TSDL_Rect;
     Bottom : TSDL_Rect;
     Left : TSDL_Rect;
     Right: TSDL_Rect;
   end;

implementation

end.
