unit timestamps.types;

{$mode ObjFPC}{$H+}

interface

type
  {$IFDEF CPU86}{$IFDEF CPU32}
    TLargerFloat = Extended;
  {$ENDIF}{$ENDIF}

  {$IFDEF CPUX86_64}
    TLargerFloat = Double;
  {$ENDIF}

implementation

end.

