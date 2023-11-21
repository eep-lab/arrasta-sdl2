unit customparameters;

{$mode objfpc}{$H+}

interface

uses session.parameters, session.parameters.types;

type

  { TCustomObject }

  TCustomObject = class(TParametricObject)
    private
      FCount : integer;
      FHasConsequence : Boolean;
      FLabel : string;
    public
      constructor Create;
      destructor Destroy; override;
  end;

implementation

{ TCustomObject }

constructor TCustomObject.Create;
begin
  inherited Create;
  FCount := 0;
  FHasConsequence := True;
  FLabel := 'CustomObject 1';

  RegisterParameter('Count', @FCount, FCount);
  RegisterParameter('HasConsequence', @FHasConsequence, FHasConsequence);
  RegisterParameter('Label', @FLabel, FLabel);
end;

destructor TCustomObject.Destroy;
begin
  inherited Destroy;
end;

end.

