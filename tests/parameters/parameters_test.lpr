program parameters_test;

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

  TCustomObjectClass = class of TCustomObject;

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

var
  CustomObjectClass : TCustomObjectClass;
  CustomObject : TCustomObject;
  Parameter : TParameter;

begin
  CustomObjectClass := TCustomObject;
  CustomObject := CustomObjectClass.Create;
  for Parameter in CustomObject.Parameters do
      WriteLn(Parameter.ToString);

  try
    CustomObject['Count'] := '40';

    for Parameter in CustomObject.Parameters do
      WriteLn(Parameter.ToString);

  finally
    CustomObject.Free;
  end;
  ReadLn;
end.

