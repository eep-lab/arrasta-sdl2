unit session.parameters.types;

{$mode ObjFPC}{$H+}

{$INTERFACES CORBA}
{$MODESWITCH ADVANCEDRECORDS}
{$MODESWITCH TYPEHELPERS}

interface

uses
  Classes, SysUtils, Variants, fgl;

type

  TParameterData = record
    Data: Variant;
    ClassField: Pointer;
  end;

  { TVariantHelper }

  { TParameterDataHelper }

  TParameterDataHelper = type helper for TParameterData
    function ToString : string;
    function DataToString : string;
  end;

  TParameter = record
    Key : string;
    Value : TParameterData;
  end;

  { TParameterHelper }

  TParameterHelper = type helper for TParameter
    function ToString : string;
  end;


  TParametersBase = specialize TFPGMap<string, TParameterData>;

  IParameterEnumerator = interface(IInterface)
    ['{7D96F7BE-37EE-4EF2-A9BB-5C68597C7D19}']
    function GetCurrent: TParameter;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: TParameter read GetCurrent;
  end;

  IEnumerableParameters = interface
    ['{A6F4C5F2-6076-464B-BEB0-47C9939E71BF}']
    function GetEnumerator: IParameterEnumerator;
  end;

  { TParametersEnumerator }

  TParametersEnumerator = class(TInterfacedObject, IParameterEnumerator)
  private
    FParameters: TParametersBase;
    FIndex: Integer;
  public
    constructor Create(AParameters: TParametersBase);
    function GetCurrent: TParameter;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: TParameter read GetCurrent;
  end;

  TParameters = class(TParametersBase, IEnumerableParameters)
    function GetEnumerator : IParameterEnumerator;
  end;

implementation

{ TParameterDataHelper }

function TParameterDataHelper.ToString: string;
var
  LField : string;
begin
  case VarType(Self.Data) of
    varInteger: begin
      LField := IntToStr(Integer(Self.ClassField^));
    end;

    varBoolean: begin
      LField := BoolToStr(Boolean(Self.ClassField^), True);
    end;

    varString: begin
      LField := String(Self.ClassField^);
    end

    else
      raise EConvertError.Create('TVariantHelper.ToString');
  end;

  Result := string.Join('=', [VarToStr(Self.Data), LField]);
end;

function TParameterDataHelper.DataToString: string;
begin
  Result := VarToStr(Self.Data);
end;

{ TParameterHelper }

function TParameterHelper.ToString: string;
begin
  Result := string.Join('=', [Self.Key, Self.Value.ToString]);
end;

{ TParametersEnumerator }

constructor TParametersEnumerator.Create(AParameters: TParametersBase);
begin
  FParameters := AParameters;
  FIndex := -1;
end;

function TParametersEnumerator.GetCurrent: TParameter;
begin
  Result.Key   := FParameters.Keys[FIndex];
  Result.Value := FParameters.Data[FIndex];
end;

function TParametersEnumerator.MoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < FParameters.Count;
end;

procedure TParametersEnumerator.Reset;
begin
  FIndex := -1;
end;

function TParameters.GetEnumerator: IParameterEnumerator;
begin
  // TParametersEnumerator is reference counted
  Result := TParametersEnumerator.Create(Self) as IParameterEnumerator;
end;

end.

