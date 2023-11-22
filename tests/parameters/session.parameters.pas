unit session.parameters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Variants, fgl
  , session.parameters.types;

type

  { TParametricObject }

  TParametricObject = class
  private
    FParameters: TParameters;
    function GetValue(const AKey: string): string;
    procedure SetValue(const AKey: string; AValue: string);
  protected
    procedure RegisterParameter(const AKey: string;
      AClassField: Pointer; ADefaultValue: Variant);
  public
    constructor Create;
    destructor Destroy; override;
    property Values[const AKey: string]: string
      read GetValue write SetValue; default;
    property Parameters : TParameters read FParameters;
  end;

implementation

{ TParametricObject }

constructor TParametricObject.Create;
begin
  FParameters := TParameters.Create;
  FParameters.Sorted := True;
  FParameters.Duplicates := dupIgnore;
end;

destructor TParametricObject.Destroy;
begin
  FParameters.Free;
  inherited;
end;

procedure TParametricObject.RegisterParameter(const AKey: string; AClassField: Pointer; ADefaultValue: Variant);
var
  Parameter: TParameterData;
begin
  Parameter.Data := ADefaultValue;
  Parameter.ClassField := AClassField;
  FParameters.Add(AKey, Parameter);
end;

function TParametricObject.GetValue(const AKey: string): string;
begin
  Result := FParameters[AKey].DataToString;
end;

procedure TParametricObject.SetValue(const AKey: string; AValue: string);
var
  LParameter : TParameterData;
begin
  LParameter := FParameters[AKey];
  case VarType(LParameter.Data) of
    varInteger: begin
      LParameter.Data :=
        StrToIntDef(AValue, Integer(FParameters[AKey].Data));
      Integer(FParameters[AKey].ClassField^) := LParameter.Data;
    end;

    varBoolean: begin
      LParameter.Data :=
        StrToBoolDef(AValue, Boolean(FParameters[AKey].Data));
      Boolean(FParameters[AKey].ClassField^) := LParameter.Data;
    end;

    varString: begin
      LParameter.Data := AValue;
      String(FParameters[AKey].ClassField^) := LParameter.Data;
    end

    else
      raise EConvertError.Create(ClassName+'.SetValue:'+AKey);
  end;
  FParameters[AKey] := LParameter;
end;

end.
