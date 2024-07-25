unit eye;

interface

function VisualAngle(Value, Distance: Double): Double;

implementation

{ Monitor size }

const
  Width = 41.476 { cm };
  Height = 25.922 { cm };

uses
  Math;

function RadToDeg(Rad: Double): Double;
begin
  Result := Rad * (180 / PI);
end;

function VisualAngle(Value, Distance: Double): Double;
begin
  Result := 2 * RadToDeg(ArcTan(Value / (2 * Distance)));
end;

end.
