unit sdl.app.controller.types;

{$mode ObjFPC}{$H+}

interface

type
  TControllerCode = (gcMouse, gcKeyboard, gcWii, gcPS4);

  TDirection = (None,
    Top,     Left,     Bottom,     Right,
    TopLeft, TopRight, BottomLeft, BottomRight);

  TPoint = record
    X : Double;
    Y : Double;
  end;

function CalculateAngleDegrees(APoint : TPoint): Double;
function GetDirection(Degree: Double): TDirection;

var
  SectorPad : Double;
  Sector0 : Double;
  Sector1 : Double;
  Sector2 : Double;
  Sector3 : Double;
  Sector4 : Double;
  Sector5 : Double;
  Sector6 : Double;
  Sector7 : Double;

implementation

uses Math;

const
  SectorSize: Double = 45;
  MaxDegress : Double = 360;
  HalfDegree : Double = 180;

function CalculateAngleDegrees(APoint: TPoint): Double;
var
  AngleRadians, AngleDegrees: Double;
begin
  AngleRadians := ArcTan2(APoint.Y, APoint.X);
  AngleDegrees := AngleRadians * (HalfDegree/PI);
  AngleDegrees := AngleDegrees + MaxDegress;
  Result := AngleDegrees - Trunc(AngleDegrees/MaxDegress) * MaxDegress;
end;

function GetDirection(Degree: Double): TDirection;
begin
  if      (Degree >= Sector0) or  (Degree < Sector1) then
    Result := TDirection.Right
  else if (Degree >= Sector1) and (Degree < Sector2) then
    Result := TDirection.BottomRight
  else if (Degree >= Sector2) and (Degree < Sector3) then
    Result := TDirection.Bottom
  else if (Degree >= Sector3) and (Degree < Sector4) then
    Result := TDirection.BottomLeft
  else if (Degree >= Sector4) and (Degree < Sector5) then
    Result := TDirection.Left
  else if (Degree >= Sector5) and (Degree < Sector6) then
    Result := TDirection.TopLeft
  else if (Degree >= Sector6) and (Degree < Sector7) then
    Result := TDirection.Top
  else if (Degree >= Sector7) and (Degree < Sector0) then
    Result := TDirection.TopRight;
end;

initialization
  SectorPad := SectorSize/2;
  Sector0:= MaxDegress-SectorPad;
  Sector1:= (Sectorsize * 1)-SectorPad;
  Sector2:= (SectorSize * 2)-SectorPad;
  Sector3:= (SectorSize * 3)-SectorPad;
  Sector4:= (SectorSize * 4)-SectorPad;
  Sector5:= (SectorSize * 5)-SectorPad;
  Sector6:= (SectorSize * 6)-SectorPad;
  Sector7:= (SectorSize * 7)-SectorPad;


end.

