unit sdl.app.graphics.debug;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

  procedure PaintDebugGraphics;
  procedure DrawDebugCircle(ADegree : Double);

implementation

uses sdl2, sdl2_gfx, sdl.app.video.methods, sdl.app.graphics.text, sdl.colors;

type
  TPoint = record
    X : SmallInt;
    Y : SmallInt;
  end;

  Direction = (
    Top,     Left,     Bottom,     Right,
    TopLeft, TopRight, BottomLeft, BottomRight);

const
  SectorSize: SmallInt = 45;
  MaxDegress : SmallInt = 360;

var
  SectorPad : Double;
  Degress : array [0..7] of Double;

const
  CenterX = 150;
  CenterY = 150;
  Radius = 100;

var
  Text : TText = nil;
  Aim : TPoint;
  AimDegree : Double;


  function GetDirection(Degree: Double): Direction;
  begin
    if      (Degree >= Degress[0]) or (Degree < Degress[1]) then
      Result := Direction.Right
    else if (Degree >= Degress[1]) and (Degree < Degress[2]) then
      Result := Direction.BottomRight
    else if (Degree >= Degress[2]) and (Degree < Degress[3]) then
      Result := Direction.Bottom
    else if (Degree >= Degress[3]) and (Degree < Degress[4]) then
      Result := Direction.BottomLeft
    else if (Degree >= Degress[4]) and (Degree < Degress[5]) then
      Result := Direction.Left
    else if (Degree >= Degress[5]) and (Degree < Degress[6]) then
      Result := Direction.TopLeft
    else if (Degree >= Degress[6]) and (Degree < Degress[7]) then
      Result := Direction.Top
    else if (Degree >= Degress[7]) and (Degree < Degress[0]) then
      Result := Direction.TopRight;
  end;


function CircumferencePoint(ADegree : Double) : TPoint;
var
  LAngleRadians: Double = 0;
begin
  LAngleRadians := ADegree * (Pi / 180);

  // Calculate coordinates on the circumference
  Result.X := Round(CenterX + Radius * Cos(LAngleRadians));
  Result.Y := Round(CenterY + Radius * Sin(LAngleRadians));
end;

procedure PaintDebugGraphics;
  procedure DrawLines(Color: TSDL_Color; ADegress : array of Double);
  var
    Degree : Double;
    Point : TPoint;
  begin
    for Degree in ADegress do begin
      Point := CircumferencePoint(Degree);
      with Color do begin
        aalineRGBA(PSDLRenderer,
              CenterX, CenterY, Point.X, Point.Y, r, g, b, a);
      end;
    end;
  end;

begin
  DrawLines(clLightBlueShaded1, Degress);

  case GetDirection(AimDegree) of
    Right:
      DrawLines(clBlack, [Degress[0], Degress[1]]);
    BottomRight:
      DrawLines(clBlack, [Degress[1], Degress[2]]);
    Bottom:
      DrawLines(clBlack, [Degress[2], Degress[3]]);
    BottomLeft:
      DrawLines(clBlack, [Degress[3], Degress[4]]);
    Left:
      DrawLines(clBlack, [Degress[4], Degress[5]]);
    TopLeft:
      DrawLines(clBlack, [Degress[5], Degress[6]]);
    Top:
      DrawLines(clBlack, [Degress[6], Degress[7]]);
    TopRight:
      DrawLines(clBlack, [Degress[7], Degress[0]]);
  end;


  with clRed do begin
    aacircleRGBA(PSDLRenderer,
      CenterX, CenterY, Radius, r, g, b, a);

    aalineRGBA(PSDLRenderer,
      CenterX, CenterY, Aim.X, Aim.Y, r, g, b, a);
  end;


  if Assigned(Text) then begin
    Text.AsIPaintable.Paint;
  end;
end;

procedure DrawDebugCircle(ADegree: Double);
begin
  if not Assigned(Text) then begin
    Text := TText.Create;
    Text.FontName := 'Arimo-Regular';
    Text.FontSize := 14;
    Text.Visible := True;
  end;
  Text.Load(ADegree.ToString);
  Text.Left := 100;
  Text.Top := 100;
  AimDegree := ADegree;
  Aim := CircumferencePoint(ADegree);
end;

initialization
  SectorPad := SectorSize/2;
  Degress[0]:= MaxDegress-SectorPad;
  Degress[1]:= (Sectorsize * 1)-SectorPad;
  Degress[2]:= (SectorSize * 2)-SectorPad;
  Degress[3]:= (SectorSize * 3)-SectorPad;
  Degress[4]:= (SectorSize * 4)-SectorPad;
  Degress[5]:= (SectorSize * 5)-SectorPad;
  Degress[6]:= (SectorSize * 6)-SectorPad;
  Degress[7]:= (SectorSize * 7)-SectorPad;

finalization
  if not Assigned(Text) then begin
    Text.Free;
  end;

end.

