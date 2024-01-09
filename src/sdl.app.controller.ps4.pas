unit sdl.app.controller.ps4;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ctypes, SDL2,
  sdl.app.events.abstract,
  sdl.app.controller;

type

  { TSDLPS4Controller }

  TSDLPS4Controller = class(TController)
  private
    FWidth : integer;
    FHeight: integer;
    FGameController : PSDL_GameController;
    FOnControllerAxisMotion: TOnControllerAxisMotionEvent;
    FOnControllerButtonDown: TOnControllerButtonDownEvent;
    FOnControllerButtonUp: TOnControllerButtonUpEvent;
    FOnControllerTouchPadMotion: TOnControllerTouchPadMotionEvent;
    FOnControllerSensorUpdate: TOnControllerSensorUpdateEvent;
    procedure ControllerAxisMotion(const event: TSDL_ControllerAxisEvent);
    procedure ControllerButtonDown(const event: TSDL_ControllerButtonEvent);
    procedure ControllerButtonUp(const event: TSDL_ControllerButtonEvent);
    procedure ControllerTouchPadMotion(const event: TSDL_ControllerTouchpadEvent);
    procedure ControllerSensorUpdate(const event: TSDL_ControllerSensorEvent);
    procedure SetOnControllerAxisMotion(AValue: TOnControllerAxisMotionEvent);
    procedure SetOnControllerButtonDown(AValue: TOnControllerButtonDownEvent);
    procedure SetOnControllerButtonUp(AValue: TOnControllerButtonUpEvent);
    procedure SetOnControllerTouchPadMotion(
      AValue: TOnControllerTouchPadMotionEvent);
    procedure SetOnControllerSensorUpdate(AValue: TOnControllerSensorUpdateEvent);
  public
    constructor Create; override;
    destructor Destroy; override;
    property OnControllerAxisMotion : TOnControllerAxisMotionEvent
      read FOnControllerAxisMotion write SetOnControllerAxisMotion;

    property OnControllerButtonDown : TOnControllerButtonDownEvent
      read FOnControllerButtonDown write SetOnControllerButtonDown;

    property OnControllerButtonUp : TOnControllerButtonUpEvent
      read FOnControllerButtonUp write SetOnControllerButtonUp;

    property OnControllerTouchPadMotion : TOnControllerTouchPadMotionEvent
      read FOnControllerTouchPadMotion write SetOnControllerTouchPadMotion;

    property OnControllerSensorUpdate : TOnControllerSensorUpdateEvent
      read FOnControllerSensorUpdate write SetOnControllerSensorUpdate;
  end;

implementation

uses
  Math,
  sdl.app.output,
  sdl.app.events.custom,
  sdl.app.video.methods,
  sdl.app.graphics.debug,
  timestamps.types,
  timestamps;

{ TSDLPS4Controller }

const
  PS4_BUTTON_SHARE = 4;
  PS4_BUTTON_HOME = 5;
  PS4_BUTTON_OPTIONS = 6;
  LHumbleTime = 100;

type
  Direction = (
    Top,     Left,     Bottom,     Right,
    TopLeft, TopRight, BottomLeft, BottomRight);

const
  SectorSize: Double = 45;
  MaxDegress : Double = 360;
  HalfDegree : Double = 180;

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

function GetDirection(Degree: Double): Direction;
begin
  if      (Degree >= Sector0) or  (Degree < Sector1) then
    Result := Direction.Right
  else if (Degree >= Sector1) and (Degree < Sector2) then
    Result := Direction.BottomRight
  else if (Degree >= Sector2) and (Degree < Sector3) then
    Result := Direction.Bottom
  else if (Degree >= Sector3) and (Degree < Sector4) then
    Result := Direction.BottomLeft
  else if (Degree >= Sector4) and (Degree < Sector5) then
    Result := Direction.Left
  else if (Degree >= Sector5) and (Degree < Sector6) then
    Result := Direction.TopLeft
  else if (Degree >= Sector6) and (Degree < Sector7) then
    Result := Direction.Top
  else if (Degree >= Sector7) and (Degree < Sector0) then
    Result := Direction.TopRight;
end;

procedure NormalizeDiagonals(var AX: Double; var AY : Double);
var
  Magnitude: Double;
begin
  Magnitude := Hypot(AX, AY);
  if Magnitude > 1.0 then begin
    AX := AX / Magnitude;
    AY := AY / Magnitude;
  end;
end;

// https://github.com/Minimuino/thumbstick-deadzones
type
  TPoint = record
    X : Double;
    Y : Double;
  end;

const
  DeadZoneThreshold = 0.2;

var
  LastClock : TLargerFloat = 0;

function MapRange(AValue, OldMin, OldMax, NewMin, NewMax: Double): Double;
begin
  Result :=
    (NewMin + (NewMax - NewMin)) * ((AValue - OldMin) / (OldMax - OldMin));
end;

function SlopedScaledAxialDeadzone(AX, AY: Double): TPoint;
var
  LX, LY, DeadzoneX, DeadzoneY: Double;
  SignX: Double;
  SignY: Double;
begin
  LX := 0;
  LY := 0;
  DeadzoneX := DeadZoneThreshold * Power(Abs(AX), 2);
  DeadzoneY := DeadZoneThreshold * Power(Abs(AY), 2);
  SignX := Sign(AX);
  SignY := Sign(AY);

  if Abs(AX) > DeadzoneX then
    LX := SignX * MapRange(Abs(AX), DeadzoneX, 1, 0, 1);

  if Abs(AY) > DeadzoneY then
    LY := SignY * MapRange(Abs(AY), DeadzoneY, 1, 0, 1);

  Result.X := LX;
  Result.Y := LY;
end;

function ScaledRadialDeadzone(AX, AY : Double): TPoint;
var
  Magnitude: Double;
  NormalizedX : Double;
  NormalizedY : Double;
begin
  Magnitude := Hypot(AX, AY);

  if Magnitude < DeadZoneThreshold then begin
    Result.X := 0;
    Result.Y := 0;
  end else begin
    NormalizedX := AX / Magnitude;
    NormalizedY := AY / Magnitude;

    Result.X := NormalizedX * MapRange(Magnitude, DeadZoneThreshold, 1, 0, 1);
    Result.Y := NormalizedY * MapRange(Magnitude, DeadZoneThreshold, 1, 0, 1);
  end;
end;

function IsOutsideDeadZone(var AX : Double; var AY : Double) : Boolean;
var
  Magnitude: Double;
  Output : TPoint;
begin
  Magnitude := Hypot(AX, AY);

  if Magnitude < DeadZoneThreshold then begin
    AX := 0;
    AY := 0;
    Result := False;
  end else begin
    Output := ScaledRadialDeadzone(AX, AY);
    Output := SlopedScaledAxialDeadzone(Output.X, Output.Y);
    AX := Output.X;
    AY := Output.Y;
    Result := True;
  end;
end;

//var
//  PreviousX : Double;
//  PreviousY : Double;
//
//function IsDeadZoneLeaveEvent(AX, AY : Double) : Boolean;
//begin
//  Result := (not IsOutsideDeadZone(PreviousX, PreviousY)) and IsOutsideDeadZone(AX, AY);
//  PreviousX := AX;
//  PreviousY := AY;
//end;

procedure TSDLPS4Controller.ControllerAxisMotion(
  const event: TSDL_ControllerAxisEvent);
var
  LX : Double;
  LY , LDegrees: Double;

  function CalculateAngleDegrees(AX, AY: Double): Double;
  var
    AngleRadians, AngleDegrees: Double;
  begin
    AngleRadians := ArcTan2(AY, AX);
    AngleDegrees := AngleRadians * (HalfDegree/PI);
    AngleDegrees := AngleDegrees + MaxDegress;
    Result := AngleDegrees - Trunc(AngleDegrees/MaxDegress) * MaxDegress;
  end;
begin
  if (ClockMonotonic-LastClock) < 0.250 then Exit;

  LX := SDL_GameControllerGetAxis(
    FGameController, SDL_CONTROLLER_AXIS_LEFTX)/MaxSmallint;

  LY := SDL_GameControllerGetAxis(
    FGameController, SDL_CONTROLLER_AXIS_LEFTY)/MaxSmallint;

  if IsOutsideDeadZone(LX, LY) then begin
    LastClock := ClockMonotonic;
    LDegrees := CalculateAngleDegrees(LX, LY);
    //DrawDebugCircle(LDegrees);
    case GetDirection(LDegrees) of
      Top:
        FNavigator.GoTop;
      Left:
        FNavigator.GoLeft;
      Bottom:
        FNavigator.GoBottom;
      Right:
        FNavigator.GoRight;
      TopLeft:
        FNavigator.GoTopLeft;
      TopRight:
        FNavigator.GoTopRight;
      BottomLeft:
        FNavigator.GoBottomLeft;
      BottomRight:
        FNavigator.GoBottomRight;
    end;
  end;
end;

procedure TSDLPS4Controller.ControllerButtonDown(
  const event: TSDL_ControllerButtonEvent);
begin
  Print('');
  Print('Button Event');
  Print(event.button.ToString);
  Print(IntToStr(event.which));
end;

procedure TSDLPS4Controller.ControllerButtonUp(
  const event: TSDL_ControllerButtonEvent);
begin
  case event.button of
    SDL_CONTROLLER_BUTTON_A: ;
    SDL_CONTROLLER_BUTTON_B: ;
    SDL_CONTROLLER_BUTTON_X: ;
    SDL_CONTROLLER_BUTTON_Y: ;

    PS4_BUTTON_SHARE: ;
    PS4_BUTTON_HOME: ;
    PS4_BUTTON_OPTIONS: ;

    SDL_CONTROLLER_BUTTON_LEFTSTICK: ;
    SDL_CONTROLLER_BUTTON_RIGHTSTICK: ;
    SDL_CONTROLLER_BUTTON_LEFTSHOULDER: FNavigator.ConfirmSelection;
    SDL_CONTROLLER_BUTTON_RIGHTSHOULDER: FNavigator.ConfirmSelection;

    SDL_CONTROLLER_BUTTON_DPAD_UP: begin
      FNavigator.GoTop;
      SDL_GameControllerRumble(
        FGameController, $FFFF, $FFFF, LHumbleTime);
    end;

    SDL_CONTROLLER_BUTTON_DPAD_DOWN: begin
      FNavigator.GoBottom;
      SDL_GameControllerRumble(
        FGameController, $FFFF, $FFFF, LHumbleTime);
    end;

    SDL_CONTROLLER_BUTTON_DPAD_LEFT: begin
      FNavigator.GoLeft;
      SDL_GameControllerRumble(
        FGameController, $FFFF, $FFFF, LHumbleTime);
    end;

    SDL_CONTROLLER_BUTTON_DPAD_RIGHT: begin
      FNavigator.GoRight;
      SDL_GameControllerRumble(
        FGameController, $FFFF, $FFFF, LHumbleTime);
    end;

    SDL_CONTROLLER_BUTTON_TOUCHPAD: ;
  end;
end;

procedure TSDLPS4Controller.ControllerTouchPadMotion(
  const event: TSDL_ControllerTouchpadEvent);
var
  touchpad, finger: cint32;
  state: Uint8;
  x, y, pressure: Single;
begin
  // Get the touchpad and finger indices
  touchpad := event.touchpad;
  finger := event.finger;

  // Get the finger state, position, and pressure
  SDL_GameControllerGetTouchpadFinger(
    FGameController, touchpad, finger, @state, @x, @y, @pressure);

  // Print the finger information
  Print(Format('Touchpad %d, Finger %d, State %d, X %f, Y %f, Pressure %f',
    [touchpad, finger, state, x, y, pressure]));
end;

procedure TSDLPS4Controller.ControllerSensorUpdate(const event: TSDL_ControllerSensorEvent);
var
  LSensor: PSDL_Sensor;
begin
  LSensor := SDL_SensorFromInstanceID(event.which);
  if not Assigned(LSensor) then
  begin
    Print('Couldn''t get sensor for sensor event');
    Exit;
  end;

  case SDL_SensorGetType(LSensor) of
    SDL_SENSOR_ACCEL: begin
      Print(Format('Accelerometer update: %.2f, %.2f, %.2f',
        [event.data[0], event.data[1], event.data[2]]));
    end;

    SDL_SENSOR_GYRO: begin
      Print(Format('Gyro update: %.2f, %.2f, %.2f',
        [event.data[0], event.data[1], event.data[2]]));
    end

    otherwise begin
      Print(Format('Sensor update for sensor type %1d',
        [SDL_SensorGetType(LSensor)]));
    end;
  end;
end;

procedure TSDLPS4Controller.SetOnControllerAxisMotion(
  AValue: TOnControllerAxisMotionEvent);
begin
  if FOnControllerAxisMotion = AValue then Exit;
  FOnControllerAxisMotion := AValue;
end;

procedure TSDLPS4Controller.SetOnControllerButtonDown(
  AValue: TOnControllerButtonDownEvent);
begin
  if FOnControllerButtonDown = AValue then Exit;
  FOnControllerButtonDown := AValue;
end;

procedure TSDLPS4Controller.SetOnControllerButtonUp(
  AValue: TOnControllerButtonUpEvent);
begin
  if FOnControllerButtonUp = AValue then Exit;
  FOnControllerButtonUp := AValue;
end;

procedure TSDLPS4Controller.SetOnControllerTouchPadMotion(
  AValue: TOnControllerTouchPadMotionEvent);
begin
  if FOnControllerTouchPadMotion = AValue then Exit;
  FOnControllerTouchPadMotion := AValue;
end;

procedure TSDLPS4Controller.SetOnControllerSensorUpdate(
  AValue: TOnControllerSensorUpdateEvent);
begin
  if FOnControllerSensorUpdate = AValue then Exit;
  FOnControllerSensorUpdate := AValue;
end;

constructor TSDLPS4Controller.Create;
var
  LGameControllers : integer;
  LWindowRect : TSDL_Rect;
begin
  inherited Create;
  LWindowRect := WindowSize;
  FWidth := LWindowRect.w;
  FHeight := LWindowRect.h;

  SDLEvents.OnControllerButtonDown := @ControllerButtonDown;
  SDLEvents.OnControllerAxisMotion := @ControllerAxisMotion;
  SDLEvents.OnControllerButtonUp := @ControllerButtonUp;
  SDLEvents.OnControllerSensorUpdate := @ControllerSensorUpdate;
  SDLEvents.OnControllerTouchPadMotion := @ControllerTouchPadMotion;

  SDL_JoystickUpdate;
  LGameControllers := SDL_NumJoysticks;
  Print('NumJoysticks:'+LGameControllers.ToString);
  if LGameControllers > 0 then begin
    // 'PS4 Controller'
    Print(SDL_GameControllerNameForIndex(0));
    FGameController := SDL_GameControllerOpen(0);
    if SDL_GameControllerHasRumble(FGameController) = SDL_TRUE then begin
      Print('Has Rumble');
    end;

    if SDL_GameControllerHasRumbleTriggers(FGameController) = SDL_TRUE then begin
      Print('Has Rumble Triggers');
    end;

    if SDL_GameControllerHasSensor(FGameController, SDL_SENSOR_ACCEL) = SDL_TRUE then begin
      Print('Has Accelerometer');
    end;

    if SDL_GameControllerHasSensor(FGameController, SDL_SENSOR_GYRO) = SDL_TRUE then begin
      Print('Has Gyroscope');
    end;

  end else begin

  end;
end;

destructor TSDLPS4Controller.Destroy;
begin
  SDLEvents.OnControllerButtonDown := nil;
  SDLEvents.OnControllerAxisMotion := nil;
  SDLEvents.OnControllerButtonUp := nil;
  SDLEvents.OnControllerSensorUpdate := nil;
  SDLEvents.OnControllerTouchPadMotion := nil;
  SDL_GameControllerClose(FGameController);
  inherited Destroy;
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


