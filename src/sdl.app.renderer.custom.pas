{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.renderer.custom;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, SDL2, fgl
  , sdl.app.paintable.contract
  , sdl.app.clickable.contract
  , sdl.app.moveable.contract
  , sdl.app.lookable.contract
  , sdl.app.events.abstract
  ;

type

  TOnEyeFixationEvent = procedure(Sender: TObject) of object;
  TOnEyeSaccadeEvent = procedure(Sender: TObject) of object;
  TOnMouseEvent = procedure(Sender:TObject; Shift: TCustomShiftState; X, Y: Integer) of object;

  TCallbacks = record
    OnResponse : TNotifyEvent;
    OnMouseMove: TOnMouseEvent;
    OnMouseDown: TOnMouseEvent;
    OnMouseUp: TOnMouseEvent;
    OnMouseEnter: TNotifyEvent;
    OnMouseExit: TNotifyEvent;
  end;

  TChilds = specialize TFPGList<TComponent>;

  { TCustomRenderer }

  TCustomRenderer = class(TComponent, IClickable, IPaintable, IMoveable, ILookable)
  private
    FOnGazeEnter: TNotifyEvent;
    FOnGazeExit: TNotifyEvent;
    FOnGazeMove: TOnMouseEvent;
    FRect : TSDL_Rect;
    FGazeInside : Boolean;
    FMouseInside : Boolean;
    FOnMouseMove: TOnMouseEvent;
    FOnMouseDown: TOnMouseEvent;
    FOnMouseUp: TOnMouseEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseExit: TNotifyEvent;
    FParent : TCustomRenderer;
    function GetInFront: Boolean;
    function GetSDLMouseMotion    : TOnMouseMotionEvent;
    function GetSDLMouseButtonDown: TOnMouseButtonDownEvent;
    function GetSDLMouseButtonUp  : TOnMouseButtonUpEvent;
    function GetZIndex: integer;
    procedure SetOnGazeEnter(AValue: TNotifyEvent);
    procedure SetOnGazeExit(AValue: TNotifyEvent);
    procedure SetOnGazeMove(AValue: TOnMouseEvent);
    procedure SetOnMouseDown(AValue: TOnMouseEvent);
    procedure SetOnMouseEnter(AValue: TNotifyEvent);
    procedure SetOnMouseExit(AValue: TNotifyEvent);
    procedure SetOnMouseMove(AValue: TOnMouseEvent);
    procedure SetOnMouseUp(AValue: TOnMouseEvent);
    procedure SetParent(AParent: TCustomRenderer);
    procedure AddChild(AChild: TComponent);
    procedure SDLMouseMotion(const event: TSDL_MouseMotionEvent);
    procedure SDLMouseButtonDown(const event: TSDL_MouseButtonEvent);
    procedure SDLMouseButtonUp(const event: TSDL_MouseButtonEvent);
    procedure BringChildToFront(AChild : TComponent);
  protected
    FChilds : TChilds;
    function GetGazeInside : Boolean; virtual;
    function GetMouseInside : Boolean; virtual;
    function PointInside(SDLPoint : TSDL_Point) : Boolean;
    function GetBoundsRect : TSDL_Rect; virtual;
    procedure SetBoundsRect(AValue : TSDL_Rect); virtual;
    procedure SetMouseInside(AValue : Boolean);
    procedure SetGazeInside(AValue : Boolean);
    procedure Paint; virtual; abstract;
    procedure GazeEnter(Sender: TObject); virtual;
    procedure GazeExit(Sender: TObject); virtual;
    procedure GazeMove(Sender: TObject;
      Shift: TCustomShiftState; X,Y: Integer); virtual;
    procedure MouseMove(Sender: TObject;
      Shift: TCustomShiftState; X,Y: Integer); virtual;
    procedure MouseDown(Sender: TObject;
      Shift: TCustomShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Sender: TObject;
      Shift: TCustomShiftState; X, Y: Integer); virtual;
    procedure MouseEnter(Sender: TObject); virtual;
    procedure MouseExit(Sender: TObject); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AsIClickable : IClickable;
    function AsIPaintable : IPaintable;
    function AsIMoveable : IMoveable;
    function AsILookable : ILookable;
    procedure BringToFront;
    property Parent : TCustomRenderer read FParent write SetParent;
    property BoundsRect : TSDL_Rect read GetBoundsRect;
    property OnGazeEnter : TNotifyEvent read FOnGazeEnter write SetOnGazeEnter;
    property OnGazeExit : TNotifyEvent read FOnGazeExit write SetOnGazeExit;
    property OnGazeMove : TOnMouseEvent read FOnGazeMove write SetOnGazeMove;
    property OnMouseMove : TOnMouseEvent read FOnMouseMove write SetOnMouseMove;
    property OnMouseDown : TOnMouseEvent read FOnMouseDown write SetOnMouseDown;
    property OnMouseUp   : TOnMouseEvent read FOnMouseUp write SetOnMouseUp;
    property OnMouseEnter : TNotifyEvent read FOnMouseEnter write SetOnMouseEnter;
    property OnMouseExit : TNotifyEvent read FOnMouseExit write SetOnMouseExit;
    property MouseInside : Boolean read GetMouseInside write SetMouseInside;
    property ZIndex : integer read GetZIndex;
    property InFront : Boolean read GetInFront;
  end;

implementation

uses sdl.app.video.methods;

{ TCustomRenderer }

procedure TCustomRenderer.SetParent(AParent: TCustomRenderer);
begin
  if FParent=AParent then Exit;
  AParent.AddChild(Self);
  FParent:=AParent;
end;

procedure TCustomRenderer.SetOnMouseEnter(AValue: TNotifyEvent);
begin
  if FOnMouseEnter=AValue then Exit;
  FOnMouseEnter:=AValue;
end;

procedure TCustomRenderer.SetOnMouseExit(AValue: TNotifyEvent);
begin
  if FOnMouseExit=AValue then Exit;
  FOnMouseExit:=AValue;
end;
                                        
procedure TCustomRenderer.SetOnMouseMove(AValue: TOnMouseEvent);
begin
  if FOnMouseMove=AValue then Exit;
  FOnMouseMove:=AValue;
end;
                                        
procedure TCustomRenderer.SetOnMouseDown(AValue: TOnMouseEvent);
begin
  if FOnMouseDown=AValue then Exit;
  FOnMouseDown:=AValue;
end;

procedure TCustomRenderer.SetOnMouseUp(AValue: TOnMouseEvent);
begin
  if FOnMouseUp=AValue then Exit;
  FOnMouseUp:=AValue;
end;

function TCustomRenderer.GetSDLMouseMotion: TOnMouseMotionEvent;
begin
  Result := @SDLMouseMotion;
end;

function TCustomRenderer.GetInFront: Boolean;
begin
  Result := ZIndex = Parent.FChilds.Count-1;
end;

function TCustomRenderer.GetSDLMouseButtonDown: TOnMouseButtonDownEvent;
begin
  Result := @SDLMouseButtonDown;
end;

function TCustomRenderer.GetSDLMouseButtonUp: TOnMouseButtonUpEvent;
begin
  Result := @SDLMouseButtonUp;
end;

function TCustomRenderer.GetZIndex: integer;
begin
  Result := Parent.FChilds.IndexOf(Self);
end;

procedure TCustomRenderer.SetOnGazeEnter(AValue: TNotifyEvent);
begin
  if FOnGazeEnter = AValue then Exit;
  FOnGazeEnter := AValue;
end;

procedure TCustomRenderer.SetOnGazeExit(AValue: TNotifyEvent);
begin
  if FOnGazeExit = AValue then Exit;
  FOnGazeExit := AValue;
end;

procedure TCustomRenderer.SetOnGazeMove(AValue: TOnMouseEvent);
begin
  if FOnGazeMove = AValue then Exit;
  FOnGazeMove := AValue;
end;

procedure TCustomRenderer.AddChild(AChild: TComponent);
begin
  FChilds.Add(AChild);
end;

function TCustomRenderer.GetBoundsRect: TSDL_Rect;
begin
  Result := MonitorFromWindow;
end;

procedure TCustomRenderer.SetBoundsRect(AValue: TSDL_Rect);
begin
  FRect := AValue;
end;

procedure TCustomRenderer.SetMouseInside(AValue: Boolean);
begin
  if FMouseInside=AValue then Exit;
  FMouseInside:=AValue;
end;

procedure TCustomRenderer.SetGazeInside(AValue: Boolean);
begin
  if FGazeInside=AValue then Exit;
  FGazeInside:=AValue;
end;

procedure TCustomRenderer.GazeEnter(Sender: TObject);
begin
  if Assigned(OnGazeEnter) then
    OnGazeEnter(Sender);
end;

procedure TCustomRenderer.GazeExit(Sender: TObject);
begin
  if Assigned(OnGazeExit) then
    OnGazeExit(Sender);
end;

procedure TCustomRenderer.GazeMove(Sender: TObject; Shift: TCustomShiftState;
  X, Y: Integer);
begin
  if Assigned(OnGazeMove) then
    OnGazeMove(Sender, GetShiftState, X, Y);
end;

procedure TCustomRenderer.SDLMouseButtonDown(const event: TSDL_MouseButtonEvent);
begin
  MouseDown(Self, GetShiftState, event.x, event.y);
end;

procedure TCustomRenderer.SDLMouseButtonUp(const event: TSDL_MouseButtonEvent);
begin
  MouseUp(Self, GetShiftState, event.x, event.y);
end;

procedure TCustomRenderer.BringChildToFront(AChild: TComponent);
begin
  if FChilds.Count <= 1 then Exit;
  with FChilds do Add(Extract(AChild));
end;

function TCustomRenderer.GetGazeInside: Boolean;
begin
  Result := FGazeInside;
end;

function TCustomRenderer.GetMouseInside: Boolean;
begin
  Result := FMouseInside;
end;

function TCustomRenderer.PointInside(SDLPoint: TSDL_Point): Boolean;
var
  SDLRect  : TSDL_Rect;
begin
  SDLRect := GetBoundsRect;
  Result := SDL_PointInRect(@SDLPoint, @SDLRect);
end;

procedure TCustomRenderer.SDLMouseMotion(const event: TSDL_MouseMotionEvent);
begin
  MouseMove(Self, GetShiftState, event.x, event.y);
end;

procedure TCustomRenderer.MouseMove(Sender: TObject; Shift: TCustomShiftState;
  X, Y: Integer);
begin
  if Assigned(OnMouseMove) then
    OnMouseMove(Sender, Shift, X, Y);
end;

procedure TCustomRenderer.MouseDown(Sender: TObject; Shift: TCustomShiftState;
  X, Y: Integer);
begin
  if Assigned(OnMouseDown) then
    OnMouseDown(Sender, Shift, X, Y);
end;

procedure TCustomRenderer.MouseUp(Sender: TObject; Shift: TCustomShiftState;
  X, Y: Integer);
begin
  if Assigned(OnMouseUp) then
    OnMouseUp(Sender, Shift, X, Y);
end;

procedure TCustomRenderer.MouseEnter(Sender: TObject);
begin
  if Assigned(OnMouseEnter) then
    OnMouseEnter(Sender);
end;

procedure TCustomRenderer.MouseExit(Sender: TObject);
begin
  if Assigned(OnMouseExit) then
    OnMouseExit(Sender);
end;

constructor TCustomRenderer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FChilds := TChilds.Create;
  FMouseInside := False;
  FGazeInside := False;
  Parent := nil;
end;

destructor TCustomRenderer.Destroy;
begin
  FChilds.Free;
  inherited Destroy;
end;

function TCustomRenderer.AsIClickable: IClickable;
begin
  Result := Self as IClickable;
end;

function TCustomRenderer.AsIPaintable: IPaintable;
begin
  Result := Self as IPaintable;
end;

function TCustomRenderer.AsIMoveable: IMoveable;
begin
  Result := Self as IMoveable;
end;

function TCustomRenderer.AsILookable: ILookable;
begin
  Result := Self as ILookable;
end;

procedure TCustomRenderer.BringToFront;
begin
  Parent.BringChildToFront(Self);
end;

end.

