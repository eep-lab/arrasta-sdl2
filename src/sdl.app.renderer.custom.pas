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
  , sdl.app.events.abstract
  ;

type

  TOnEyeFixationEvent = procedure(Sender: TObject) of object;
  TOnEyeSaccadeEvent = procedure(Sender: TObject) of object;
  TOnMouseEvent = procedure(Sender:TObject; Shift: TCustomShiftState; X, Y: Integer) of object;

  TChilds = specialize TFPGList<TComponent>;

  { TCustomRenderer }

  TCustomRenderer = class(TComponent, IClickable, IPaintable, IMoveable)
  private
    FMouseInside : Boolean;
    FOnMouseMove: TOnMouseEvent;
    FOnMouseDown: TOnMouseEvent;
    FOnMouseUp: TOnMouseEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseExit: TNotifyEvent;
    FParent : TCustomRenderer;
    function GetSDLMouseMotion    : TOnMouseMotionEvent;
    function GetSDLMouseButtonDown: TOnMouseButtonDownEvent;
    function GetSDLMouseButtonUp  : TOnMouseButtonUpEvent;
    procedure SetOnMouseDown(AValue: TOnMouseEvent);
    procedure SetOnMouseEnter(AValue: TNotifyEvent);
    procedure SetOnMouseExit(AValue: TNotifyEvent);
    procedure SetOnMouseMove(AValue: TOnMouseEvent);
    procedure SetOnMouseUp(AValue: TOnMouseEvent);
    procedure SetParent(AParent: TCustomRenderer);
    procedure AddChild(AChild: TComponent);
    procedure EyeLinkFixation(Sender: TObject);
    procedure EyeLinkSaccade(Sender: TObject);
    procedure SDLMouseMotion(const event: TSDL_MouseMotionEvent);
    procedure SDLMouseButtonDown(const event: TSDL_MouseButtonEvent);
    procedure SDLMouseButtonUp(const event: TSDL_MouseButtonEvent);
  protected
    FChilds : TChilds;
    function GetMouseInside : Boolean; virtual;
    function PointInside(SDLPoint : TSDL_Point) : Boolean;
    function GetBoundsRect : TSDL_Rect; virtual;
    procedure SetMouseInside(AValue : Boolean);
    procedure Paint; virtual; abstract;
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
    property Parent : TCustomRenderer read FParent write SetParent;
    property BoundsRect : TSDL_Rect read GetBoundsRect;
    //property OnEyeFixation :
    //property OnEyeSaccade  :
    property OnMouseMove : TOnMouseEvent read FOnMouseMove write SetOnMouseMove;
    property OnMouseDown : TOnMouseEvent read FOnMouseDown write SetOnMouseDown;
    property OnMouseUp   : TOnMouseEvent read FOnMouseUp write SetOnMouseUp;
    property OnMouseEnter : TNotifyEvent read FOnMouseEnter write SetOnMouseEnter;
    property OnMouseExit : TNotifyEvent read FOnMouseExit write SetOnMouseExit;
    property MouseInside : Boolean read GetMouseInside write SetMouseInside;
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

function TCustomRenderer.GetSDLMouseButtonDown: TOnMouseButtonDownEvent;
begin
  Result := @SDLMouseButtonDown;
end;

function TCustomRenderer.GetSDLMouseButtonUp: TOnMouseButtonUpEvent;
begin
  Result := @SDLMouseButtonUp;
end;

procedure TCustomRenderer.AddChild(AChild: TComponent);
begin
  FChilds.Add(AChild);
end;

procedure TCustomRenderer.EyeLinkFixation(Sender: TObject);
begin

end;

procedure TCustomRenderer.EyeLinkSaccade(Sender: TObject);
begin

end;

function TCustomRenderer.GetBoundsRect: TSDL_Rect;
begin
  Result := MonitorFromWindow;
end;

procedure TCustomRenderer.SetMouseInside(AValue: Boolean);
begin
  if FMouseInside=AValue then Exit;
  FMouseInside:=AValue;
end;

procedure TCustomRenderer.SDLMouseButtonDown(const event: TSDL_MouseButtonEvent);
var
  Shift : TCustomShiftState;
begin
  Shift := GetShiftState;
  MouseDown(Self, Shift, event.x, event.y);
end;

procedure TCustomRenderer.SDLMouseButtonUp(const event: TSDL_MouseButtonEvent);
var
  Shift: TCustomShiftState;
begin
  Shift := GetShiftState;
  MouseUp(Self, Shift, event.x, event.y);
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
var
  Shift: TCustomShiftState;
begin
  Shift := GetShiftState;
  MouseMove(Self, Shift, event.x, event.y);
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

end.

