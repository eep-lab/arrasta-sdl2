{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.graphics.rectangule;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , SDL2
  , ctypes
  , sdl.app.renderer.custom
  ;

type

  { TRectangule }

  TRectangule = class(TCustomRenderer)
  private
    FOriginalBounds : TSDL_Rect;
    FEdgeColor: TSDL_Color;
    FVisible: Boolean;
    function GetHeight: cint;
    function GetLeft: cint;
    function GetTop: cint;
    function GetWidth: cint;
    procedure SetBoundsRect(AValue: TSDL_Rect);
    procedure SetHeight(AValue: cint);
    procedure SetLeft(AValue: cint);
    procedure SetTop(AValue: cint);
    procedure SetWidth(AValue: cint);
  protected
    FRect    : TSDL_Rect;
    function GetBoundsRect: TSDL_Rect; override;
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Centralize;
    procedure Show;
    procedure Hide;
    function IntersectsWith(ARect: TSDL_Rect) : Boolean;
    procedure CentralizeWith(ARect: TSDL_Rect);
    procedure Inflate(AValue : cint);
    procedure ToOriginalBounds;
    procedure SetOriginalBounds;
    property Left   : cint read GetLeft write SetLeft;
    property Top    : cint read GetTop write SetTop;
    property Width  : cint read GetWidth write SetWidth;
    property Height : cint read GetHeight write SetHeight;
    property BoundsRect : TSDL_Rect read GetBoundsRect write SetBoundsRect;
    property Visible : Boolean read FVisible write FVisible;
    property EdgeColor: TSDL_Color read FEdgeColor write FEdgeColor;
  end;



implementation

uses sdl.app.video.methods, sdl.colors;

{ TStimulus }

function TRectangule.GetHeight: cint;
begin
  Result := FRect.h;
end;

function TRectangule.GetLeft: cint;
begin
  Result := FRect.x;
end;

function TRectangule.GetTop: cint;
begin
  Result := FRect.y;
end;

function TRectangule.GetWidth: cint;
begin
  Result := FRect.w;
end;

procedure TRectangule.SetBoundsRect(AValue: TSDL_Rect);
begin
  FRect := AValue;
end;

procedure TRectangule.SetHeight(AValue: cint);
begin
  if FRect.h=AValue then Exit;
  FRect.h := AValue;
end;

procedure TRectangule.SetLeft(AValue: cint);
begin
  if FRect.x=AValue then Exit;
  FRect.x:=AValue;
end;

procedure TRectangule.SetTop(AValue: cint);
begin
  if FRect.y=AValue then Exit;
  FRect.y:=AValue;
end;

procedure TRectangule.SetWidth(AValue: cint);
begin
  if FRect.w=AValue then Exit;
  FRect.w:=AValue;
end;

function TRectangule.GetBoundsRect: TSDL_Rect;
begin
  Result:=FRect;
end;

procedure TRectangule.Paint;
begin
  SDL_SetRenderDrawColor(PSDLRenderer, 128, 128, 128, 255);
  SDL_RenderFillRect(PSDLRenderer, @FRect);
end;

constructor TRectangule.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FVisible := False;
  with FRect do begin
    x := 0;
    y := 0;
    w := 200;
    h := 200;
  end;
  SetOriginalBounds;
  EdgeColor := clRed
end;

destructor TRectangule.Destroy;
begin
  { free stuff }
  inherited Destroy;
end;

procedure TRectangule.Centralize;
begin
  Left := (Parent.BoundsRect.w div 2) - (Width  div 2);
  Top  := (Parent.BoundsRect.h div 2) - (Height div 2);
end;

procedure TRectangule.Show;
begin
  FVisible := True;
end;

procedure TRectangule.Hide;
begin
  FVisible := False;
end;

function TRectangule.IntersectsWith(ARect: TSDL_Rect): Boolean;
begin
  Result := SDL_HasIntersection(@FRect, @ARect);
end;

procedure TRectangule.CentralizeWith(ARect: TSDL_Rect);
begin
  Left := ARect.x + (ARect.w div 2) - (Width  div 2);
  Top  := ARect.y + (ARect.h div 2) - (Height div 2);
end;

procedure TRectangule.Inflate(AValue: cint);
begin
  FRect.x := FRect.x - AValue;
  FRect.y := FRect.y - AValue;
  FRect.w := FRect.w + (2 * AValue);
  FRect.h := FRect.h + (2 * AValue);
end;

procedure TRectangule.ToOriginalBounds;
begin
  FRect := FOriginalBounds;
end;

procedure TRectangule.SetOriginalBounds;
begin
  FOriginalBounds := FRect;
end;

end.

