unit sdl.app.graphics.animation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , SDL2
  //, ctypes
  , sdl.app.graphics.rectangule
  , sdl.app.paintable.contract
  , sdl.app.grids
  //, sdl.timer
  ;

type
  { TAnimation }

  TAnimation = class(TRectangule, IPaintable)
  private
    FSibling : TRectangule;
    FVisible: Boolean;
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Animate(ASibling : TRectangule);
    procedure Join(ASample, AComparison : TRectangule;
      AGridOrientation : TGridOrientation);
    procedure Stop;
    property Sibling : TRectangule read FSibling;
    property Visible : Boolean read FVisible write FVisible;
  end;

implementation

uses
  LazFileUtils
  , sdl.app.video.methods
  ;

type

  TAnimationData = record
    Acum: double;
    Growing: boolean;
    Step: double;
    FixedHeight : integer;
    MinHeight : integer;
    MaxHeight : integer;
    MinWidth  : integer;
    MaxWidth  : integer;
  end;

var
  AnimationData : TAnimationData;

{ TAnimation }

procedure TAnimation.Paint;
var
  TempSize: double;
  function easeInOutQuad(t: double): double;
  begin
    if t < 0.5 then
      Result := 2 * t * t
    else
      Result := -1 + (4 - 2 * t) * t;
  end;
begin
  if Assigned(FSibling) and FVisible then begin
    with AnimationData do begin
      Acum := Acum + Step;
      if Step > 1 then
        Step := 1;
      TempSize := easeInOutQuad(Acum);
      if Growing then
      begin
        Height := Round(FixedHeight * TempSize);
        Width := Height;
        if Height >= FixedHeight then
        begin
          Height := FixedHeight;
          Width := Height;
          Growing := False;
          Acum:= 0;
        end;
      end else begin
        TempSize := FixedHeight - Round(FixedHeight * TempSize);
        if TempSize <= MinHeight then
        begin
          Height := MinHeight;
          Width := MinWidth;
          Growing := true;
          Acum:= 0;
        end else begin
          Height := Trunc(TempSize);
          Width := Height;
        end;
      end;
    end;
    CentralizeWith(FSibling.BoundsRect);
  end;
  SDL_SetRenderDrawColor(PSDLRenderer, 255, 0, 0 , 0);
  SDL_RenderDrawRect(PSDLRenderer, @FRect);
end;

procedure TAnimation.Animate(ASibling : TRectangule);
begin
  FSibling := ASibling;
  FRect := ASibling.BoundsRect;
  Inflate(10);

  AnimationData.MinHeight := ASibling.BoundsRect.h;
  AnimationData.MinWidth := ASibling.BoundsRect.w;
  AnimationData.FixedHeight := FRect.h + ((FRect.h*10) div 100);
  FVisible := True;
end;

procedure TAnimation.Join(ASample, AComparison: TRectangule;
  AGridOrientation : TGridOrientation);
begin
  Stop;

  //ASample.EdgeColor:=clInactiveCaption;
  case AGridOrientation of
    goNone : begin
        { do something }
    end;
    goTopToBottom : begin
      Top := ASample.Top -10;
      Left := ASample.Left -15;
      Height := ASample.Height + AComparison.Height + 30;
      Width := ASample.Width + 30;
    end;
    goBottomToTop : begin
      Top := AComparison.Top -10;
      Left := AComparison.Left -15;
      Height := AComparison.Height + ASample.Height + 30;
      Width := AComparison.Width + 30;
    end;
    goLeftToRight : begin
      Top := ASample.Top -15;
      Left := ASample.Left -10;
      Width := ASample.Width + AComparison.Width + 30;
      Height := ASample.Height + 30;
    end;
    goRightToLeft : begin
      Top := AComparison.Top -15;
      Left := AComparison.Left -10;
      Width := AComparison.Width + ASample.Width + 30;
      Height := AComparison.Height + 30;
    end;
  end;
end;

procedure TAnimation.Stop;
begin

  // change color
end;

constructor TAnimation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  AnimationData.Step := 0.025; // for 50 fps
  //FPenWidth := 6;
  FVisible := False;
  FSibling := nil;
end;

destructor TAnimation.Destroy;
begin

  inherited Destroy;
end;

end.

