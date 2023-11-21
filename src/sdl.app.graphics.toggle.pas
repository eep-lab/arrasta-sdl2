{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.graphics.toggle;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , SDL2
  , sdl.app.graphics.rectangule
  , sdl.app.paintable.contract
  , sdl.app.events.abstract
  , timestamps.types
  ;

type

  TAnimationData = record
    Acum: TLargerFloat;
    Growing: boolean;
    Step: TLargerFloat;
    Alpha : byte;
    FixedAlpha : integer;
    MinAlpha : integer;
    MaxAlpha : integer;
  end;

  { TToggleButton }

  TToggleButton = class(TRectangule, IPaintable)
  private
    FEnabled: Boolean;
    FOwner: TObject;
    FShaded : Boolean;
    FCanShade : Boolean;
    FSibling: TToggleButton;
    FTexture1  : PSDL_Texture;
    FTexture2  : PSDL_Texture;
    FIsTexture1 : Boolean;
    FAnimationData : TAnimationData;
    procedure SetEnabled(AValue: Boolean);
    procedure SetOwner(AValue: TObject);
    procedure SetSibling(AValue: TToggleButton);
  protected
    procedure SetBoundsRect(AValue : TSDL_Rect); override;
    procedure MouseMove(Sender: TObject; Shift: TCustomShiftState;
      X, Y: Integer); override;
    procedure MouseDown(Sender: TObject; Shift: TCustomShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Sender: TObject; Shift: TCustomShiftState;
      X, Y: Integer); override;
    procedure MouseEnter(Sender: TObject); override;
    procedure MouseExit(Sender: TObject); override;
    procedure Paint; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromFile(AFilename1, AFilename2: string); virtual;
    procedure Toggle;
    property Owner : TObject read FOwner write SetOwner;
    property Sibling : TToggleButton read FSibling write SetSibling;
    property Enabled : Boolean read FEnabled write SetEnabled;
  end;

const
  IMG_EXT = '.png';

implementation

uses
  sdl2_image
  , sdl.colors
  , sdl.app.video.methods
  , sdl.app.output
  , session.pool
  , sdl.app.testmode
  ;

{ TToggleButton }

constructor TToggleButton.Create;
begin
  FSibling := nil;
  FEnabled := True;
  FCanShade := True;
  FIsTexture1 := True;
  FAnimationData.Step := 0.025;
  FAnimationData.MinAlpha := 0;
  FAnimationData.MaxAlpha := 255;
  FAnimationData.FixedAlpha := 125;
end;

destructor TToggleButton.Destroy;
begin
  SDL_DestroyTexture(FTexture1);
  SDL_DestroyTexture(FTexture2);
  inherited Destroy;
end;

procedure TToggleButton.SetSibling(AValue: TToggleButton);
begin
  if FSibling=AValue then Exit;
  FSibling:=AValue;
  CentralizeAtTopWith(FSibling.BoundsRect);
end;

procedure TToggleButton.SetEnabled(AValue: Boolean);
begin
  if FEnabled=AValue then Exit;
  FEnabled:=AValue;
  if FEnabled then begin
    FCanShade := True;
    FIsTexture1 := True;
  end else begin
    FCanShade := False;
    FIsTexture1 := True;
  end;
end;

procedure TToggleButton.SetOwner(AValue: TObject);
begin
  if FOwner = AValue then Exit;
  FOwner := AValue;
end;

procedure TToggleButton.SetBoundsRect(AValue: TSDL_Rect);
begin
  inherited SetBoundsRect(AValue);
  Height := Height div 3;
  CentralizeWith(AValue);
end;

procedure TToggleButton.MouseMove(Sender: TObject; Shift: TCustomShiftState; X,
  Y: Integer);
begin
  if Visible then
    inherited MouseMove(Self, Shift, X, Y);
end;

procedure TToggleButton.MouseDown(Sender: TObject; Shift: TCustomShiftState; X,
  Y: Integer);
begin
  if Visible and Enabled then
    inherited MouseDown(Self, Shift, X, Y);
end;

procedure TToggleButton.MouseUp(Sender: TObject; Shift: TCustomShiftState; X,
  Y: Integer);
begin
  if Visible and Enabled then begin
    inherited MouseUp(Self, Shift, X, Y);

  end;
end;

procedure TToggleButton.MouseEnter(Sender: TObject);
begin
  if Visible then begin
    inherited MouseEnter(Self);
    if FCanShade then begin
      FShaded := True;
    end;
  end;
end;

procedure TToggleButton.MouseExit(Sender: TObject);
begin
  if Visible then begin
    inherited MouseExit(Self);
    if FCanShade then begin
      FShaded := False;
    end;
  end;
end;

procedure TToggleButton.Paint;
var
  TempSize: TLargerFloat;
  function easeInOutQuad(t: TLargerFloat): TLargerFloat;
  begin
    if t < 0.5 then
      Result := 2 * t * t
    else
      Result := -1 + (4 - 2 * t) * t;
  end;

begin
  if Visible then begin
    SDL_SetRenderDrawBlendMode(PSDLRenderer, SDL_BLENDMODE_BLEND);
    if FIsTexture1 then begin
      if FEnabled then begin
        with clLightRedShaded1 do
          SDL_SetRenderDrawColor(PSDLRenderer, r, g, b, a);
        if FShaded then
          SDL_RenderFillRect(PSDLRenderer, @FRect);
        SDL_RenderCopy(PSDLRenderer, FTexture1, nil, @FRect);
      end else begin
        SDL_RenderCopy(PSDLRenderer, FTexture1, nil, @FRect);
        with clLightGrayShaded1 do
          SDL_SetRenderDrawColor(PSDLRenderer, r, g, b, a);
        SDL_RenderFillRect(PSDLRenderer, @FRect);
      end;
    end else begin
      with FAnimationData do begin
        if Step > 1 then
           Step := 1;
        Acum := Acum + Step;
        TempSize := easeInOutQuad(Acum);
        if Growing then begin
          Alpha := Round(FixedAlpha * TempSize);
          if Alpha >= FixedAlpha then
          begin
            Alpha := FixedAlpha;
            Growing := False;
            Acum:= 0;
          end;
        end else begin
          TempSize := FixedAlpha - Round(FixedAlpha * TempSize);
          if TempSize <= MinAlpha then
          begin
            Alpha := MinAlpha;
            Growing := true;
            Acum:= 0;
          end else begin
            Alpha := Trunc(TempSize);
          end;
        end;
      end;
      with clLightRedShaded1 do
        with FAnimationData do
          SDL_SetRenderDrawColor(PSDLRenderer, r, g, b, Alpha);
      SDL_RenderFillRect(PSDLRenderer, @FRect);
      SDL_RenderCopy(PSDLRenderer, FTexture2, nil, @FRect);
    end;
  end else begin
    if TestMode then begin
      with clGray do
        SDL_SetRenderDrawColor(PSDLRenderer, r, g, b, a);
      SDL_RenderFillRect(PSDLRenderer, @FRect);
    end;
  end;
end;

procedure TToggleButton.LoadFromFile(AFilename1, AFilename2: string);
var
  Media : PAnsiChar;
begin
  Media := PAnsiChar(Pool.RootMedia+AFilename1);
  FTexture1 := IMG_LoadTexture(PSDLRenderer, Media);
  Media := PAnsiChar(Pool.RootMedia+AFilename2);
  FTexture2 := IMG_LoadTexture(PSDLRenderer, Media);
end;

procedure TToggleButton.Toggle;
begin
  if FEnabled then begin
    FIsTexture1 := not FIsTexture1;
    FAnimationData.Acum := 0;
  end;
end;

end.

