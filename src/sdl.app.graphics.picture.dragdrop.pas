{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.graphics.picture.dragdrop;

{$mode ObjFPC}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils
  , sdl.app.graphics.picture
  , sdl.app.choiceable.rectangule
  , sdl.app.dragdropable.contract
  , sdl.app.events.abstract
  ;

type

  { TDragDropablePicture }

  TDragDropablePicture = class(TPicture, IDragDropable)
  private
    FDraggable: Boolean;
    FIsDragging : Boolean;
    FOnOtherDragDrop: TDragDropEvent;
    FOnRightDragDrop: TDragDropEvent;
    FOnWrongDragDrop: TDragDropEvent;
    FOffSet : TPoint;
    function IsDragging : Boolean;
    procedure SetDraggable(AValue: Boolean);
    procedure SetOnOtherDragDrop(AValue: TDragDropEvent);
    procedure SetOnRightDragDrop(AValue: TDragDropEvent);
    procedure SetOnWrongDragDrop(AValue: TDragDropEvent);
  protected
    procedure DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure MouseMove(Sender: TObject; Shift: TCustomShiftState; X, Y: Integer); override;
    procedure MouseDown(Sender: TObject; Shift: TCustomShiftState; X, Y: Integer); override;
    procedure MouseUp(Sender: TObject; Shift: TCustomShiftState; X, Y: Integer); override;
    procedure MouseEnter(Sender: TObject); override;
    procedure MouseExit(Sender: TObject); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnRightDragDrop : TDragDropEvent read FOnRightDragDrop write SetOnRightDragDrop;
    property OnWrongDragDrop : TDragDropEvent read FOnWrongDragDrop write SetOnWrongDragDrop;
    property OnOtherDragDrop : TDragDropEvent read FOnOtherDragDrop write SetOnOtherDragDrop;
    property Draggable : Boolean read FDraggable write SetDraggable;
  end;

implementation

//uses SDL2, math.bresenhamline.classes;

{ TChoiceablePicture }

procedure TDragDropablePicture.SetOnOtherDragDrop(AValue: TDragDropEvent);
begin
  if FOnOtherDragDrop=AValue then Exit;
  FOnOtherDragDrop:=AValue;
end;

function TDragDropablePicture.IsDragging: Boolean;
begin
  Result := FIsDragging;
end;

procedure TDragDropablePicture.SetDraggable(AValue: Boolean);
begin
  if FDraggable=AValue then Exit;
  FDraggable:=AValue;
end;

procedure TDragDropablePicture.SetOnRightDragDrop(AValue: TDragDropEvent);
begin
  if FOnRightDragDrop=AValue then Exit;
  FOnRightDragDrop:=AValue;
end;

procedure TDragDropablePicture.SetOnWrongDragDrop(AValue: TDragDropEvent);
begin
  if FOnWrongDragDrop=AValue then Exit;
  FOnWrongDragDrop:=AValue;
end;

procedure TDragDropablePicture.DragDrop(Sender, Source: TObject; X, Y: Integer);
begin

end;

procedure TDragDropablePicture.MouseDown(Sender: TObject;
  Shift: TCustomShiftState; X, Y: Integer);
begin
  if FDraggable then begin
    if not FIsDragging then begin
      //if ssLeft in Shift then begin
        FOffSet.X := X - Left;
        FOffSet.Y := Y - Top;
        FIsDragging := True;
      //end;
    end;
  end;
  inherited MouseDown(Self, Shift, X, Y);
end;

procedure TDragDropablePicture.MouseMove(Sender: TObject;
  Shift: TCustomShiftState; X, Y: Integer);
begin
  if Draggable then begin
    if FIsDragging then begin
      Left := X - FOffSet.X;
      Top  := Y - FOffSet.Y;
      //BorderColision;
    end;
  end;
  inherited MouseMove(Self, Shift, X, Y);
end;

procedure TDragDropablePicture.MouseUp(Sender: TObject;
  Shift: TCustomShiftState; X, Y: Integer);
var
  i : integer;
  LTarget : TDragDropablePicture;
  LIntersected : Boolean;
begin
  if FDraggable then begin
    //if ssLeft in Shift then begin
      FIsDragging := False;
      for i := 0 to Choices.Count -1 do begin
        if Choices[i] is TDragDropablePicture then begin
          LTarget := Choices[i] as TDragDropablePicture;
          LIntersected := IntersectsWith(LTarget.BoundsRect);
          if LIntersected then Break;
        end;
      end;

      if LIntersected then begin
        case i of
          0 :
            if Assigned(OnRightDragDrop) then
              OnRightDragDrop(LTarget, Self, X, Y);

          else
            if Assigned(OnWrongDragDrop) then
              OnWrongDragDrop(LTarget, Self, X, Y);
        end
      end else begin
        if Assigned(OnOtherDragDrop) then
          OnOtherDragDrop(nil, Self, X, Y);
      end;
    //end;
  end;
  inherited MouseUp(Sender, Shift, X, Y);
end;

procedure TDragDropablePicture.MouseEnter(Sender: TObject);
begin
  inherited MouseEnter(Self);
end;

procedure TDragDropablePicture.MouseExit(Sender: TObject);
begin
  inherited MouseExit(Self);
end;

procedure TDragDropablePicture.Paint;
begin
  inherited Paint;
end;

constructor TDragDropablePicture.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TDragDropablePicture.Destroy;
begin

  inherited Destroy;
end;


end.

