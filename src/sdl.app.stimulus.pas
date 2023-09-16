{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.stimulus;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , fgl
  , SDL2
  , sdl.app.stimulus.contract
  , sdl.app.choiceable.contract
  , sdl.app.renderer.custom
  , sdl.app.events.abstract
  ;

type

  TChoices = specialize TFPGList<TObject>;

  { TStimulus }

  TStimulus = class(TComponent, IStimulus)
    private
      //FChoices : TChoices;
      FIndex : integer;
      FIsSample: Boolean;
      FOnMouseDown: TOnMouseEvent;
      FOnMouseEnter: TNotifyEvent;
      FOnMouseExit: TNotifyEvent;
      FOnMouseMove: TOnMouseEvent;
      FOnMouseUp: TOnMouseEvent;
      FOnResponse: TNotifyEvent;
      //function GetTargetChoice: TObject;
      procedure SetIsSample(AValue: Boolean);
      procedure SetOnMouseDown(AValue: TOnMouseEvent);
      procedure SetOnMouseEnter(AValue: TNotifyEvent);
      procedure SetOnMouseExit(AValue: TNotifyEvent);
      procedure SetOnMouseMove(AValue: TOnMouseEvent);
      procedure SetOnMouseUp(AValue: TOnMouseEvent);
      procedure SetOnResponse(AValue: TNotifyEvent);
    protected
      procedure MouseDown(Sender:TObject; Shift: TCustomShiftState; X, Y: Integer); virtual; abstract;
      procedure MouseEnter(Sender:TObject); virtual; abstract;
      procedure MouseExit(Sender:TObject); virtual; abstract;
    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      procedure Load(AParameters : TStringList;
        AParent : TObject; ARect: TSDL_Rect); virtual; abstract;
      procedure Start; virtual; abstract;
      procedure Stop; virtual; abstract;
      procedure DoResponse; virtual;
      //procedure AddOrderedChoice(AChoice: TObject);
      //property TargetChoice : TObject read GetTargetChoice;
      //property Choices : TChoices read FChoices;
      property OnMouseMove: TOnMouseEvent read FOnMouseMove write SetOnMouseMove;
      property OnMouseDown: TOnMouseEvent read FOnMouseDown write SetOnMouseDown;
      property OnMouseUp: TOnMouseEvent read FOnMouseUp write SetOnMouseUp;
      property OnMouseEnter: TNotifyEvent read FOnMouseEnter write SetOnMouseEnter;
      property OnMouseExit: TNotifyEvent read FOnMouseExit write SetOnMouseExit;
      property OnResponse : TNotifyEvent read FOnResponse write SetOnResponse;
      property IsSample : Boolean read FIsSample write SetIsSample;
      property Index : integer read FIndex write FIndex;
  end;


implementation

{ TStimulus }

//function TStimulus.GetTargetChoice: TObject;
//begin
//  if FChoices.Count > 0 then
//    Result := FChoices[0] as TObject
//  else
//    Result := nil;
//end;

procedure TStimulus.SetIsSample(AValue: Boolean);
begin
  if FIsSample=AValue then Exit;
  FIsSample:=AValue;
end;

procedure TStimulus.SetOnMouseDown(AValue: TOnMouseEvent);
begin
  if FOnMouseDown=AValue then Exit;
  FOnMouseDown:=AValue;
end;

procedure TStimulus.SetOnMouseEnter(AValue: TNotifyEvent);
begin
  if FOnMouseEnter=AValue then Exit;
  FOnMouseEnter:=AValue;
end;

procedure TStimulus.SetOnMouseExit(AValue: TNotifyEvent);
begin
  if FOnMouseExit=AValue then Exit;
  FOnMouseExit:=AValue;
end;

procedure TStimulus.SetOnMouseMove(AValue: TOnMouseEvent);
begin
  if FOnMouseMove=AValue then Exit;
  FOnMouseMove:=AValue;
end;

procedure TStimulus.SetOnMouseUp(AValue: TOnMouseEvent);
begin
  if FOnMouseUp=AValue then Exit;
  FOnMouseUp:=AValue;
end;

procedure TStimulus.SetOnResponse(AValue: TNotifyEvent);
begin
  if FOnResponse=AValue then Exit;
  FOnResponse:=AValue;
end;

constructor TStimulus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //FChoices := TChoices.Create;
  //OnMouseDown := @MouseDown;
end;

destructor TStimulus.Destroy;
begin
  inherited Destroy;
  //FChoices.Free;
end;

procedure TStimulus.DoResponse;
begin
  if Assigned(OnResponse) then
    OnResponse(Self);
end;

//procedure TStimulus.AddOrderedChoice(AChoice: TObject);
//begin
//  FChoices.Add(AChoice);
//end;

end.

