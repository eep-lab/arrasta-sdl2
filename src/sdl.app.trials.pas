{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.trials;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , SDL2
  , sdl.timer
  , sdl.app.renderer.custom
  , sdl.app.trials.contract
  , sdl.app.stimuli.contract
  , sdl.app.events.abstract
  , sdl.app.events.custom
  , sdl.app.graphics.text
  , session.configuration
  , eye.tracker.types
  ;

type

  { TTrial }

  TTrial = class(TCustomRenderer, ITrial)
    private
      FText : TText;
      FParent : TCustomRenderer;
      FLimitedHoldTimer    : TSDLTimer;
      FTestMode: Boolean;
      FVisible: Boolean;
      FIStimuli : IStimuli;
      FICalibration : IStimuli;
      FIInstruction : IStimuli;
      procedure SetParent(AValue: TCustomRenderer);
      procedure SetTestMode(AValue: Boolean);
      procedure EndStarterCallBack(Sender : TObject);
      procedure CreateStartersIfRequired(AParameters : TStringList);
      procedure GazeOnScreen(Sender : TObject;  AGazes : TGazes);
    protected
      FOnTrialEnd : TNotifyEvent;
      FData : TTrialData;
      procedure Paint; override;
      procedure EndTrialCallBack(Sender : TObject);
      procedure MouseMove(Sender:TObject; Shift: TCustomShiftState; X, Y: Integer); override;
      procedure MouseDown(Sender:TObject; Shift: TCustomShiftState; X, Y: Integer); override;
      procedure MouseUp(Sender:TObject; Shift: TCustomShiftState; X, Y: Integer); override;
      procedure SetOnTrialEnd(ANotifyEvent: TNotifyEvent);
      procedure SetTrialData(ATrialData: TTrialData); virtual;
      function GetOnTrialEnd: TNotifyEvent;
      function GetTrialData: TTrialData;
      function GetIStimuli : IStimuli; virtual; abstract;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      function ConsequenceDelay: Cardinal; virtual;
      function ConsequenceInterval: Cardinal; virtual;
      function InterTrialInterval : Cardinal; virtual;
      function AsITrial : ITrial;
      procedure EndTrial; virtual;
      procedure Show; virtual;
      procedure Hide; virtual;
      procedure DoExpectedResponse;
      property Visible : Boolean read FVisible;
      property Data : TTrialData read GetTrialData write SetTrialData;
      property OnTrialEnd : TNotifyEvent read GetOnTrialEnd write SetOnTrialEnd;
      property TestMode : Boolean read FTestMode write SetTestMode;
      property Parent : TCustomRenderer read FParent write SetParent;
  end;

const
  SESSION_TRIALEND = SDL_USEREVENT+0;

implementation

uses session.constants.trials
  , sdl.app.stimuli.instruction
  , sdl.app.stimuli.calibration.pupil
  , sdl.app.paintable.contract
  , sdl.app.clickable.contract
  , sdl.app.moveable.contract
  , sdl.app.lookable.contract
  , session.loggers.writerow.timestamp;

{ TTrial }

constructor TTrial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  EventHandler.AssignEvents;
  EventHandler.OnMouseButtonDown := AsIClickable.GetSDLMouseButtonDown;
  EventHandler.OnMouseButtonUp := AsIClickable.GetSDLMouseButtonUp;
  EventHandler.OnMouseMotion := AsIMoveable.GetSDLMouseMotion;
  EventHandler.OnGazeOnScreen := @GazeOnScreen;
  FICalibration := nil;
  FIInstruction := nil;
  FVisible := False;
  FTestMode := False;
  FLimitedHoldTimer := TSDLTimer.Create;
  FLimitedHoldTimer.Interval := 0;
end;

destructor TTrial.Destroy;
begin
  EventHandler.OnMouseButtonDown := nil;
  EventHandler.OnMouseButtonUp := nil;
  EventHandler.OnMouseMotion := nil;
  EventHandler.OnUserEvent:=nil;
  FLimitedHoldTimer.Free;
  inherited Destroy;
end;

function TTrial.ConsequenceDelay: Cardinal;
begin
  Result := 0;
end;

function TTrial.ConsequenceInterval: Cardinal;
begin
  Result := 0;
end;

function TTrial.InterTrialInterval: Cardinal;
begin
  Result := 0;
end;

function TTrial.AsITrial: ITrial;
begin
  Result := Self as ITrial;
end;

procedure TTrial.MouseMove(Sender: TObject; Shift: TCustomShiftState; X,
  Y: Integer);
var
  Child : TComponent;
  SDLPoint : TSDL_Point;
  IChild : IMoveable;
begin
  if FVisible then begin
    for Child in FChilds do begin
      SDLPoint.x := X;
      SDLPoint.y := Y;
      IChild := IMoveable(TCustomRenderer(Child));
      if IChild.PointInside(SDLPoint) then begin
        if not IChild.MouseInside then begin
          IChild.MouseInside:=True;
          IChild.MouseEnter(Sender);
        end;
        IChild.MouseMove(Sender, Shift, X, Y);
      end else begin
        if IChild.MouseInside then begin
          IChild.MouseInside:=False;
          IChild.MouseExit(Sender);
        end;
      end;
    end;
  end;
end;

procedure TTrial.MouseDown(Sender:TObject; Shift: TCustomShiftState; X, Y: Integer);
var
  Child : TComponent;
  SDLPoint : TSDL_Point;
  IChild : IClickable;
begin
  if FVisible then begin
    for Child in FChilds do begin
      SDLPoint.x := X;
      SDLPoint.y := Y;
      IChild := IClickable(TCustomRenderer(Child));
      if IChild.PointInside(SDLPoint) then
        IChild.MouseDown(Sender, Shift, X, Y);
    end;
  end;
end;

procedure TTrial.MouseUp(Sender: TObject; Shift: TCustomShiftState; X,
  Y: Integer);
var
  Child : TComponent;
  SDLPoint : TSDL_Point;
  IChild   : IClickable;
begin
  if FVisible then begin
    for Child in FChilds do begin
      SDLPoint.x := X;
      SDLPoint.y := Y;
      IChild := IClickable(TCustomRenderer(Child));
      if IChild.PointInside(SDLPoint) then
        IChild.MouseUp(Sender, Shift, X, Y);
    end;
  end;
end;

procedure DoEndTrial(PSelf: Pointer);
var
  event : TSDL_Event;
begin
  event.type_ := SESSION_TRIALEND;
  event.user.data1 := PSelf;
  SDL_PushEvent(@event);
end;

procedure TTrial.EndTrial;
begin
  Hide;
  DoEndTrial(Pointer(Self));
end;

procedure TTrial.EndTrialCallBack(Sender: TObject);
var
  LStimuli : TComponent;
  //LTrial    : TComponent;
begin
  if Sender is TComponent then begin
    LStimuli := Sender as TComponent;
    if LStimuli.Owner.Name = Self.Name then
      EndTrial;
  end;
end;

// todo: refactor starters as an IStimuliList, call next stimuli here...
procedure TTrial.EndStarterCallBack(Sender: TObject);
begin
  if Sender is TInstructionStimuli then begin
    TInstructionStimuli(Sender).Stop;
    if Assigned(FICalibration) then begin
      FICalibration.Start;
    end else begin
      FIStimuli := GetIStimuli;
      Show;
    end;
  end;

  if Sender is TPupilCalibrationStimuli then begin
    TPupilCalibrationStimuli(Sender).Stop;
    FIStimuli := GetIStimuli;
    Show;
  end;
end;


// todo: refactor starters as an IStimuliList to allow many instructions ...
procedure TTrial.CreateStartersIfRequired(AParameters: TStringList);
var
  LInstruction : TInstructionStimuli;
  LCalibration : TPupilCalibrationStimuli;
begin
  with AParameters, TrialKeys do begin
    if StrToBoolDef(Values[InstructionKey], False) then begin
      LInstruction := TInstructionStimuli.Create(Self);
      LInstruction.OnFinalize := @EndStarterCallBack;
      FIInstruction := LInstruction;
      FIInstruction.Load(AParameters, Self);
      FIStimuli := FIInstruction;
    end;
    if StrToBoolDef(Values[DoCalibrationKey], False) then begin
      LCalibration := TPupilCalibrationStimuli.Create(Self);
      LCalibration.OnFinalize := @EndStarterCallBack;
      FICalibration := LCalibration;
      FICalibration.Load(AParameters, Self);
      FIStimuli := LCalibration;
    end;
  end;
end;

procedure TTrial.GazeOnScreen(Sender: TObject; AGazes: TGazes);
var
  Child : TComponent;
  SDLPoint : TSDL_Point;
  IChild : ILookable;
  i: Integer;
begin
  if FVisible then begin
    if Length(AGazes) > 0 then begin
      for i := Low(AGazes) to High(AGazes) do begin
        for Child in FChilds do begin
          SDLPoint.x := AGazes[i].X;
          SDLPoint.y := AGazes[i].Y;
          IChild := ILookable(TCustomRenderer(Child));
          if IChild.PointInside(SDLPoint) then begin
            if not IChild.GazeInside then begin
              IChild.GazeInside:=True;
              IChild.GazeEnter(Sender);
            end;
            IChild.GazeMove(Sender, GetShiftState, AGazes[i].X, AGazes[i].Y);
          end else begin
            if IChild.GazeInside then begin
              IChild.GazeInside:=False;
              IChild.GazeExit(Sender);
            end;
          end;
        end;
      end;
    end;
  end;
end;


procedure TTrial.SetParent(AValue: TCustomRenderer);
begin
  if FParent = AValue then Exit;
  FParent := AValue;
end;

procedure TTrial.SetTestMode(AValue: Boolean);
begin
  if FTestMode = AValue then Exit;
  FTestMode := AValue;
  FText := TText.Create(Self);
  FText.FontName := 'Raleway-Regular';
  FText.FontSize := 50;
  FText.Load(Name);
  FText.Parent := Self;
  FText.Centralize;
end;

procedure TTrial.Paint;
var
  Child : TComponent;
begin
  if FVisible then begin
    for Child in FChilds do
      IPaintable(TCustomRenderer(Child)).Paint;
  end;
end;

procedure TTrial.SetOnTrialEnd(ANotifyEvent: TNotifyEvent);
begin
  if FOnTrialEnd = ANotifyEvent then Exit;
  FOnTrialEnd := ANotifyEvent;
end;

procedure TTrial.SetTrialData(ATrialData: TTrialData);
var
  Parameters : TStringList;
begin
  FData := ATrialData;
  Parameters := FData.Parameters;
  if Assigned(Parameters) then begin
    FIStimuli := GetIStimuli;
    FIStimuli.Load(Parameters, Self);
    with Parameters, TrialKeys do begin
      if StrToIntDef(Values[LimitedHoldKey], 0) > 0 then begin
        FLimitedHoldTimer.Interval := Values[LimitedHoldKey].ToInteger;
      end;
    end;
    CreateStartersIfRequired(Parameters);
  end else begin
    raise Exception.Create('TTrial.SetTrialData: Parameters not assigned.');
  end;
end;

function TTrial.GetOnTrialEnd: TNotifyEvent;
begin
  Result := FOnTrialEnd;
end;

function TTrial.GetTrialData: TTrialData;
begin
  Result := FData;
end;

procedure TTrial.Show;
begin
  if TestMode then begin
    DoExpectedResponse;
  end else begin
    FIStimuli.Start;
    FLimitedHoldTimer.Start;
    FVisible := True;
    Timestamp(FIStimuli.CustomName+'.Show');
  end;
end;

procedure TTrial.Hide;
begin
  if TestMode then begin
    // FVisible := False;
  end else begin
    FVisible := False;
    Timestamp(FIStimuli.CustomName+'.Hide');
    FIStimuli.Stop;
    FLimitedHoldTimer.Stop;
  end;
end;

// test mode
procedure TTrial.DoExpectedResponse;
begin
  FVisible := True;
  FText.Show;
  FIStimuli.DoExpectedResponse;
end;

end.

