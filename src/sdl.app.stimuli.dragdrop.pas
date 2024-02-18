{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.stimuli.dragdrop;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections
  , sdl.app.stimuli.contract
  , sdl.app.stimuli
  , sdl.app.events.abstract
  , sdl.app.graphics.animation
  , sdl.app.graphics.picture.dragdrop
  , sdl.app.grids.types
  , sdl.app.grids
  , sdl.app.audio.contract
  , sdl.app.trials.types
  , sdl.app.renderer.thread
  ;

type

  TDragDropablePictures = specialize TList<TDragDropablePicture>;
  TAnimations = specialize TList<TAnimation>;

  { TDragDropStimuli }

  TDragDropStimuli = class(TStimuli)
  private
    FRenderer : TRendererThread;
    FResult : TTrialResult;
    FSoundRight : ISound;
    FSoundWrong : ISound;
    FOnDragDropDone: TNotifyEvent;
    FOnOtherDragDrop: TDragDropEvent;
    FOnRightDragDrop: TDragDropEvent;
    FOnWrongDragDrop: TDragDropEvent;
    FComparisons : TDragDropablePictures;
    FSamples : TDragDropablePictures;
    FAnimation : TAnimation;
    FDoneAnimations : TAnimations;
    FGridOrientation : TGridOrientation;
    function GetRandomSample : TDragDropablePicture;
    procedure OtherDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure RightDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SetFocus(Sender:TObject; Shift: TCustomShiftState; X, Y: Integer);
    procedure SetOnDragDropDone(AValue: TNotifyEvent);
    procedure SetOnOtherDragDrop(AValue: TDragDropEvent);
    procedure SetOnRightDragDrop(AValue: TDragDropEvent);
    procedure SetOnWrongDragDrop(AValue: TDragDropEvent);
    procedure FreeGridItems;
    procedure WrongDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Animate(ASample : TDragDropablePicture);
  protected
    function MyResult : TTrialResult; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    //procedure ResetGrid;
    function AsIStimuli : IStimuli;
    procedure DoExpectedResponse; override;
    procedure Load(AParameters : TStringList; AParent: TObject); override;
    procedure Start; override;
    procedure Stop; override;
    property OnDragDropDone : TNotifyEvent read FOnDragDropDone write SetOnDragDropDone;
    property OnRightDragDrop : TDragDropEvent read FOnRightDragDrop write SetOnRightDragDrop;
    property OnWrongDragDrop : TDragDropEvent read FOnWrongDragDrop write SetOnWrongDragDrop;
    property OnOtherDragDrop : TDragDropEvent read FOnOtherDragDrop write SetOnOtherDragDrop;
  end;

implementation

uses
  Math
  , StrUtils
  , math.bresenhamline.classes
  , sdl.app.stimuli.dragdrop.types
  , sdl.colors
  , sdl.app.controls.custom
  //, sdl.app.trials
  , sdl.app.audio
  , session.constants.dragdrop
  , session.constants.mts
  , session.pool
  , session.strutils
  ;

{ TDragDropStimuli }

procedure TDragDropStimuli.DoExpectedResponse;
begin

end;

procedure TDragDropStimuli.Load(AParameters: TStringList; AParent: TObject);
var
  S1           : string;
  SampleLetter : string;
  ComparLetter : string;
  GridSize     : Byte;
  LSamples      : integer;
  LComparisons  : integer;
  LItem : TDragDropablePicture;
  i: Integer;

  function DragDropToGridOrientation(
      ADragDropOrientation : TDragDropOrientation) : TGridOrientation;
  begin
    case ADragDropOrientation of
      None : Result := TGridOrientation.goNone;
      TopToBottom : Result := TGridOrientation.goTopToBottom;
      BottomToTop : Result := TGridOrientation.goBottomToTop;
      LeftToRight : Result := TGridOrientation.goLeftToRight;
      RightToLeft : Result := TGridOrientation.goRightToLeft;
      Random : Result:= Grid.GetRandomGridOrientation;
    end;
  end;

  procedure NewGridItems(ASamples, AComparisons: integer;
    AGridOrientation: TGridOrientation);
  var
    LItem : TDragDropablePicture;
    LComparison : TDragDropablePicture;
    LComparisons : TDragDropablePictures;
    i : integer;
  begin
    Grid.UpdatePositions(GridSize,
      ASamples, AComparisons, AGridOrientation,
      Grid.FixedSample, Grid.FixedComparison);
    with Grid.RandomPositions do begin
      LComparisons := TDragDropablePictures.Create;
      for i := low(Comparisons) to high(Comparisons) do
      begin
        LItem := TDragDropablePicture.Create;
        LItem.BoundsRect := Comparisons[i].Rect;
        LItem.Position := Comparisons[i].Position;
        LItem.IsSample := False;
        Comparisons[i].Item := LItem as TObject;
        FComparisons.Add(LItem);
        LComparisons.Add(LItem);
      end;

      for i := low(Samples) to high(Samples) do
      begin
        LItem := TDragDropablePicture.Create;
        LItem.OnMouseDown := @SetFocus;
        LItem.OnRightDragDrop:=@RightDragDrop;
        LItem.OnWrongDragDrop:=@WrongDragDrop;
        LItem.OnOtherDragDrop:=@OtherDragDrop;
        LItem.BoundsRect := Samples[i].Rect;
        LItem.SetOriginalBounds;
        LItem.Draggable := True;
        LItem.IsSample := True;
        case i of
          0 : // do nothing;

          else begin                     // making sure that we have always
            LComparisons.Exchange(0, i); // the right comparison as the first one
          end;                           // inside the sample targets
        end;
        for LComparison in LComparisons do
          LItem.AddOrderedChoice(LComparison);

        LItem.Position := Samples[i].Position;
        Samples[i].Item := LItem as TObject;
        FSamples.Add(LItem);
      end;
      LComparisons.Free;
    end;
  end;

begin
  inherited Load(AParameters, AParent);
  Grid.FixedSample := False;
  //Cursor := StrToIntDef(AParameters.Values['Cursor'], -1);
  FAnimation.Parent := TSDLControl(AParent);
  FSoundRight := SDLAudio.SoundFromName('acerto');
  FSoundWrong := SDLAudio.SoundFromName('erro');

  with DragDropKeys, MTSKeys do begin
    //ChannelDragMouseMoveFactor :=
      //AParameters.Values[DragMoveFactor].ToInteger;
    //DragMouseMoveMode :=
    //  AParameters.Values[SamplesDragMode].ToDragMouseMoveMode;

    case AParameters.Values[GridSizeKey].ToInteger of
      2..255: GridSize := Byte(AParameters.Values[GridSizeKey].ToInteger);
      otherwise
        raise Exception.Create('GridSize not supported.');
    end;

    S1 := AParameters.Values[RelationKey];
    SampleLetter := ExtractDelimited(1,S1,['-']);
    ComparLetter := ExtractDelimited(2,S1,['-']);

    LSamples := AParameters.Values[SamplesKey].ToInteger;
    LComparisons := AParameters.Values[ComparisonsKey].ToInteger;
    FGridOrientation := DragDropToGridOrientation(
      AParameters.Values[DragDropOrientationKey].ToDragDropOrientation);
  end;

  NewGridItems(LSamples, LComparisons, FGridOrientation);
  with Grid.RandomPositions do begin
    for i := low(Comparisons) to high(Comparisons) do
    begin
      LItem := Comparisons[i].Item as TDragDropablePicture;
      //LItem.Cursor := Cursor;
      LItem.LoadFromFile(AsImage(ComparLetter+(i+1).ToString));
      LItem.Parent := TSDLControl(AParent);
    end;

    for i := low(Samples) to high(Samples) do
    begin
      LItem := Samples[i].Item as TDragDropablePicture;
      //LItem.Cursor := Cursor;
      LItem.LoadFromFile(AsImage(SampleLetter+(i+1).ToString));
      //LItem.DragMouseMoveMode:=DragMouseMoveMode;
      LItem.Parent := TSDLControl(AParent);
      //with DragDropKeys do begin
      //  LItem.MoveToPoint(AParameters.Values[Distance].ToInteger);
      //end;
    end;
  end;
end;

procedure TDragDropStimuli.Start;
var
  LItem : TDragDropablePicture;
begin
  for LItem in FComparisons do LItem.Show;
  for LItem in FSamples do LItem.Show;
  LItem := GetRandomSample;
  Animate(LItem);
  FRenderer.Render;
end;

procedure TDragDropStimuli.Stop;
var
  LItem : TDragDropablePicture;
  LAnimation: TAnimation;
begin
  FRenderer.Stop;
  for LItem in FComparisons do
    LItem.Hide;
  for LItem in FSamples do
    LItem.Hide;

  FAnimation.Stop;
  FAnimation.Hide;
  for LAnimation in FDoneAnimations do
    LAnimation.Hide;
end;

procedure TDragDropStimuli.OtherDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  LSample , LSourceSample: TDragDropablePicture;
begin
  LSourceSample := Source as TDragDropablePicture;
  for LSample in FSamples do begin
    if LSample = LSourceSample then Continue;
    if LSample.IntersectsWith(LSourceSample) then begin
      LSourceSample.ToOriginalBounds;
    end;
  end;
  //LItem.Color := clWhite;
  if Assigned(OnOtherDragDrop) then
    OnOtherDragDrop(Sender, Source, X, Y);
end;

procedure TDragDropStimuli.RightDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  Sample : TDragDropablePicture;
  Comparison : TDragDropablePicture;
  LAnimation : TAnimation;
  FDragDropDone : Boolean = False;
  S1 : string;
begin
  FSoundRight.Play;
  Sample := Source as TDragDropablePicture;
  Comparison := Sender as TDragDropablePicture;

  //Sample.Color := clGreen;
  case FGridOrientation of
    TGridOrientation.goTopToBottom : begin
      Sample.Left := Comparison.Left;
      Sample.Top := Comparison.Top - Sample.Height - 10;
    end;
    TGridOrientation.goBottomToTop : begin
      Sample.Left := Comparison.Left;
      Sample.Top := Comparison.Top + Sample.Height + 10;
    end;
    TGridOrientation.goLeftToRight : begin
      Sample.Left := Comparison.Left - Sample.Width - 10;
      Sample.Top := Comparison.Top;
    end;
    TGridOrientation.goRightToLeft : begin
      Sample.Left := Comparison.Left + Sample.Width + 10;
      Sample.Top := Comparison.Top;
    end;
    otherwise begin
      WriteStr(S1, FGridOrientation);
      raise Exception.Create(
        'TDragDropStimuli.RightDragDrop: ' + S1);
    end;
  end;

  LAnimation := TAnimation.Create;
  //LAnimation.Cursor:=Cursor;
  LAnimation.Join(Sample, Comparison, FGridOrientation);
  LAnimation.Show;
  FDoneAnimations.Add(LAnimation);

  Sample.Draggable:=False;

  for Sample in FSamples do
    if Sample.Draggable then begin
      FDragDropDone := False;
      Animate(Sample);
      Break;
    end else begin
      //Sample.EdgeColor:=clInactiveCaption;
      FDragDropDone := True;
    end;

  if Assigned(OnRightDragDrop) then
    OnRightDragDrop(Sender, Source, X, Y);

  if FDragDropDone then begin
    //FResult := Hit; todo: dragdrop trial have different hit types
    Pool.Counters.Hit;
    FResult := Hit;
    FAnimation.Stop;
    FAnimation.Hide;
    if Assigned(OnFinalize) then begin

      OnFinalize(Self);
    end;
  end;
end;

procedure TDragDropStimuli.SetFocus(Sender:TObject; Shift: TCustomShiftState; X, Y: Integer);
var
  LSample : TDragDropablePicture;
begin
  LSample := Sender as TDragDropablePicture;
  if Assigned(OnResponse) then begin
    OnResponse(LSample);
  end;

  if LSample.Draggable then begin
    if (FAnimation.Sibling as TDragDropablePicture) <> LSample then begin
      if Assigned(FAnimation.Sibling) then begin
        FAnimation.Sibling.EdgeColor:=clGray;
      end;
      Animate(LSample);
    end;
  end;
end;

procedure TDragDropStimuli.SetOnDragDropDone(AValue: TNotifyEvent);
begin
  if FOnDragDropDone=AValue then Exit;
  FOnDragDropDone:=AValue;
end;

procedure TDragDropStimuli.SetOnOtherDragDrop(AValue: TDragDropEvent);
begin
  if FOnOtherDragDrop=AValue then Exit;
  FOnOtherDragDrop:=AValue;
end;

procedure TDragDropStimuli.SetOnRightDragDrop(AValue: TDragDropEvent);
begin
  if FOnRightDragDrop=AValue then Exit;
  FOnRightDragDrop:=AValue;
end;

procedure TDragDropStimuli.SetOnWrongDragDrop(AValue: TDragDropEvent);
begin
  if FOnWrongDragDrop=AValue then Exit;
  FOnWrongDragDrop:=AValue;
end;

function TDragDropStimuli.GetRandomSample: TDragDropablePicture;
begin
  Result := FSamples[RandomRange(0, FSamples.Count)];
end;

procedure TDragDropStimuli.FreeGridItems;
var
  i: Integer;
begin
  with Grid.RandomPositions do begin
    for i := low(Comparisons) to high(Comparisons) do begin
      Comparisons[i].Item.Free;
    end;

    for i := low(Samples) to high(Samples) do begin
      Samples[i].Item.Free
    end;
  end;
end;

procedure TDragDropStimuli.WrongDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  Sample : TDragDropablePicture;
begin
  FSoundWrong.Play;
  Sample := Source as TDragDropablePicture;
  Sample.ToOriginalBounds;
  //FAnimation.Animate(Sample);
  if Assigned(OnWrongDragDrop) then begin
    OnWrongDragDrop(Sender, Source, X, Y);
  end;
end;

procedure TDragDropStimuli.Animate(ASample: TDragDropablePicture);
begin
  //FAnimation.Cursor := Cursor;
  //ASample.UpdateDragMouseMoveMode;
  FAnimation.Animate(ASample);
  FAnimation.Show;
  FAnimation.BringToFront;
  ASample.BringToFront;
  //Parent.Invalidate;
end;

function TDragDropStimuli.MyResult: TTrialResult;
begin
  Result := FResult;
end;

constructor TDragDropStimuli.Create;
begin
  inherited Create;
  DragDropLine := TBresenhamLine.Create;
  FSamples := TDragDropablePictures.Create;
  FComparisons := TDragDropablePictures.Create;
  FAnimation := TAnimation.Create;
  FDoneAnimations := TAnimations.Create;
  FRenderer := TRendererThread.Create;
end;

destructor TDragDropStimuli.Destroy;
var
  LDragDropablePicture : TDragDropablePicture;
  LAnimation : TAnimation;
begin
  FRenderer.Close;
  FRenderer.Terminate;
  DragDropLine.Free;
  FAnimation.Free;

  for LAnimation in FDoneAnimations do begin
    LAnimation.Free;
  end;
  FDoneAnimations.Free;

  for LDragDropablePicture in FSamples do begin
    LDragDropablePicture.Free;
  end;
  FSamples.Free;

  for LDragDropablePicture in FComparisons do begin
    LDragDropablePicture.Free;
  end;
  FComparisons.Free;
  inherited Destroy;
end;

function TDragDropStimuli.AsIStimuli: IStimuli;
begin
  Result := Self as IStimuli;
end;

end.
