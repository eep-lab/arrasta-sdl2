{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.stimuli.mts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , fgl
  , sdl.app.graphics.button
  , sdl.app.stimuli.contract
  , sdl.app.stimuli
  , sdl.app.stimulus.contract
  , sdl.app.stimulus
  , sdl.app.events.abstract
  , sdl.app.audio.contract
  ;

type

  TModality = (ModalityNone, ModalityA, ModalityB, ModalityC, ModalityD);

  TMTSModality = record
    Samples     : TModality;
    Comparisons : TModality;
  end;

  TCustomStimulusList = specialize TFPGList<TStimulus>;

  { TMTSStimuli }

  TMTSStimuli = class sealed (TStimuli, IStimuli)
    private
      FMTSModality : TMTSModality;
      FHasConsequence : Boolean;
      FButton : TButton;
      FSoundCorrect : ISound;
      FSoundWrong   : ISound;
      FComparisons : TCustomStimulusList;
      FSamples : TCustomStimulusList;
      procedure DoConsequence(Sender : TObject);
      procedure ConsequenceDone(Sender: TObject);
      procedure ConsequenceStart(Sender: TObject);
      procedure StimulusMouseEnter(Sender: TObject);
      procedure StimulusMouseExit(Sender: TObject);
      procedure StimulusMouseDown(Sender: TObject; Shift: TCustomShiftState; X, Y: Integer);
      procedure StimulusMouseUp(Sender: TObject; Shift: TCustomShiftState; X, Y: Integer);
      procedure ButtonClick(Sender: TObject);
      procedure ComparisonResponse(Sender: TObject);
      procedure SampleResponse(Sender: TObject);
    public
      constructor Create; override;
      destructor Destroy; override;
      function AsInterface : IStimuli;
      procedure DoExpectedResponse; override;
      procedure Load(AParameters : TStringList;
        AParent : TObject); override;
      procedure Start; override;
      procedure Stop; override;
  end;

implementation

uses
  StrUtils
  , math
  , sdl.app.renderer.custom
  , sdl.app.graphics.picture
  , sdl.app.output
  , sdl.app.audio
  , sdl.app.grids.types
  , sdl.app.grids
  , sdl.app.stimulus.factory
  , sdl.app.stimulus.audio
  , sdl.app.stimulus.speech
  , session.loggers.writerow
  , session.loggers.writerow.timestamp
  , session.constants.trials
  , session.constants.mts
  , session.pool
  ;

{ TMTSStimuli }

procedure TMTSStimuli.DoConsequence(Sender: TObject);
var
  LStimulus : TStimulus;
  LIsHit : Boolean = False;
begin
  if Sender is TStimulus then begin
    LStimulus := Sender as TStimulus;

    if LStimulus is TSpeechStimulus then begin
      LIsHit := LStimulus.IsCorrectResponse;
    end else begin
      LIsHit := LStimulus = FComparisons[0];
    end;

    if LIsHit then begin
      Pool.Counters.Hit;
    end else begin
      Pool.Counters.Miss;
    end;

    if FHasConsequence then begin
      if LIsHit then begin
        FSoundCorrect.Play;
      end else begin
        FSoundWrong.Play;
      end;
    end else begin
      if Assigned(OnFinalize) then
        OnFinalize(Self);
    end;
  end;
end;

procedure TMTSStimuli.ConsequenceDone(Sender: TObject);
begin
  if (Sender as ISound) = FSoundCorrect then begin
    Timestamp('Hit.Stop');
  end;

  if (Sender as ISound) = FSoundWrong then begin
    Timestamp('Miss.Stop');
  end;

  FSoundCorrect.SetOnStop(nil);
  FSoundWrong.SetOnStop(nil);
  if Assigned(OnFinalize) then
    OnFinalize(Self);
end;

procedure TMTSStimuli.ConsequenceStart(Sender: TObject);
begin
  if (Sender as ISound) = FSoundCorrect then begin
    Timestamp('Hit.Start');
  end;

  if (Sender as ISound) = FSoundWrong then begin
    Timestamp('Miss.Start');
  end;
end;

procedure TMTSStimuli.StimulusMouseEnter(Sender: TObject);
begin
  //IStimulus(Sender as TStimulus).Start;
end;

procedure TMTSStimuli.StimulusMouseExit(Sender: TObject);
begin
  //IStimulus(Sender as TStimulus).Stop;
end;

procedure TMTSStimuli.StimulusMouseDown(Sender: TObject;
  Shift: TCustomShiftState; X, Y: Integer);
var
  LStimulus: TStimulus;
begin
  if (Sender is TAudioStimulus) then begin
    LStimulus := Sender as TStimulus;
    if not LStimulus.IsSample then begin
      FButton.Hide;
    end;
  end;
end;

procedure TMTSStimuli.StimulusMouseUp(Sender: TObject;
  Shift: TCustomShiftState; X, Y: Integer);
begin

end;

procedure TMTSStimuli.ButtonClick(Sender: TObject);
var
  LStimulus: TStimulus;
begin
  if Sender = FButton then begin
    if FButton.Sender <> nil then begin
      if FButton.Sender is TStimulus then begin
        LStimulus := FButton.Sender as TStimulus;
        DoConsequence(LStimulus);
      end else begin
        raise Exception.Create('TMTSStimuli.ButtonClick');
      end;
    end;
  end;
end;

procedure TMTSStimuli.ComparisonResponse(Sender: TObject);
var
  LStimulus : TStimulus;
  LIStimulus : IStimulus;
begin
  if Sender is TStimulus then begin
    //Timestamp('Comparison.Response');
    LStimulus := Sender as TStimulus;
    if (LStimulus is TSpeechStimulus) or
       (LStimulus is TAudioStimulus) then begin
      FButton.Sibling := LStimulus.Rectangule;
      FButton.Sender := Sender;
      FButton.Show;
      Exit;
    end;

    for LIStimulus in FComparisons do begin
      LIStimulus.Stop;
    end;

    DoConsequence(Sender as TStimulus);
  end;
end;

procedure TMTSStimuli.SampleResponse(Sender: TObject);
var
  LStimulus: IStimulus;
begin
  //if FIsDMTS then
  //for LStimulus in FSamples do begin
  //  LStimulus.Stop;
  //end;

  for LStimulus in FComparisons do begin
    LStimulus.Start;
  end;
  Timestamp('Comparison.Start');
end;


constructor TMTSStimuli.Create;
begin
  inherited Create;
  FButton := TButton.Create;
  FSamples := TCustomStimulusList.Create;
  FComparisons := TCustomStimulusList.Create;
  FMTSModality.Samples := ModalityNone;
  FMTSModality.Comparisons := ModalityNone;

  TStimulusFactory.Clear;
end;

destructor TMTSStimuli.Destroy;
var
  i : integer;
begin
  TStimulusFactory.Clear;
  FButton.Free;
  FSamples.Free;
  FComparisons.Free;
  inherited Destroy;
end;

function TMTSStimuli.AsInterface: IStimuli;
begin
  Result := Self as IStimuli;
end;

procedure TMTSStimuli.DoExpectedResponse;
begin
  // for real time simulations
  //case High(FComparisons) of
  //  0 : FComparisons[0].DoResponse(False);
  //  else begin
  //    if Random < (0.9/1.0) then begin
  //      FComparisons[0].DoResponse(False);
  //    end else begin
  //      FComparisons[RandomRange(1, Length(FComparisons))].DoResponse(False);
  //    end;
  //  end;
  //end;
  //if Assigned(OnFinalize) then
  //  OnFinalize(Self);

  // for short time simulations
  case FComparisons.Count-1 of
    -MaxInt..-1: { do nothing };
    0 : Pool.Counters.Hit;
    else begin
      if Random < (0.9/1.0) then begin
        Pool.Counters.Hit;
      end else begin
        Pool.Counters.Miss;
      end;
    end;
  end;
  if Assigned(OnFinalize) then
    OnFinalize(Self);
end;

procedure TMTSStimuli.Load(AParameters: TStringList; AParent: TObject);
var
  LRelation    : string;
  SampleLetter : string;
  ComparLetter : string;
  LSamples      : integer;
  LComparisons  : integer;
  //LCycle : string;

  procedure NewGridItems(ASamples, AComparisons: integer;
    AGridOrientation: TGridOrientation);
  var
    LItem : TStimulus;
    i : integer;
    LCallbacks : TCallbacks;
  begin
    LCallbacks.OnMouseExit := @StimulusMouseExit;
    LCallbacks.OnMouseEnter := @StimulusMouseEnter;
    LCallbacks.OnMouseDown := @StimulusMouseDown;
    LCallbacks.OnMouseUp := @StimulusMouseUp;
    LCallbacks.OnResponse  := @ComparisonResponse;

    if not Assigned(Grid) then begin
      Grid := TGrid.Create(3);
    end;
    Grid.FixedSample:=True;

    if AComparisons = 1 then begin
      Grid.FixedComparison:=True;
    end else begin
      Grid.FixedComparison:=False;
    end;

    Grid.UpdatePositions(ASamples, AComparisons, AGridOrientation);
    with Grid.RandomPositions, MTSKeys do begin
      for i := Low(Comparisons) to High(Comparisons) do begin
        LItem := TStimulusFactory.New(Self, ComparLetter, LCallbacks);
        LItem.IsSample := False;
        LItem.Index := i;
        LItem.Position := Comparisons[i].Position;

        LItem.Name:=ComparisonsKey+(i+1).ToString;
        LItem.Load(AParameters, AParent, Comparisons[i].Rect);

        Comparisons[i].Item := LItem as TObject;
        FComparisons.Add(LItem);
      end;

      LCallbacks.OnResponse := @SampleResponse;
      for i := Low(Samples) to High(Samples) do begin
        LItem := TStimulusFactory.New(Self, SampleLetter, LCallbacks);
        LItem.IsSample := True;
        LItem.Index := i;
        LItem.Position := Samples[i].Position;

        LItem.Name := SamplesKey+(i+1).ToString;
        LItem.Load(AParameters, AParent, Samples[i].Rect);

        Samples[i].Item := LItem as TObject;
        FSamples.Add(LItem);
      end;
      AppendToTrialHeader(Pool.Session.Trial.Events.Header);
      AppendToTrialHeader(Grid.Header);
    end;
  end;

begin
  if not Assigned(AParent) then
    raise Exception.Create('You must assign a parent.');
  FSoundCorrect := SDLAudio.SoundFromName('acerto');
  FSoundWrong   := SDLAudio.SoundFromName('erro');
  FSoundCorrect.SetOnStart(@ConsequenceStart);
  FSoundWrong.SetOnStart(@ConsequenceStart);
  FSoundCorrect.SetOnStop(@ConsequenceDone);
  FSoundWrong.SetOnStop(@ConsequenceDone);

  with TrialKeys, MTSKeys do begin
    FHasConsequence := AParameters.Values[HasConsequenceKey].ToBoolean;
    LRelation := AParameters.Values[RelationKey];
    SampleLetter := ExtractDelimited(1,LRelation,['-']);
    ComparLetter := ExtractDelimited(2,LRelation,['-']);
    LSamples := AParameters.Values[SamplesKey].ToInteger;
    LComparisons := AParameters.Values[ComparisonsKey].ToInteger;

    case SampleLetter of
      'A' : FMTSModality.Samples := ModalityA;
      'B' : FMTSModality.Samples := ModalityB;
      'C' : FMTSModality.Samples := ModalityC;
      'D' : FMTSModality.Samples := ModalityD;
      else
        raise Exception.Create('Unknown Samples modality: ' + SampleLetter);
    end;
    case ComparLetter of
      'A' : FMTSModality.Comparisons := ModalityA;
      'B' : FMTSModality.Comparisons := ModalityB;
      'C' : FMTSModality.Comparisons := ModalityC;
      'D' : FMTSModality.Comparisons := ModalityD;
      else
        raise Exception.Create('Unknown Comparisons modality: ' + SampleLetter);
    end;

    if (FMTSModality.Comparisons = ModalityA) or
       (FMTSModality.Comparisons = ModalityD)
    then begin
      FButton.LoadFromFile(Pool.AssetsBasePath+'ConfirmButton'+IMG_EXT);
      FButton.Parent := TCustomRenderer(AParent);
      FButton.OnClick:=@ButtonClick;
    end;

    if FMTSModality.Comparisons = ModalityD then begin
      LComparisons := 1;
    end;
  end;

  NewGridItems(LSamples, LComparisons, goTopToBottom);
end;

procedure TMTSStimuli.Start;
var
  LStimulus : TStimulus;
begin
  for LStimulus in FSamples do
    LStimulus.Start;
end;

procedure TMTSStimuli.Stop;
var
  LStimulus : TStimulus;
begin
  for LStimulus in FComparisons do
    LStimulus.Stop;

  for LStimulus in FSamples do
    LStimulus.Stop;
end;

end.

