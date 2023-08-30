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
  , sdl.app.stimuli
  , sdl.app.stimulus.contract
  , sdl.app.stimuli.contract
  , sdl.app.graphics.picture
  , sdl.app.events.abstract
  , sdl.app.audio.contract
  , sdl.app.graphics.text
  ;

type

  TStimulusIList = specialize TFPGList<IStimulus>;

  { TMTSStimuli }

  TMTSStimuli = class sealed (TStimuli, IStimuli)
    private
      FSound : ISound;
      FComparisons : TStimulusIList;
      FSamples : TStimulusIList;
      procedure StimulusMouseEnter(Sender: TObject);
      procedure StimulusMouseExit(Sender: TObject);
      procedure StimulusMouseDown(Sender: TObject; Shift: TCustomShiftState; X, Y: Integer);
    public
      constructor Create(AOwner : TComponent); override;
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
  , sdl.app.renderer.custom
  //, sdl.app.trials
  , sdl.app.output
  , sdl.app.audio
  , sdl.app.grids
  , sdl.app.stimulus.factory
  , sdl.app.stimulus
  , session.constants.trials
  , session.constants.mts
  ;

{ TMTSStimuli }

procedure TMTSStimuli.StimulusMouseEnter(Sender: TObject);
begin
  IStimulus(Sender as TStimulus).Start;
end;

procedure TMTSStimuli.StimulusMouseExit(Sender: TObject);
begin
  IStimulus(Sender as TStimulus).Stop;
end;

procedure TMTSStimuli.StimulusMouseDown(Sender: TObject;
  Shift: TCustomShiftState; X, Y: Integer);
begin
  if IStimulus(Sender as TStimulus) = FComparisons[0] then begin
    FSound.Play;
    if Assigned(OnFinalize) then
      OnFinalize(Sender);
  end;
end;

constructor TMTSStimuli.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSamples := TStimulusIList.Create;
  FComparisons := TStimulusIList.Create;
end;

destructor TMTSStimuli.Destroy;
begin
  FSamples.Free;
  FComparisons.Free;
  inherited Destroy;
end;

function TMTSStimuli.AsInterface: IStimuli;
begin
  Result := Self as IStimuli
end;

procedure TMTSStimuli.DoExpectedResponse;
begin
  //FComparisons[0].DoResponse;
end;

procedure TMTSStimuli.Load(AParameters: TStringList; AParent: TObject);
var
  LRelation    : string;
  SampleLetter : string;
  ComparLetter : string;
  LSamples      : integer;
  LComparisons  : integer;
  LCycle : string;
  S : string;

  procedure NewGridItems(ASamples, AComparisons: integer;
    AGridOrientation: TGridOrientation);
  type
    TStimulusList = specialize TFPGList<TStimulus>;
  var
    LItem : TStimulus;
    LComparison : TStimulus;
    LComparisons : TStimulusList;
    i : integer;
    LParameters : TStringList;
    LCallbacks : TCallbacks;
  begin
    LCallbacks.OnMouseExit := @StimulusMouseExit;
    LCallbacks.OnMouseEnter := @StimulusMouseEnter;
    LCallbacks.OnMouseDown := @StimulusMouseDown;

    LParameters := TStringList.Create;
    try
      if not Assigned(Grid) then
        Grid := TGrid.Create(3);
        Grid.FixedSample:=True;
      Grid.UpdatePositions(ASamples, AComparisons, AGridOrientation);
      with Grid.RandomPositions do begin
        LComparisons := TStimulusList.Create;
        for i := low(Comparisons) to high(Comparisons) do
        begin
          LItem := TStimulusFactory.New(Self, ComparLetter, LCallbacks);

          LParameters.Clear;
          LParameters.Values['Media'] :=
            LCycle+PathDelim+ComparLetter+(i+1).ToString;
          S := LParameters.Values['Media'];
          LItem.Name:=MTSKeys.Comparisons+(i+1).ToString;
          LItem.Load(LParameters, AParent, Comparisons[i].Rect);

          Comparisons[i].Item := LItem as TObject;
          FComparisons.Add(LItem);
          LComparisons.Add(LItem);
        end;

        for i := low(Samples) to high(Samples) do
        begin
          LItem := TStimulusFactory.New(Self, SampleLetter, LCallbacks);

          LParameters.Clear;
          LParameters.Values['Media'] :=
            LCycle+PathDelim+SampleLetter+(i+1).ToString;
          LItem.Name := MTSKeys.Samples+(i+1).ToString;
          LItem.Load(LParameters, AParent, Samples[i].Rect);

          case i of
            0 : // do nothing;

            else begin                     // making sure that we have always
              LComparisons.Exchange(0, i); // the right comparison as the first one
            end;                           // inside the sample targets
          end;
          for LComparison in LComparisons do
            LItem.AddOrderedChoice(LComparison);

          Samples[i].Item := LItem as TObject;
          FSamples.Add(LItem);
        end;
        LComparisons.Free;
      end;
    finally
      LParameters.Free;
    end;
  end;

begin
  if not Assigned(AParent) then
    raise Exception.Create('You must assign a parent.');

  FSound := SDLAudio.SoundFromName('acerto');
  //FText  := TText.Create(Self);
  //FText.Load('BOFÁ', 'Hanna_Serif');
  //FText.Top := 300;
  //FText.Parent := TCustomRenderer(AParent);

  with MTSKeys do begin
    LRelation := AParameters.Values[Relation];
    LCycle    := AParameters.Values[Cycle];
    SampleLetter := ExtractDelimited(1,LRelation,['-']);
    ComparLetter := ExtractDelimited(2,LRelation,['-']);

    LSamples := AParameters.Values[Samples].ToInteger;
    LComparisons := AParameters.Values[Comparisons].ToInteger;
  end;

  NewGridItems(LSamples, LComparisons, goTopToBottom);
end;

procedure TMTSStimuli.Start;
var
  LStimulus : IStimulus;
begin
  for LStimulus in FComparisons do
    LStimulus.Start;

  for LStimulus in FSamples do
    LStimulus.Start;
end;

procedure TMTSStimuli.Stop;
var
  LStimulus : IStimulus;
begin
  for LStimulus in FComparisons do
    LStimulus.Stop;

  for LStimulus in FSamples do
    LStimulus.Stop;
end;

end.

