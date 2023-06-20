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
  , sdl.app.stimuli.contract
  , sdl.app.graphics.picture
  , sdl.app.events.abstract
  , sdl.app.audio.contract
  , sdl.app.graphics.text
  ;

type

  TPictures = specialize TFPGList<TPicture>;

  { TMTSStimuli }

  TMTSStimuli = class sealed (TStimuli, IStimuli)
    private
      FSound : ISound;
      FText  : TText;
      FComparisons : TPictures;
      FSamples : TPictures;
      procedure PictureMouseEnter(Sender: TObject);
      procedure PictureMouseExit(Sender: TObject);
      procedure PictureMouseDown(Sender: TObject; Shift: TCustomShiftState; X, Y: Integer);
    public
      constructor Create(AOwner : TComponent); override;
      destructor Destroy; override;
      function AsInterface : IStimuli;
      procedure DoExpectedResponse; override;
      procedure Load(AParameters : TStringList; AParent : TObject); override;
      procedure Start; override;
      procedure Stop; override;
  end;

implementation

uses
  StrUtils
  , sdl.app.renderer.custom
  , sdl.app.trials
  , sdl.app.output
  , sdl.app.audio
  , sdl.app.grids
  , session.constants.trials
  , session.constants.mts
  ;

{ TMTSStimuli }

procedure TMTSStimuli.PictureMouseEnter(Sender: TObject);
begin
  TPicture(Sender).Visible := True;
end;

procedure TMTSStimuli.PictureMouseExit(Sender: TObject);
begin
  TPicture(Sender).Visible := False;
end;

procedure TMTSStimuli.PictureMouseDown(Sender: TObject;
  Shift: TCustomShiftState; X, Y: Integer);
begin
  FSound.Play;
  TTrial(TPicture(Sender).Parent).EndTrial;
end;

constructor TMTSStimuli.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSamples := TPictures.Create;
  FComparisons := TPictures.Create;
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

end;



procedure TMTSStimuli.Load(AParameters: TStringList; AParent: TObject);
var
  LExt : string;
  LRelation    : string;
  SampleLetter : string;
  ComparLetter : string;
  LSamples      : integer;
  LComparisons  : integer;
  LItem : TPicture;
  i: Integer;

  procedure NewGridItems(ASamples, AComparisons: integer;
    AGridOrientation: TGridOrientation);
  var
    LItem : TPicture;
    LComparison : TPicture;
    LComparisons : TPictures;
    i : integer;
  begin
    if not Assigned(Grid) then
      Grid := TGrid.Create(3);
    Grid.UpdatePositions(ASamples, AComparisons, AGridOrientation);
    with Grid.RandomPositions do begin
      LComparisons := TPictures.Create;
      for i := low(Comparisons) to high(Comparisons) do
      begin
        LItem := TPicture.Create(Self);
        LItem.Name := MTSKeys.Comparisons+(i+1).ToString;
        LItem.OnMouseDown:=@PictureMouseDown;
        LItem.OnMouseEnter:=@PictureMouseEnter;
        LItem.OnMouseExit:=@PictureMouseExit;
        LItem.BoundsRect := Comparisons[i].Rect;

        Comparisons[i].Item := LItem as TObject;
        FComparisons.Add(LItem);
        LComparisons.Add(LItem);
      end;

      for i := low(Samples) to high(Samples) do
      begin
        LItem := TPicture.Create(Self);
        LItem.Name := MTSKeys.Samples+(i+1).ToString;
        LItem.OnMouseDown:=@PictureMouseDown;
        LItem.OnMouseEnter:=@PictureMouseEnter;
        LItem.OnMouseExit:=@PictureMouseExit;
        //LItem.OnMouseDown := @SetFocus;
        LItem.BoundsRect := Samples[i].Rect;
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
  end;

begin
  if not Assigned(AParent) then
    raise Exception.Create('You must assign a parent.');

  FSound := SDLAudio.SoundFromName('acerto');
  FText  := TText.Create(Self);
  FText.Load('Hello World!', 'Raleway-Regular');
  FText.Top := 300;
  FText.Parent := TCustomRenderer(AParent);

  with TrialKeys do begin
    LExt := AParameters.Values[ImageFilesExtension];
  end;
  with MTSKeys do begin
    LRelation := AParameters.Values[Relation];
    SampleLetter := ExtractDelimited(1,LRelation,['-']);
    ComparLetter := ExtractDelimited(2,LRelation,['-']);

    LSamples := AParameters.Values[Samples].ToInteger;
    LComparisons := AParameters.Values[Comparisons].ToInteger;
  end;

  NewGridItems(LSamples, LComparisons, Grid.GetRandomGridOrientation);
  with Grid.RandomPositions do begin
    for i := low(Comparisons) to high(Comparisons) do
    begin
      LItem := Comparisons[i].Item as TPicture;
      //LItem.Cursor := Cursor;
      LItem.LoadFromFile(ComparLetter+(i+1).ToString+LExt);
      LItem.Parent := TCustomRenderer(AParent);
      //LItem.Invalidate;
    end;

    for i := low(Samples) to high(Samples) do
    begin
      LItem := Samples[i].Item as TPicture;
      //LItem.Cursor := Cursor;
      LItem.LoadFromFile(SampleLetter+(i+1).ToString+LExt);
      LItem.Parent := TCustomRenderer(AParent);
      //LItem.Invalidate;
    end;
  end;
end;

procedure TMTSStimuli.Start;
var
  LItem : TPicture;
begin
  FText.Show;
  for LItem in FComparisons do LItem.Show;
  for LItem in FSamples do LItem.Show;
end;

procedure TMTSStimuli.Stop;
var
  LItem : TPicture;
begin
  for LItem in FComparisons do LItem.Hide;
  for LItem in FSamples do LItem.Hide;
end;

end.

