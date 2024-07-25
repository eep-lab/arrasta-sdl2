unit session.csv.trials.multisample;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , session.csv.trials.mts;

type

  { TCSVMultiSample }

  TCSVMultiSample = class(TCSVTrialsMTS)
    private // registered parameters
      FDragDropOrientation: string;
      FAutoAnimateOnstart: Boolean;
      FUseHelpProgression: Boolean;
      FDistance: Integer;
      FDragMode: string;
      FDragMoveFactor: Integer;
      FDragableAnimation: string;
      FGridSize: Integer;
      FRefName : string;
      FName : string;
      FStimuliFolder : string;
      procedure AfterLoadingParameters(Sender: TObject); override;
    public
      constructor Create(ASource: string); override;
      property Values[const AKey: string]: string
        read GetValue write SetValue;
  end;

implementation

uses
  sdl.app.trials.dragdrop,
  session.constants.trials,
  session.constants.trials.dragdrop,
  session.pool;

{ TCSVMultiSample }

procedure TCSVMultiSample.AfterLoadingParameters(Sender: TObject);
begin
  inherited AfterLoadingParameters(Sender);
  if FName.IsEmpty then begin
    FName :=
      TrialID.ToString + '-' +
      Relation + '-' +
      FStimuliFolder + '-' +
      'S' + Samples.ToString + '-' +
      'C' + Comparisons.ToString + '-' +
      'G' + FGridSize.ToString;
  end;

  if FRefName.IsEmpty then begin
    FRefName :=
      Relation + '-' +
      FStimuliFolder + '-' +
      Samples.ToString + '-' +
      Comparisons.ToString;
  end;
end;

constructor TCSVMultiSample.Create(ASource: string);
begin
  inherited Create(ASource);
  FKind := TDragDrop.ClassName;
  FDragDropOrientation := '';
  FAutoAnimateOnstart := False;
  FUseHelpProgression := False;
  FDistance := 0;
  FDragMode := '';
  FDragMoveFactor := 0;
  FDragableAnimation := '';
  FGridSize := 0;
  FStimuliFolder := '';
  FName        := '';
  FRefName     := '';

  with TrialKeys, DragDropKeys do begin
    RegisterParameter(AutoAnimateOnStartKey,
      @FAutoAnimateOnstart, FAutoAnimateOnstart);
    RegisterParameter(DragDropOrientationKey,
      @FDragDropOrientation, FDragDropOrientation);
    RegisterParameter(UseHelpProgressionKey,
      @FUseHelpProgression, FUseHelpProgression);
    RegisterParameter(DistanceKey,
      @FDistance, FDistance);
    RegisterParameter(DragModeKey,
      @FDragMode, FDragMode);
    RegisterParameter(DragMoveFactorKey,
      @FDragMoveFactor, FDragMoveFactor);
    RegisterParameter(DragableAnimationKey,
      @FDragableAnimation, FDragableAnimation);
    RegisterParameter(GridSizeKey,
      @FGridSize, FGridSize);
    RegisterParameter(NameKey,
      @FName, FName);
    RegisterParameter(ReferenceNameKey,
      @FRefName, FRefName);
    RegisterParameter(StimuliFolderKey,
      @FStimuliFolder, FStimuliFolder);
  end;
end;

end.


