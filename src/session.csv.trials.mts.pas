unit session.csv.trials.mts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, session.csv.trials.base;

type

  { TCSVTrialsMTS }

  TCSVTrialsMTS = class(TCSVTrialsBase)
    private
      FSamples : integer;
      FComparisons : integer;
      FRelation : string;
    public
      constructor Create; override;
      property Comparisons : integer read FComparisons;
      property Relation : string read FRelation;
      property Samples : integer read FSamples;
  end;

implementation

uses
  sdl.app.trials.mts
  , session.constants.mts;

{ TCSVTrialsMTS }

constructor TCSVTrialsMTS.Create;
begin
  inherited Create;
  FKind := TMTS.ClassName;
  FSamples     := 1;
  FComparisons := 0;
  FRelation    := '';

  with MTSKeys do begin
    RegisterParameter(SamplesKey,
      @FSamples, FSamples);
    RegisterParameter(ComparisonsKey,
      @FComparisons, FComparisons);
    RegisterParameter(RelationKey,
      @FRelation, FRelation);
  end;
end;

end.


