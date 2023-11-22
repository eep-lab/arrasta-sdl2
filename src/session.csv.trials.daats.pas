unit session.csv.trials.daats;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, session.csv.trials.mts;

type

  { TCSVDAATSTrials }

  TCSVDAATSTrials = class(TCSVTrialsMTS)
    private
      FTrialID  : Integer;
      procedure AfterLoadingParameters(Sender: TObject); virtual;
    public
      constructor Create; override;
      property TrialID : integer read FTrialID write FTrialID;
      property Values[const AKey: string]: string
        read GetValue write SetValue;
  end;

implementation

uses session.constants.trials;

{ TCSVDAATSTrials }

procedure TCSVDAATSTrials.AfterLoadingParameters(Sender: TObject);
begin
  { do nothing }
end;

constructor TCSVDAATSTrials.Create;
begin
  inherited Create;
  OnAfterLoadingParameters := @AfterLoadingParameters;
  FTrialID := 0;

  with ParserTrialsSourceKeys do begin
    RegisterParameter(TrialIDKey,
      @FTrialID, FTrialID);
  end;
end;

end.


