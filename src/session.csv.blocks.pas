unit session.csv.blocks;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, session.csv;

type

  { TCSVBlock }

  TCSVBlock = class(TCSVRows)
    private
      FMyName                      : string;
      FBlockID                     : Integer; // base 0
      FBackUpBlock                 : Integer; // base 0
      FNextBlockOnHitCriterion     : Integer; // base 0
      FBackUpBlockErrors           : Integer;
      FMaxBlockRepetition          : Integer;
      FMaxBlockRepetitionInSession : Integer;
      FHitCriterion                : Integer; // percentage
      FReinforcement               : Integer; // percentage
      FEndOnHitCriterion           : Boolean;
    protected
      procedure AfterLoadingParameters(Sender: TObject);
      procedure AssignParameters(AParameters: TStringList); override;
    public
      constructor Create; override;
      property MyName : string read FMyName write FMyName;
      property ID : integer read FBlockID write FBlockID;
      property BackUpBlock : Integer
        read FBackUpBlock write FBackUpBlock;
      property NextBlockOnHitCriterion : Integer
        read FNextBlockOnHitCriterion write FNextBlockOnHitCriterion;
      property BackUpBlockErrors : Integer
        read FBackUpBlockErrors write FBackUpBlockErrors;
      property MaxBlockRepetition : Integer
        read FMaxBlockRepetition write FMaxBlockRepetition;
      property MaxBlockRepetitionInSession : Integer
        read FMaxBlockRepetitionInSession write FMaxBlockRepetitionInSession;
      property HitCriterion : Integer
        read FHitCriterion write FHitCriterion;
      property Reinforcement : Integer
        read FReinforcement write FReinforcement;
      property EndOnHitCriterion : Boolean
        read FEndOnHitCriterion write FEndOnHitCriterion;
      property Values[const AKey: string]: string
        read GetValue write SetValue;
  end;

implementation

uses session.constants.blocks;

{ TCSVBlock }

procedure TCSVBlock.AfterLoadingParameters(Sender: TObject);
begin
  // base 0
  FBlockID -= 1;
  FBackUpBlock -= 1;
  FNextBlockOnHitCriterion -= 1;

  if FMyName.IsEmpty then begin
    FMyName := 'Block ' + (FBlockID+1).ToString;
  end;
end;

procedure TCSVBlock.AssignParameters(AParameters: TStringList);
begin
  inherited AssignParameters(AParameters);
end;

constructor TCSVBlock.Create;
begin
  inherited Create;
  OnAfterLoadingParameters := @AfterLoadingParameters;
  FMyName := '';
  FBlockID := 0;
  FBackUpBlock := 0;
  FNextBlockOnHitCriterion := 0;
  FBackUpBlockErrors := 0;
  FMaxBlockRepetition := 0;
  FMaxBlockRepetitionInSession := 0;
  FHitCriterion := 0;
  FReinforcement := 0;
  FEndOnHitCriterion := False;

  with ParserBlockKeys do begin
    RegisterParameter(IDKey,
      @FBlockID, FBlockID);
    RegisterParameter(NameKey,
      @FMyName, FMyName);
    RegisterParameter(NextBlockOnNotCriterionKey,
      @FBackUpBlock, FBackUpBlock);
    RegisterParameter(NextBlockOnHitCriterionKey,
      @FNextBlockOnHitCriterion, FNextBlockOnHitCriterion);
    RegisterParameter(BackUpBlockErrorsKey,
      @FBackUpBlockErrors, FBackUpBlockErrors);
    RegisterParameter(MaxBlockRepetitionKey,
      @FMaxBlockRepetition, FMaxBlockRepetition);
    RegisterParameter(MaxBlockRepetitionInSessionKey,
      @FMaxBlockRepetitionInSession, FMaxBlockRepetitionInSession);
    RegisterParameter(CrtHitPorcentageKey,
      @FHitCriterion, FHitCriterion);
    RegisterParameter(ReinforcementKey,
      @FReinforcement, FReinforcement);
    RegisterParameter(EndSessionOnHitCriterionKey,
      @FEndOnHitCriterion, FEndOnHitCriterion);
  end;
end;

end.

