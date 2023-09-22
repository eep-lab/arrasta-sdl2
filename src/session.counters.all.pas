unit session.counters.all;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , session.counters.consecutive
  , session.counters.mutuallyexclusive;

type

    TTrialID = Word;
    TBlockID = Word;

    { TCustomUIDCounter }

    TCustomUIDCounter = class(TUIDCounter)
    protected
      FEvents : TMutuallyExclusiveCounters;
      function ToString : string; override;
    public
      constructor Create;
      destructor Destroy; override;
      procedure NextID(AValue : TTrialID); virtual; abstract;
      property Events : TMutuallyExclusiveCounters read FEvents;
    end;

    { TTrialCounters }

    TTrialCounters = class(TCustomUIDCounter)
    public
      ID : TTrialID; static;
      procedure Reset; override;
      procedure Next; override;
      procedure NextID(AValue : TTrialID); override;
      procedure NextConsecutive; override;
    end;

    { TBlockCounters }

    TBlockCounters = class(TCustomUIDCounter)
    strict private
      FTrial : TTrialCounters;
    protected
      function GetTrials: Word; overload;
    public
      ID : TBlockID; static;
      constructor Create;
      destructor Destroy; override;
      procedure Reset; override;
      procedure Next; override;
      procedure NextID(AValue : TTrialID); override;
      procedure NextConsecutive; override;
      property Trial : TTrialCounters read FTrial write FTrial;
      property Trials : Word read GetTrials;
    end;

    { TSessionCounters }

    TSessionCounters = class(TCustomUIDCounter)
    strict private
      FBlock : TBlockCounters;
      FTrial : TTrialCounters;
    protected
      function GetTrials : Word;
      function GetBlocks : Word;
      procedure NextTrial;
    public
      ID : Word; static;
      constructor Create;
      destructor Destroy; override;
      procedure Reset; override;
      procedure NextTrialConsecutive;
      procedure ResetTrialConsecutive;
      procedure NextTrialID(ATrialID : TTrialID);
      procedure NextBlockConsecutive;
      procedure ResetBlockConsecutive;
      procedure NextBlockID(ABlockID : TBlockID);
      property Block : TBlockCounters read FBlock;
      property Trial : TTrialCounters read FTrial;
      property Blocks : Word read GetBlocks;
      property Trials : Word read GetTrials;
    end;

implementation


{ TCustomUIDCounter }

function TCustomUIDCounter.ToString: string;
begin
  Result := inherited ToString;
end;

constructor TCustomUIDCounter.Create;
begin
  FEvents := TMutuallyExclusiveCounters.Create;
end;

destructor TCustomUIDCounter.Destroy;
begin
  FEvents.Free;
  inherited Destroy;
end;

{ TTrialCounters }

procedure TTrialCounters.Reset;
begin
  inherited Reset;
  ID := 0;
end;

procedure TTrialCounters.Next;
begin
  inherited Next;
  FEvents.Reset;
end;

procedure TTrialCounters.NextID(AValue: TTrialID);
begin
  Next;
  ID := AValue;
end;

procedure TTrialCounters.NextConsecutive;
begin
  inherited NextConsecutive;
  FEvents.Reset;
end;

{ TBlockCounters }

function TBlockCounters.GetTrials: Word;
begin
  Result := Trial.UID;
end;

procedure TBlockCounters.Next;
begin
  inherited Next;
  Trial.Reset;
end;

procedure TBlockCounters.NextID(AValue: TTrialID);
begin
  Next;
  Inc(ID);
end;

procedure TBlockCounters.NextConsecutive;
begin
  inherited NextConsecutive;
  FTrial.Reset;
end;

constructor TBlockCounters.Create;
begin
  FTrial := TTrialCounters.Create;
end;

destructor TBlockCounters.Destroy;
begin
  FTrial.Free;
  inherited Destroy;
end;

procedure TBlockCounters.Reset;
begin
  inherited Reset;
  ID := 0;
end;

{ TSessionCounters }

function TSessionCounters.GetTrials: Word;
begin
  Result := Block.Trial.UID;
end;

function TSessionCounters.GetBlocks: Word;
begin
  Result := FBlock.Count;
end;

procedure TSessionCounters.NextTrial;
begin
  Trial.Next;
end;

constructor TSessionCounters.Create;
begin
  FBlock := TBlockCounters.Create;
  FTrial := TTrialCounters.Create;
end;

destructor TSessionCounters.Destroy;
begin
  FBlock := TBlockCounters.Create;
  inherited Destroy;
end;

procedure TSessionCounters.Reset;
begin
  inherited Reset;
  FTrial.Reset;
  FBlock.Trial.Reset;
  FBlock.Reset;
end;

procedure TSessionCounters.NextTrialConsecutive;
begin
  //FTrial.NextConsecutive; // redundant
  FBlock.Trial.NextConsecutive;
end;

procedure TSessionCounters.ResetTrialConsecutive;
begin
  //FTrial.ResetConsecutive;  // redundant
  FBlock.Trial.ResetConsecutive;
end;

procedure TSessionCounters.NextTrialID(ATrialID: TTrialID);
begin
  FTrial.Next; // session trials
  FBlock.Trial.NextID(ATrialID); // block trials
end;

procedure TSessionCounters.NextBlockConsecutive;
begin
  FTrial.Reset;
  FBlock.NextConsecutive;
end;

procedure TSessionCounters.ResetBlockConsecutive;
begin
  FTrial.Reset;
  FBlock.ResetConsecutive;
end;

procedure TSessionCounters.NextBlockID(ABlockID: TBlockID);
begin
  FTrial.Reset;
  FBlock.NextID(ABlockID);
end;

end.

