unit session.csv.experiments.base;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, ComCtrls
  , session.configurationfile
  , session.configurationfile.writer
  , session.parameters
  , session.csv;

type
  { TBaseExperimentWriter }

  TBaseExperimentWriter = class(TParametricObject)
  private
    FProgressBar : TProgressBar;
    FDesignFilename : string;
    FMultiTrialType: boolean;
    FWriter: TConfigurationWriter;
  protected
    procedure WriteBlocks; virtual;
    procedure WriteTrials; virtual;
    procedure WriteTrialsStarters; virtual;
  public
    class procedure PopulateBooleanStrings;
    constructor Create(AConfigurationFile : TConfigurationFile;
      ADesignFilename: string);
    destructor Destroy; override;
    procedure Write;
    property MultiTrialType : boolean
      read FMultiTrialType write FMultiTrialType;
    property ProgressBar : TProgressBar read FProgressBar write FProgressBar;
  end;

implementation

uses
  Forms
  , sdl.app.output
  , session.csv.trials.base
  , session.csv.blocks
  , session.csv.trials
  , session.csv.trials.factory
  , session.csv.trials.starters;

{ TBaseExperimentWriter }

class procedure TBaseExperimentWriter.PopulateBooleanStrings;
begin
  SetLength(TrueBoolStrs, 1);
  TrueBoolStrs[0] := 'T';

  SetLength(FalseBoolStrs, 1);
  FalseBoolStrs[0] := 'F';
end;

constructor TBaseExperimentWriter.Create(
  AConfigurationFile: TConfigurationFile; ADesignFilename: string);
begin
  FWriter := TConfigurationWriter.Create(AConfigurationFile);
  FDesignFilename := ADesignFilename;
  FMultiTrialType := False;
end;

destructor TBaseExperimentWriter.Destroy;
begin
  FWriter.Free;
  inherited Destroy;
end;

procedure TBaseExperimentWriter.Write;
begin
  WriteBlocks;
  WriteTrials;
  WriteTrialsStarters;
  FWriter.Invalidate;
end;

procedure TBaseExperimentWriter.WriteBlocks;
var
  LBlockParser : TCSVBlock;
  LRow : TStringList;
begin
  if BlocksFileExists(FDesignFilename) then begin
    LBlockParser := TCSVBlock.Create;
    try
      LBlockParser.Clear;
      LBlockParser.LoadFromFile(InsideBlocksSubFolder(FDesignFilename));

      FProgressBar.Visible := True;
      FProgressBar.Position := 0;
      FProgressBar.Max := LBlockParser.RowCount;

      for LRow in LBlockParser do begin
        //Print(LineEnding+LineEnding+LRow.Text+LineEnding+LineEnding);
        LBlockParser.LoadParameters(LRow);
        LBlockParser.AssignParameters(FWriter.BlockConfig);
        //Print(LineEnding+LineEnding+FWriter.BlockConfig.Text+LineEnding+LineEnding);
        FWriter.CurrentBlock := LBlockParser.ID;
        FWriter.WriteBlock;

        FProgressBar.StepIt;
        Application.ProcessMessages;
      end;
    finally
      FProgressBar.StepIt;
      FProgressBar.Visible := False;
      LBlockParser.Free;
    end;
  end;
end;

procedure TBaseExperimentWriter.WriteTrials;
var
  LTrialSourceParser : TCSVTrialsSource;
  LCSVTrial : TCSVTrialsBase;
  LRow : TStringList;
  LTrial : TStringList;
  LStartAt : TStartAt;
  i: Integer;
begin
  if BaseFileExists(FDesignFilename) then begin
    LTrialSourceParser := TCSVTrialsSource.Create;
    try
      LTrialSourceParser.LoadFromFile(InsideBaseFolder(FDesignFilename));

      FProgressBar.Visible := True;
      FProgressBar.Position := 0;
      FProgressBar.Max := LTrialSourceParser.RowCount;

      for LRow in LTrialSourceParser do begin
        LTrialSourceParser.LoadParameters(LRow);
        FWriter.CurrentBlock := LTrialSourceParser.BlockID;
        if BlocksFileExists(FDesignFilename) then begin
          { do nothing, assume block file written }
        end else begin
          with FWriter.BlockConfig do begin
            Values['Name'] := 'Block ' + (FWriter.CurrentBlock+1).ToString;
          end;
          FWriter.WriteBlock;
        end;

        if FMultiTrialType then begin
          // multi trial type
        //if TrialsFileExists(LTrialSourceParser.TrialIDSource) then begin
        //  LCSVTrial := TCSVTrialsFactory.New(
        //    LTrialSourceParser.TrialIDSource);
        //  try
        //    LCSVTrial.LoadFromFile(
        //      InsideTrialsSubFolder(LTrialSourceParser.TrialIDSource));
        //    for LTrial in LCSVTrial then begin
        //      LCSVTrial.LoadParameters(LTrial);
        //      if LCSVTrial.TrialID = LTrialSourceParser.TrialID then begin
        //        LCSVTrial.AssignParameters(FWriter.TrialConfig);
        //        for i := 0 to LCSVTrial.TrialCount -1 do begin
        //          FWriter.WriteTrial;
        //        end;
        //      end;
        //    end;
        //
        //  finally
        //    LCSVTrial.Free;
        //  end;
        //end;
        end else begin
          // single trial type
          if TrialsFileExists(LTrialSourceParser.TrialIDSource) then begin
            LCSVTrial := TCSVTrialsFactory.New(
              LTrialSourceParser.TrialIDSource);
            LCSVTrial.LoadFromFile(
              InsideTrialsSubFolder(LTrialSourceParser.TrialIDSource));
            try
              LTrial :=
                LCSVTrial.GetEnumerator.IndexOf[LTrialSourceParser.TrialID];
              LCSVTrial.LoadParameters(LTrial);
              if LCSVTrial.TrialID = LTrialSourceParser.TrialID then begin
                LCSVTrial.AssignParameters(FWriter.TrialConfig);
                for i := 0 to LCSVTrial.TrialCount -1 do begin
                  FWriter.WriteTrial;
                end;
              end;

            finally
              LCSVTrial.Free;
            end;
          end;
        end;

        FProgressBar.StepIt;
        Application.ProcessMessages;
      end;
    finally
      FProgressBar.StepIt;
      FProgressBar.Visible := False;
      LTrialSourceParser.Free;
    end;
    LStartAt.Block := 0;
    LStartAt.Trial:= 22;
    FWriter.StartAt := LStartAt;
  end;
end;

procedure TBaseExperimentWriter.WriteTrialsStarters;
var
  LCSVStarter : TCSVTrialsStarters;
  LRow    : TStringList;
begin
  if InstructionsFileExist(FDesignFilename) then begin
    LCSVStarter := TCSVTrialsStarters.Create;
    try
      LCSVStarter.LoadFromFile(InsideInstructionsSubFolder(FDesignFilename));

      FProgressBar.Visible := True;
      FProgressBar.Position := 0;
      FProgressBar.Max := LCSVStarter.RowCount;


      for LRow in LCSVStarter do begin
        LCSVStarter.LoadParameters(LRow);
        LCSVStarter.AssignParameters(FWriter.TrialStartersConfig);
        FWriter.WriteStarter(LCSVStarter.BlockID, LCSVStarter.TrialID);

        FProgressBar.StepIt;
        Application.ProcessMessages;
      end;
    finally
      FProgressBar.StepIt;
      FProgressBar.Visible := False;
      LCSVStarter.Free;
    end;
  end;
end;

initialization
  TBaseExperimentWriter.PopulateBooleanStrings;

end.

