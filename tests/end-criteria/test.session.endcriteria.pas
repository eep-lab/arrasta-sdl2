unit test.session.endcriteria;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry,
  session.endcriteria,
  session.configuration,
  session.configurationfile;

type

  { TTestEndCriteria }

  TTestEndCriteria = class(TTestCase)
  private
    FEndCriteria: TEndCriteria;
    FBlockConfig: TBlockConfiguration;
    FTrialConfig: TTrialConfiguration;
    FTotalTrials: Word;
    procedure Play;
    procedure DoResponse(ATrialResult : TTrialResult);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestNextTrial;
    procedure TestNextBlock;
    procedure TestSession;
    procedure TestHitPercentageCriterionAchieved;
  end;

implementation

uses session.pool, session.csv.experiments, session.fileutils;

const
  GPseudowordsFolder =
    'C:\Users\Rafael\Documents\GitHub\stimulus-control-sdl2\design\pseudowords\';

const
  GTargetSession =
    'Ciclo1-2b-Treino-AC-Ref-Intermitente.csv';

const
  ParticipantFolderName = '01-Teste';

procedure TTestEndCriteria.Play;
type
  TTrialResult = (Hit, Miss, None);
var
  TrialResult : TTrialResult;
  Results : array [];
  i : integer;
begin
  if FEndCriteria.OfBlock then begin
    // end block
    if FEndCriteria.OfSession then begin
      // end session
    end else begin
      FEndCriteria.InvalidateBlock
      Play;
    end;
  end else begin
    FEndCriteria.InvalidateTrial(ConfigurationFile.CurrentTrial);
    //DoResponse;
  end;
end;


procedure TTestEndCriteria.SetUp;
var
  LFilename : string;
  procedure CreateMockSubjectIDFile;
  var
    LStringList : TStringList;
    LFilename : string;
  begin
    LFilename := ConcatPaths([Pool.BaseDataPath, 'ID']);
    if FileExists(LFilename) then Exit;
    LStringList := TStringList.Create;
    try
      LStringList.Append('1');
      LStringList.SaveToFile(LFilename);
    finally
      LStringList.Clear;
      LStringList.Free;
    end;
  end;
begin
  inherited SetUp;
  // setup configuration file
  LFilename :=
    ConcatPaths([GPseudowordsFolder, GTargetSession]);
  Pool.ConfigurationFilename := MakeConfigurationFile(LFilename);

  // setup data folder
  Pool.BaseDataPath :=
    ConcatPaths([Pool.DataRootBasePath, ParticipantFolderName]);
  ForceDirectories(Pool.BaseDataPath);

  // setup participant id
  CreateMockSubjectIDFile;

  // setup data filename
  Pool.BaseFileName := '000';

  // setup counters
  Pool.Counters.BeforeBeginSession;

  FEndCriteria := TEndCriteria.Create;
end;

procedure TTestEndCriteria.TearDown;
begin
  inherited TearDown;
  Pool.Counters.BeforeEndSession;
  FreeConfigurationFile;
  FEndCriteria.Free;
end;

procedure TTestEndCriteria.TestNextTrial;
begin
  // Implement test cases for the NextTrial method
  Fail('Write your own test');
end;

procedure TTestEndCriteria.TestNextBlock;
begin
  // Implement test cases for the NextBlock method
end;

procedure TTestEndCriteria.TestSession;
begin
  Play;
end;

procedure TTestEndCriteria.TestHitPercentageCriterionAchieved;
begin
  // Implement test cases for the HitPercentageCriterionAchieved method
end;

initialization

  RegisterTest(TTestEndCriteria);

end.
