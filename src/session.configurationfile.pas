{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.configurationfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , Session.Configuration
  , IniFiles
  ;

type

  TStartAt = record
    Trial : integer;
    Block  : integer;
  end;
  { TConfigurationFile }

  TConfigurationFile = class(TIniFile)
  private
    FBlockCount : integer;
    class function TrialSection(BlockIndex, TrialIndex : integer) : string;
    class function BlockSection(BlockIndex : integer) : string;
    function CurrentBlockSection : string;
    function GetBlockCount : integer;
    function GetTrialCount(BlockIndex : integer): integer;
    function GetBlock(BlockIndex : integer): TBlockData;
    function GetTrial(BlockIndex, TrialIndex : integer): TTrialData;
    procedure CopySection(AFrom, ATo : string; AConfigurationFile : TConfigurationFile);
    procedure WriteSection(ASectionName:string; ASection : TStrings);

  public
    constructor Create(const AConfigurationFile: string; AEscapeLineFeeds:Boolean=False); override;
    destructor Destroy; override;
    class function FullTrialSection(ABlock, ATrial : integer) : string;
    function ReadTrialString(ABlock : integer; ATrial : integer; AName:string):string;
    function ReadTrialInteger(ABlock : integer; ATrial : integer; AName:string):LongInt;
    function CurrentBlock: TBlockData;
    function CurrentTrial: TTrialData;
    function BeginTableName : string;
    function EndTableName : string;
    procedure Invalidate;
    procedure ReadPositionsInBlock(ABlock:integer; APositionsList : TStrings);
    procedure WriteToBlock(ABlock : integer;AName, AValue: string);
    procedure WriteToTrial(ATrial : integer; AStrings : TStrings); overload;
    procedure WriteToTrial(ATrial : integer; AName, AValue: string); overload;
    procedure WriteToTrial(ATrial : integer; ABlock : integer; AName, AValue: string); overload;
    procedure WriteToMain(AKey: string; AValue: string);
    procedure WriteMain(AMain : TStrings);
    procedure WriteBlockFromTarget(ATargetBlock : integer; ATargetConfigurationFile : TConfigurationFile;
      AlsoAppendTrials : Boolean = True);
    procedure WriteTrialFromTarget(ATargetBlock,ATargetTrial: integer; ATargetConfigurationFile : TConfigurationFile);
    procedure WriteBlockIfEmpty(ABlock : integer; ABlockSection : TStrings);
    //procedure WriteBlock(ABlock: TBlockData; AlsoAppendTrials: Boolean);
    //procedure WriteTrial(ATrial : TTrialData);
    property BlockCount : integer read GetBlockCount;
    property TrialCount[BlockIndex : integer] : integer read GetTrialCount;
    property Block[BlockIndex : integer] : TBlockData read GetBlock {write SetBlock};
    property Trial[BlockIndex, TrialIndex : integer] : TTrialData read GetTrial {write SetTrial};
  end;

var
  ConfigurationFile : TConfigurationFile;

implementation

uses StrUtils
  , session.constants
  , session.pool;

{ TConfigurationFile }

function TConfigurationFile.GetBlockCount: integer;
begin
  FBlockCount := 0;
  while SectionExists(BlockSection(FBlockCount+1)) do
    Inc(FBlockCount);
  Result := FBlockCount;
end;

class function TConfigurationFile.BlockSection(BlockIndex: integer): string;
begin
  Result := _Block + #32 + IntToStr(BlockIndex);
end;

function TConfigurationFile.GetTrialCount(BlockIndex : integer): integer;
begin
  Result := 0;
  while SectionExists(TrialSection(BlockIndex,Result+1)) do
    Inc(Result);
end;

class function TConfigurationFile.TrialSection(BlockIndex,
  TrialIndex: integer): string;
begin
  Result := BlockSection(BlockIndex) + ' - ' + _Trial + IntToStr(TrialIndex);
end;

function TConfigurationFile.CurrentBlock: TBlockData;
begin
  Result := Block[Counters.CurrentBlock+1];
end;

function TConfigurationFile.CurrentTrial: TTrialData;
begin
  Result := Trial[
    Counters.CurrentBlock+1,
    Counters.CurrentTrial+1];
end;

function TConfigurationFile.BeginTableName: string;
begin
  Result := ReadString(CurrentBlockSection, 'BeginTable', '');
end;

function TConfigurationFile.EndTableName: string;
begin
  Result := ReadString(CurrentBlockSection, 'EndTable', '');
end;

function TConfigurationFile.CurrentBlockSection: string;
begin
  Result := BlockSection(Counters.CurrentBlock+1);
end;

function TConfigurationFile.GetBlock(BlockIndex: integer): TBlockData;
var
  LBlockSection , s1: string;
begin
  LBlockSection := BlockSection(BlockIndex);
  with Result do
    begin
      ID := BlockIndex;
      s1 := ReadString(LBlockSection, _NumTrials, '0 0');
      TotalTrials:=StrToIntDef(ExtractDelimited(1,s1,[#32]),0);
      VirtualTrialValue:= StrToIntDef(ExtractDelimited(2,s1,[#32]),0);

      Name:= ReadString(LBlockSection, _Name, '');
      BkGnd:= ReadInteger(LBlockSection, _BkGnd, 0);
      ITI:= ReadInteger(LBlockSection, _ITI, 0);

      AutoEndSession := ReadBool(LBlockSection, _AutoEndSession, True);
      CrtHitPorcentage := ReadInteger(LBlockSection, _CrtHitPorcentage, -1);
      CrtConsecutiveHit := ReadInteger(LBlockSection, _CrtConsecutiveHit, -1);
      CrtConsecutiveMiss := ReadInteger(LBlockSection, _CrtConsecutiveMiss, -1);
      CrtConsecutiveHitPerType := ReadInteger(LBlockSection, _CrtConsecutiveHitPerType, -1);
      CrtHitValue := ReadInteger(LBlockSection, _CrtHitValue, -1);
      CrtMaxTrials:= ReadInteger(LBlockSection, _CrtMaxTrials, -1);
      CrtCsqHit := ReadInteger(LBlockSection, _CsqCriterion, -1);
      NextBlockOnCriteria := ReadInteger(LBlockSection, _NextBlockOnCriteria, -1);
      NextBlockOnNotCriteria := ReadInteger(LBlockSection, _NextBlockOnNotCriteria, -1);
      DefNextBlock:= ReadString(LBlockSection, _DefNextBlock, '');

      Counter:= ReadString(LBlockSection, _Counter, 'NONE');
      MaxCorrection:= ReadInteger(LBlockSection, _MaxCorrection, 0);
      MaxBlockRepetition := ReadInteger(LBlockSection, _MaxBlockRepetition, 0);
    end;
end;

function TConfigurationFile.GetTrial(BlockIndex, TrialIndex: integer): TTrialData;
var
  LTrialSection : string;
begin
  LTrialSection := TrialSection(BlockIndex,TrialIndex);
  with Result do
    begin
      Id :=  TrialIndex + 1;
      Kind:= ReadString(LTrialSection, _Kind, '');
      Parameters := TStringList.Create;
      Parameters.CaseSensitive := False;
      Parameters.Duplicates := dupIgnore;
      ReadSectionValues(LTrialSection, Parameters);
    end;
end;

procedure TConfigurationFile.CopySection(AFrom, ATo: string;
  AConfigurationFile: TConfigurationFile);
var
  LSection : TStringList;
  LTargetSectionName,
  LSelfSectionName : string;
begin
  if AConfigurationFile.SectionExists(AFrom) then
    begin
      LSection := TStringList.Create;
      LSection.CaseSensitive := False;
      LSection.Duplicates := dupIgnore;
      try
        LTargetSectionName:= AFrom;
        LSelfSectionName := ATo;
        AConfigurationFile.ReadSectionValues(LTargetSectionName,LSection);
        WriteSection(LSelfSectionName,LSection);
      finally
        LSection.Free;
      end;
    end;
end;

procedure TConfigurationFile.WriteSection(ASectionName: string;
  ASection: TStrings);
var
  LLine, LKeyName: String;
begin
  for LLine in ASection do
    begin
      LKeyName := ASection.ExtractName(LLine);
      WriteString(ASectionName, LKeyName, ASection.Values[LKeyName]);
    end;
end;

procedure TConfigurationFile.Invalidate;
var
  i: Integer;
begin
  WriteInteger(_Main,_NumBlock,BlockCount);
  for i := 0 to BlockCount-1 do
    WriteString(BlockSection(i+1),_NumTrials,TrialCount[i+1].ToString+' 1');
end;

procedure TConfigurationFile.ReadPositionsInBlock(ABlock: integer;
  APositionsList: TStrings);
var
  L : TStringList;
  LNumComp: LongInt;
  LTrialSection, LKeyName, S: String;
  j, i: Integer;
begin
  L := TStringList.Create;
  L.Sorted := True;
  L.Duplicates:=dupIgnore;
  try
    for i := 0 to TrialCount[ABlock]-1 do
      begin
        LTrialSection := TrialSection(ABlock,i+1);

        // sample
        if ReadString(LTrialSection,_Kind,'') = T_MTS then
          begin
            LKeyName := _Samp+_cBnd;
            S := ReadString(LTrialSection,LKeyName,'');
            if S <> '' then
              L.Append(S);
          end;

        // comparisons
        LNumComp := ReadInteger(LTrialSection,_NumComp,0);
        if LNumComp > 0 then
          for j := 0 to  LNumComp-1 do
            begin
              LKeyName := _Comp+IntToStr(j+1)+_cBnd;
              S := ReadString(LTrialSection,LKeyName,'');
              if S <> '' then
                L.Append(S);
            end;
      end;

    j := 0;
    for i := L.Count-1 downto 0 do
      begin
        APositionsList.Values[IntToStr(j+1)] := L[i];
        Inc(j);
      end;

  finally
    L.Free;
  end;
end;

function TConfigurationFile.ReadTrialString(ABlock: integer; ATrial: integer;
  AName: string): string;
begin
  Result := ReadString(TrialSection(ABlock, ATrial), AName, '');
end;

function TConfigurationFile.ReadTrialInteger(ABlock: integer; ATrial: integer;
  AName: string): LongInt;
begin
  Result := ReadInteger(TrialSection(ABlock, ATrial), AName, 0);
end;

constructor TConfigurationFile.Create(const AConfigurationFile: string;
  AEscapeLineFeeds: Boolean);
begin
  inherited Create(AConfigurationFile, AEscapeLineFeeds);
  FBlockCount := 0;
  GetBlockCount;
end;

destructor TConfigurationFile.Destroy;
begin
  inherited Destroy;
end;

class function TConfigurationFile.FullTrialSection(ABlock,
  ATrial: integer): string;
begin
  Result := '[' + TrialSection(ABlock, ATrial) + ']';
end;

procedure TConfigurationFile.WriteToBlock(ABlock: integer; AName, AValue: string);
begin
  WriteString(BlockSection(ABlock),AName,AValue);
end;

procedure TConfigurationFile.WriteToTrial(ATrial: integer; AStrings: TStrings);
begin
  WriteSection(TrialSection(BlockCount,ATrial),AStrings);
end;

procedure TConfigurationFile.WriteToTrial(ATrial: integer;
  AName, AValue: string);
begin
  WriteString(TrialSection(BlockCount,ATrial),AName,AValue);
end;

procedure TConfigurationFile.WriteToTrial(ATrial: integer; ABlock: integer;
  AName, AValue: string);
begin
  WriteString(TrialSection(ABlock,ATrial),AName,AValue);
end;

procedure TConfigurationFile.WriteToMain(AKey: string; AValue: string);
begin
  WriteString(_Main, AKey, AValue);
end;

procedure TConfigurationFile.WriteMain(AMain: TStrings);
begin
  WriteSection(_Main, AMain);
end;

procedure TConfigurationFile.WriteBlockFromTarget(ATargetBlock: integer;
  ATargetConfigurationFile: TConfigurationFile; AlsoAppendTrials: Boolean);
var
  LSelfSectionName,
  LTargetSectionName : string;
  i: integer;
begin
  LSelfSectionName := BlockSection(BlockCount+1);
  LTargetSectionName := BlockSection(ATargetBlock);
  CopySection(LTargetSectionName,LSelfSectionName, ATargetConfigurationFile);
  if AlsoAppendTrials then
    if ATargetConfigurationFile.TrialCount[ATargetBlock] > 0 then
      for i := 0 to ATargetConfigurationFile.TrialCount[ATargetBlock]-1 do
        WriteTrialFromTarget(ATargetBlock,i+1,ATargetConfigurationFile);
end;

procedure TConfigurationFile.WriteTrialFromTarget(ATargetBlock,
  ATargetTrial: integer; ATargetConfigurationFile: TConfigurationFile);
var
  LSelfSectionName,
  LTargetSectionName : string;
begin
  LSelfSectionName := TrialSection(BlockCount, TrialCount[BlockCount]+1);
  LTargetSectionName:= TrialSection(ATargetBlock,ATargetTrial);
  CopySection(LTargetSectionName,LSelfSectionName, ATargetConfigurationFile);
end;

procedure TConfigurationFile.WriteBlockIfEmpty(ABlock: integer;
  ABlockSection: TStrings);
var
  LBlockSection,
  LLine, LKeyName: String;
  function EmptyKey : Boolean;
  var
    S : string;
  begin
    S := ReadString(LBlockSection, LKeyName, '');
    case Length(S) of
      0: Result := True;
      1: Result := not (S[1] in [#0..#32]);
      2..MaxInt : Result := False;
    end;
  end;

begin
  LBlockSection:=BlockSection(ABlock);
  for LLine in ABlockSection do
    begin
      LKeyName := ABlockSection.ExtractName(LLine);
      if ValueExists(LBlockSection, LKeyName) then
        begin
          if EmptyKey then
            WriteString(LBlockSection, LKeyName, ABlockSection.Values[LKeyName])
          else; // do nothing
        end
      else
        WriteString(LBlockSection, LKeyName, ABlockSection.Values[LKeyName]);
    end;
end;

finalization
  if Assigned(ConfigurationFile) then
    ConfigurationFile.Free;

end.

