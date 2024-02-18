{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.configuration;

{$mode objfpc}{$H+}
{$modeswitch AdvancedRecords}

interface

uses Classes, SysUtils;

type
  TRepeatStyle = (repsNone, repsGlobal, repsConsecutive);

  { TTrialConfiguration }

  TTrialConfiguration = record
    ID : integer;
    Kind: string;
    ReferenceName: string;
    Parameters: TStringList;
    class operator = (A, B: TTrialConfiguration): Boolean;
    function ToData : string;
  end;

  TTrials = array of TTrialConfiguration;

  { TBlockConfiguration }

  TBlockConfiguration = record
    ID : integer;
    Name: string;
    TotalTrials: integer;
    BackUpBlock	: integer;
    BackUpBlockErrors	: integer;
    MaxBlockRepetition : integer;
    MaxBlockRepetitionInSession	: integer;
    EndSessionOnHitCriterion	: Boolean;
    NextBlockOnHitCriterion : integer;
    NextBlockOnNotCriterion : integer;
    CrtHitPorcentage : integer;
    Reinforcement : integer;

    Counter : string;
    AutoEndSession : Boolean;
    MaxCorrection: integer;
    BkGnd: integer;
    ITI: integer;
    DefNextBlock: string;
    CrtConsecutive: integer;
    CrtHitValue: integer;
    CrtConsecutiveHit: integer;
    CrtConsecutiveHitPerType : integer;
    CrtConsecutiveMiss : integer;
    CrtMaxTrials : integer;
    CrtCsqHit : integer;
    Trials: TTrials;
    class operator = (A, B: TBlockConfiguration): Boolean;
    function ToData : string;
  end;

  TBlocks = array of TBlockConfiguration;

implementation

function AsNameValue(AName, AValue: string):string;
begin
  Result := String.Join('=', [AName, AValue]);
end;

{ TTrialConfiguration }

class operator TTrialConfiguration.=(A, B: TTrialConfiguration): Boolean;
begin
  Result := A.ID = B.ID;
end;

function TTrialConfiguration.ToData: string;
begin
  Result := String.Join(LineEnding,[
    AsNameValue('ID', ID.ToString),
    AsNameValue('Kind', Kind),
    AsNameValue('ReferenceName', ReferenceName),
    'Parameters:',
    Parameters.Text]);
end;

{ TBlockConfiguration }

class operator TBlockConfiguration.=(A, B: TBlockConfiguration): Boolean;
begin
  Result := A.ID = B.ID;
end;

function TBlockConfiguration.ToData: string;
begin
  Result := String.Join(LineEnding,[
    AsNameValue('ID', ID.ToString),
    AsNameValue('Name', Name),
    AsNameValue('TotalTrials', TotalTrials.ToString),
    AsNameValue('BackUpBlock', BackUpBlock.ToString),
    AsNameValue('BackUpBlockErrors', BackUpBlockErrors.ToString),
    AsNameValue('MaxBlockRepetition', MaxBlockRepetition.ToString),
    AsNameValue('MaxBlockRepetitionInSession', MaxBlockRepetitionInSession.ToString),
    AsNameValue('EndSessionOnHitCriterion', EndSessionOnHitCriterion.ToString),
    AsNameValue('NextBlockOnHitCriterion', NextBlockOnHitCriterion.ToString),
    AsNameValue('NextBlockOnNotCriterion', NextBlockOnNotCriterion.ToString),
    AsNameValue('CrtHitPorcentage', CrtHitPorcentage.ToString),
    AsNameValue('Reinforcement', Reinforcement.ToString),

    //AsNameValue('Counter', Counter),
    //AsNameValue('AutoEndSession', AutoEndSession.ToString),
    //AsNameValue('MaxCorrection', MaxCorrection.ToString),
    //AsNameValue('BackgroundColor', BkGnd.ToString),
    //AsNameValue('InterTrialInterval', ITI.ToString),
    //AsNameValue(DefNextBlock: string;
    //AsNameValue(CrtConsecutive: integer;
    //AsNameValue(CrtHitValue: integer;
    //AsNameValue(CrtConsecutiveHit: integer;
    //AsNameValue(CrtConsecutiveHitPerType : integer;
    //AsNameValue(CrtConsecutiveMiss : integer;
    //AsNameValue(CrtMaxTrials : integer;
    //AsNameValue(CrtCsqHit : integer;
    AsNameValue('TrialsLength', Length(Trials).ToString)]);
end;

end.







