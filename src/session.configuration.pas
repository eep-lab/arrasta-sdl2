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

  { TTrialData }

  TTrialData = record
    ID : integer;
    Kind: string;
    Parameters: TStringList;
    class operator = (A, B: TTrialData): Boolean;
  end;

  TTrials = array of TTrialData;

  { TBlockData }

  TBlockData = record
    ID : integer;
    Name: string;
    ITI: integer;
    BkGnd: integer;
    Counter : string;
    TotalTrials: integer;
    AutoEndSession : Boolean;
    MaxCorrection: integer;
    MaxBlockRepetition: integer;

    DefNextBlock: string;
    CrtConsecutive: integer;
    CrtHitValue: integer;
    CrtConsecutiveHit: integer;
    CrtHitPorcentage : integer;
    CrtConsecutiveHitPerType : integer;
    CrtConsecutiveMiss : integer;
    CrtMaxTrials : integer;
    CrtCsqHit : integer;
    Trials: TTrials;
    NextBlockOnCriteria : integer;
    NextBlockOnNotCriteria : integer;
    class operator = (A, B: TBlockData): Boolean;
  end;

  TBlocks = array of TBlockData;

implementation


{ TTrialData }

class operator TTrialData.=(A, B: TTrialData): Boolean;
begin
  Result := A.ID = B.ID;
end;

{ TBlockData }

class operator TBlockData.=(A, B: TBlockData): Boolean;
begin
  Result := A.ID = B.ID;
end;

end.







