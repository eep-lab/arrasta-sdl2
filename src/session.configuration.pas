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

  { TBlocData }

  TBlocData = record
    ID : integer;
    Name: string;
    ITI: integer;
    BkGnd: integer;
    Counter : string;
    TotalTrials: integer;
    VirtualTrialValue: integer;
    AutoEndSession : Boolean;
    MaxCorrection: integer;
    MaxBlcRepetition: integer;

    DefNextBlc: string;
    CrtConsecutive: integer;
    CrtHitValue: integer;
    CrtConsecutiveHit: integer;
    CrtHitPorcentage : integer;
    CrtConsecutiveHitPerType : integer;
    CrtConsecutiveMiss : integer;
    CrtMaxTrials : integer;
    CrtCsqHit : integer;
    Trials: TTrials;
    NextBlocOnCriteria : integer;
    NextBlocOnNotCriteria : integer;
    class operator = (A, B: TBlocData): Boolean;
  end;

  TBlocs = array of TBlocData;

implementation


{ TTrialData }

class operator TTrialData.=(A, B: TTrialData): Boolean;
begin
  Result := A.ID = B.ID;
end;

{ TBlocData }

class operator TBlocData.=(A, B: TBlocData): Boolean;
begin
  Result := A.ID = B.ID;
end;

end.







