{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.loggers.writerow;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

procedure WriteDataRow;

var
  BlockName,
  LastTrialHeader,
  TrialHeader,
  TrialName,
  TrialData,
  TrialResult: string;
  ITIBegin,
  ITIEnd: Extended;

implementation

uses session.constants
   , timestamps
   , session.loggers
   , session.loggers.instances
   , session.pool;

procedure WriteDataRow;
var
  LSaveData : TDataProcedure;
  i, j : integer;
  LTrialNo, LBlockID,
  LTrialID, ITIData, LData : string;
const
  DoNotApply = #32#32#32#32#32#32 + 'NA';
begin
  if TrialHeader <> LastTrialHeader then begin
    LData := TLogger.Row([
      rsReportTrialNO,
      rsReportBlockID,
      rsReportBlockName,
      rsReportTrialID,
      rsReportTrialName,
      rsReportITIBeg,
      rsReportITIEnd,
      TrialHeader]);
  end;
  LastTrialHeader := TrialHeader;

  i := Pool.Counters.CurrentTrial;
  j := Pool.Counters.CurrentBlock;
  LTrialNo := (Pool.Counters.SessionTrials + 1).ToString;
  LBlockID := (j + 1).ToString;
  LTrialID := (i + 1).ToString;

  // FTrial Name
  if TrialName = '' then
    TrialName := '--------';

  // iti
  if Pool.Counters.SessionTrials = 0 then
    ITIData := DoNotApply + #9 + TimestampToStr(0)
  else
    ITIData :=
      TimestampToStr(ITIBegin) + #9 +
      TimestampToStr(ITIEnd);

  // write data
  LSaveData := GetSaveDataProc(LGData);
  LData := TLogger.Row([LData +
    LTrialNo,
    LBlockID,
    BlockName,
    LTrialID,
    TrialName,
    ITIData,
    TrialData]);
  LSaveData(LData);
end;

end.

