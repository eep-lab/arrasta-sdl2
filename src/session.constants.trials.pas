{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.constants.trials;

{$mode ObjFPC}{$H+}

interface

type

  TTrialParameters = record
    Name : string;
    ReferenceName : string;
    Kind : string;
    Instruction : string;
    Cursor : integer;
    RepeatTrials : integer;
    LimitedHold : integer;
    InterTrialInterval : integer;
    TimeOutInterval : integer;
    HasConsequence : Boolean;
  end;

  TTrialKeys = record
    NameKey : string;
    ReferenceNameKey : string;
    CursorKey : string;
    KindKey : string;
    InstructionKey : string;
    DoCalibrationKey : string;
    LimitedHoldKey : string;
    InterTrialIntervalKey : string;
    ConsequenceIntervalKey : string;
    RepeatTrialsKey : string;
    HasConsequenceKey : string;
    IsTrue : string;
    IsFalse : string;
  end;


const
  TrialKeys : TTrialKeys = (
    NameKey : 'Name';
    ReferenceNameKey : 'ReferenceName';
    CursorKey : 'Cursor';
    KindKey : 'Kind';
    InstructionKey : 'Instruction';
    DoCalibrationKey : 'DoCalibration';
    LimitedHoldKey : 'LimitedHold';
    InterTrialIntervalKey : 'InterTrialInterval';
    ConsequenceIntervalKey : 'ConsequenceInterval';
    RepeatTrialsKey : 'RepeatTrial';
    HasConsequenceKey : 'HasConsequence';
    IsTrue : 'T';
    IsFalse : 'F');

implementation

end.

