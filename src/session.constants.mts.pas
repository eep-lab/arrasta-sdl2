{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.constants.mts;

{$mode ObjFPC}{$H+}

interface

type
  TMTSParameters = record
    Cycle       : integer;
    Condition   : integer;
    Samples     : integer;
    Comparisons : integer;
    Relation    : string;
    Word        : string;
  end;

  TMTSKeysGlobal = record
    CycleKey       : string;
    ConditionKey   : string;
  end;

  TMTSKeys = record
    RelationKey    : string;
    SamplesKey     : string;
    ComparisonsKey : string;
    WordKey        : string;
    ComparisonKey  : string;
    HasPromptKey   : string;
  end;

const
  MTSKeysGlobal : TMTSKeysGlobal = (
    CycleKey       : 'Cycle';
    ConditionKey   : 'Condition');

  MTSKeys : TMTSKeys = (
    RelationKey    : 'Relation';
    SamplesKey     : 'Samples';
    ComparisonsKey : 'Comparisons';
    WordKey        : 'Word';
    ComparisonKey  : 'C';
    HasPromptKey   : 'HasPrompt');

implementation

end.

