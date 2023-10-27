{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit picanco.experiments.constants.parser;

{$mode ObjFPC}{$H+}

interface


type
  TParserMTSKeys = record
    IDKey          : string;
    CycleKey       : string;
    ConditionKey   : string;
    BlockKey       : string;
    TrialsKey      : string;
    ComparisonsKey : string;
    RelationKey    : string;
    CodeKey        : string;
  end;

const
  ParserMTSKeys : TParserMTSKeys = (
    IDKey          : 'ID';
    CycleKey       : 'Cycle';
    ConditionKey   : 'Condition';
    BlockKey       : 'Block';
    TrialsKey      : 'Trials';
    ComparisonsKey : 'Comparisons';
    RelationKey    : 'Relation';
    CodeKey        : 'Code');

implementation

end.

