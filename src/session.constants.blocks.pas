
{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.constants.blocks;

{$mode ObjFPC}{$H+}

interface

type

  TParserBlockKeys = record
    IDKey : string;
    NameKey : string;
    BackUpBlockKey : string;
    BackUpBlockErrorsKey: string;
    MaxBlockRepetitionKey : string;
    MaxBlockRepetitionInSessionKey	: string;
    EndSessionOnHitCriterionKey	: string;
    NextBlockOnHitCriterionKey : string;
    NextBlockOnNotCriterionKey : string;
    CrtHitPorcentageKey : string;
    CrtConsecutiveHitKey :string;
    ReinforcementKey : string;
  end;

const
  ParserBlockKeys : TParserBlockKeys = (
    IDKey : 'ID';
    NameKey : 'Name';
    BackUpBlockKey : 'BackUpBlock';
    BackUpBlockErrorsKey: 'BackUpBlockErrors';
    MaxBlockRepetitionKey : 'MaxBlockRepetition';
    MaxBlockRepetitionInSessionKey	: 'MaxBlockRepetitionInSession';
    EndSessionOnHitCriterionKey	: 'EndSessionOnHitCriterion';
    NextBlockOnHitCriterionKey : 'NextBlockOnHitCriterion';
    NextBlockOnNotCriterionKey : 'NextBlockOnNotCriterion';
    CrtHitPorcentageKey : 'HitCriterion';
    CrtConsecutiveHitKey : 'ConsecutiveHitCriterion';
    ReinforcementKey: 'Reinforcement');

implementation

end.

