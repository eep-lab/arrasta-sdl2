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
    Cycle       : string;
    Condition   : string;
  end;

  TMTSKeys = record
    Relation    : string;
    Samples     : string;
    Comparisons : string;
    Word        : string;
    Comparison  : string;
  end;

const
  MTSKeysGlobal : TMTSKeysGlobal = (
    Cycle       : 'Cycle';
    Condition   : 'Condition');

  MTSKeys : TMTSKeys = (
    Relation    : 'Relation';
    Samples     : 'Samples';
    Comparisons : 'Comparisons';
    Word        : 'Word';
    Comparison  : 'C');

implementation

end.

