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

  TMTSKeys = record
    Relation : string;
    Samples  : string;
    Comparisons : string;
    Cycle    : string;
  end;

const
  MTSKeys : TMTSKeys = (
    Relation : 'Relation';
    Samples : 'Samples';
    Comparisons : 'Comparisons';
    Cycle : 'Cycle');

implementation

end.

