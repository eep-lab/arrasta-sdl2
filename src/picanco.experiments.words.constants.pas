{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit picanco.experiments.words.constants;

{$mode ObjFPC}{$H+}

interface

uses
  picanco.experiments.words.types;

const
  PlosiveBilabial : TConsonant =
    (Ord: csPlosiveBilabial; IPA: 'b'; HumanReadable: 'b');

  NonSibilantFricative : TConsonant =
    (Ord: csNonSibilantFricative; IPA: 'f'; HumanReadable: 'f');

  LateralApproximantAlveolar : TConsonant =
    (Ord: csLateralApproximantAlveolar; IPA: 'l'; HumanReadable: 'l');

  NasalAlveolar : TConsonant =
    (Ord: csNasalAlveolar; IPA: 'n'; HumanReadable: 'n');

  OpenFront : TVowel =
    (Ord: vsOpenFront; IPA: 'a'; HumanReadable: 'a'; HumanReadableStress: 'á');

  OpenMidFront : TVowel =
    (Ord: vsOpenMidFront; IPA: 'ɛ'; HumanReadable: 'e'; HumanReadableStress: 'é');

  CloseFront : TVowel =
    (Ord: vsCloseFront; IPA: 'i'; HumanReadable: 'i'; HumanReadableStress: 'í');

  OpenMidBack : TVowel =
    (Ord: vsOpenMidBack; IPA: 'ɔ'; HumanReadable: 'o'; HumanReadableStress: 'ó');

implementation

end.

