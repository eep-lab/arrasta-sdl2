{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit eye.tracker.types;

{$mode ObjFPC}{$H+}

interface

type

  TEyeTrackerCode = (etNone, etPupilLabs, etEyeLink);

  TGaze = record
    X : Integer;
    Y : Integer;
  end;

  TGazes = array of TGaze;

  TGazeOnScreenEvent = procedure (Sender : TObject; AGazes : TGazes) of object;

implementation

end.

