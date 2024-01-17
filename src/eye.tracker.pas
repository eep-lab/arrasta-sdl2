{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit eye.tracker;

{$mode ObjFPC}{$H+}

interface

uses eye.tracker.contract;

procedure InitializeEyeTracker(AItemIndex : integer);
procedure FinalizeEyeTracker;

var
  EyeTracker : IEyeTracker;

implementation

uses eye.tracker.types, eye.tracker.factory;

procedure InitializeEyeTracker(AItemIndex: integer);
begin
  EyeTracker := TEyeTrackerFactory.New(TEyeTrackerCode(AItemIndex));
end;

procedure FinalizeEyeTracker;
begin
  TEyeTrackerFactory.Finalize;
end;

end.

