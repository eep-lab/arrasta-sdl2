{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit eye.tracker.factory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, eye.tracker.types, eye.tracker.contract;

type

  { TEyeTrackerFactory }

  TEyeTrackerFactory = class
      class function New(ACode : TEyeTrackerCode) : IEyeTracker;
  end;

implementation

uses eye.tracker.pupil, eye.tracker.eyelink;

{ TEyeTrackerFactory }

class function TEyeTrackerFactory.New(ACode: TEyeTrackerCode): IEyeTracker;
begin
  Result := nil;
  case ACode of
    etNone : { do nothing };
    etPupilLabs : Result := TPupilEyeTracker.Create as IEyeTracker;
    etEyeLink : Result := TEyeLinkEyeTracker.Create as IEyeTracker;
  end;
end;

end.

