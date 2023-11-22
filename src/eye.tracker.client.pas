{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit eye.tracker.client;

{$mode ObjFPC}{$H+}

interface

uses eye.tracker.contract, eye.tracker.types;

type

  { TEyeTrackerClient }

  TEyeTrackerClient = class(IEyeTracker)
    protected
      function GetGazeOnScreenEvent : TGazeOnScreenEvent; virtual; abstract;
      procedure SetGazeOnScreenEvent(
        AGazeOnScreenEvent: TGazeOnScreenEvent); virtual; abstract;
      procedure StartRecording; virtual; abstract;
      procedure StopRecording; virtual; abstract;
      procedure StartCalibration; virtual; abstract;
      procedure StopCalibration; virtual; abstract;
    public
      class function Exists : Boolean;

  end;

implementation

uses eye.tracker;

{ TEyeTrackerClient }

class function TEyeTrackerClient.Exists: Boolean;
begin
  Result := EyeTracker <> nil;
end;

end.

