{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.trials.calibration;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , session.configuration
  , sdl.app.trials
  , sdl.app.stimuli.contract
  , sdl.app.stimuli.calibration.pupil
  ;

type

  { TPupilCalibration }

  TPupilCalibration = class sealed (TTrial)
    private
      FStimuli : TCalibrationStimuli;
    protected
      function GetIStimuli: IStimuli; override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure EndTrial; override;
      procedure Show; override;
      procedure Hide; override;
  end;


implementation

{ TPupilCalibration }

function TPupilCalibration.GetIStimuli: IStimuli;
begin
  Result := FStimuli.AsInterface;
end;

constructor TPupilCalibration.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStimuli := TCalibrationStimuli.Create(Self);
  FStimuli.Trial := Self as TObject;
  FStimuli.OnFinalize := @EndTrialCallBack;
end;

destructor TPupilCalibration.Destroy;
begin
  FStimuli.Free;
  inherited Destroy;
end;

procedure TPupilCalibration.EndTrial;
begin
  inherited EndTrial;
end;

procedure TPupilCalibration.Show;
begin
  inherited Show;

end;

procedure TPupilCalibration.Hide;
begin
  inherited Hide;

end;

end.

