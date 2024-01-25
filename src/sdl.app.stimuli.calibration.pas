{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.stimuli.calibration;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , sdl.app.graphics.text
  , sdl.app.stimuli
  , sdl.app.stimuli.contract
  , sdl.app.events.abstract;

type

  { TCalibrationStimuli }

  TCalibrationStimuli = class sealed (TStimuli)
    private
      FText : TText;
      procedure CalibrationSuccessful(Sender: TObject);
      procedure CalibrationFailed(Sender: TObject);
      procedure MouseDown(Sender: TObject; Shift: TCustomShiftState;
        X, Y: Integer);
    public
      constructor Create; override;
      destructor Destroy; override;
      function AsInterface : IStimuli;
      procedure DoExpectedResponse; override;
      procedure Load(AParameters : TStringList; AParent : TObject); override;
      procedure Start; override;
      procedure Stop; override;
  end;

implementation

uses sdl.app.video.methods, sdl.app.controls.custom, eye.tracker;

{ TCalibrationStimuli }

procedure TCalibrationStimuli.CalibrationSuccessful(Sender: TObject);
begin
  RaiseWindow;
  OnFinalize(Self);
end;

procedure TCalibrationStimuli.CalibrationFailed(Sender: TObject);
begin
  RaiseWindow;
  FText.Load('A calibragem falhou.');
  FText.Centralize;
  FText.Show;
end;

procedure TCalibrationStimuli.MouseDown(Sender: TObject;
  Shift: TCustomShiftState; X, Y: Integer);
begin
  FText.Hide;
  EyeTracker.StartCalibration;
end;

constructor TCalibrationStimuli.Create;
begin
  inherited Create;
  FText := TText.Create;
end;

destructor TCalibrationStimuli.Destroy;
begin
  FText.Free;
  inherited Destroy;
end;

function TCalibrationStimuli.AsInterface: IStimuli;
begin
  Result := Self as IStimuli;
end;

procedure TCalibrationStimuli.DoExpectedResponse;
begin

end;

procedure TCalibrationStimuli.Load(AParameters: TStringList; AParent: TObject);
begin
  inherited Load(AParameters, AParent);
  FText.FontName := 'Raleway-Regular';
  FText.FontSize := 50;
  FText.Parent := TSDLControl(AParent);
  FText.OnMouseDown := @MouseDown;

  EyeTracker.SetOnCalibrationSuccessful(@CalibrationSuccessful);
  EyeTracker.SetOnCalibrationFailed(@CalibrationFailed);
end;

procedure TCalibrationStimuli.Start;
begin
  EyeTracker.StartCalibration;
end;

procedure TCalibrationStimuli.Stop;
begin
  FText.Hide;
end;

end.

