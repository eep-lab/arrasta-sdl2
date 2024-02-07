{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.stimulus.speech;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , SDL2
  , sdl.app.graphics.rectangule
  , sdl.app.stimulus
  , sdl.app.audio
  , sdl.app.audio.recorder.devices
  ;

type

  { TSpeechStimulus }

  TSpeechStimulus = class(TStimulus)
  private
    FRectangule : TRectangule;
    FRecorder : TAudioRecorderComponent;
    procedure RecordingFinished(Sender: TObject);
    procedure RecordingStopped(Sender: TObject);
  protected
    function GetRect: TRectangule; override;
    function GetStimulusName : string; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function IsCorrectResponse : Boolean; override;
    procedure DoResponse(AHuman: Boolean); override;
    procedure Load(AParameters : TStringList;
        AParent : TObject; ARect: TSDL_Rect); override;
    procedure Start; override;
    procedure Stop; override;
  end;

implementation

uses Controls
   , session.pool
   , sdl.app.output
   , session.strutils.mts
   , session.loggers.writerow.timestamp
   , session.parameters.global
   , forms.modal.speechvalidation
   ;

{ TSpeechStimulus }

procedure TSpeechStimulus.DoResponse(AHuman: Boolean);
var
  LName: String;
begin
  LName := 'Speech-'+GetID.ToSpeechString;
  if AHuman then begin
    FRecorder.SaveToFile(Pool.DataResponsesBasePath+LName);
    FormManualSpeechValidation.ExpectedText := FCustomName;
  end else begin
    { do nothing }
  end;
  inherited DoResponse(AHuman);
end;

procedure TSpeechStimulus.RecordingFinished(Sender: TObject);
begin
  DoResponse(False);
end;

procedure TSpeechStimulus.RecordingStopped(Sender: TObject);
begin
  DoResponse(True);
end;

function TSpeechStimulus.GetRect: TRectangule;
begin
  Result := FRectangule;
end;

function TSpeechStimulus.GetStimulusName: string;
begin
  Result := 'Speech' + #9 + FCustomName;
end;

constructor TSpeechStimulus.Create;
begin
  inherited Create;
  FormManualSpeechValidation.ExpectedText := '';
  FRectangule := TRectangule.Create;
end;

destructor TSpeechStimulus.Destroy;
begin
  FRectangule.Free;
  inherited Destroy;
end;

function TSpeechStimulus.IsCorrectResponse: Boolean;
begin
  if FormManualSpeechValidation.ExpectedText.IsEmpty then begin
    Result := False;
  end else begin
    if GlobalTrialParameters.ShowModalFormForSpeechResponses then begin
      Result := FormManualSpeechValidation.ShowModal = mrYes;
    end else begin
      Result := True;
    end;
  end;

  if not Result then begin
    Timestamp(
      'Incorrect.Response' + #9 + FormManualSpeechValidation.EditSpeech.Text);
  end;
end;

procedure TSpeechStimulus.Load(AParameters: TStringList; AParent: TObject;
  ARect: TSDL_Rect);
begin
  FRectangule.BoundsRect := ARect;
  // FRectangule.Visible := False;
  // FRectangule.Parent := AParent; // speech do not have a visible square
  FCustomName := GetWordValue(AParameters, IsSample, Index);

  SDLAUdio.RecorderDevice.Recorder.Clear;
  SDLAUdio.RecorderDevice.OnStopped := @RecordingStopped;
  SDLAUdio.RecorderDevice.OnFinished := @RecordingFinished;
  FRecorder := SDLAudio.RecorderDevice.Recorder;
end;

procedure TSpeechStimulus.Start;
begin
  if IsSample then begin
    { do nothing }
  end else begin
    if FRecorder.CanRecord then begin
      FRecorder.StartRecording(Self);
    end;
  end;
end;

procedure TSpeechStimulus.Stop;
begin
  { do nothing }
end;

end.

