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
  //, fgl
  , sdl.app.stimulus
  , sdl.app.graphics.toggle
  , sdl.app.events.abstract
  , sdl.app.audio
  , sdl.app.audio.recorder.devices
  ;

type

  { TSpeechStimulus }

  TSpeechStimulus = class(TStimulus)
  private
    FRect : TSDL_Rect;
    FRecorder : TAudioRecorderComponent;
    FPlayback : TAudioPlaybackComponent;
    FRecorderButton : TToggleButton;
    FPlaybackButton : TToggleButton;
  protected
    //procedure RecorderTerminated(Sender: TObject);
    //procedure PlaybackTerminated(Sender: TObject);
    procedure MouseUp(Sender: TObject; Shift: TCustomShiftState;
      X, Y: Integer); override;
  public
    destructor Destroy; override;
    procedure Load(AParameters : TStringList;
        AParent : TObject; ARect: TSDL_Rect); override;
    procedure Start; override;
    procedure Stop; override;
  end;

implementation

uses session.pool
   , sdl.app.renderer.custom
   , session.constants.mts;

{ TSpeechStimulus }

procedure TSpeechStimulus.MouseUp(Sender: TObject; Shift: TCustomShiftState; X,
  Y: Integer);
begin
  if Sender = FRecorderButton then begin
    if FPlayback.Suspended then begin
      if FRecorder.Suspended then begin
        FRecorder.Start(FRecorderButton);
        FRecorderButton.Toggle;
      end else begin
        FRecorder.Stop;
      end;
      Exit;
    end;
  end;

  if Sender = FPlaybackButton then begin
    if FRecorder.Finished then begin
      if FPlayback.Suspended then begin
        FPlayback.Start(FPlaybackButton);
        FPlaybackButton.Toggle;
      end else begin
        FPlayback.Stop;
      end;
    end;
  end;
end;

destructor TSpeechStimulus.Destroy;
begin

  inherited Destroy;
end;

procedure TSpeechStimulus.Load(AParameters: TStringList; AParent: TObject;
  ARect: TSDL_Rect);
const
  LRecordButtonOn  : string = 'RecordButtonOn' +IMG_EXT;
  LRecordButtonOff : string = 'RecordButtonOff'+IMG_EXT;
  LPlayButtonOn  : string = 'PlayButtonOn' +IMG_EXT;
  LPlayButtonOff : string = 'PlayButtonOff'+IMG_EXT;
begin
  FRect := ARect;
  SDLAudio.RecorderDevice.Clear;
  FRecorder := SDLAudio.RecorderDevice.Recorder;
  FPlayback := SDLAudio.RecorderDevice.Playback;

  if  FPlayback.Opened then begin
    FPlaybackButton := TToggleButton.Create(Self);
    FPlaybackButton.LoadFromFile(
      Pool.AssetsBasePath+LPlayButtonOff,
      Pool.AssetsBasePath+LPlayButtonOn);
    FPlaybackButton.BoundsRect := ARect;
    FPlaybackButton.Parent := TCustomRenderer(AParent);
    //FPlaybackButton.OnMouseDown := @MouseDown;
    FPlaybackButton.OnMouseUp := @MouseUp;
    SDLAudio.RecorderDevice.Append(FPlaybackButton);
  end;

  if FRecorder.Opened then begin
    FRecorderButton := TToggleButton.Create(Self);
    FRecorderButton.LoadFromFile(
      Pool.AssetsBasePath+LRecordButtonOff,
      Pool.AssetsBasePath+LRecordButtonOn);
    FRecorderButton.BoundsRect := ARect;
    FRecorderButton.Sibling := FPlaybackButton;
    FRecorderButton.Parent := TCustomRenderer(AParent);
    //FRecordButton.OnMouseDown := @MouseDown;
    FRecorderButton.OnMouseUp := @MouseUp;
    SDLAudio.RecorderDevice.Append(FRecorderButton);
  end;
end;

procedure TSpeechStimulus.Start;
begin
  if IsSample then begin
    { do nothing }
  end else begin
    if Assigned(FRecorderButton) then begin
      FRecorderButton.Show;
      FRecorderButton.Enabled := True;
    end;
    if Assigned(FPlaybackButton) then begin
      FPlaybackButton.Show;
      FPlaybackButton.Enabled := False;
    end;
  end;
end;

procedure TSpeechStimulus.Stop;
begin
  if IsSample then begin
    { do nothing }
  end else begin
    if Assigned(FRecorderButton) then begin
      FRecorderButton.Hide;
    end;
    if Assigned(FPlaybackButton) then begin
      FPlaybackButton.Hide;
    end;
  end;
end;

end.

