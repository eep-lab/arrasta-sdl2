{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.audio.recorder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SDL2, ctypes;

type

  { TAudioDevice }

  TAudioDevice = class(TThread)
  protected
    FBufferBytePosition: Uint32; static;
    FBufferByteMaxPosition: Uint32; static;
    FDesiredAudioSpec: TSDL_AudioSpec; static;
    FRecordingBuffer: PUInt8;
    FBufferByteSize: Uint32;
    FAudioSpec: TSDL_AudioSpec; static;
    FDeviceId: TSDL_AudioDeviceID;
    procedure AudioCallback(userdata: Pointer; stream: PUInt8; len: Integer); virtual; abstract;
    procedure Execute; override;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function Open: Boolean;
    procedure Close;
    procedure Stop; virtual;
    property DeviceId: TSDL_AudioDeviceID read FDeviceId;
    property AudioSpec: TSDL_AudioSpec read FAudioSpec write FDesiredAudioSpec;
  end;

  TAudioRecordComponent = class(TAudioDevice)
  protected
    procedure AudioCallback(userdata: Pointer; stream: PUInt8; len: Integer); override;
  end;

  TAudioPlaybackComponent = class(TAudioDevice)
  protected
    procedure AudioCallback(userdata: Pointer; stream: PUInt8; len: Integer); override;
  end;

implementation

const
  MAX_RECORDING_SECONDS = 4;
  RECORDING_BUFFER_SECONDS = MAX_RECORDING_SECONDS + 1;

procedure TAudioDevice.Execute;
begin
  FBufferBytePosition := 0;
  SDL_PauseAudioDevice(FDeviceId, SDL_FALSE);
  while not Terminated do
  begin
    SDL_LockAudioDevice(FDeviceId);
    if (FBufferBytePosition > FBufferByteMaxPosition) or Terminated then
    begin
      SDL_PauseAudioDevice(FDeviceId, SDL_TRUE);
      Break;
    end;
    SDL_UnlockAudioDevice(FDeviceId);
  end;
end;

constructor TAudioDevice.Create;
begin
  FDeviceId := 0;
  FillChar(FAudioSpec, SizeOf(FAudioSpec), 0);
end;

destructor TAudioDevice.Destroy;
begin
  Close;
  inherited Destroy;
end;

function TAudioDevice.Open: Boolean;
var
  LDevice : PAnsiChar;
  LBytesPerSample: cint;
  LBytesPerSecond: cint;
  LIsCapture: TSDL_Bool;
begin
  Close;
  if Self is TAudioRecordComponent then begin
    LIsCapture := SDL_TRUE;
  end else begin
    LIsCapture := SDL_FALSE;
  end;
  FDeviceId := SDL_OpenAudioDevice(nil, LIsCapture,
    @FDesiredAudioSpec, @FAudioSpec, SDL_AUDIO_ALLOW_FORMAT_CHANGE);
  Result := FDeviceId <> 0;

  LBytesPerSample := FAudioSpec.channels * (SDL_AUDIO_BITSIZE(FAudioSpec.format) div 8);
  LBytesPerSecond := FAudioSpec.freq * LBytesPerSample;
  FBufferByteSize := RECORDING_BUFFER_SECONDS * LBytesPerSecond;
  FBufferByteMaxPosition := MAX_RECORDING_SECONDS * LBytesPerSecond;

  GetMem(FRecordingBuffer, FBufferByteSize);
  FillChar(FRecordingBuffer^, FBufferByteSize, 0);
end;

procedure TAudioDevice.Close;
begin
  if FDeviceId <> 0 then
  begin
    SDL_CloseAudioDevice(FDeviceId);
    FDeviceId := 0;
    FillChar(FAudioSpec, SizeOf(FAudioSpec), 0);
  end;
end;

procedure TAudioDevice.Start;
begin
  if FDeviceId <> 0 then
    SDL_PauseAudioDevice(FDeviceId, SDL_FALSE);
end;

procedure TAudioDevice.Stop;
begin
  Terminate;
  WaitFor;
end;

procedure TAudioRecordComponent.AudioCallback(userdata: Pointer; stream: PUInt8; len: Integer);
begin
  // Copy audio from stream
  Move(FRecordingBuffer[gBufferBytePosition], stream^, len);

  // Move along buffer
  Inc(gBufferBytePosition, len);
end;

procedure TAudioPlaybackComponent.AudioCallback(userdata: Pointer; stream: PUInt8; len: Integer);
begin
  // Copy audio to stream
  Move(stream^, FRecordingBuffer[gBufferBytePosition], len);

  // Move along buffer
  Inc(gBufferBytePosition, len);
end;


end.

