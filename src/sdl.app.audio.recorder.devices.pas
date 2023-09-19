{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.audio.recorder.devices;

{$mode objfpc}{$H+}

interface

uses
  Classes, SDL2, ctypes, fgl, fpwavwriter;

type


  TDevices = specialize TFPGMap<PAnsiChar, PSDL_AudioSpec>;

  { TAudioDevice }

  TAudioDevice = class (TThread)
  private
    FStarter : TObject;
    function GetAudioSpec: TSDL_AudioSpec;
    function GetOpened: Boolean;
    //procedure SetAudioSpec(AValue: TSDL_AudioSpec);
  protected
    FDesiredAudioSpec: TSDL_AudioSpec;
    FAudioSpec: TSDL_AudioSpec;
    FDeviceId: TSDL_AudioDeviceID;
    FDevices : TDevices;
    function BytesPerSample : cint;
    function FileSize : cint;
    procedure Execute; override;
    procedure ListDevices(AIsCapture : cint; ADevices: TDevices);
  public
    constructor Create; virtual; reintroduce;
    destructor Destroy; override;
    function Open: Boolean; virtual; abstract;
    procedure Close;
    procedure Stop; virtual;
    procedure Start(AStarter : TObject); reintroduce;
    property AudioSpec: TSDL_AudioSpec read GetAudioSpec;
    property DeviceId: TSDL_AudioDeviceID read FDeviceId;
    property Opened : Boolean read GetOpened;
    property Starter : TObject read FStarter;
  end;

  { TAudioRecorderComponent }

  TAudioRecorderComponent = class sealed (TAudioDevice)
    function Open: Boolean; override;
    procedure SaveToFile(AFilename : string);
  end;

  { TAudioPlaybackComponent }

  TAudioPlaybackComponent = class sealed (TAudioDevice)
    function Open: Boolean; override;
  end;

implementation

uses SysUtils, sdl.app.output, fpwavformat;

const
  MAX_RECORDING_SECONDS = 4;
  RECORDING_BUFFER_SECONDS = MAX_RECORDING_SECONDS + 1;

var
  GRecordingBuffer: PUInt8;
  GBufferByteSize: Uint32;
  GBufferBytePosition: Uint32;
  GBufferByteMaxPosition: Uint32;

procedure TAudioDevice.Execute;
var
  T : Uint32;
begin
  GBufferBytePosition := 0;
  SDL_PauseAudioDevice(FDeviceId, 0);
  while not Terminated do
  begin
    SDL_LockAudioDevice(FDeviceId);
    T := GBufferBytePosition;
    if (T > GBufferByteMaxPosition) or Terminated then
    begin
      SDL_PauseAudioDevice(FDeviceId, 1);
      Break;
    end;
    SDL_UnlockAudioDevice(FDeviceId);
  end;
end;

procedure AudioRecorderCallback(AUserdata: Pointer;
  AStream: PUInt8; ALen: Integer); cdecl;
begin
  // Copy audio from stream
  Move(AStream^, GRecordingBuffer[GBufferBytePosition], ALen);

  // Move along buffer
  Inc(GBufferBytePosition, ALen);
end;

procedure AudioPlaybackCallback(AUserdata: Pointer;
  AStream: PUInt8; ALen: Integer); cdecl;
begin
  // Copy audio to stream
  Move(GRecordingBuffer[GBufferBytePosition], AStream^, ALen);

  // Move along buffer
  Inc(GBufferBytePosition, ALen);
end;

procedure TAudioDevice.ListDevices(AIsCapture : cint; ADevices: TDevices);
var
  LDeviceName: PAnsiChar;
  i: Integer;
  LRecordingDeviceCount: cint;
  //LString : string;
begin
  LRecordingDeviceCount := SDL_GetNumAudioDevices(AIsCapture);
  if LRecordingDeviceCount < 0 then begin
    raise
      Exception.Create(TAudioDevice.ClassName+'.Open error: No devices found.');
    Exit;
  end;
  for i := 0 to LRecordingDeviceCount-1 do begin
    LDeviceName := SDL_GetAudioDeviceName(i, AIsCapture);
    //LString := StrPas(LDeviceName);
    SDL_GetAudioDeviceSpec(i, AIsCapture, @FDesiredAudioSpec);
    FDesiredAudioSpec.format := AUDIO_S16SYS;
    FDesiredAudioSpec.freq:= 44100;
    FDesiredAudioSpec.samples := 1024;
    if AIsCapture = 1 then begin
      FDesiredAudioSpec.callback:=@AudioRecorderCallback;
    end else begin
      FDesiredAudioSpec.callback:=@AudioPlaybackCallback;
    end;
    ADevices[LDeviceName] := @FDesiredAudioSpec;
  end;
end;

constructor TAudioDevice.Create;
begin
  inherited Create(True);
  Priority := tpNormal;
  FreeOnTerminate := False;
  FDeviceId := 0;
  FillChar(FAudioSpec, SizeOf(FAudioSpec), 0);
  FillChar(FDesiredAudioSpec, SizeOf(FDesiredAudioSpec), 0);
  FDevices := TDevices.Create;
end;

destructor TAudioDevice.Destroy;
begin
  Close;
  FDevices.Free;
  inherited Destroy;
end;

procedure TAudioDevice.Close;
begin
  if FDeviceId <> 0 then
  begin
    SDL_CloseAudioDevice(FDeviceId);
    FDeviceId := 0;
    FillChar(FAudioSpec, SizeOf(FAudioSpec), 0);
    FillChar(FDesiredAudioSpec, SizeOf(FDesiredAudioSpec), 0);
  end;
end;

function TAudioDevice.GetAudioSpec: TSDL_AudioSpec;
begin
  Result := FAudioSpec
end;

function TAudioDevice.GetOpened: Boolean;
begin
  Result := FDeviceId <> 0;
end;

function TAudioDevice.BytesPerSample: cint;
begin
  Result := FAudioSpec.channels * (SDL_AUDIO_BITSIZE(FAudioSpec.format) div 8);
end;

function TAudioDevice.FileSize: cint;
begin
  with FAudioSpec do begin
    Result := RECORDING_BUFFER_SECONDS *
      freq * SDL_AUDIO_BITSIZE(format) div 8 div 1024;
    Print(Result.ToString);
  end;
end;

//procedure TAudioDevice.SetAudioSpec(AValue: TSDL_AudioSpec);
//begin
//  FDevices.Data[0] := ;
//end;

procedure TAudioDevice.Start(AStarter: TObject);
begin
  FStarter := AStarter;
  inherited Start;
end;

procedure TAudioDevice.Stop;
begin
  Terminate;
  WaitFor;
end;

function TAudioRecorderComponent.Open: Boolean;
var
  LBytesPerSecond: cint;
begin
  ListDevices(1, FDevices);
  FDeviceId := SDL_OpenAudioDevice(FDevices.Keys[0], 1,
    FDevices.Data[0], @FAudioSpec, SDL_AUDIO_ALLOW_FREQUENCY_CHANGE);

  Print('Name:'+StrPas(FDevices.Keys[0]));
  Print('Channels:' + FAudioSpec.channels.ToString);
  Print('Format:' + FAudioSpec.format.ToHexString);
  Print('Samples:' + FAudioSpec.samples.ToString);
  Print('Sample Rate:' + FAudioSpec.freq.ToString);
  Result := FDeviceId <> 0;
  if not Result then begin
    raise
      Exception.Create(TAudioDevice.ClassName+'.Open error: '+SDL_GetError);
    Exit;
  end;

  LBytesPerSecond := FAudioSpec.freq * BytesPerSample;
  GBufferByteSize := RECORDING_BUFFER_SECONDS * LBytesPerSecond;
  GBufferByteMaxPosition := MAX_RECORDING_SECONDS * LBytesPerSecond;

  GetMem(GRecordingBuffer, GBufferByteSize);
  FillChar(GRecordingBuffer^, GBufferByteSize, 0);
end;

procedure TAudioRecorderComponent.SaveToFile(AFilename: string);
var
  LWavWriter : TWavWriter;
begin
  LWavWriter := TWavWriter.Create;
  try
    if LWavWriter.StoreToFile(aFileName) then begin
      with LWavWriter, FAudioSpec do begin
        fmt.Channels         := channels;
        fmt.SampleRate       := freq;
        fmt.BitsPerSample    := 16;
        fmt.ByteRate         :=
          fmt.SampleRate * fmt.Channels * fmt.BitsPerSample div 8;
        fmt.BlockAlign       := fmt.Channels * fmt.BitsPerSample div 8;
      end;
      LWavWriter.WriteBuf(GRecordingBuffer^, GBufferByteSize);
      LWavWriter.FlushHeader;
    end else begin
      Print('Error creating WAV file: ' + AFileName);
    end;
  finally
    LWavWriter.Free;
  end;
end;

function TAudioPlaybackComponent.Open: Boolean;
begin
  ListDevices(0, FDevices);
  Print(StrPas(FDevices.Keys[0]));
  FDeviceId := SDL_OpenAudioDevice(FDevices.Keys[0], 0,
    FDevices.Data[0], @FAudioSpec, SDL_AUDIO_ALLOW_FORMAT_CHANGE);
  Result := FDeviceId <> 0;
  if Result then begin
    { do nothing }
  end else begin
    raise Exception.Create(TAudioDevice.ClassName+'.Open error: '+SDL_GetError);
  end;
end;


end.
