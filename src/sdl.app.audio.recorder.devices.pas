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
  Classes, SDL2, ctypes, Generics.Collections, fpwavwriter;

type


  TDevices = specialize TDictionary<PAnsiChar, PSDL_AudioSpec>;

  { TAudioDevice }

  TAudioDevice = class (TThread)
  strict private
    FMsg: string;
    FStarter : TObject;
    FRTLEventMainThread : PRTLEvent;
    procedure BufferFinished;
    procedure PrintMessage;
    function GetAudioSpec: TSDL_AudioSpec;
    function GetOpened: Boolean;
  protected
    FDesiredAudioSpec: TSDL_AudioSpec;
    FAudioSpec: TSDL_AudioSpec;
    FDeviceID: TSDL_AudioDeviceID;
    FDevices : TDevices;
    function BytesPerSample : cint;
    function FileSize : cint;
    procedure Execute; override;
    procedure ListDevices(AIsCapture : cint; var ADevices: TDevices);
    procedure StartDevice(AStarter : TObject);
    procedure DeviceFinished(Sender: TObject); virtual; abstract;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Open: Boolean; virtual; abstract;
    procedure Close; virtual;
    procedure Log(const AMsg: string; AppendLineEnd: boolean = true);
    property AudioSpec: TSDL_AudioSpec read GetAudioSpec;
    property DeviceId: TSDL_AudioDeviceID read FDeviceID;
    property Opened : Boolean read GetOpened;
    property Starter : TObject read FStarter;
    property Msg : string read FMsg write FMsg;
  end;

  { TAudioRecorderComponent }

  TAudioRecorderComponent = class sealed (TAudioDevice)
  private
    FOnRecordingFinished: TNotifyEvent;
    procedure SetOnRecordingFinished(AValue: TNotifyEvent);
  protected
    procedure DeviceFinished(Sender: TObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function CanRecord : Boolean;
    function HasRecording : Boolean;
    function Open: Boolean; override;
    procedure Close; override;
    procedure Clear;
    procedure SaveToFile(AFilename : string); overload;
    procedure StartRecording(AStarter : TObject);
    property OnRecordingFinished : TNotifyEvent read FOnRecordingFinished write SetOnRecordingFinished;
  end;

  { TAudioPlaybackComponent }

  TAudioPlaybackComponent = class sealed (TAudioDevice)
  private
    FOnPlaybackFinished: TNotifyEvent;
    procedure SetOnPlaybackFinished(AValue: TNotifyEvent);
  protected
    procedure DeviceFinished(Sender: TObject); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Open: Boolean; override;
    procedure StartPlayback(AStarter : TObject);
    property OnPlaybackFinished : TNotifyEvent read FOnPlaybackFinished write SetOnPlaybackFinished;
  end;

implementation

uses SysUtils, sdl.app.output, fpwavformat
  //, sdl.app.stimulus.contract
  , session.loggers.writerow
  , session.pool;

const
  MAX_RECORDING_SECONDS = 4;
  RECORDING_BUFFER_SECONDS = MAX_RECORDING_SECONDS + 1;


var
  GPRecordingBuffer: PUInt8 = nil;
  GBufferByteSize: Uint32;
  GBufferBytePosition: Uint32;
  GBufferByteMaxPosition: Uint32;
  ACriticalSection : TRTLCriticalSection;

procedure TAudioDevice.Execute;
  procedure DoWork;
  begin
    if not Terminated then begin
      SDL_PauseAudioDevice(FDeviceID, 0);
      while not Terminated do begin
        SDL_LockAudioDevice(FDeviceID);
        if (GBufferBytePosition > GBufferByteMaxPosition) then begin
          SDL_PauseAudioDevice(FDeviceID, 1);
          SDL_UnlockAudioDevice(FDeviceID);
          Break
        end;
        SDL_UnlockAudioDevice(FDeviceID);
        Sleep(15);
        //Log(ClassName+':'+ (GetTickCount64 - First).ToString+ ':'+ GBufferBytePosition.ToString);
      end;
      EnterCriticalSection(ACriticalSection);
      Synchronize(@BufferFinished);
      LeaveCriticalSection(ACriticalSection);
    end;
  end;
begin
  NameThreadForDebugging(ClassName);
  FRTLEventMainThread := RTLEventCreate;
  while not Terminated do begin
    //Log(ClassName+': Waiting for main ...');
    RTLEventWaitFor(FRTLEventMainThread);
    //Log(ClassName+': Working ...');
    DoWork;
  end;
  Log(ClassName+': Terminated ...');
end;

procedure AudioRecorderCallback(AUserdata: Pointer;
  AStream: PUInt8; ALen: Integer); cdecl;
begin
  Move(AStream^, GPRecordingBuffer[GBufferBytePosition], ALen);
  Inc(GBufferBytePosition, ALen);
end;

procedure AudioPlaybackCallback(AUserdata: Pointer;
  AStream: PUInt8; ALen: Integer); cdecl;
begin
  Move(GPRecordingBuffer[GBufferBytePosition], AStream^, ALen);
  Inc(GBufferBytePosition, ALen);
end;

procedure TAudioDevice.ListDevices(AIsCapture : cint; var ADevices: TDevices);
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
    ADevices.Add(LDeviceName, @FDesiredAudioSpec);
  end;
end;

constructor TAudioDevice.Create;
begin
  inherited Create(False);
  FDeviceID := 0;
  FillChar(FAudioSpec, SizeOf(FAudioSpec), 0);
  FillChar(FDesiredAudioSpec, SizeOf(FDesiredAudioSpec), 0);
  FDevices := TDevices.Create;
  FreeOnTerminate := True;
end;

destructor TAudioDevice.Destroy;
begin
  FDevices.Free;
  inherited Destroy;
end;

procedure TAudioDevice.Close;
begin
  if FDeviceID <> 0 then
  begin
    SDL_CloseAudioDevice(FDeviceID);
    FDeviceID := 0;
    FillChar(FAudioSpec, SizeOf(FAudioSpec), 0);
    FillChar(FDesiredAudioSpec, SizeOf(FDesiredAudioSpec), 0);
  end;
  RTLEventSetEvent(FRTLEventMainThread);
  RTLEventDestroy(FRTLEventMainThread);
end;

procedure TAudioDevice.BufferFinished;
begin
  DeviceFinished(Self);
end;

procedure TAudioDevice.PrintMessage;
begin
  Print(Msg);
end;

procedure TAudioDevice.Log(const AMsg: string; AppendLineEnd: boolean);
var
  s: String;
begin
  EnterCriticalsection(ACriticalSection);
  s:=AMsg;
  if AppendLineEnd then
    s:=s+LineEnding;
  Msg:=s;
  Synchronize(@PrintMessage);
  LeaveCriticalsection(ACriticalSection);
end;

function TAudioDevice.GetAudioSpec: TSDL_AudioSpec;
begin
  Result := FAudioSpec
end;

function TAudioDevice.GetOpened: Boolean;
begin
  Result := FDeviceID <> 0;
end;

function TAudioDevice.BytesPerSample: cint;
begin
  Result := FAudioSpec.channels * (SDL_AUDIO_BITSIZE(FAudioSpec.format) div 8);
end;

function TAudioDevice.FileSize: cint; // kilobytes
begin
  with FAudioSpec do begin
    Result := RECORDING_BUFFER_SECONDS *
      freq * SDL_AUDIO_BITSIZE(format) div 8 div 1024;
    Print(Result.ToString);
  end;
end;

procedure TAudioDevice.StartDevice(AStarter: TObject);
begin
  GBufferBytePosition := 0;
  FStarter := AStarter;
  RTLEventSetEvent(FRTLEventMainThread);
end;

procedure TAudioRecorderComponent.SetOnRecordingFinished(AValue: TNotifyEvent);
begin
  if FOnRecordingFinished = AValue then Exit;
  FOnRecordingFinished := AValue;
end;

procedure TAudioRecorderComponent.DeviceFinished(Sender: TObject);
begin
  if Assigned(OnRecordingFinished) then
    OnRecordingFinished(Self);
end;

constructor TAudioRecorderComponent.Create;
begin
  inherited Create;
end;

destructor TAudioRecorderComponent.Destroy;
begin
  inherited Destroy;
end;

function TAudioRecorderComponent.CanRecord: Boolean;
begin
  Result := Opened;
end;

function TAudioRecorderComponent.HasRecording: Boolean;
  function EmptyRecording : Boolean;
  var
    i : integer = 0;
    Zeros : array of Byte = nil;
  begin
    Result := True;
    SetLength(Zeros, GBufferByteSize);
    FillChar(Zeros[0], GBufferByteSize, 0);
    while (i < GBufferByteMaxPosition) and Result do begin
      if not CompareMem(GPRecordingBuffer, @Zeros[0], GBufferByteSize) then
        Result := False;
      Inc(i, GBufferByteSize);
    end;
  end;
begin
  Result := not EmptyRecording;
end;

function TAudioRecorderComponent.Open: Boolean;
var
  LBytesPerSecond: cint;
begin
  ListDevices(1, FDevices);
  FDeviceID := SDL_OpenAudioDevice(FDevices.Keys.ToArray[0], 1,
    FDevices.Values.ToArray[0], @FAudioSpec, SDL_AUDIO_ALLOW_FREQUENCY_CHANGE);

  Print('Name:'+StrPas(FDevices.Keys.ToArray[0]));
  Print('Channels:' + FAudioSpec.channels.ToString);
  Print('Format:' + FAudioSpec.format.ToHexString);
  Print('Samples:' + FAudioSpec.samples.ToString);
  Print('Sample Rate:' + FAudioSpec.freq.ToString);
  Result := FDeviceID <> 0;
  if not Result then begin
    raise
      Exception.Create(TAudioDevice.ClassName+'.Open error: '+SDL_GetError);
    Exit;
  end;

  LBytesPerSecond := FAudioSpec.freq * BytesPerSample;
  GBufferByteSize := RECORDING_BUFFER_SECONDS * LBytesPerSecond;
  GBufferByteMaxPosition := MAX_RECORDING_SECONDS * LBytesPerSecond;
  GetMem(GPRecordingBuffer, GBufferByteSize);
end;

procedure TAudioRecorderComponent.Close;
begin
  inherited Close;
  if Assigned(GPRecordingBuffer) then begin
    FreeMem(GPRecordingBuffer, GBufferByteSize);
    GPRecordingBuffer := nil;
  end;
end;

procedure TAudioRecorderComponent.Clear;
begin
  // fill the entire memory block pointed to by GPRecordingBuffer with zeros
  GBufferBytePosition := 0;
  FillChar(GPRecordingBuffer^, GBufferByteSize, 0);
end;

procedure TAudioRecorderComponent.SaveToFile(AFilename: string);
var
  LWavWriter : TWavWriter;
begin
  LWavWriter := TWavWriter.Create;
  try
    if LWavWriter.StoreToFile(AFileName+'.wav') then begin
      with LWavWriter, FAudioSpec do begin
        fmt.Channels         := channels;
        fmt.SampleRate       := freq;
        fmt.BitsPerSample    := 16;
        fmt.ByteRate         :=
          fmt.SampleRate * fmt.Channels * fmt.BitsPerSample div 8;
        fmt.BlockAlign       := fmt.Channels * fmt.BitsPerSample div 8;
      end;
      LWavWriter.WriteBuf(GPRecordingBuffer^, GBufferByteSize);
      LWavWriter.FlushHeader;
    end else begin
      Print('Error creating WAV file: ' + AFileName);
    end;
  finally
    LWavWriter.Free;
  end;
end;

procedure TAudioRecorderComponent.StartRecording(AStarter: TObject);
begin
  Clear;
  inherited StartDevice(AStarter);
end;

procedure TAudioPlaybackComponent.SetOnPlaybackFinished(AValue: TNotifyEvent);
begin
  if FOnPlaybackFinished = AValue then Exit;
  FOnPlaybackFinished := AValue;
end;

procedure TAudioPlaybackComponent.DeviceFinished(Sender: TObject);
begin
  if Assigned(OnPlaybackFinished) then
    OnPlaybackFinished(Self);
end;

constructor TAudioPlaybackComponent.Create;
begin
  inherited Create;
end;

destructor TAudioPlaybackComponent.Destroy;
begin
  inherited Destroy;
end;

function TAudioPlaybackComponent.Open: Boolean;
begin
  ListDevices(0, FDevices);
  Print(StrPas(FDevices.Keys.ToArray[0]));
  FDeviceID := SDL_OpenAudioDevice(FDevices.Keys.ToArray[0], 0,
    FDevices.Values.ToArray[0], @FAudioSpec, SDL_AUDIO_ALLOW_FREQUENCY_CHANGE);
  Result := FDeviceID <> 0;
  if Result then begin
    { do nothing }
  end else begin
    raise Exception.Create(TAudioDevice.ClassName+'.Open error: '+SDL_GetError);
  end;
end;

procedure TAudioPlaybackComponent.StartPlayback(AStarter: TObject);
begin
  inherited StartDevice(AStarter);
end;

initialization
  InitCriticalSection(ACriticalSection);

finalization
  DoneCriticalsection(ACriticalSection);


end.
