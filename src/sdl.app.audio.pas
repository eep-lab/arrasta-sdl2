{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.audio;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils
  , fgl
  , ctypes
  , sdl2
  , sdl2_mixer
  , sdl.app.audio.contract
  ;

type

  TChannels = specialize TFPGList<ISound>;

  { TSDLAudio }

  TSDLAudio = class
  private
    FOnChannelFinished: TMix_Channel_Finished;
    FVolume : int32;
    FChannels : TChannels;
    function GetPlaying: Boolean;
    function GetSetVolume: int32;
    // Returns the current number of channels without changing anything.
    function AllocatedChannels : cint;
    procedure SDLAudioChannelFinished(const AChannel: cint32);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure UnregisterChannel(Sound : ISound);
    function LoadFromFile(AFilename : string) : ISound;
    function SoundFromName(AName : string) : ISound;
    function RegisterChannel(Sound : ISound) : cint;
    property Volume : int32 read GetSetVolume write FVolume;
    property Playing : Boolean read GetPlaying;
  end;

  procedure AllocateDefaultAudioChannels;

const
  SESSION_CHUNK_STOPPED = SDL_USEREVENT+2;

var
  SDLAudio : TSDLAudio;

implementation

uses sdl.app.output, sdl.app.audio.chunk, sdl.app.events.custom, session.pool;

procedure ChannelFinishedCallback(channel : cint); cdecl;
var
  event : TSDL_Event;
begin
  event.type_ := SESSION_CHUNK_STOPPED;
  event.user.code := channel;
  SDL_PushEvent(@event);
end;

procedure AllocateDefaultAudioChannels;
begin
  SDLAudio.LoadFromFile(Pool.AssetsBasePath+DirectorySeparator+'acerto.wav');
  SDLAudio.LoadFromFile(Pool.AssetsBasePath+DirectorySeparator+'erro.wav');
end;

{ TSDLAudio }

procedure TSDLAudio.SDLAudioChannelFinished(const AChannel: cint32);
begin
  FChannels[AChannel].DoOnStop;
end;

function TSDLAudio.GetSetVolume: int32;
begin
  Result := (MIX_MAX_VOLUME*FVolume) div 100;
end;

function TSDLAudio.GetPlaying: Boolean;
var
  IChunk: ISound;
begin
  Result := False;
  if FChannels.Count = 0 then Exit;
  for IChunk in FChannels do
    if IChunk.Playing then begin
      Result := True;
      Exit;
    end;
end;

function TSDLAudio.AllocatedChannels: cint;
begin
  Result := Mix_AllocateChannels(-1);
end;

constructor TSDLAudio.Create;
begin
  EventHandler.OnAudioChannelFinished := @SDLAudioChannelFinished;
  FChannels := TChannels.Create;
  if Mix_OpenAudio(44100, MIX_DEFAULT_FORMAT, 2, 1048) < 0 then begin
    Print('Audio not initialized:'+Mix_GetError^);
  end else begin
    Print('Audio initialized.');
  end;
  Mix_AllocateChannels(1);
  Mix_ChannelFinished(@ChannelFinishedCallback);
end;

destructor TSDLAudio.Destroy;
var
  IChunk : ISound;
begin
  for IChunk in FChannels do IChunk.Free;
  Mix_AllocateChannels(0);
  FChannels.Free;
  Mix_CloseAudio;
  inherited Destroy;
end;

function TSDLAudio.SoundFromName(AName: string): ISound;
var
  IChunk : ISound;
begin
  if FChannels.Count = 0 then Exit;
  Result := nil;
  for IChunk in FChannels do
    if IChunk.ShortName.ToUpper = AName.ToUpper then begin
      Result := IChunk;
      Exit;
    end;
  raise EFileNotFoundException.Create('TSDLAudio.SoundFromName'+AName);
end;

function TSDLAudio.LoadFromFile(AFilename: string): ISound;
var
  LChunk : TChunk;
begin
  LChunk := TChunk.Create;
  LChunk.LoadFromFile(AFilename);
  LChunk.Channel := SDLAudio.RegisterChannel(LChunk.AsInterface);
  Result := LChunk.AsInterface;
end;

procedure TSDLAudio.UnregisterChannel(Sound: ISound);
begin
  FChannels.Remove(Sound);
  Mix_AllocateChannels(FChannels.Count);
  if FChannels.Count <> AllocatedChannels then
    raise Exception.Create(
      'Error: TSDLAudio.UnregisterChannel incosistency. Audio may not play.');
end;

function TSDLAudio.RegisterChannel(Sound: ISound) : cint;
begin
  Result:= FChannels.Add(Sound);
  if AllocatedChannels < FChannels.Count then begin
    Mix_AllocateChannels(FChannels.Count);
  end;
end;

end.

