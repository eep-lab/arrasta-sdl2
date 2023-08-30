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
    FVolume : int32;
    FChannels : TChannels;
    function GetSetVolume: int32;
    function AllocatedChannels : cint;
    procedure SDLAudioChannelFinished(const AChannel: cint32);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function SoundFromName(AName : string) : ISound;
    function SoundFromShortPath(AShortPath : string) : ISound;
    procedure LoadFromFile(AFilename : string);
    function RegisterChannel(Sound : ISound) : cint;
    property Volume : int32 read GetSetVolume write FVolume;
  end;

  procedure AllocateAudioChannels;

const
  SESSION_CHUNK_STOPPED = SDL_USEREVENT+2;

var
  SDLAudio : TSDLAudio;

implementation

uses sdl.app.output, sdl.app.audio.chunk, sdl.app.events.custom;

procedure ChannelFinishedCallback(channel : cint); cdecl;
var
  event : TSDL_Event;
begin
  event.type_ := SESSION_CHUNK_STOPPED;
  event.user.code := channel;
  SDL_PushEvent(@event);
end;

procedure AllocateAudioChannels;
var
  i, j: Integer;
begin
  SDLAudio.LoadFromFile('acerto.wav');
  SDLAudio.LoadFromFile('erro.wav');
  for i := 1 to 6 do begin
    for j := 1 to 4 do begin
      SDLAudio.LoadFromFile(
        Format('%02d', [i])+PathSep+'A'+j.ToString+'.wav');
    end;
  end;
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
  FChannels.Free;
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
end;

function TSDLAudio.SoundFromShortPath(AShortPath: string): ISound;
var
  IChunk : ISound;
begin
  if FChannels.Count = 0 then Exit;
  Result := nil;
  for IChunk in FChannels do
    if IChunk.ShortPath.ToUpper = AShortPath.ToUpper then begin
      Result := IChunk;
      Exit;
    end;
end;

procedure TSDLAudio.LoadFromFile(AFilename: string);
var
  LChunk : TChunk;
begin
  LChunk := TChunk.Create;
  LChunk.LoadFromFile(AFilename);
end;

function TSDLAudio.RegisterChannel(Sound: ISound) : cint;
begin
  if FChannels.Count = AllocatedChannels then begin
    Mix_AllocateChannels(FChannels.Count+1)
  end;
  Result:= FChannels.Add(Sound);
end;

end.

