{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.stimulus.audio;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , SDL2
  , fgl
  , sdl.app.audio.contract
  , sdl.app.stimulus
  , sdl.app.graphics.picture
  ;

type

  { TAudioStimulus }

  TAudioStimulus = class(TStimulus)
    private
      FSound : ISound;
      FPicture : TPicture;
    public
    procedure DoResponse; override;
    procedure Load(AParameters : TStringList;
        AParent : TObject; ARect: TSDL_Rect); override;
    procedure Start; override;
    procedure Stop; override;
  end;

implementation

uses sdl.app.audio
   , sdl.app.renderer.custom
   ;

{ TAudioStimulus }

procedure TAudioStimulus.DoResponse;
begin

end;

procedure TAudioStimulus.Load(AParameters: TStringList; AParent: TObject;
  ARect: TSDL_Rect);
var
  LAudioPicture : string;
begin
  LAudioPicture:=AParameters.Values['Media']+'.wav';
  FSound := SDLAudio.SoundFromShortPath(AParameters.Values['Media']+'.wav');
  FPicture := TPicture.Create(Self);
  if AParameters.Values['AudioPicture'].IsEmpty then begin
    LAudioPicture := 'AudioPicture';
  end else begin
    LAudioPicture := AParameters.Values['AudioPicture'];
  end;
  FPicture.LoadFromFile(LAudioPicture+'.png');
  FPicture.BoundsRect := ARect;
  FPicture.Parent := TCustomRenderer(AParent);
  FPicture.Show;
end;

procedure TAudioStimulus.Start;
begin
  FSound.Play;
end;

procedure TAudioStimulus.Stop;
begin
  FPicture.Hide;
end;

end.

