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
  //, fgl
  , sdl.app.audio.contract
  , sdl.app.stimulus
  , sdl.app.graphics.picture
  , sdl.app.events.abstract
  ;

type

  { TAudioStimulus }

  TAudioStimulus = class(TStimulus)
  private
    FRect : TSDL_Rect;
    FSound : ISound;
    FPicture : TPicture;
  protected
    procedure SoundFinished(Sender: TObject);
    procedure MouseDown(Sender: TObject; Shift: TCustomShiftState;
      X, Y: Integer); override;
    procedure MouseEnter(Sender: TObject); override;
    procedure MouseExit(Sender: TObject); override;
  public
    destructor Destroy; override;
    procedure Load(AParameters : TStringList;
        AParent : TObject; ARect: TSDL_Rect); override;
    procedure Start; override;
    procedure Stop; override;
    property Picture : TPicture read FPicture;
  end;

implementation

uses sdl.app.audio
   , sdl.app.renderer.custom
   , session.constants.mts;

{ TAudioStimulus }

procedure TAudioStimulus.SoundFinished(Sender: TObject);
begin
  if IsSample then begin
    DoResponse;
    FSound.SetOnStop(nil);
  end else begin
    DoResponse;
  end;
end;

procedure TAudioStimulus.MouseDown(Sender: TObject; Shift: TCustomShiftState;
  X, Y: Integer);
begin
  if SDLAudio.Playing then begin
    { do nothing }
  end else begin
    if Assigned(OnMouseDown) then
      OnMouseDown(Self, Shift, X, Y);
    FSound.Play;
  end;
end;

procedure TAudioStimulus.MouseEnter(Sender: TObject);
begin
  if IsSample then begin
    { do nothing }
  end else begin
    //FButtonPicture.Show;
  end;
end;

procedure TAudioStimulus.MouseExit(Sender: TObject);
begin
  if IsSample then begin
    { do nothing }
  end else begin
    //FButtonPicture.Hide;
  end;
end;

destructor TAudioStimulus.Destroy;
begin
  //SDLAudio.UnregisterChannel(FSound);
  //FSound.Free;
  inherited Destroy;
end;

procedure TAudioStimulus.Load(AParameters: TStringList; AParent: TObject;
  ARect: TSDL_Rect);
var
  LSound: string;
  LAudioPicture : string;
begin
  FRect := ARect;
  FPicture := TPicture.Create(Self);
  if AParameters.Values['AudioPicture'].IsEmpty then begin
    LAudioPicture := 'AudioPicture';
  end else begin
    LAudioPicture := AParameters.Values['AudioPicture'];
  end;
  FPicture.LoadFromFile(LAudioPicture+'.png');
  FPicture.BoundsRect := ARect;
  FPicture.Parent := TCustomRenderer(AParent);
  FPicture.OnMouseDown := @MouseDown;
  FPicture.OnMouseEnter := @MouseEnter;
  FPicture.OnMouseExit := @MouseExit;

  if IsSample then begin
    LSound := AParameters.Values[MTSKeys.Word];
  end else begin
    LSound := AParameters.Values[MTSKeys.Comparison+(Index+1).ToString];
  end;

  FSound := SDLAudio.LoadFromFile(LSound+'.wav');
  FSound.SetOnStop(@SoundFinished);
end;

procedure TAudioStimulus.Start;
begin
  if IsSample then begin
    FSound.Play;
    FPicture.Show;
  end else begin
    FPicture.Show;
  end;
end;

procedure TAudioStimulus.Stop;
begin
  FPicture.Hide;
end;

end.

