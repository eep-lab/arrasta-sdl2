{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.stimulus.picture;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , SDL2
  , fgl
  , sdl.app.graphics.picture
  , sdl.app.stimulus
  ;

type

  { TPictureStimulus }

  TPictureStimulus = class(TStimulus)
    private
      FPicture : TPicture;
    public
      procedure Load(AParameters : TStringList;
        AParent : TObject; ARect: TSDL_Rect); override;
      procedure Start; override;
      procedure Stop; override;
      procedure DoResponse; override;
  end;

implementation

uses
   sdl.app.renderer.custom;


{ TPictureStimulus }

procedure TPictureStimulus.DoResponse;
begin

end;

procedure TPictureStimulus.Load(AParameters: TStringList; AParent: TObject;
  ARect: TSDL_Rect);
var
  S : string;
begin
  S := AParameters.Values['Media']+'.jpg';
  FPicture.LoadFromFile(AParameters.Values['Media']+'.jpg');
  FPicture.BoundsRect := ARect;
  FPicture.Parent := TCustomRenderer(AParent);
  FPicture.Show;
end;

procedure TPictureStimulus.Start;
begin
  FPicture.Show;
end;

procedure TPictureStimulus.Stop;
begin
  FPicture.Hide;
end;

end.

