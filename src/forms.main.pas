{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit forms.main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormBackground }

  TFormBackground = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure BeginSession(Sender: TObject);
    procedure EndSession(Sender : TObject);
  private
    //FEyeLink : TEyeLink;
  public

  end;

var
  FormBackground: TFormBackground;

implementation

{$R *.lfm}

uses
  session
  , session.pool
  , session.loggers
  , experiments
  , common.helpers
  , sdl.app
  , sdl.app.output
  ;

{ TFormBackground }

procedure TFormBackground.Button1Click(Sender: TObject);
var
  ITI : integer = 4;
  LH : integer  = 1;
begin
  Pool.RootData := Pool.RootData +
    'Participant' + DirectorySeparator;
  ForceDirectories(Pool.RootData);
  ConfigurationFilename :=
    Experiments.MakeConfigurationFile(
      5,
      ITI.SecondsToMiliseconds,
      LH.MinutesToMiliseconds,
      'A-A',
      1,
      3,
      True,
      True);

  SDLApp := TSDLApplication.Create('Stimulus Control', 1);
  SDLApp.SetupEvents;
  SDLApp.SetupAudio;
  SDLApp.SetupText;

  SDLSession := TSession.Create(Self);
  //SDLSession.OnBeforeStart := @BeginSession;
  //SDLSession.OnEndSession  := @EndSession;
  SDLSession.Play;

  SDLApp.Run;
end;

procedure TFormBackground.BeginSession(Sender: TObject);
begin
  TLogger.SetHeader('Session Name', 'Participant');
end;

procedure TFormBackground.EndSession(Sender: TObject);
begin
  TLogger.SetFooter;
end;

end.

