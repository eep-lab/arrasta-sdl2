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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  IniPropStorage;

type

  { TFormBackground }

  TFormBackground = class(TForm)
    ButtonLoadConfigurationFile: TButton;
    ButtonNewConfigurationFile: TButton;
    ButtonRunSession: TButton;
    ComboBoxParticipant: TComboBox;
    IniPropStorage1: TIniPropStorage;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    procedure ButtonLoadConfigurationFileClick(Sender: TObject);
    procedure ButtonNewConfigurationFileClick(Sender: TObject);
    procedure ButtonRunSessionClick(Sender: TObject);
    procedure BeginSession(Sender: TObject);
    procedure EndSession(Sender : TObject);
    procedure CloseSDLApp(Sender : TObject);
  private
    //FEyeLink : TEyeLink;
    function ParticipantName : string;
  public

  end;

var
  FormBackground: TFormBackground;

implementation

{$R *.lfm}

uses
  FileUtil
  , session
  , session.pool
  , session.loggers
  , session.fileutils
  , experiments
  , sdl.app
  , sdl.app.output
  , eyelink.classes
  ;

{ TFormBackground }

procedure TFormBackground.ButtonRunSessionClick(Sender: TObject);
begin

  if ConfigurationFilename.IsEmpty then begin
    ShowMessage('Crie uma nova sessão ou abra uma pronta.');
    Exit;
  end;

  if ComboBoxParticipant.ItemIndex < 0 then begin
    ShowMessage('Escolha um participante.');
    Exit;
  end;

  Pool.RootData := Pool.RootData +
    ParticipantName + DirectorySeparator;
  ForceDirectories(Pool.RootData);

  SDLApp := TSDLApplication.Create('Stimulus Control', 0);
  SDLApp.SetupEvents;
  SDLApp.SetupAudio;
  SDLApp.SetupText;
  SDLApp.OnClose := @CloseSDLApp;

  SDLSession := TSession.Create(Self);
  SDLSession.OnBeforeStart := @BeginSession;
  SDLSession.OnEndSession  := @EndSession;
  SDLSession.Play;

  SDLApp.Run;
end;

procedure TFormBackground.ButtonNewConfigurationFileClick(Sender: TObject);
begin
  ConfigurationFilename := Experiments.MakeConfigurationFile;
end;

procedure TFormBackground.ButtonLoadConfigurationFileClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
    ConfigurationFilename := LoadConfigurationFile(OpenDialog1.FileName);
end;

procedure TFormBackground.BeginSession(Sender: TObject);
begin
  CopyFile(ConfigurationFilename, Pool.BaseFilename+'.ini');
  TLogger.SetHeader('Session Name', 'Participant');
end;

procedure TFormBackground.EndSession(Sender: TObject);
begin
  TLogger.SetFooter;
end;

procedure TFormBackground.CloseSDLApp(Sender: TObject);
begin
  SDLSession.Free;
end;

function TFormBackground.ParticipantName: string;
begin
  Result := ComboBoxParticipant.Items[ComboBoxParticipant.ItemIndex];
end;

end.

