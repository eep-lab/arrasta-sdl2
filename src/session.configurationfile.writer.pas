{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.configurationfile.writer;

interface

uses
  Classes, SysUtils, Session.ConfigurationFile;

type

  TStartAt = record
    Trial : integer;
    Bloc  : integer;
  end;

  { TConfigurationWriter }

  TConfigurationWriter = class
  private
    FBlocConfig: TStringList;
    FStartAt: TStartAt;
    FTrialConfig: TStringList;
    FCurrentBloc : integer;
    FConfigurationFile: TConfigurationFile;
    function GetCurrentTrial: integer;
    procedure SetStartTrial(AValue: TStartAt);
  public
    constructor Create(AConfigurationFile: TConfigurationFile); reintroduce;
    destructor Destroy; override;
    procedure WriteBloc;
    procedure WriteTrial;
    property BlocConfig : TStringList read FBlocConfig;
    property TrialConfig: TStringList read FTrialConfig;
    property CurrentBloc  : integer read FCurrentBloc write FCurrentBloc;
    property CurrentTrial : integer read GetCurrentTrial;
    property StartAt : TStartAt read FStartAt write SetStartTrial;
  end;

implementation

{ TConfigurationWriter }

function TConfigurationWriter.GetCurrentTrial: integer;
begin
  Result := FConfigurationFile.TrialCount[CurrentBloc+1];
end;

procedure TConfigurationWriter.SetStartTrial(AValue: TStartAt);
begin
  if (FStartAt.Bloc=AValue.Bloc) and (FStartAt.Trial=AValue.Trial) then Exit;
  FStartAt:=AValue;
  with FConfigurationFile do begin
    WriteToMain('StartAt', StartAt.Bloc.ToString + '-' + StartAt.Trial.ToString);
  end;
end;

constructor TConfigurationWriter.Create(AConfigurationFile: TConfigurationFile);
begin
  if not Assigned(ConfigurationFile) then
    raise EFilerError.Create('Configuration file not assigned.');

  FConfigurationFile := AConfigurationFile;
  FBlocConfig := TStringList.Create;
  FTrialConfig:= TStringList.Create;
  FCurrentBloc := 0;
end;

destructor TConfigurationWriter.Destroy;
begin
  FTrialConfig.Free;
  FBlocConfig.Free;
  FConfigurationFile := nil;
  inherited Destroy;
end;

procedure TConfigurationWriter.WriteBloc;
var
  i: integer;
  LName , LValue: string;
begin
  with FConfigurationFile do begin
    for i := 0 to FBlocConfig.Count -1 do begin
      FBlocConfig.GetNameValue(i, LName, LValue);
      WriteToBloc(Self.CurrentBloc+1, LName, LValue);
    end;
  end;
end;

procedure TConfigurationWriter.WriteTrial;
var
  i: integer;
  LName, LValue : string;
  LCurrentTrial : integer;
  LCurrentBloc  : integer;
begin
  LCurrentTrial := CurrentTrial+1;
  LCurrentBloc  := CurrentBloc+1;
  with FConfigurationFile do begin
    for i := 0 to FTrialConfig.Count -1 do begin
      FTrialConfig.GetNameValue(i, LName, LValue);
      WriteToTrial(LCurrentTrial, LCurrentBloc, LName, LValue);
    end;
  end;
end;

end.
