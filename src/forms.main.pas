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
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, ExtCtrls,
  IniPropStorage, ComCtrls, Menus, PropertyStorage;

type

  { TFormBackground }

  TFormBackground = class(TForm)
    ButtonOpenSpeechValidation: TButton;
    ButtonTestCondition: TButton;
    ButtonMisc: TButton;
    ButtonNewParticipant: TButton;
    ButtonLoadConfigurationFile: TButton;
    ButtonNewConfigurationFile: TButton;
    ButtonRunSession: TButton;
    ComboBoxDesignFolder: TComboBox;
    ComboBoxParticipant: TComboBox;
    IniPropStorage1: TIniPropStorage;
    LabelSessionEndCriteria: TLabel;
    LabelLastSessionName: TLabel;
    ListBoxCondition: TListBox;
    MenuItemShowWordsPerCycle: TMenuItem;
    MenuItemOutputWordsPerCyle: TMenuItem;
    MenuItemCyclesFromTemplate: TMenuItem;
    MenuItemConvertDesignFile: TMenuItem;
    MenuItemRemoveParticipant: TMenuItem;
    MenuItemCopyPNGFiles: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    Panel1: TPanel;
    PopupMenuParticipants: TPopupMenu;
    PopupMenuMisc: TPopupMenu;
    ProgressBar: TProgressBar;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure ButtonOpenSpeechValidationClick(Sender: TObject);
    procedure ButtonTestConditionClick(Sender: TObject);
    procedure ButtonLoadConfigurationFileClick(Sender: TObject);
    procedure ButtonMiscClick(Sender: TObject);
    procedure ButtonNewConfigurationFileClick(Sender: TObject);
    procedure ButtonNewParticipantClick(Sender: TObject);
    procedure ButtonRunSessionClick(Sender: TObject);
    procedure BeginSession(Sender: TObject);
    procedure ComboBoxDesignFolderEditingDone(Sender: TObject);
    procedure ComboBoxParticipantEditingDone(Sender: TObject);
    procedure EndSession(Sender : TObject);
    procedure CloseSDLApp(Sender : TObject);
    procedure FormCreate(Sender: TObject);
    procedure IniPropStorage1RestoreProperties(Sender: TObject);
    procedure IniPropStorage1StoredValues0Restore(Sender: TStoredValue;
      var Value: TStoredType);
    procedure IniPropStorage1StoredValues0Save(Sender: TStoredValue;
      var Value: TStoredType);
    procedure IniPropStorage1StoredValues1Restore(Sender: TStoredValue;
      var Value: TStoredType);
    procedure IniPropStorage1StoredValues1Save(Sender: TStoredValue;
      var Value: TStoredType);
    procedure MenuItemConvertDesignFileClick(Sender: TObject);
    procedure MenuItemCopyPNGFilesClick(Sender: TObject);
    procedure MenuItemCyclesFromTemplateClick(Sender: TObject);
    procedure MenuItemOutputWordsPerCyleClick(Sender: TObject);
    procedure MenuItemRemoveParticipantClick(Sender: TObject);
    procedure MenuItemShowWordsPerCycleClick(Sender: TObject);
  private
    FSessionName : string;
    procedure HitCriteriaAtSessionEnd(Sender : TObject);
    procedure NotHitCriteriaAtSessionEnd(Sender : TObject);
    procedure AssignGlobalVariables;
    procedure ToogleControlPanelEnabled(AException: TComponent = nil);
    function ParticipantFolderName : string;
    function SessionName : string;
    function SetupFolders : Boolean;
    function Validated : Boolean;
  public

  end;

var
  FormBackground: TFormBackground;

implementation

{$R *.lfm}

uses
  FileUtil
  , Graphics
  , common.helpers
  , forms.main.misc
  , forms.test.session.endcriteria
  , forms.speechvalidation
  , session
  , session.information
  , session.parameters.global
  , session.pool
  , session.fileutils
  , session.csv.experiments
  , session.design.conversion
  , sdl.app
  , sdl.app.controller.manager
  , sdl.app.grids.types
  , sdl.app.testmode
  , eye.tracker
  , ui.backup
  , picanco.experiments.images
  , picanco.experiments.output
  ;

{ ToDo: show next designed session of selected participant.
        for data in data folder get next session of last session}

{ ToDo: point-and-click design tools }

{ TFormBackground }

procedure TFormBackground.ButtonRunSessionClick(Sender: TObject);
begin
  if not Validated then Exit;

  FormMisc.Initialize;
  AssignGlobalVariables;
  ToogleControlPanelEnabled;

  SDLApp := TSDLApplication.Create(@Pool.AppName[1]);
  SDLApp.SetupVideo(FormMisc.ComboBoxMonitor.ItemIndex);
  //SDLApp.PrintRendererSetup;
  SDLApp.SetupAudio;
  SDLApp.SetupText;
  SDLApp.OnClose := @CloseSDLApp;
  SDLApp.ShowMarkers := FormMisc.CheckBoxShowMarkers.Checked;

  Controllers := TControllerManager.Create;
  Controllers.CreateController(FormMisc.ComboBoxController.ItemIndex);

  Pool.App := SDLApp;

  InitializeEyeTracker(FormMisc.ComboBoxEyeTracker.ItemIndex);

  Pool.SessionName := SessionName;
  Pool.ParticipantName := ParticipantFolderName;

  SDLSession := TSession.Create(Self);
  SDLSession.OnBeforeStart := @BeginSession;
  SDLSession.OnEndSession  := @EndSession;
  SDLSession.OnHitCriteriaAtSessionEnd := @HitCriteriaAtSessionEnd;
  SDLSession.OnNotHitCriteriaAtSessionEnd := @NotHitCriteriaAtSessionEnd;
  SDLSession.Play;

  SDLApp.Run;
end;

procedure TFormBackground.ButtonNewConfigurationFileClick(Sender: TObject);
var
  LFilename : string;
begin
  AssignGlobalVariables;
  if ListBoxCondition.Items.Count = 0 then begin
    ShowMessage('A pasta de parâmetros (design) está vazia.');
    Exit;
  end;

  if ListBoxCondition.ItemIndex = -1 then begin
    ShowMessage('Escolha um parâmetro.');
    Exit;
  end else begin
    with ListBoxCondition do begin
      LFilename := Items[ItemIndex];
    end;
  end;

  ToogleControlPanelEnabled(ProgressBar);
  Pool.ConfigurationFilename := MakeConfigurationFile(LFilename);
  ProgressBar.Visible := True;
  ToogleControlPanelEnabled(ProgressBar);
end;

procedure TFormBackground.ButtonNewParticipantClick(Sender: TObject);
var
  LNewParticipant : string;
begin
  with ComboBoxParticipant do begin
    LNewParticipant :=
      InputBox(Pool.AppName,
      'Nome: mínimo de 3 caracteres',
      '');

    if LNewParticipant.IsEmpty or (Length(LNewParticipant) < 3) then Exit;
    Items.Append(LNewParticipant);
  end;
end;

procedure TFormBackground.ButtonLoadConfigurationFileClick(Sender: TObject);
begin
  SetupFolders; // todo: pass filename id of loaded file into session.counters.loadfromfile
  OpenDialog1.InitialDir := Pool.BaseDataPath;
  if OpenDialog1.Execute then begin
    Pool.ConfigurationFilename := LoadConfigurationFile(OpenDialog1.FileName);
    FSessionName := LoadInformationFromFile(OpenDialog1.FileName).SessionName;
    ShowMessage(FSessionName);
    ProgressBar.Max := 1;
    ProgressBar.StepIt;
    ProgressBar.Visible := True;
  end;
end;

procedure TFormBackground.ButtonTestConditionClick(Sender: TObject);
begin
  FormEndCriteriaTest.Show;
end;

procedure TFormBackground.ButtonOpenSpeechValidationClick(Sender: TObject);
begin
  FormSpeechValidationQueue.Show;
end;

procedure TFormBackground.ButtonMiscClick(Sender: TObject);
begin
  FormMisc.ShowModal;
end;

procedure TFormBackground.BeginSession(Sender: TObject);
begin
  if Assigned(EyeTracker) then begin
    EyeTracker.StartRecording;
  end;
end;

procedure TFormBackground.ComboBoxDesignFolderEditingDone(Sender: TObject);
begin
  if ComboBoxDesignFolder.Items.Count > 0 then begin
    with Pool, ComboBoxDesignFolder do begin
      DesignBasePath := Items[ItemIndex];
    end;
    ListBoxCondition.Clear;
    GetDesignFilesFor(ListBoxCondition.Items);
  end;
end;

procedure TFormBackground.ComboBoxParticipantEditingDone(Sender: TObject);
var
  LInformation : TInformation;
begin
  if ComboBoxParticipant.Items.Count > 0 then begin
    SetupFolders;
    if LastValidBaseFilenameFileExists then begin
      LInformation := LoadInformationFromFile(GetLastValidInformationFile);
    end else begin
      ShowMessage('Guessing last valid information file');
      LInformation := LoadInformationFromFile(GuessLastValidInformationFile);
    end;

    if LInformation.Basename.IsEmpty then begin
      LabelLastSessionName.Caption := '?';
      LabelSessionEndCriteria.Caption := '?';
    end else begin
      LabelLastSessionName.Caption := LInformation.SessionName;
      LabelSessionEndCriteria.Caption := LInformation.SessionResult;
    end;
  end;
end;

procedure TFormBackground.EndSession(Sender: TObject);
begin

end;

procedure TFormBackground.CloseSDLApp(Sender: TObject);
begin
  if Assigned(EyeTracker) then begin
    EyeTracker.StopRecording;
    FinalizeEyeTracker;
  end;
  FormMisc.Finalize;
  SDLSession.Free;
  SDLApp.Free;
  FreeConfigurationFile;
  Controllers.Free;
  ToogleControlPanelEnabled;
  ProgressBar.Visible := False;
  FSessionName := '';
end;

procedure TFormBackground.FormCreate(Sender: TObject);
begin
  FSessionName := '';
end;

procedure TFormBackground.IniPropStorage1RestoreProperties(Sender: TObject);
begin
  SetupFolders;
end;

procedure TFormBackground.IniPropStorage1StoredValues0Restore(
  Sender: TStoredValue; var Value: TStoredType);
var
  LValue : integer;
begin
  GetDesignFoldersFor(ComboBoxDesignFolder.Items);
  with Pool, ComboBoxDesignFolder do begin
    LValue := Value.ToInteger;
    if (LValue < Items.Count) and (LValue <> -1) then begin
      DesignBasePath := Items[LValue];
    end;
  end;
end;

procedure TFormBackground.IniPropStorage1StoredValues0Save(
  Sender: TStoredValue; var Value: TStoredType);
begin
  Value := ComboBoxDesignFolder.ItemIndex.ToString;
end;

procedure TFormBackground.IniPropStorage1StoredValues1Restore(
  Sender: TStoredValue; var Value: TStoredType);
var
  LValue : integer;
begin
  GetDesignFilesFor(ListBoxCondition.Items);
  LValue := Value.ToInteger;
  if LValue < ListBoxCondition.Count then begin
    ListBoxCondition.ItemIndex := LValue;
  end;
end;

procedure TFormBackground.IniPropStorage1StoredValues1Save(
  Sender: TStoredValue; var Value: TStoredType);
begin
  Value := ListBoxCondition.ItemIndex.ToString;
end;

procedure TFormBackground.MenuItemConvertDesignFileClick(Sender: TObject);
begin
  CovertToSingleFilename;
end;

procedure TFormBackground.MenuItemCopyPNGFilesClick(Sender: TObject);
begin
  SetupFolders;
  E1CopyRandomImagesToParticipantFolder;
end;

procedure TFormBackground.MenuItemCyclesFromTemplateClick(Sender: TObject);
begin
  CyclesFromTemplate;
end;

procedure TFormBackground.MenuItemOutputWordsPerCyleClick(Sender: TObject);
begin
  PrintWordsPerCycle;
end;

procedure TFormBackground.MenuItemRemoveParticipantClick(Sender: TObject);
begin
  with ComboBoxParticipant do
    Items.Delete(ItemIndex);
end;

procedure TFormBackground.MenuItemShowWordsPerCycleClick(Sender: TObject);
begin
  ShowWordsPerCycle;
end;

procedure TFormBackground.HitCriteriaAtSessionEnd(Sender: TObject);
const
  LResult = 'Critério atingido';
begin
  SetSessionResult(LResult);
  with ListBoxCondition do begin
    LabelLastSessionName.Caption := Items[ItemIndex];
  end;
  LabelSessionEndCriteria.Color := clGreen;
  LabelSessionEndCriteria.Caption := LResult;
end;

procedure TFormBackground.NotHitCriteriaAtSessionEnd(Sender: TObject);
const
  LResult = 'Critério não atingido';
begin
  SetSessionResult(LResult);
  with ListBoxCondition do begin
    LabelLastSessionName.Caption := Items[ItemIndex];
  end;
  LabelSessionEndCriteria.Color := clRed;
  LabelSessionEndCriteria.Caption := LResult;
end;

procedure TFormBackground.AssignGlobalVariables;
begin
  TestMode := FormMisc.CheckBoxTestMode.Checked;

  GlobalTrialParameters.Cursor := 1;

  with GlobalTrialParameters, FormMisc.FloatSpinEditScreenWidth do
    ScreenInCentimeters := Value;

  with GlobalTrialParameters, FormMisc.FloatSpinEditCellsSize do
    CellsSizeInCentimenter := Value;

  with GlobalTrialParameters, FormMisc.CheckBoxHideMouse do
    HideMouse := Checked;

  with GlobalTrialParameters, FormMisc.SpinEditAprilTagsSize do
    MarkerSize := Value;

  with GlobalTrialParameters,
       FormMisc.CheckBoxShowModalFormForSpeechResponses do
    ShowModalFormForSpeechResponses := Checked;

  with GlobalTrialParameters, FormMisc.ComboBoxShouldRestartAt do
    ShouldRestartAtBlockStart := ItemIndex = 0;

  with GlobalTrialParameters, FormMisc.ComboBoxAudioPromptForText do
    AudioPromptForText := Items[ItemIndex];

  with GlobalTrialParameters, FormMisc.ComboBoxFontName do
    FontName := Items[ItemIndex];

  with GlobalTrialParameters, FormMisc.SpinEditFontSize do
    FontSize := Value;

  with GlobalTrialParameters, FormMisc.SpinEditRecordingSeconds do
    RecordingSeconds := Value;

  with GlobalTrialParameters, FormMisc.SpinEditInterTrialInterval do
    InterTrialInterval := Value;

  with GlobalTrialParameters, FormMisc.SpinEditLimitedHold do
    LimitedHold := Value.MinutesToMiliseconds;

  with GlobalTrialParameters, FormMisc.SpinEditAudioLoopInterval do
    AudioLoopInterval := Value;

  with GlobalTrialParameters, FormMisc.SpinEditDefaultAudioLoops do
    DefaultAudioLoops := Value;

  with GlobalTrialParameters, FormMisc.SpinEditTimeOut do
    TimeOutInterval := Value;

  with GlobalTrialParameters, FormMisc.ComboBoxAudioFolder do
    Pool.AudioBasePath := Items[ItemIndex];

  with GlobalTrialParameters, FormMisc.ComboBoxFixedSamplePosition do begin
    GridOrientation := goCustom;
    case ItemIndex of
      1: begin // centralize sample, use 4 corners for comparisons
        FixedSamplePosition := 4;
        FixedComparisonPosition := 4;
        SetLength(ComparisonPositions, 4);
        ComparisonPositions[0] := 0;
        ComparisonPositions[1] := 2;
        ComparisonPositions[2] := 6;
        ComparisonPositions[3] := 8;
      end
      else begin // upper sample, use 3 bottom positions for comparisons
        FixedSamplePosition := 1;
        SetLength(ComparisonPositions, 3);
        ComparisonPositions[0] := 6;
        ComparisonPositions[1] := 7;
        ComparisonPositions[2] := 8;
      end;
    end;
  end;
end;

procedure TFormBackground.ToogleControlPanelEnabled(AException: TComponent);
var
  i: Integer;
begin
  for i := 0 to ComponentCount -1 do begin
    if (Components[i] is TControl) and
       (Components[i] <> AException) then begin
      TControl(Components[i]).Enabled := not TControl(Components[i]).Enabled;
    end;
  end;
end;

function TFormBackground.ParticipantFolderName: string;
begin
  if ComboBoxParticipant.Items.Count > 0 then begin
    Pool.Counters.Subject := ComboBoxParticipant.ItemIndex;
    Result := Pool.Counters.Subject.ToString +'-'+
        ComboBoxParticipant.Items[Pool.Counters.Subject] +
        DirectorySeparator;
  end;
end;

function TFormBackground.SessionName: string;
begin
  if FSessionName.IsEmpty then begin
    with ListBoxCondition do begin
      Result := Items[ItemIndex];
    end;
  end else begin
    Result := FSessionName;
  end;
end;

function TFormBackground.SetupFolders: Boolean;
begin
  Pool.ImageBasePath := ParticipantFolderName;
  Pool.BaseDataPath :=
    ConcatPaths([Pool.DataRootBasePath, ParticipantFolderName]);

  Pool.DataResponsesBasePath :=
    ConcatPaths([Pool.BaseDataPath, Pool.ResponsesBasePath]);

  Result :=
    ForceDirectories(Pool.BaseDataPath) and
    ForceDirectories(Pool.DataResponsesBasePath);
end;

function TFormBackground.Validated: Boolean;
  function SetupParticipantID : Boolean;
  var
    LParticipantID: TStringList;
    LIDFile : string;
    LID : string;
  begin
    LIDFile := Pool.DataRootBasePath + ParticipantFolderName + 'ID';
    LID := Pool.Counters.Subject.ToString;
    LParticipantID := TStringList.Create;
    try
      if FileExists(LIDFile) then begin
        LParticipantID.LoadFromFile(LIDFile);
        if LID = LParticipantID[0] then begin
          Result := True;
        end else begin
          Result := False;
          ShowMessage(
            'Inconsistência:' + LineEnding +
            'LID:' + LID + ' <> ' + LParticipantID[0]);

        end;
      end else begin
        LParticipantID.Clear;
        LParticipantID.Append(LID);
        try
          Result := True;
          LParticipantID.SaveToFile(LIDFile);
        except
          on EFilerError do begin
            Result := False;
          end;
        end;

      end;
    finally
      LParticipantID.Free;
    end;
  end;

begin
  Result := False;

  with FormMisc.ComboBoxMonitor do begin
    if ItemIndex = -1 then begin
      if Items.Count > 0 then begin
        ItemIndex := Items.Count-1;
        ShowMessage('O último monitor foi selecionado: ' + Items[ItemIndex]);
      end else begin
        ShowMessage('Nenhum monitor foi reconhecido.');
        Exit;
      end;
    end;
  end;

  if Pool.ConfigurationFilename.IsEmpty then begin
    with ListBoxCondition do begin
      if ItemIndex > -1 then begin
        ShowMessage(
          'Uma nova sessão será criada:' + LineEnding + Items[ItemIndex]);
        ButtonNewConfigurationFileClick(Self);
      end else begin
        ShowMessage(
          'Crie uma nova sessão ou carregue uma sessão interrompida.');
        Exit;
      end;
    end;
  end;

  if ComboBoxParticipant.Items.Count = 0 then begin
    ShowMessage('Crie um novo participante.');
    Exit;
  end;

  if ComboBoxParticipant.ItemIndex < 0 then begin
    ShowMessage('Escolha um participante.');
    Exit;
  end;

  if not SetupFolders then begin
    ShowMessage('Não foi possível criar a estrutura de diretórios.');
    Exit;
  end;

  if not SetupParticipantID then begin
    ShowMessage('Não foi possível criar o arquivo ID do participante.');
    Exit;
  end;

  Result := True;
end;

end.

