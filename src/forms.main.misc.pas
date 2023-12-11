unit forms.main.misc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  IniPropStorage, Spin;

type

  { TFormMisc }

  TFormMisc = class(TForm)
    CheckBoxShowModalFormForSpeechResponses: TCheckBox;
    ComboBoxAudioPromptForText: TComboBox;
    ComboBoxFixedSamplePosition: TComboBox;
    ComboBoxAudioFolder: TComboBox;
    ComboBoxFontName: TComboBox;
    IniPropStorage1: TIniPropStorage;
    LabelRecordingSeconds: TLabel;
    LabelAudioPromptForText: TLabel;
    LabelShowModal: TLabel;
    LabelFontsize: TLabel;
    LabelFixedSamplePosition: TLabel;
    LabelAudioFolder: TLabel;
    LabelTimeOut: TLabel;
    LabelLimitedHold: TLabel;
    LabelInterTrialInterval: TLabel;
    LabelFont: TLabel;
    SpinEditRecordingSeconds: TSpinEdit;
    SpinEditFontSize: TSpinEdit;
    SpinEditTimeOut: TSpinEdit;
    SpinEditLimitedHold: TSpinEdit;
    SpinEditInterTrialInterval: TSpinEdit;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  FormMisc: TFormMisc;

implementation

uses session.fileutils;

{$R *.lfm}

{ TFormMisc }

procedure TFormMisc.FormCreate(Sender: TObject);
begin
  GetAudioFilesFor(ComboBoxAudioPromptForText.Items);
  GetAudioFoldersFor(ComboBoxAudioFolder.Items);
  GetFontFilesFor(ComboBoxFontName.Items);
end;

{ TFormMisc }

end.

