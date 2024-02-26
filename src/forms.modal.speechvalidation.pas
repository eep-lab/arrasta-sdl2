unit forms.modal.speechvalidation;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, Dialogs, StdCtrls;

type

  { TFormManualSpeechValidation }

  TFormManualSpeechValidation = class(TForm)
    EditSpeech: TEdit;
    LabelQuestion1: TLabel;
    LabelQuestion2: TLabel;
    procedure EditSpeechEditingDone(Sender: TObject);
  private
    FExpectedText: string;
    FSpeechToText: string;
    procedure SetExpectedText(AValue: string);
  public
    property ExpectedText : string read FExpectedText write SetExpectedText;
    property SpeechToText : string read FSpeechToText;
  end;

var
  FormManualSpeechValidation: TFormManualSpeechValidation;

implementation

{$R *.lfm}

{ TFormManualSpeechValidation }

procedure TFormManualSpeechValidation.EditSpeechEditingDone(Sender: TObject);
begin
  Close;
end;

procedure TFormManualSpeechValidation.SetExpectedText(AValue: string);
begin
  FExpectedText := AValue;
  EditSpeech.Text := AValue;
  LabelQuestion1.Caption :=
    'O participante falou' + LineEnding + AValue + LineEnding + ' ?';
end;


end.

