unit forms.question;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TFormQuestion }

  TFormQuestion = class(TForm)
    ButtonCancel: TButton;
    ButtonInterrupt: TButton;
    procedure ButtonCancelKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ButtonInterruptKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private

  public

  end;

var
  FormQuestion: TFormQuestion;

implementation

{$R *.lfm}

uses LCLType;

{ TFormQuestion }

procedure TFormQuestion.ButtonCancelKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RIGHT, VK_RETURN : { do nothing };
    otherwise Key := 0;
  end;
end;

procedure TFormQuestion.ButtonInterruptKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_LEFT, VK_RETURN : { do nothing };
    otherwise Key := 0;
  end;
end;

end.

