unit forms.main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  session.parameters, session.parameters.types, customparameters;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  CustomObject : TCustomObject;
  Parameter : TParameter;
  LText : string;
begin
  CustomObject := TCustomObject.Create;
  for Parameter in CustomObject.Parameters do
      LText := Parameter.ToString + LineEnding;

  try
    CustomObject['Count'] := '40';

    for Parameter in CustomObject.Parameters do
      LText := LText + Parameter.ToString + LineEnding;

  finally
    CustomObject.Free;
  end;

  ShowMessage(LText);
end;

end.

