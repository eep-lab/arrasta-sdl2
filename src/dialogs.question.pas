unit dialogs.question;

{$mode ObjFPC}{$H+}

interface

  function IsSessionCanceled : Boolean;

implementation

uses Controls, forms.question;

function IsSessionCanceled: Boolean;
begin
  FormQuestion := TFormQuestion.Create(nil);
  try
    case FormQuestion.ShowModal of
      mrCancel : begin
        Result := True;
      end;

      otherwise begin
        Result := False;
      end;
    end;
  finally
    FormQuestion.Free;
    FormQuestion := nil;
  end;
end;

end.

