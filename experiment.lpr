program experiment;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Forms.Main, Forms.Modal.SpeechValidation;

{$R *.res}

begin
  Randomize;
  RequireDerivedFormResource:=True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFormBackground, FormBackground);
  Application.CreateForm(
    TFormManualSpeechValidation, FormManualSpeechValidation);
  Application.Run;
end.

