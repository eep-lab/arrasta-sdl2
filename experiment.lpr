program experiment;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  {$IFDEF UNIX}cthreads,{$ENDIF}
  {$IFDEF HASAMIGA}athreads,{$ENDIF}
  SysUtils,
  Forms, Forms.Main, Forms.Modal.SpeechValidation;

{$R *.res}

begin
  if FileExists('heaptrc.txt') then begin
    DeleteFile('heaptrc.txt');
  end;
  {$IF Declared(heaptrc)}
  SetHeapTraceOutput('heaptrc.txt');
  {$ENDIF}
  Randomize;
  RequireDerivedFormResource:=True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TFormBackground, FormBackground);
  Application.CreateForm(
    TFormManualSpeechValidation, FormManualSpeechValidation);
  Application.Run;
end.

