program standalone_experiment;
uses
  {$IFNDEF NO_LCL}Interfaces,{$ENDIF}
  sdl.app
  {$IFDEF NO_LCL}, sdl.app.renderer.nolcl{$ENDIF}
  ;
var
  SDLApp : TSDLApplication;

begin
  SDLApp := TSDLApplication.Create;
{$IFDEF NO_LCL}
  Monitor := SDLApp.Monitor;
{$ENDIF}
  SDLApp.SetupEvents;
  SDLApp.Run;
end.

