program standalone_experiment;
uses
  Interfaces,
  Classes, SysUtils, sdl.app
  , timestamps.methods
  //, eyelink.classes
  {$IFDEF NO_LCL}
  , sdl.app.renderer.nolcl
  , sdl.app.video.writer.windows
  {$ENDIF}
  ;
var
  SDLApp : TSDLApplication;
  //EyeLink : TEyeLink;
begin
  WriteLn('Renderer fps reference: ', 1000 div 50);
  //EyeLink := TEyeLink.Create(nil);
  //EyeLink.InitializeLibraryAndConnectToDevice;
  //EyeLink.DataReceiveFile;
  StartEpikTimer;
  StartTimestamp := ET.Elapsed;

  //EyeLink := TEyeLink.Create(nil);
  //EyeLink.InitializeLibraryAndConnectToDevice;
  //EyeLink.HostApp := SDLApp;
  //EyeLink.DoTrackerSetup;
  //EyeLink.OpenDataFile;

  SDLApp := TSDLApplication.Create;
  try
    SDLApp.SetupVideo(0);
    SDLApp.SetupEvents;
    Sleep(50);
    VideoWriter := TVideoWriter.Create(SDLApp.Monitor);
    VideoWriter.StartRecording;
    SDLApp.Run;
  finally
    //EyeLink.ReceiveDataFile;
    //EyeLink.Free;
    VideoWriter.MainThreadSynchronize;
    VideoWriter.Stop;
    //CheckSynchronize;
    SDLApp.Free;
  end;
  ReadLn;
end.

