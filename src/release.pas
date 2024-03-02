unit release;

{$mode ObjFPC}{$H+}

interface

procedure CreateReleaseFolder;

implementation

uses
  Classes, SysUtils, SDL2, FileUtil;

var
  Root : string;
  ReleaseFolder : string = 'release';
  ReleaseFiles : array [0..10] of string = (
    'SDL2.dll',
    'SDL2_gfx.dll',
    'SDL2_image.dll',
    'SDL2_mixer.dll',
    'SDL2_ttf.dll',
    'configurations.ini',
    'configurations_global.ini',
    'experiment.exe',
    'eyelink_core_graphics_sdl2x64.dll',
    'eyelink_core64.dll',
    'libzmq.dll');

procedure CreateReleaseFolder;
var
  LFile : string;
  LSource : string;
  LDestin : string;
begin
  ForceDirectories(ReleaseFolder);
  for LFile in Releasefiles do begin
    LSource := ConcatPaths([Root, LFile]);
    LDestin := ConcatPaths([ReleaseFolder, LFile]);
    CopyFile(LSource, LDestin);
  end;
end;


initialization
  Root := SDL_GetBasePath();
  ReleaseFolder := ConcatPaths([Root, ReleaseFolder]);

end.
