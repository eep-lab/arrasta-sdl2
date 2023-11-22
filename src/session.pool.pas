{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.pool;

{$mode objfpc}{$H+}

interface

uses
  sdl.app
  , session.endcriteria
  , session.counters
  , session.counters.all
  , timestamps.types;

type

  { TPool }

  TPool = class
  public
    AppName : string;
    RootData : string;
    RootDataResponses: string;
    RootMedia : string;
    RootAudio : string;
    BaseFileName : string;
    BaseFilePath : string;
    AssetsBasePath : string;
    AudioBasePath : string;
    ResponsesBasePath : string;
    ConfigurationFilename : string;
    TimeStart : TLargerFloat;
    TestMode : Boolean;
    MonitorToShow : Byte;
    Counters : TCounters;
    Session : TSessionCounters;
    Trial : TTrialCounters;
    Block : TBlockCounters;
    EndCriteria : TEndCriteria;
    App : TSDLApplication;
  end;
var
  Pool : TPool;

implementation

uses SysUtils, SDL2
  , FileUtil
  , session.strutils
  ;

initialization
  Pool := TPool.Create;
  with Pool do begin
    AppName := 'Stimulus Control';
    BaseFileName := '';
    BaseFilePath := SDL_GetBasePath();
    RootData := AsPath(BaseFilePath, 'data');
    RootDataResponses := '';
    RootMedia := AsPath('media');
    AudioBasePath := AsPath('wav', 'rafael');
    AssetsBasePath := AsPath('assets');
    ResponsesBasePath:= AsPath('responses');
    ConfigurationFilename := '';
    ForceDirectories(RootData);
    ForceDirectories(RootMedia);
    ForceDirectories(AsPath(RootMedia, AssetsBasePath));
    MonitorToShow := 0;
    TestMode := False;
    EndCriteria := nil;
  end

finalization
  Pool.Free;

end.

