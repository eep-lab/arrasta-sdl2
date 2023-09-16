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
  session.endcriteria, session.counters;

type

  { TPool }

  TPool = class
    RootData : string;
    RootMedia : string;
    BaseFileName : string;
    BaseFilePath : string;
    TimeStart : Extended;
    TestMode : Boolean;
    MonitorToShow : Byte;
    Counters : TCounterManager;
    EndCriteria : TEndCriteria;
  end;
var
  Pool : TPool;

implementation

uses SysUtils, SDL2, FileUtil;

initialization
  Pool := TPool.Create;
  with Pool do
  begin
    BaseFileName := '';
    BaseFilePath := SDL_GetBasePath();
    RootData := BaseFilePath + 'data' + DirectorySeparator;
    RootMedia := BaseFilePath +  'media' + DirectorySeparator;
    ForceDirectories(RootData);
    ForceDirectories(RootMedia);
    MonitorToShow := 0;
    TestMode := False;
    Counters := nil;
    EndCriteria := nil;
  end

finalization
  Pool.Free;

end.

