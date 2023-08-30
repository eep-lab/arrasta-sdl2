{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.audio.contract;

{$mode ObjFPC}{$H+}

{$INTERFACES CORBA}

interface

uses ctypes;

type

  { ISound }

  ISound = interface
    ['{362597B2-A0D1-4F6C-8536-DE41AA52A7A4}']
    function Duration : cuint32;
    function Playing : Boolean;
    function ShortName : string;
    function ShortPath : string;
    procedure DoOnStop;
    procedure LoadFromFile(AFilename : string);
    procedure Play;
    procedure Stop;
    procedure Free;
  end;

implementation

end.

