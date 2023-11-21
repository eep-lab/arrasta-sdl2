{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit debug.output;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

  procedure Print1(S : string);
  procedure Print2(S : string);

implementation

const
  DebugOutputFilename1 : string = 'DebugOutput1.txt';
  DebugOutputFilename2 : string = 'DebugOutput2.txt';
var
  DebugOutput1 : TextFile;
  DebugOutput2 : TextFile;

procedure Print1(S: string);
begin
  WriteLn(DebugOutput1, S);
  System.Flush(DebugOutput1);
end;

procedure Print2(S: string);
begin
  WriteLn(DebugOutput2, S);
  System.Flush(DebugOutput2);
end;

initialization
  AssignFile(DebugOutput1, DebugOutputFilename1);
  if FileExists(DebugOutputFilename1) then
    System.Erase(DebugOutput1);
  System.Rewrite(DebugOutput1);
  System.Append(DebugOutput1);

  AssignFile(DebugOutput2, DebugOutputFilename2);
  if FileExists(DebugOutputFilename2) then
    System.Erase(DebugOutput2);
  System.Rewrite(DebugOutput2);
  System.Append(DebugOutput2);

finalization
  System.Close(DebugOutput1);
  System.Close(DebugOutput2);

end.

