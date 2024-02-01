{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit timestamps.methods;

{$mode objfpc}{$H+}
{$MODESWITCH TYPEHELPERS}

interface

uses  SysUtils, timestamps.types;


function GetLatency(AStart, ALatency : TLargerFloat) : string;
function Elapsed : TLargerFloat;

type

  TLargerFloatHelper = type helper for TLargerFloat
    function Elapsed: TLargerFloat;
    function ToString: string;
  end;

var
  TimeStart : TLargerFloat;

implementation

uses timestamps;

function GetLatency(AStart, ALatency: TLargerFloat): string;
begin
  if ALatency > 0 then begin
      Result := (ALatency - AStart).ToString;
    end else begin
      Result := 'NA';
    end;
end;

function Elapsed: TLargerFloat;
begin
  Result := ClockMonotonic - TimeStart;
end;

{ TLargerFloatHelper }

function TLargerFloatHelper.Elapsed: TLargerFloat;
begin
  Result := Self - TimeStart;
end;

function TLargerFloatHelper.ToString: string;
begin
  Result := FloatToStrF(Self, ffFixed, 0, 9);
end;


end.

