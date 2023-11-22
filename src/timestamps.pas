{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit timestamps;

{$mode objfpc}{$H+}


interface

uses
  SysUtils, timestamps.types

{$ifdef LINUX}
  , Linux
  , UnixType
{$endif}

{$ifdef WINDOWS}
  , Windows
{$endif}

{$ifdef DARWIN}
  , ctypes
  , MachTime
{$endif}
  ;

function ClockMonotonic : TLargerFloat;

implementation

{$ifdef LINUX}
function ClockMonotonic: TLargerFloat;
var
  tp: timespec;
  a, b : TLargerFloat;
begin
  clock_gettime(CLOCK_MONOTONIC, @tp);
  a := TLargerFloat(tp.tv_sec);
  b := TLargerFloat(tp.tv_nsec) * 1e-9;
  Result := a+b;
end;
{$endif}

{$ifdef WINDOWS}
var
  PerSecond : TLargeInteger;

function ClockMonotonic: TLargerFloat;
var
  Count : TLargeInteger;
begin
  QueryPerformanceCounter(Count);
  Result := Count / PerSecond;
end;


initialization
   QueryPerformanceFrequency(PerSecond);
{$endif}

{$ifdef DARWIN}
{credits: https://github.com/pupil-labs/pyuvc/blob/master/pyuvc-source/darwin_time.pxi}

var
  timeConvert: TLargerFloat = 0.0;

//function get_sys_time_monotonic: TLargerFloat;
function ClockMonotonic : TLargerFloat;
var
  timeBase: mach_timebase_info_data_t;
begin
  if timeConvert = 0.0 then begin
    mach_timebase_info(@timeBase);
    timeConvert := (timeBase.numer / timeBase.denom) / 1000000000.0;
  end;
  Result := mach_absolute_time() * timeConvert;
end;

{$endif}

end.

