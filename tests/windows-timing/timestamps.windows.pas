{
  Tested on Windows 11, Samsung NP350XAA,
  Intel(R) Core(TM) i5-8250U CPU @ 1.60GHz   1.80 GHz.

  Scale:
    Seconds.

  Resolution:
    10000000
    1e−7

  Precision:
    < 1 us
    1e-6

    Safe calling from a single thread.
    Ambiguous around 1 us calling from multi-thread.

  Documentation:
    https://learn.microsoft.com/en-us/windows/win32/sysinfo/acquiring-high-resolution-time-stamps

  Credits:
    2018-08-22: jws, https://stackoverflow.com/questions/5404277/porting-clock-gettime-to-windows
    2023-11-21: cpicanco
}
unit timestamps.windows;

interface

uses Windows;

type

  timespec = record
    tv_sec: TLargeInteger;
    tv_nsec: TLargeInteger;
  end;

function clock_monotonic: Double;
function clock_gettime_monotonic: timespec;

implementation

uses SysUtils;

var
  TicksPerSecond: TLargeInteger = 0;

function clock_monotonic: Double;
var
  Ticks: TLargeInteger;
begin
  QueryPerformanceCounter(@Ticks);
  Result := Ticks / TicksPerSecond;
end;

function clock_gettime_monotonic: timespec;
var
  Ticks: TLargeInteger;
begin
  QueryPerformanceCounter(@Ticks);
  result.tv_sec := Ticks div TicksPerSecond;
  result.tv_nsec := (Ticks mod TicksPerSecond * 1000) div TicksPerSecond;
end;

initialization
  if QueryPerformanceFrequency(@TicksPerSecond) then begin
    WriteLn('Frequency:', TicksPerSecond);
  end else begin
    raise Exception.Create('QueryPerformanceFrequency failed');
  end;

end.
