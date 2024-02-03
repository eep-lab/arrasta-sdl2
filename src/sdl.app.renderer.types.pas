unit sdl.app.renderer.types;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

var
  DELTA_TIME : ShortInt;

implementation

const
  MONITOR_HZ = 40;

initialization
  DELTA_TIME := 1000 div MONITOR_HZ;

end.

