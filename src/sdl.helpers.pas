unit sdl.helpers;

{$mode ObjFPC}{$H+}

{$ModeSwitch AdvancedRecords}

interface

uses
  Classes, SysUtils, SDL2;


type

  { TSDL_RectHelper }

  TSDL_RectHelper = record helper for TSDL_Rect
    function ToJSON : string;
  end;

implementation

{ TSDL_RectHelper }

function TSDL_RectHelper.ToJSON: string;
begin
  Result := '{'+String.Join(',', [
    'x:' + x.ToString,
    'y:' + y.ToString,
    'w:' + w.ToString,
    'h:' + h.ToString])+'}';
end;

end.

