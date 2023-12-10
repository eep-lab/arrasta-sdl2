unit session.parameters.global;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, sdl.app.grids.types;

type

  TGlobalTrialParameters = record
    Cursor : integer;
    LimitedHold : integer;
    InterTrialInterval : integer;
    TimeOutInterval : integer;
    HasConsequence : Boolean;
    FontName : string;
    FixedSamplePosition : UInt8;
    ComparisonPositions : array of UInt8;
    GridOrientation : TGridOrientation;
  end;

var
  GlobalTrialParameters : TGlobalTrialParameters;

implementation


end.

