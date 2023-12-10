unit session.parameters.global;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type

  TGlobalTrialParameters = record
    Cursor : integer;
    LimitedHold : integer;
    InterTrialInterval : integer;
    TimeOutInterval : integer;
    HasConsequence : Boolean;
    FontName : string;
  end;

var
  GlobalTrialParameters : TGlobalTrialParameters;

implementation


end.

