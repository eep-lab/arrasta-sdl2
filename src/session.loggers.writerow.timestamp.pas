unit session.loggers.writerow.timestamp;

{$mode ObjFPC}{$H+}

interface

uses
  session.loggers.types;

procedure Timestamp(AEvent : string);
procedure InitializeBaseHeader;

var
  SaveData : TDataProcedure;

implementation

uses SysUtils, session.pool, timestamps, timestamps.methods, session.loggers;

procedure InitializeBaseHeader;
begin
 SaveData(TLogger.Row([
    'Timestamp',
    'Session.Trial.UID',
    'Session.Block.UID',
    'Event',
    'Event.Annotation']));
end;

procedure Timestamp(AEvent : string);
var
  LTimestamp : string;
begin
  LTimestamp := (ClockMonotonic - Pool.TimeStart).ToString;
  SaveData(TLogger.Row([
    LTimestamp,
    (Pool.Session.Trial.UID + 1).ToString,
    (Pool.Session.Block.UID + 1).ToString,
    AEvent]));
end;

end.

