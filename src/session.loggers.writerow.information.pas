unit session.loggers.writerow.information;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, session.loggers.types;

procedure InitializeBaseHeader;
procedure Finalize;

const
  GExtention = '.info';
  GSeparator = ':';

var
  SaveData : TDataProcedure;
  SessionResult : string;

resourcestring
  HSUBJECT_NAME      = 'Nome_do_sujeito';
  HSESSION_NAME      = 'Nome_da_sessao';
  HFIRST_TIMESTAMP   = 'Primeira_timestamp';
  HBEGIN_TIME        = 'Inicio';
  HEND_TIME          = 'Termino';
  HSESSION_RESULT    = 'Resultado';
  HGRID              = 'Grade_de_estimulos';
  HMONITOR           = 'Monitor';
  HSESSION_CANCELED  = '----------Sessao Cancelada----------';
  HTEST_MODE         = '(Modo de Teste)';
  HDURATION          = 'Duration';

implementation

uses
  session.pool,
  sdl.helpers,
  sdl.app.video.methods,
  sdl.app.grids;

var
  StartTime : TDateTime;

function Line(ALine : array of string) : string;
begin
  Result := String.Join(#9, ALine) + LineEnding;
end;

procedure InitializeBaseHeader;
begin
  StartTime := Time;

  SaveData(
    Line([HSUBJECT_NAME, GSeparator, Pool.ParticipantName]) +
    Line([HSESSION_NAME, GSeparator, Pool.SessionName]) +
    Line([HGRID, GSeparator, Grid.ToJSON]) +
    Line([HMONITOR, GSeparator, WindowSize.ToJSON]) +
    Line([HBEGIN_TIME, GSeparator, DateTimeToStr(Date), TimeToStr(StartTime)])
  );
end;

procedure Finalize;
var
  LStopTime : TDateTime;
begin
  LStopTime := Time;
  SaveData(
    Line([HEND_TIME, GSeparator, DateTimeToStr(Date), TimeToStr(LStopTime)]) +
    Line([HDURATION, GSeparator, TimeToStr(Time - StartTime)]) +
    Line([HSESSION_RESULT, GSeparator, SessionResult]));
end;

function MockHeader: string;
begin
  Result :=
    Line([HSUBJECT_NAME, GSeparator, 'Sujeito X']) +
    Line([HSESSION_NAME, GSeparator, 'Sessão X']) +
    Line([HBEGIN_TIME, GSeparator, DateTimeToStr(Date), TimeToStr(Time)]);
end;

end.

