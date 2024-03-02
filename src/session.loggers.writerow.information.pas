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
  HBEGIN_DATE        = 'Data_Inicio';
  HBEGIN_TIME        = 'Hora_Inicio';
  HEND_DATE          = 'Data_Termino';
  HEND_TIME          = 'Hora_Termino';
  HSESSION_RESULT    = 'Resultado';
  HSESSION_ID        = 'ID';
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

function AsNameValue(AName, AValue: string):string;
begin
  Result := String.Join(GSeparator, [AName, AValue]);
end;

function Line(ALine : array of string) : string;
begin
  Result := String.Join(#9, ALine) + LineEnding;
end;

procedure InitializeBaseHeader;
begin
  StartTime := Time;

  SaveData(
    Line([AsNameValue(HSUBJECT_NAME, Pool.ParticipantName)]) +
    Line([AsNameValue(HSESSION_NAME, Pool.SessionName)]) +
    Line([AsNameValue(HGRID, Grid.ToJSON)]) +
    Line([AsNameValue(HMONITOR, WindowSize.ToJSON)]) +
    Line([AsNameValue(HBEGIN_DATE, DateTimeToStr(Date))]) +
    Line([AsNameValue(HBEGIN_TIME, TimeToStr(StartTime))])
  );
end;

procedure Finalize;
var
  LStopTime : TDateTime;
begin
  LStopTime := Time;
  SaveData(
    Line([AsNameValue(HEND_DATE, DateTimeToStr(Date))]) +
    Line([AsNameValue(HEND_TIME, TimeToStr(LStopTime))]) +
    Line([AsNameValue(HDURATION, TimeToStr(LStopTime - StartTime))]) +
    Line([AsNameValue(HSESSION_RESULT, SessionResult)]));
end;

function MockHeader: string;
begin
  Result :=
    Line([AsNameValue(HSUBJECT_NAME, 'Sujeito X')]) +
    Line([AsNameValue(HSESSION_NAME, 'Sessão X')]) +
    Line([AsNameValue(HBEGIN_DATE, DateTimeToStr(Date))]) +
    Line([AsNameValue(HBEGIN_TIME, TimeToStr(Time))]);
end;

end.

