unit session.loggers.writerow.information;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, session.loggers.types;

procedure InitializeBaseHeader;
procedure Finalize;

var
  SaveData : TDataProcedure;

resourcestring
  HSUBJECT_NAME      = 'Nome_do_sujeito:';
  HSESSION_NAME      = 'Nome_da_sessao:';
  HFIRST_TIMESTAMP   = 'Primeira_timestamp:';
  HBEGIN_TIME        = 'Inicio:';
  HEND_TIME          = 'Termino:';
  HGRID              = 'Grade_de_estimulos:';
  HMONITOR           = 'Monitor:';
  HSESSION_CANCELED  = '----------Sessao Cancelada----------';
  HTEST_MODE         = '(Modo de Teste)';
  HDURATION          = 'Duration:';

implementation

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
    Line([HSUBJECT_NAME, AParticipantName]) +
    Line([HSESSION_NAME, ASessionName]) +
    Line([HGRID, Grid.ToJSON]) +
    Line([HMONITOR, WindowSize.ToJSON]) +
    Line([HBEGIN_TIME, DateTimeToStr(Date), TimeToStr(StartTime)])
  );
end;

procedure Finalize;
var
  LStopTime : TDateTime;
begin
  LStopTime := Time;
  SaveData(
    Line([HEND_TIME, DateTimeToStr(Date), TimeToStr(LStopTime)]) +
    Line([HDURATION, TimeToStr(Time - StartTime)])
  );
end;

function MockHeader: string;
begin
  Result := HSUBJECT_NAME + #9 + 'Sujeito X' + LineEnding +
            HSESSION_NAME + #9 + 'Sessão X' + LineEnding +
            HBEGIN_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time) + LineEnding;
end;

end.

