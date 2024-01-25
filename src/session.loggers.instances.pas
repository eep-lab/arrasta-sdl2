{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.loggers.instances;

{$mode objfpc}{$H+}

interface

uses session.loggers, session.loggers.types;

function GetSaveDataProc(ALogger: TLoggers): TDataProcedure;
function GetLogger(var ALogger: TLoggers) : TLogger;
function CreateLogger(ALogger: TLoggers;
  AFilename, AHeader : string) : string;

procedure FreeLogger(ALogger: TLoggers; AFooter : string);
function MockHeader : string;

var
  DataFilename : string = '';
  TimestampsFilename : string = '';

resourcestring
  HSUBJECT_NAME      = 'Nome_do_sujeito:';
  HSESSION_NAME      = 'Nome_da_sessao:';
  HFIRST_TIMESTAMP   = 'Primeira_timestamp:';
  HBEGIN_TIME        = 'Inicio:';
  HEND_TIME          = 'Termino:';
  HGRID              = 'Grade_de_estimulos:';
  HSESSION_CANCELED  = '----------Sessao Cancelada----------';
  HTEST_MODE         = '(Modo de Teste)';

implementation

uses SysUtils
   , session.pool
   , session.loggers.writerow
   , session.loggers.writerow.timestamp
   ;

var
  TimestampsLog : TLogger;
  DataLog : TLogger;

function GetSaveDataProc(ALogger: TLoggers): TDataProcedure;
var LRegdata : TLogger;
begin
  LRegdata := GetLogger(ALogger);
  Result := @LRegdata.SaveData;
end;

function GetLogger(var ALogger: TLoggers): TLogger;
begin
  case ALogger of
    LGTimestamps: Result := TimestampsLog;
    LGData: Result := DataLog;
  end;
end;

procedure FreeLogger(ALogger: TLoggers; AFooter: string);
var LRegdata : TLogger;
begin
  LRegdata := GetLogger(ALogger);
  LRegdata.SaveData(AFooter);
  LRegdata.Free;
  LRegdata := nil;
end;

function MockHeader: string;
begin
  Result := HSUBJECT_NAME + #9 + 'Sujeito X' + LineEnding +
            HSESSION_NAME + #9 + 'Sessão X' + LineEnding +
            HBEGIN_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time) + LineEnding;
end;

function CreateLogger(ALogger: TLoggers; AFilename, AHeader: string) : string;
var
  LLogger: TLogger;
begin
  case ALogger of
    LGTimestamps: begin
      TimestampsLog := TLogger.Create(AFilename + '.timestamps');
      TimestampsLog.SaveData(AHeader);
      Session.Loggers.WriteRow.Timestamp.SaveData := @TimestampsLog.SaveData;
      Session.Loggers.WriteRow.Timestamp.InitializeBaseHeader;
      LLogger := TimestampsLog;
    end;
    LGData: begin
      DataLog := TLogger.Create(AFilename + '.data');
      DataLog.SaveData(AHeader);
      Session.Loggers.Writerow.SaveData := @DataLog.SaveData;
      Session.Loggers.Writerow.InitializeBaseHeader;
      LLogger := DataLog;
    end;
  end;
  Result := LLogger.FileName;
end;


end.

