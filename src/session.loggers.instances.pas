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

uses session.loggers;

type

  { TDataProcedure }
  TDataProcedure = procedure (S : string) of object;

  // LGData have blc, trial data.
  // LGTimestamps for stm and response data.
  TLoggers = (LGData, LGTimestamps);

  function GetSaveDataProc(ALogger: TLoggers): TDataProcedure;
  function GetLogger(ALogger: TLoggers) : TLogger;
  function CreateLogger(ALogger: TLoggers;
    AFilename, AHeader : string) : string;

  procedure FreeLogger(ALogger: TLoggers; AFooter : string);
  function LoggerAlive(Logger : TLoggers) : Boolean;
  function MockHeader : string;

var
  DataFilename : string = '';
  TimestampsFilename : string = '';

resourcestring
  HSUBJECT_NAME      = 'Nome_Sujeito:';
  HSESSION_NAME      = 'Nome_Sessão:';
  HFIRST_TIMESTAMP   = 'Primeira_Timestamp:';
  HBEGIN_TIME        = 'Início:';
  HEND_TIME          = 'Término:';
  HSESSION_CANCELED  = '----------Sessão Cancelada----------';
  HTEST_MODE         = '(Modo de Teste)';

implementation

uses SysUtils
   , session.pool
   ;

var
  TimestampsLog : TLogger = nil;
  DataLog : TLogger = nil;

function GetSaveDataProc(ALogger: TLoggers): TDataProcedure;
var LRegdata : TLogger;
begin
  LRegdata := GetLogger(ALogger);
  Result := @LRegdata.SaveData;
end;

function GetLogger(ALogger: TLoggers): TLogger;
var
  FileName : string;
begin
  if not LoggerAlive(ALogger) then begin
    FileName := Pool.RootData + '001';
    case ALogger of
      LGTimestamps:
        TimestampsFilename := CreateLogger(ALogger, FileName, MockHeader);
      LGData:
        DataFilename := CreateLogger(ALogger, FileName, MockHeader);
    end;
  end;
  case ALogger of
    LGTimestamps: Result := TimestampsLog;
    LGData: Result := DataLog;
  end;
end;

procedure FreeLogger(ALogger: TLoggers; AFooter: string);
var LRegdata : TLogger;
begin
  LRegdata := GetLogger(ALogger);
  with LRegdata do
    begin
      SaveData(AFooter);
      Free;
    end;
end;

function LoggerAlive(Logger: TLoggers): Boolean;
begin
  case Logger of
    LGTimestamps: Result := Assigned(TimestampsLog);
    LGData: Result := Assigned(DataLog);
  end;
end;

function MockHeader: string;
begin
  Result := HSUBJECT_NAME + #9 + 'Sujeito X' + LineEnding +
            HSESSION_NAME + #9 + 'Sessão X' + LineEnding +
            HBEGIN_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time) + LineEnding;
end;

function CreateLogger(ALogger: TLoggers; AFilename, AHeader: string) : string;
var
  LLogger : TLogger;

  function Ext(LG : TLoggers): string;
  begin
    case LG of
      LGTimestamps: Result := '.timestamps';
      LGData: Result := '.data';
    end;
  end;

  function LoggerFunc:TLogger;
  begin

    Result := TLogger.Create(AFilename + Ext(ALogger));
    Result.SaveData(AHeader);
  end;
begin
  LLogger := LoggerFunc;
  case ALogger of
    LGTimestamps: TimestampsLog := LLogger;
    LGData: DataLog := LLogger;
  end;
  Result := LLogger.FileName;
end;


end.

