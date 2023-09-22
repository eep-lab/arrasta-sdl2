{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.counters;

{$mode objfpc}{$H+}

{$ModeSwitch advancedrecords}

interface

uses session.counters.consecutive
  , session.counters.all;

type


  { TCounters }

  TCounters = record
  public
    Subject : Word;
    Session : TSessionCounters;
    Block : TBlockCounters;
    Trial : TTrialCounters;
    function EndTrial(ARepeatValue : Word; AGoToValue : SmallInt) : Boolean;
    procedure EndGoToTrial(ATrialID : TTrialID);
    procedure Hit;
    procedure Miss;
    procedure None;
    procedure BeforeBeginSession;
    function EndBlock(ARepeatValue : Word; AGoToValue : SmallInt) : Boolean;
    procedure EndSession;
  end;

var
  Counters : TCounters;

implementation

uses Classes, SysUtils, session.pool, sdl.app.output;

{ TCounterManager }

function GetSubjectIDFromFile : Word;
var
  LStringList : TStringList;
begin
  LStringList := TStringList.Create;
  try
    try
      LStringList.LoadFromFile(Pool.BaseFileName+'ID');
    except
      on EFileNotFoundException do

    end;
    Result := LStringList[0].ToInteger;
  finally
    LStringList.Free;
  end;
end;

procedure TCounters.BeforeBeginSession;
begin
  Subject := GetSubjectIDFromFile;
  Session := TSessionCounters.Create;
  Session.Reset;
  Block := Session.Block;
  Trial := Block.Trial;

  Pool.Session := Session;
  Pool.Block   := Block;
  Pool.Trial   := Trial;
end;

function TCounters.EndBlock(ARepeatValue: Word; AGoToValue: SmallInt): Boolean;
begin
  Result := True;
  if ARepeatValue > 0 then begin
    if Session.Block.Consecutives < ARepeatValue then begin
      Result := False;
      Session.NextBlockConsecutive;
      Session.ResetBlockConsecutive;
    end;
  end else begin
    // TODO : AGoToValue
    Session.NextBlockID(Trial.ID+1);
  end;
end;

procedure TCounters.EndSession;
begin
  Session.Free;
end;

function TCounters.EndTrial(ARepeatValue: Word; AGoToValue: SmallInt): Boolean;
begin
  Result := True;
  if ARepeatValue > 0 then begin
    if Session.Block.Trial.Consecutives < ARepeatValue then begin
      Result := False;
      Session.NextTrialConsecutive;
      Session.ResetTrialConsecutive;
    end;
  end else begin
    // TODO : AGoToValue
    Session.NextTrialID(Trial.ID+1);
  end;
end;

procedure TCounters.EndGoToTrial(ATrialID: TTrialID);
begin
  if ATrialID = Trial.ID then begin
    Trial.NextConsecutive;
  end else begin
    Trial.NextID(ATrialID);
  end;
  //Block.Trial.Count;
  //Session.Trial.Count;
end;

procedure TCounters.Hit;
begin
  Session.Events.Hit;
  Block.Events.Hit;
  Trial.Events.Hit;
end;

procedure TCounters.Miss;
begin
  Session.Events.Miss;
  Block.Events.Miss;
  Trial.Events.Miss;
end;

procedure TCounters.None;
begin
  Session.Events.None;
  Block.Events.None;
  Trial.Events.None;
end;

end.
