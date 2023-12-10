unit sdl.app.audio.loops;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  sdl.app.audio.contract,
  sdl.timer,
  ctypes;

type

  { TSoundLoop }

  TSoundLoop = class
    private
      FTotalLoops : SmallInt;
      FCurrentLoopCount : SmallInt;
      FSound : ISound;
      FTimer : TSDLTimer;
      function GetInterval: cuint32;
      procedure SetInterval(AValue: cuint32);
      procedure PlaySound(Sender: TObject);
    public
      constructor Create;
      destructor Destroy; override;
      property Sound : ISound read FSound write FSound;
      property Interval : cuint32 read GetInterval write SetInterval;
      property TotalLoops : SmallInt read FTotalLoops write FTotalLoops;
      procedure Start;
      procedure Stop;
  end;

implementation

{ TSoundLoop }

function TSoundLoop.GetInterval: cuint32;
begin
  Result := FTimer.Interval;
end;

procedure TSoundLoop.SetInterval(AValue: cuint32);
begin
  FTimer.Interval := AValue;
end;

procedure TSoundLoop.PlaySound(Sender: TObject);
begin
  if FTotalLoops > 0 then begin
    FSound.Play;
    Inc(FCurrentLoopCount);
    if FCurrentLoopCount >= FTotalLoops then begin
      FTimer.Stop;
      FCurrentLoopCount := 0;
    end;
  end else begin
    FTimer.Stop;
  end;
end;

constructor TSoundLoop.Create;
begin
  FTimer := TSDLTimer.Create;
  FTimer.OnTimer := @PlaySound;
  FTimer.Interval := 2000;
  FCurrentLoopCount := 0;
  FTotalLoops := -1;
end;

destructor TSoundLoop.Destroy;
begin
  FTimer.OnTimer := nil;
  FTimer.Free;
  inherited Destroy;
end;

procedure TSoundLoop.Start;
begin
  FSound.Play;
  FTimer.Start;
end;

procedure TSoundLoop.Stop;
begin
  FTimer.Stop;
end;

end.

