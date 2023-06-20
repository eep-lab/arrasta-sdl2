{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit counters.manager;

{$mode objfpc}{$H+}

interface

uses Classes, Dialogs, SysUtils;

type

  { TCounterManager }

  TCounterManager = class
  public
    SessionTrials : integer;
    RepeatedBlocs : integer;
    RepeatedTrials : integer;
    CurrentBloc : integer;
    CurrentTrial : integer;
    BlcCscHits : integer;
    BlcTrials  : integer;
    BlcHits    : integer;
    procedure EndTrial;
    procedure Hit;
    procedure Miss;
    procedure BeginSess;
    procedure EndBlc;
  end;

implementation

{ TCounterManager }

procedure TCounterManager.BeginSess;
begin
  CurrentBloc := 0;
  CurrentTrial := 0;
end;

procedure TCounterManager.EndBlc;
begin
  RepeatedBlocs := 0;
  CurrentTrial := 0;
  //BlcTrials := 0;
  Inc(CurrentBloc);
end;

procedure TCounterManager.EndTrial;
begin
  Inc(SessionTrials);
  //Inc(BlcTrials);
  Inc(CurrentTrial);
end;

procedure TCounterManager.Hit;
begin
  //Inc(FBlcHits);           //Contador de corretas no bloco
  //Inc(FBlcCscHits);        //Contador de corretas consecutivas do bloco
  //Inc(FBlcCscHitsType1);        //Contador de corretas consecutivas t1
  //Inc(FBlcCscHitsType2);        //Contador de corretas consecutivas t2
  //Inc(FBlcCsqHits);        //Contador de corretas consecutivas para liberação de consequências
  //
  //if FBlcCscMisses > FBlcHighCscMisses then FBlcHighCscMisses := FBlcCscMisses;
  //FBlcCscMisses := 0;
  //if FBlcCscHits > FBlcHighCscHits then FBlcHighCscHits := FBlcCscHits;
end;

procedure TCounterManager.Miss;
begin
  //FBlcCsqHits := 0; //Para a liberação de consequências
  //Inc(FBlcMisses);
  //Inc(FBlcCscMisses);
  //
  //if FBlcCscHits > FBlcHighCscHits then FBlcHighCscHits := FBlcCscHits;
  //FBlcCscHits := 0;
  //FBlcCscHitsType1 := 0;
  //FBlcCscHitsType2 := 0;
  //if FBlcCscMisses > FBlcHighCscMisses then FBlcHighCscMisses := FBlcCscMisses;
end;

end.
