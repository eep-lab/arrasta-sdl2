{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.counters;

{$mode objfpc}{$H+}

interface

type

  { TCounterManager }

  TCounterManager = class
  public
    SessionTrials : integer;
    RepeatedBlocks : integer;
    RepeatedTrials : integer;
    CurrentBlock : integer;
    CurrentTrial : integer;
    BlockCscHits : integer;
    BlockTrials  : integer;
    BlockHits    : integer;
    procedure EndTrial;
    procedure Hit;
    procedure Miss;
    procedure BeginSess;
    procedure EndBlock;
  end;

var
  Counters : TCounterManager;

implementation

{ TCounterManager }

procedure TCounterManager.BeginSess;
begin
  CurrentBlock := 0;
  CurrentTrial := 0;
end;

procedure TCounterManager.EndBlock;
begin
  RepeatedBlocks := 0;
  CurrentTrial := 0;
  //BlockTrials := 0;
  Inc(CurrentBlock);
end;

procedure TCounterManager.EndTrial;
begin
  Inc(SessionTrials);
  //Inc(BlockTrials);
  Inc(CurrentTrial);
end;

procedure TCounterManager.Hit;
begin
  //Inc(FBlockHits);           //Contador de corretas no Block
  //Inc(FBlockCscHits);        //Contador de corretas consecutivas do Block
  //Inc(FBlockCscHitsType1);        //Contador de corretas consecutivas t1
  //Inc(FBlockCscHitsType2);        //Contador de corretas consecutivas t2
  //Inc(FBlockCsqHits);        //Contador de corretas consecutivas para liberação de consequências
  //
  //if FBlockCscMisses > FBlockHighCscMisses then FBlockHighCscMisses := FBlockCscMisses;
  //FBlockCscMisses := 0;
  //if FBlockCscHits > FBlockHighCscHits then FBlockHighCscHits := FBlockCscHits;
end;

procedure TCounterManager.Miss;
begin
  //FBlockCsqHits := 0; //Para a liberação de consequências
  //Inc(FBlockMisses);
  //Inc(FBlockCscMisses);
  //
  //if FBlockCscHits > FBlockHighCscHits then FBlockHighCscHits := FBlockCscHits;
  //FBlockCscHits := 0;
  //FBlockCscHitsType1 := 0;
  //FBlockCscHitsType2 := 0;
  //if FBlockCscMisses > FBlockHighCscMisses then FBlockHighCscMisses := FBlockCscMisses;
end;

end.
