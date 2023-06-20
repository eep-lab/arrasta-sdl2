{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.trials.mts;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , session.configuration
  , sdl.app.trials
  , sdl.app.stimuli.mts
  ;

type

  { TMTS }

  TMTS = class sealed (TTrial)
    private
      FStimuli : TMTSStimuli;
    protected
      procedure SetTrialData(ATrialData: TTrialData); override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure EndTrial; override;
      procedure Show; override;
      procedure Hide; override;
  end;


implementation

{ TMTS }

procedure TMTS.SetTrialData(ATrialData: TTrialData);
begin
  inherited SetTrialData(ATrialData);
  inherited Show;
end;

constructor TMTS.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStimuli := TMTSStimuli.Create(Self);
  FIStimuli := FStimuli.AsInterface;
end;

destructor TMTS.Destroy;
begin
  { free stuff }
  inherited Destroy;
end;

procedure TMTS.EndTrial;
begin
  inherited EndTrial;
end;

procedure TMTS.Show;
begin
  inherited Show;
end;

procedure TMTS.Hide;
begin
  inherited Hide;
end;

end.

