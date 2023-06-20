{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit sdl.app.trials.last;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils
  , session.configuration
  , sdl.app.trials
  , sdl.app.graphics.text
  ;

type

  { TLastTrial }

  TLastTrial = class sealed (TTrial)
    private
      FText : TText;
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

{ TLastTrial }

procedure TLastTrial.SetTrialData(ATrialData: TTrialData);
begin
  inherited SetTrialData(ATrialData);
  inherited Show;
end;

constructor TLastTrial.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FText := TText.Create(Self);
  FText.Load('Fim.', 'Raleway-Regular');
  FText.Parent := Self;
  FText.Centralize;
  FText.Show;
end;

destructor TLastTrial.Destroy;
begin
  { free stuff }
  inherited Destroy;
end;

procedure TLastTrial.EndTrial;
begin
  inherited EndTrial;
end;

procedure TLastTrial.Show;
begin
  inherited Show;
end;

procedure TLastTrial.Hide;
begin
  inherited Hide;
end;

end.
