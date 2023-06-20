{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit experiments;

{$mode objfpc}{$H+}

interface

function MakeConfigurationFile(
  ATrials : integer;
  AITI    : integer;
  ALimitedHold : integer;
  ADesign : string;
  ASamples: integer;
  AComparisons: integer;
  AHasLimitedHold: Boolean;
  AShowMouse: Boolean) : string;

implementation

uses
  session.fileutils
   , experiments.trials
   , session.configurationfile
   ;

function MakeConfigurationFile(ATrials: integer; AITI: integer;
  ALimitedHold: integer; ADesign: string; ASamples: integer;
  AComparisons: integer; AHasLimitedHold: Boolean; AShowMouse: Boolean): string;
begin
  Result := NewConfigurationFile;
  Experiments.Trials.Parameters.InterTrialInterval := AITI;
  Experiments.Trials.Parameters.InterTrialInterval:=ALimitedHold;
  Experiments.Trials.WriteToConfigurationFile(ATrials,
    ADesign, ASamples, AComparisons, AHasLimitedHold, AShowMouse);
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;





end.
