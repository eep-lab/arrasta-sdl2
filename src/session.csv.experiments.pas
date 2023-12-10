{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.csv.experiments;

{$mode objfpc}{$H+}

interface

function MakeConfigurationFile(AFilename : string) : string;

implementation

uses
  session.fileutils
  , session.csv.experiments.base
  , session.constants.trials
  , session.parameters.global
  , session.configurationfile
  ;

function MakeConfigurationFile(AFilename : string): string;
var
  LExperimentWriter : TBaseExperimentWriter;
begin
  Result := NewConfigurationFile;

  LExperimentWriter :=
    TBaseExperimentWriter.Create(ConfigurationFile, AFilename);
  try
    LExperimentWriter.Write;
  finally
    LExperimentWriter.Free;
  end;
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;

end.
