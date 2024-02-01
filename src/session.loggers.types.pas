{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.loggers.types;

{$mode ObjFPC}{$H+}

interface

type
  { TDataProcedure }
  TDataProcedure = procedure (S : string) of object;

  // LGData have blc, trial data.
  // LGTimestamps for stm and response data.
  TLoggers = (LGData, LGTimestamps, LGInfo);


implementation

end.

