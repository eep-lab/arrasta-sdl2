{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.csv;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, csvdocument, session.csv.enumerator;

type

  { TCSVRows }

  TCSVRows = class(IEnumerable)
  private
    FCSVDocument: TCSVDocument;
  public
    constructor Create(AFilename : string);
    destructor Destroy; override;
    function GetEnumerator: IEnumerator;
  end;

implementation

constructor TCSVRows.Create(AFilename: string);
begin
  inherited Create;
  FCSVDocument := TCSVDocument.Create;
  FCSVDocument.LoadFromFile(AFilename);
end;

destructor TCSVRows.Destroy;
begin
  FCSVDocument.Free;
end;

function TCSVRows.GetEnumerator: IEnumerator;
begin
  // TCSVRowParser is reference counted
  Result := TCSVRowParser.Create(FCSVDocument) as IEnumerator;
end;

end.
