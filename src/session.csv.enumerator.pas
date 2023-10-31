{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit session.csv.enumerator;

{$mode ObjFPC}{$H+}

{$INTERFACES CORBA}

interface

uses Classes, SysUtils, csvdocument;

type

  IEnumerator = interface(IInterface)
    ['{1792C9B9-7465-42A2-8298-5E0FCD6783FC}']
    function GetCurrent: TStringList;
    function MoveNext: Boolean;
    procedure Reset;
    property Current: TStringList read GetCurrent;
  end;

  IEnumerable = interface
    ['{1377B9CC-479C-433A-BB00-9A25129F01E8}']
    function GetEnumerator: IEnumerator;
  end;

{ TCSVRowParser }

  TCSVRowParser = class(TInterfacedObject, IEnumerator)
  private
    FCSV: TCSVDocument;
    FCurrentIndex: Integer;
    FCurrent : TStringList;
    function GetCurrent: TStringList;
    function MoveNext: Boolean;
    procedure Reset;
  public
    constructor Create(CSV: TCSVDocument);
    destructor Destroy; override;
  end;


implementation

constructor TCSVRowParser.Create(CSV: TCSVDocument);
begin
  inherited Create;
  FCSV := CSV;
  FCurrent := TStringList.Create;
  FCurrent.Sorted := False;
  FCurrentIndex := 0;
end;

destructor TCSVRowParser.Destroy;
begin
  FCurrent.Clear;
  FCurrent.Free;
  inherited Destroy;
end;

function TCSVRowParser.GetCurrent: TStringList;
var
  LColIndex: Integer;
begin
  FCurrent.BeginUpdate;
  FCurrent.Clear;
  with FCSV do begin
    if (FCurrentIndex >= 0) and (FCurrentIndex < RowCount) then
      for LColIndex := 0 to MaxColCount - 1 do
        FCurrent.Values[Cells[LColIndex, 0]] := Cells[LColIndex, FCurrentIndex];
   end;
  FCurrent.EndUpdate;
  Result := FCurrent;
end;

function TCSVRowParser.MoveNext: Boolean;
begin
  Inc(FCurrentIndex);
  Result := FCurrentIndex < FCSV.RowCount;
end;

procedure TCSVRowParser.Reset;
begin
  FCurrentIndex := 0;
end;

end.

