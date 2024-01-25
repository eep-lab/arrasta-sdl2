unit session.csv.document;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, csvdocument;

type

  { TCustomCSVDocument }

  TCustomCSVDocument = class(TCSVDocument)
  private
    FSkipHeader : boolean;
    FCurrentIndex: integer;
  published
    constructor Create; override;
    property CurrentIndex : integer read FCurrentIndex write FCurrentIndex;
    property SkipHeader : boolean read FSkipHeader write FSkipHeader;
  end;

implementation

{ TCustomCSVDocument }

constructor TCustomCSVDocument.Create;
begin
  inherited Create;
  FSkipHeader := True;
  FCurrentIndex := 0;
end;

end.

