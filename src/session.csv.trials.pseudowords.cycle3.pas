unit session.csv.trials.pseudowords.cycle3;

{$mode ObjFPC}{$H+}

interface

uses SysUtils, session.csv.trials.pseudowords;

type

  { TCSVPseudowordsCycle3 }

  TCSVPseudowordsCycle3 = class(TCSVPseudowordsTrials)
    public
      constructor Create(ASource: string); override;
  end;

implementation

{ TCSVPseudowordsCycle3 }

constructor TCSVPseudowordsCycle3.Create(ASource: string);
begin
  inherited Create(ASource);
  FCycle := 3;
end;

end.
