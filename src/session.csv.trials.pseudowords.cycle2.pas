unit session.csv.trials.pseudowords.cycle2;

{$mode ObjFPC}{$H+}

interface

uses SysUtils, session.csv.trials.pseudowords;

type

  { TCSVPseudowordsCycle2 }

  TCSVPseudowordsCycle2 = class(TCSVPseudowordsTrials)
    public
      constructor Create(ASource: string); override;
  end;

implementation

{ TCSVPseudowordsCycle2 }

constructor TCSVPseudowordsCycle2.Create(ASource: string);
begin
  inherited Create(ASource);
  FCycle := 2;
end;

end.
