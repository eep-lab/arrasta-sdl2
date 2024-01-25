unit session.csv.trials.pseudowords.cycle1;

{$mode ObjFPC}{$H+}

interface

uses SysUtils, session.csv.trials.pseudowords;

type

  { TCSVPseudowordsCycle1 }

  TCSVPseudowordsCycle1 = class(TCSVPseudowordsTrials)
    public
      constructor Create(ASource: string); override;
  end;

implementation

{ TCSVPseudowordsCycle1 }

constructor TCSVPseudowordsCycle1.Create(ASource: string);
begin
  inherited Create(ASource);
  FCycle := 1;
end;

end.


