unit session.csv.trials.pseudowords.cycle5;

{$mode ObjFPC}{$H+}

interface

uses SysUtils, session.csv.trials.pseudowords;

type

  { TCSVPseudowordsCycle5 }

  TCSVPseudowordsCycle5 = class(TCSVPseudowordsTrials)
    public
      constructor Create(ASource: string); override;
  end;

implementation

{ TCSVPseudowordsCycle5 }

constructor TCSVPseudowordsCycle5.Create(ASource: string);
begin
  inherited Create(ASource);
  FCycle := 5;
end;

end.
