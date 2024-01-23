unit session.csv.trials.pseudowords.cycle6;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, session.csv.trials.pseudowords;

type

  { TCSVPseudowordsCycle6 }

  TCSVPseudowordsCycle6 = class(TCSVPseudowordsTrials)
    public
      constructor Create(ASource: string); override;
  end;

implementation

{ TCSVPseudowordsCycle6 }

constructor TCSVPseudowordsCycle6.Create(ASource: string);
begin
  inherited Create(ASource);
  FCycle := 6;
end;

end.
