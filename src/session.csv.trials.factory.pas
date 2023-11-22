unit session.csv.trials.factory;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections
  , session.csv.trials.base;

type
  TCSVTrialClass = class of TCSVTrialsBase;

  TCSVTrialRegister = specialize TDictionary<string, TCSVTrialClass>;

  { TCSVTrialsFactory }

  TCSVTrialsFactory = class
    strict private
      class var Registry : TCSVTrialRegister;
    public
      class procedure RegisterCSVTrialClass(
        ATrialSourceFilename: string; ACSVTrialClass: TCSVTrialClass); static;
      class constructor Create;
      class destructor Destroy;
      class function New(ATrialSourceFilename : string) : TCSVTrialsBase;
  end;

implementation

uses
  session.csv.trials.pseudowords,
  session.csv.trials.daats;

{ TCSVTrialsFactory }

class procedure TCSVTrialsFactory.RegisterCSVTrialClass(
  ATrialSourceFilename: string; ACSVTrialClass: TCSVTrialClass);
begin
  Registry.Add(ATrialSourceFilename, ACSVTrialClass);
end;

class constructor TCSVTrialsFactory.Create;
begin
  Registry := TCSVTrialRegister.Create;
end;

class destructor TCSVTrialsFactory.Destroy;
begin
  Registry.Free;
end;

class function TCSVTrialsFactory.New(
  ATrialSourceFilename: string): TCSVTrialsBase;
var
  CSVTrialClass : TCSVTrialClass;
begin
  if not Registry.TryGetValue(ATrialSourceFilename, CSVTrialClass) then
    raise EArgumentException.CreateFmt(
      'CSV trial not registered: %s', [ATrialSourceFilename]);
  Result := CSVTrialClass.Create;
end;

initialization
  TCSVTrialsFactory.RegisterCSVTrialClass(
    'mts-pseudowords', TCSVPseudowordsTrials);
  TCSVTrialsFactory.RegisterCSVTrialClass(
    'mts-daats', TCSVDAATSTrials);

  //TCSVTrialsFactory.RegisterTrialClass('mm-eep', TDragDrop);
end.

