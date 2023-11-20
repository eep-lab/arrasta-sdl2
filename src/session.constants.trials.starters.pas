unit session.constants.trials.starters;

{$mode ObjFPC}{$H+}

interface

uses session.constants.trials;

type
  TParserTrialsStarters = record
    BlockIDKey        : string;
    TrialIDKey        : string;
    InstructionKey    : string;
    HasInstructionKey : string;
    HasCalibrationKey : string;
  end;

const
  ParserTrialsStarters : TParserTrialsStarters = (
    BlockIDKey        : 'Block';
    TrialIDKey        : 'Trial';
    InstructionKey    : HeaderInstruction;
    HasInstructionKey : HeaderHasInstruction;
    HasCalibrationKey : HeaderHasCalibration;
  );

implementation

end.

