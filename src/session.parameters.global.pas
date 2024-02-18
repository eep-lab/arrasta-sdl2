unit session.parameters.global;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, sdl.app.grids.types;

type

  TGlobalTrialParameters = record
    Cursor : integer;
    LimitedHold : UInt32;
    InterTrialInterval : UInt32;
    TimeOutInterval : UInt32;
    HasConsequence : Boolean;
    FontName : string;
    FixedSamplePosition : UInt8;
    FixedComparisonPosition : UInt8;
    ComparisonPositions : array of UInt8;
    GridOrientation : TGridOrientation;
    FontSize : integer;
    ShowModalFormForSpeechResponses : Boolean;
    AudioPromptForText : string;
    RecordingSeconds : UInt8;
    ShouldRestartAtBlockStart : Boolean;
    AudioLoopInterval : UInt32;
    DefaultAudioLoops : SmallInt;
    MarkerSize : SmallInt;
    HideMouse : Boolean;
  end;

var
  GlobalTrialParameters : TGlobalTrialParameters;

implementation

initialization
  with GlobalTrialParameters do begin
    Cursor := 0;
    LimitedHold := 0;
    InterTrialInterval := 0;
    TimeOutInterval := 0;
    HasConsequence := False;
    FontName := 'Arimo-Regular';
    FixedSamplePosition := 4;
    FixedComparisonPosition := 4;
    ComparisonPositions := nil;
    GridOrientation := goCustom;
    FontSize := 12;
    ShowModalFormForSpeechResponses := False;
    AudioPromptForText := '';
    RecordingSeconds := 1;
    ShouldRestartAtBlockStart := False;
    AudioLoopInterval := 0;
    DefaultAudioLoops := 1;
    MarkerSize := 100;
  end;
end.

