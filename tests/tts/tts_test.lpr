program tts_test;

uses
  Classes, SysUtils, ComObj, Variants;

procedure ListInstalledVoices;
var
  SAPI: Variant;
  Voices: Variant;
  Voice: Variant;
  i: Integer;
begin
  try
    // Create a SAPI SpVoice object
    SAPI := CreateOleObject('SAPI.SpVoice');

    // Get the collection of installed voices
    Voices := SAPI.GetVoices('');

    // Iterate through the voices and display their names
    for i := 0 to Voices.Count - 1 do
    begin
      Voice := Voices.Item(i);
      Writeln('Voice Name: ' + Voice.GetAttribute('Name'));
    end;
  except
    on E: Exception do
      Writeln('Error: ' + E.Message);
  end;
  ReadLn;
end;

procedure SynthesizeWord(AVoice : string);
var
  SAPIVoice: Variant;
  VoiceString : WideString = 'FALÉ';
begin
  try
    // Create a SAPI voice object
    SAPIVoice := CreateOleObject('SAPI.SpVoice');

    // Set the voice to Brazilian Portuguese (assuming you have it installed)
    //SAPIVoice.Voice := SAPIVoice.GetVoices('Name='+).Item(0);

    // Synthesize and speak the word "FALÉ"
    SAPIVoice.Speak(VoiceString, 0);

  except
    on E: Exception do
      Writeln('Error: ' + E.Message);
  end;
end;

begin
  ListInstalledVoices;
  // Voice Name: Microsoft Maria Desktop
  // Voice Name: Microsoft Zira Desktop
  SynthesizeWord('Microsoft Maria Desktop');
end.
