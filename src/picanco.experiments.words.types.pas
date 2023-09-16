{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit picanco.experiments.words.types;

{$mode ObjFPC}{$H+}
{$modeSwitch advancedRecords}

interface

uses fgl;

const
  MaxComparisons = 4;

type

  TCycle = (CycleNone, Cycle1, Cycle2, Cycle3, Cycle4, Cycle5, Cycle6);

  TStage = (StageNone, StageTraining, StageTesting);

  TModality = (ModalityNone, ModalityA, ModalityB, ModalityC, ModalityD);

  TCondition = (
    ConditionNone,
    Condition_AB,             // Audio-To-Picture
    Condition_AC_CD,          // Audio-To-Text   and Text-To-Speech
    Condition_BC_CB_Training, // Picture-To-Text and Text-To-Picture
    Condition_BC_CB_Testing,  // Picture-To-Text and Text-To-Picture
    Condition_CD_1,           // Text-To-Speech
    Condition_AC,             // Audio-To-Text
    Condition_CD_2);          // Text-To-Speech

  TPhase = record
    Cycle : TCycle;
    Stage : TStage;
    Condition : TCondition;
    SampModality : TModality;
    CompModality : TModality;
  end;

  TOverlaps = (
    DifferentConsoantsAndVowels,
    EqualConsoants,
    EqualVowels,
    EqualConsoantsAndVowels);

  TConsoantOrd = (
    csNone,
    csPlosiveBilabial,
    csNonSibilantFricative,
    csLateralApproximantAlveolar,
    csNasalAlveolar);

  TVowelOrd = (
    vsNone,
    vsOpenFront,
    vsOpenMidFront,
    vsCloseFront,
    vsOpenMidBack);

  { TConsoant }

  TConsoant = record
    Ord: TConsoantOrd;
    IPA: string;  // International Phonetic Alphabet
    HumanReadable: string;
    procedure Next;
    class operator = (AConsoant1, AConsoant2: TConsoant): boolean;
  end;

  TConsoants = specialize TFPGList<TConsoant>;

  { TVowel }

  TVowel = record
    Ord: TVowelOrd;
    IPA: string;  // International Phonetic Alphabet
    HumanReadable: string;
    HumanReadableStress: string;
    procedure Next;
    class operator = (AVowel1, AVowel2: TVowel): boolean;
  end;

  TVowels = specialize TFPGList<TVowel>;

  { TSyllab }

  TSyllab = record
    Consoant: TConsoant;
    Vowel: TVowel;
    class operator = (ASyllab1, ASyllab2: TSyllab): boolean;
  end;

  TSyllabs = array of TSyllab;

  TWordFilenames = record
    Audio  : string;   // A
    Image  : string;   // B
    Text   : string;   // C
    Speech : string;   // D
  end;

  { TWord }

  PTWord = ^TWord;
  TWord = record
    Syllab1 : TSyllab;
    Syllab2 : TSyllab;
    Cycle : TCycle;
    Phase : TPhase;
    Overlaps : TOverlaps;
    Caption : string;
    Filenames : TWordFilenames;
    CandidateNegativeWords : array of PTWord;
    Comparisons : array [0..MaxComparisons] of record
      Audio : PTWord;
      Image : PTWord;
      Text  : PTWord;
      Speech : PTWord;
    end;
    function ToIPA : string;
    function ToHumanReadableText(AHasStress: Boolean = False): string;
    function ToString : string;
    function IsEmpty : Boolean;
  end;

  TWords = array of TWord;
  PTWords = array of PTWord;
  TWordList = specialize TFPGList<PTWord>;
  THashWords = specialize TFPGMap<string, PTWord>;

  procedure InitializeWord(var AWord: TWord);
  function GetRandomWord(var AWords: TWordList): PTWord;
  function GetWordFilenames(ACaption: string) : TWordFilenames;
  procedure SetNegativeComparisons(var AWord: TWord; var AWords: TWords);


var
  Consoants : TConsoants;
  Vowels : TVowels;
  Syllabs : TSyllabs;
  EmptyWord : TWord;

implementation

uses Classes, SysUtils, Session.Pool
  , sdl.app.output
  , picanco.experiments.constants
  , picanco.experiments.words.constants
  ;

function GetRandomWord(var AWords: TWordList): PTWord;
var
  i : integer;
begin
  i := Random(AWords.Count);
  Result := AWords[i];
  AWords.Delete(i);
end;

procedure GetWordsFromLetter(AWord: TWord; AIndex: Integer;
  ACandidateWords: TWordList; var AWords: TWords);
var
  i : Integer;
  function OtherLettersAreDifferent(AIndex: integer;
    AWord1: TWord; AWord2: TWord; AFilter: array of integer):Boolean;
  var
    i : integer;
    LWord1 : TStringList;
    LWord2 : TStringList;
    AllChars             : TStringList;
  begin
    Result := True;
    LWord1 := TStringList.Create;
    LWord1.Sorted := True;
    LWord1.Duplicates:= dupIgnore;

    LWord2 := TStringList.Create;
    LWord2.Sorted := True;
    LWord2.Duplicates:= dupIgnore;

    AllChars := TStringList.Create;
    AllChars.Sorted := True;
    AllChars.Duplicates := dupIgnore;
    try
      for i in AFilter do begin
        if i = AIndex then Continue;
        LWord1.Append(AWord1.Caption[i]);
      end;

      for i in AFilter do begin
        if i = AIndex then Continue;
        LWord2.Append(AWord2.Caption[i]);
      end;

      for i := 0 to LWord1.Count -1 do begin
        AllChars.Append(LWord1[i]);
      end;
      for i := 0 to LWord2.Count -1 do begin
        AllChars.Append(LWord2[i]);
      end;

      if AllChars.Count < LWord1.Count + LWord2.Count then
        Result := False;
    finally
      AllChars.Free;
      LWord1.Free;
      LWord2.Free;
    end;
  end;

begin
  for i := Low(AWords) to High(AWords) do begin
    if (AWord.Caption <> AWords[i].Caption) and
       (AWord.Caption[AIndex] = AWords[i].Caption[AIndex]) then begin
      if AWord.Overlaps = AWords[i].Overlaps then
        case AWord.Overlaps of
          EqualVowels: begin
            if OtherLettersAreDifferent(
              AIndex, AWord, AWords[i], [1, 3]) then begin // 1,3 consoants
               ACandidateWords.Add(@AWords[i]);
            end;
          end;
          EqualConsoants: begin
            if OtherLettersAreDifferent(
              AIndex, AWord, AWords[i], [2, 4]) then begin // 2, 4 vowels
               ACandidateWords.Add(@AWords[i]);
            end;
          end;
          EqualConsoantsAndVowels: begin
            ACandidateWords.Add(@AWords[i]);
          end;
          DifferentConsoantsAndVowels: begin
            if OtherLettersAreDifferent(
              AIndex, AWord, AWords[i], [1, 2, 3, 4]) then begin
              ACandidateWords.Add(@AWords[i]);
            end;
          end;
        end;
    end;
  end;
end;

function GetWordFilenames(ACaption: string) : TWordFilenames;
begin
  //Result.Audio  := Pool.RootMedia+DirectorySeparator+ACaption+'.wav';
  //Result.Image  := Pool.RootMedia+DirectorySeparator+ACaption+'.jpg';
  //Result.Text   := Pool.RootMedia+DirectorySeparator+ACaption+'.txt';
  //Result.Speech := Pool.RootMedia+DirectorySeparator+ACaption+'-spoken';
  Result.Audio  := ACaption;
  Result.Image  := ACaption;
  Result.Text   := ACaption;
  Result.Speech := ACaption;
end;

procedure SetNegativeComparisons(var AWord: TWord; var AWords: TWords);
var
  i: Integer;
  LNegativeComparisons : PTWords;
  LCandidateWords      : TWordList;
  LRandomWord          : PTWord;
  LIsUnique: Boolean;
  function UniqueComparison(AWord : PTWord; AWords : PTWords) : Boolean;
  var
    i : integer;
  begin
    Result := False;
    for i := Low(AWords) to High(AWords) do begin
      if AWord^.Caption = AWords[i]^.Caption then begin
         Exit;
      end;
    end;
    Result := True;
  end;

begin
  LNegativeComparisons := Default(PTWords);
  SetLength(LNegativeComparisons, Length(AWord.Caption));
  for i := Low(LNegativeComparisons) to High(LNegativeComparisons) do begin
    LNegativeComparisons[i] := @EmptyWord;
  end;

  LCandidateWords := TWordList.Create;
  try
    for i := 1 to Length(AWord.Caption) do
    begin
      LCandidateWords.Clear;
      GetWordsFromLetter(AWord, i, LCandidateWords, AWords);
      Print(LCandidateWords.Count.ToString);
      if LCandidateWords.Count > 0 then begin
        repeat
          LRandomWord := GetRandomWord(LCandidateWords);
          LIsUnique := UniqueComparison(LRandomWord, LNegativeComparisons);
        until LIsUnique or (LCandidateWords.Count = 0);
        if LIsUnique then
          LNegativeComparisons[i - 1] := LRandomWord;
      end;
    end;
  finally
    LCandidateWords.Free;
  end;
  AWord.CandidateNegativeWords := LNegativeComparisons;
end;

procedure InitializeWord(var AWord: TWord);
begin
  AWord.Caption := AWord.ToHumanReadableText;
  AWord.Filenames := GetWordFilenames(AWord.Caption);
  //AWord.Phase := GetWordPhase(AWord);

  AWord.Overlaps := DifferentConsoantsAndVowels;
  if AWord.Syllab1 = AWord.Syllab2 then begin
    AWord.Overlaps := EqualConsoantsAndVowels;
    Exit;
  end;

  if AWord.Syllab1.Consoant = AWord.Syllab2.Consoant then begin
    AWord.Overlaps := EqualConsoants;
    Exit;
  end;

  if AWord.Syllab1.Vowel = AWord.Syllab2.Vowel then begin
    AWord.Overlaps := EqualVowels;
  end;
end;

{ TConsoant }

procedure TConsoant.Next;
begin
  case Self.Ord of
    csNone : Exit;
    csPlosiveBilabial: Self := NonSibilantFricative;
    csNonSibilantFricative: Self := LateralApproximantAlveolar;
    csLateralApproximantAlveolar: Self := NasalAlveolar;
    csNasalAlveolar: Self := PlosiveBilabial;
  end;
end;

class operator TConsoant.=(AConsoant1, AConsoant2: TConsoant): boolean;
begin
  Result := AConsoant1.HumanReadable = AConsoant2.HumanReadable;
end;

{ TVowel }

procedure TVowel.Next;
begin
  case Self.Ord of
    vsNone : Exit;
    vsOpenFront: Self := OpenMidFront;
    vsOpenMidFront: Self := CloseFront;
    vsCloseFront: Self := OpenMidBack;
    vsOpenMidBack: Self := OpenFront;
  end;
end;

class operator TVowel.=(AVowel1, AVowel2: TVowel): boolean;
begin
  Result := AVowel1.HumanReadable = AVowel2.HumanReadable;
end;

{ TSyllab }

class operator TSyllab.=(ASyllab1, ASyllab2: TSyllab): boolean;
begin
  Result :=
    (ASyllab1.Consoant.HumanReadable = ASyllab2.Consoant.HumanReadable) and
    (ASyllab1.Vowel.HumanReadable = ASyllab2.Vowel.HumanReadable);
end;

{ TWord }

function TWord.ToIPA: string;
begin
  Result :=
    Syllab1.Consoant.IPA + Syllab1.Vowel.IPA + '.'+
    Syllab2.Consoant.IPA + Syllab2.Vowel.IPA + 'ˈ';
end;

function TWord.ToHumanReadableText(AHasStress: Boolean): string;
begin
  if AHasStress then begin
    Result :=
      Syllab1.Consoant.HumanReadable + Syllab1.Vowel.HumanReadable +
      Syllab2.Consoant.HumanReadable + Syllab2.Vowel.HumanReadableStress
  end else begin
    Result :=
      Syllab1.Consoant.HumanReadable + Syllab1.Vowel.HumanReadable +
      Syllab2.Consoant.HumanReadable + Syllab2.Vowel.HumanReadable;
  end;
end;

function TWord.ToString: string;
var
  LOverlaps : string;
begin
  WriteStr(LOverlaps, Overlaps);
  Result :=
    'Word: ' + Caption + LineEnding +
    'Overlaps:' + LOverlaps + LineEnding +
    'S-''s: '+ CandidateNegativeWords[0]^.Caption + #32 +
               CandidateNegativeWords[1]^.Caption + #32 +
               CandidateNegativeWords[2]^.Caption + #32 +
               CandidateNegativeWords[3]^.Caption + LineEnding +
    'A: '   + Filenames.Audio  + LineEnding +
    'B: '   + Filenames.Image  + LineEnding +
    'C: '   + Filenames.Text   + LineEnding +
    'D: '   + Filenames.Speech;
end;

function TWord.IsEmpty: Boolean;
begin
  Result := Self.Caption = EmptyWord.Caption;
end;

end.

