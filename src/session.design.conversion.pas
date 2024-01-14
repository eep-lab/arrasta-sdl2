unit session.design.conversion;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

  procedure CovertToSingleFilename;

implementation

uses session.strutils, csvdocument, session.csv.enumerable;

procedure CovertToSingleFilename;
const
  OutputFilename = 'Estudo1-Condicoes-de-00-a-07.csv';
  BlockID = 'BlockID';
  TrialID = 'TrialID';
  TrialIDSource = 'TrialIDSource';
var
  LInputFilename : string;
  LTrialIDSource : string;
  LPreTraining : string = '00 - Pre-treino.csv';
  LProbes : string = '07 - Sondas CD (20 palavras).csv';
  LCorePhases : array of string = (
    '07 - Sondas CD (20 palavras).csv',
    '01 - Treino AB.csv',
    '02 - Treino AC-CD.csv',
    '02b - Treino AC-CD (Ref Intermitente).csv',
    '03 - Sondas BC-CB (Palavras de ensino).csv',
    '04 - Sondas BC-CB (Palavras reservadas).csv',
    '05 - Sondas CD (Palavras reservadas e novas).csv',
    '06 - Sondas AC (Palavras reservadas e novas).csv');
  LPhase : string;

  LCSVOutput : TCSVDocument;
  LAnnotation : TCSVDocument;
  LRow : TStringList;

  LConvertedBlockID : integer = 0;
  LBlockID : integer;
  LLastBlock, i: integer;

  procedure ConvertDesignFile(AInputFilename: string; ACSVOutput : TCSVDocument;
    ATrialIDSource: string; var AConvertedBlockID : integer);
  var
    LCSVInput : TCSVRows;
    LFirst : Boolean = True;
  begin
    LCSVInput := TCSVRows.Create;
    try
      LCSVInput.LoadFromFile(AInputFilename);
      LLastBlock := 0;

      for LRow in LCSVInput do begin
        LBlockID := LRow.Values[BlockID].ToInteger;
        if (LBlockID <> LLastBlock) then begin
          Inc(AConvertedBlockID);
          if LFirst then begin
            LFirst := False;
            LAnnotation.AddRow(AConvertedBlockID.ToString);
            LAnnotation.Cells[1, LAnnotation.RowCount-1] := AInputFilename;
          end;
        end;

        ACSVOutput.AddRow(AConvertedBlockID.ToString);
        ACSVOutput.Cells[1, ACSVOutput.RowCount-1] := LRow.Values[TrialID];
        ACSVOutput.Cells[2, ACSVOutput.RowCount-1] := ATrialIDSource;
        LLastBlock := LBlockID;
      end;
    finally
      LCSVInput.Free;
    end;
  end;


  procedure ConvertInstructionsFile(
    AInputFilename: string; ACSVOutput : TCSVDocument;
    AWriteHeader: Boolean; AConvertedBlockID : integer);
  var
    LCSVInput : TCSVRows;
    LValue : string;
    LIsFirst : Boolean = True;
    i : integer;
  begin
    LCSVInput := TCSVRows.Create;
    try
      LCSVInput.LoadFromFile(AInputFilename);

      for LRow in LCSVInput do begin
        if AWriteHeader then begin
          AWriteHeader := False;
          for i := 0 to LRow.Count-1 do begin
            ACSVOutput.Cells[i, ACSVOutput.RowCount-1] := LRow.Names[i];
          end;
        end;

        ACSVOutput.AddRow;
        for i := 0 to LRow.Count-1 do begin
          if i = 0 then begin
            if LIsFirst then begin
              LIsFirst := False;
              LValue := AConvertedBlockID.ToString;
            end else begin
              Inc(AConvertedBlockID, LRow.Values['Block'].ToInteger-1);
              LValue := AConvertedBlockID.ToString;
            end;
          end else begin
            LValue := LRow.ValueFromIndex[i];
          end;
          ACSVOutput.Cells[i, ACSVOutput.RowCount-1] := LValue;
        end;
      end;
    finally
      LCSVInput.Free;
    end;
  end;


  procedure ConvertBlocksFile(AInputFilename: string; ACSVOutput : TCSVDocument;
    AWriteHeader: Boolean; var AConvertedBlockID : integer);
  var
    LCSVInput : TCSVRows;
    i: integer;
    LValue : integer;
    LValueIncrement : integer;
  begin
    LCSVInput := TCSVRows.Create;
    try
      LCSVInput.LoadFromFile(AInputFilename);

      LValueIncrement := AConvertedBlockID;
      for LRow in LCSVInput do begin
        if AWriteHeader then begin
          AWriteHeader := False;
          for i := 0 to LRow.Count-1 do begin
            ACSVOutput.Cells[i, ACSVOutput.RowCount-1] := LRow.Names[i];
          end;
        end;
        Inc(AConvertedBlockID);
        ACSVOutput.AddRow;
        for i := 0 to LRow.Count-1 do begin // sorted
          case i of
            0: begin // backup block
              LValue := LRow.ValueFromIndex[i].ToInteger;
              if LValue > 0 then begin
                Inc(LValue, LValueIncrement);
              end;

              ACSVOutput.Cells[i, ACSVOutput.RowCount-1] :=
                LValue.ToString;
            end;

            4: begin // ID
              ACSVOutput.Cells[i, ACSVOutput.RowCount-1] :=
              AConvertedBlockID.ToString
            end;

            7: begin // NextBlockOnHitCriterion
              LValue := LRow.ValueFromIndex[i].ToInteger;
              if LValue > 0 then begin
                LValue := AConvertedBlockID+1;
              end;

              ACSVOutput.Cells[i, ACSVOutput.RowCount-1] :=
                LValue.ToString;
            end;

            otherwise begin
              ACSVOutput.Cells[i, ACSVOutput.RowCount-1] :=
                LRow.ValueFromIndex[i];
            end;
          end;
        end;
      end;

    finally
      LCSVInput.Free;
    end;
  end;

begin
  // design folder
  LCSVOutput := TCSVDocument.Create;
  LAnnotation := TCSVDocument.Create;
  try
    LCSVOutput.AddRow;
    LCSVOutput.Cells[0, 0] := BlockID;
    LCSVOutput.Cells[1, 0] := TrialID;
    LCSVOutput.Cells[2, 0] := TrialIDSource;

    LTrialIDSource := 'mts-images';
    LInputFilename := ConcatPaths([DesignFolder, LPreTraining]);
    ConvertDesignFile(
      LInputFilename, LCSVOutput,LTrialIDSource, LConvertedBlockID);
    for i := 0 to 5 do begin
      for LPhase in LCorePhases do begin
        LTrialIDSource := 'mts-pseudowords-'+(i+1).ToString;
        LInputFilename := ConcatPaths([DesignFolder, LPhase]);
        ConvertDesignFile(
          LInputFilename, LCSVOutput, LTrialIDSource, LConvertedBlockID);
      end;
    end;
    LInputFilename := ConcatPaths([DesignFolder, LProbes]);
    ConvertDesignFile(
      LInputFilename, LCSVOutput, LTrialIDSource, LConvertedBlockID);

    LCSVOutput.SaveToFile(
      ConcatPaths([DesignFolder, OutputFilename]));
  finally
    LCSVOutput.Free;
  end;

  // instructions folder
  LCSVOutput := TCSVDocument.Create;
  LCSVOutput.AddRow;
  try
    LConvertedBlockID := LAnnotation.Cells[0, 0].ToInteger;
    LInputFilename := ConcatPaths([DesignFolder, 'instructions', LPreTraining]);
    ConvertInstructionsFile(LInputFilename, LCSVOutput, True, LConvertedBlockID);
    for i := 1 to LAnnotation.RowCount-1 do begin
      LConvertedBlockID := LAnnotation.Cells[0, i].ToInteger;
      LInputFilename := ConcatPaths([DesignFolder, 'instructions',
        ExtractFileName(LAnnotation.Cells[1, i])]);
      ConvertInstructionsFile(
        LInputFilename, LCSVOutput, False, LConvertedBlockID);
    end;

    //LAnnotation.SaveToFile(
    //  ConcatPaths([DesignFolder, '_'+OutputFilename]));

    LCSVOutput.SaveToFile(
      ConcatPaths([DesignFolder, '_'+OutputFilename]));
  finally
    LAnnotation.Free;
    LCSVOutput.Free;
  end;

  // blocks folder
  LConvertedBlockID := 0;
  LCSVOutput := TCSVDocument.Create;
  try
    LCSVOutput.AddRow;

    LInputFilename := ConcatPaths([DesignFolder, 'blocks', LPreTraining]);
    ConvertBlocksFile(LInputFilename, LCSVOutput, True, LConvertedBlockID);
    for i := 0 to 5 do begin
      for LPhase in LCorePhases do begin
        LInputFilename := ConcatPaths([DesignFolder, 'blocks', LPhase]);
        ConvertBlocksFile(LInputFilename, LCSVOutput, False, LConvertedBlockID);
      end;
    end;
    LInputFilename := ConcatPaths([DesignFolder, 'blocks', LProbes]);
    ConvertBlocksFile(LInputFilename, LCSVOutput, False, LConvertedBlockID);

    LCSVOutput.SaveToFile(
      ConcatPaths([DesignFolder, 'blocks', OutputFilename]));
  finally
    LCSVOutput.Free;
  end;
end;

end.

