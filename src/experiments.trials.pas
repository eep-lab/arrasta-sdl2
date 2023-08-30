{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit experiments.trials;

{$mode objfpc}{$H+}

interface

uses session.constants.trials;

procedure WriteToConfigurationFile(
  ATrials : integer;
  ARelation : string;
  ASamples: integer;
  AComparisons: integer;
  AHasLimitedHold : Boolean;
  AShowMouse : Boolean);

var
  Parameters : TTrialParameters;

implementation

uses
  Classes
  , SysUtils
  , session.constants.mts
  , session.constants.dragdrop
  , LazFileUtils
  , session.configurationfile
  , session.configurationfile.writer
  , sdl.app.trials.mts
  //, sdl.app.trials.dragdrop
  ;

var Writer : TConfigurationWriter;

procedure WriteTrials(ATrials: integer; AName: string;
  ARelation: string; ASamples: string; AComparisons: string;
  AHasLimitedHold : Boolean;
  AShowMouse: Boolean; ACycle:string);
begin
  case Writer.CurrentBloc of
    1 : begin
      with Writer.TrialConfig do begin
        with TrialKeys do begin
          Values[Name] := AName;
          if AShowMouse then begin
            Values[Cursor] := '0';
          end else begin
            Values[Cursor] := '-1';
          end;
          Values[Kind] := TMTS.ClassName;
          if AHasLimitedHold then
            Values[LimitedHold] := Parameters.LimitedHold.ToString;
          Values[InterTrialInterval] := Parameters.InterTrialInterval.ToString;
          Values[RepeatTrials] := ATrials.ToString;
          //case ARelation of
          //  Values[ImageFilesExtension] := '.jpg';
          //end;
        end;

        with MTSKeys do begin
          Values[Relation] := ARelation;
          Values[Samples] := ASamples;
          Values[Comparisons] := AComparisons;
          Values[Cycle] := ACycle;
        end;
      end;
      Writer.WriteTrial;
    end;
    //0 : begin
    //  with Writer.TrialConfig do begin
    //     with TrialKeys do begin
    //       Values[Name] := AName;
    //       if AShowMouse then begin
    //         Values[Cursor] := '0';
    //       end else begin
    //         Values[Cursor] := '-1';
    //       end;
    //       Values[Kind] := TDragDrop.ClassName;
    //       if AHasLimitedHold then
    //         Values[LimitedHold] := Parameters.LimitedHold.ToString;
    //       Values[InterTrialInterval] := Parameters.InterTrialInterval.ToString;
    //       Values[RepeatTrials] := ATrials.ToString;
    //       Values[ImageFilesExtension] := '.png';
    //
    //     end;
    //
    //     with DragDropKeys do begin
    //       Values[UseHelpProgression] := '0';
    //       Values[DragDropOrientation] := 'Random';
    //       Values[Relation] := 'A-C';
    //       Values[Samples] := '3';
    //       Values[Comparisons] := '3';
    //     end;
    //   end;
    //   Writer.WriteTrial;
    //end;
  end;
end;

procedure WriteToConfigurationFile(ATrials: integer;
  ARelation: string; ASamples: integer; AComparisons: integer;
  AHasLimitedHold: Boolean; AShowMouse: Boolean);
var
  LBloc : integer = 0;
  LName : string;
begin
  Writer := TConfigurationWriter.Create(ConfigurationFile);
  try
    LName := ARelation + #32 +
      'S' + ASamples.ToString + 'C' + AComparisons.ToString;

    Writer.CurrentBloc := LBloc;
    with Writer.BlocConfig do begin
      Values['Name'] := 'Bloco ' + LBloc.ToString;
    end;
    Writer.WriteBloc;
    WriteTrials(ATrials, LName, ARelation,
      ASamples.ToString, AComparisons.ToString,
      AHasLimitedHold, AShowMouse, Format('%2D', [1]));

    Inc(LBloc);
    Writer.CurrentBloc := LBloc;
    with Writer.BlocConfig do begin
      Values['Name'] := 'Bloco ' + LBloc.ToString;
    end;
    Writer.WriteBloc;
    WriteTrials(ATrials, LName, ARelation,
      ASamples.ToString, AComparisons.ToString,
      AHasLimitedHold, AShowMouse, Format('%2D', [2]));

  finally
    Writer.Free;
  end;
end;

end.
