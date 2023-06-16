{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Experiments.DragDrop;

{$mode objfpc}{$H+}

interface

procedure WriteToConfigurationFile(
  AOrientation : string;
  ATrials : integer;
  ADistance : integer;
  ARelation : string;
  ASamples: integer;
  AComparisons: integer;
  AMouseMoveMode: string;
  AFactor: string;
  AUseHelpProgression : Boolean;
  AHasLimitedHold : Boolean;
  AShowMouse : Boolean);

var
  ITI : integer;
  LimitedHold: integer;

implementation

uses Classes, SysUtils
   , Constants
   , LazFileUtils
   , Session.ConfigurationFile
   , Session.ConfigurationFile.Writer
   , Stimuli.Image.DragDropable
   , Session.Trial.HelpSeries.DragDrop
   , Constants.DragDrop
   ;


var Writer : TConfigurationWriter;
{
  Planned
  'Style.Samples.Animation'=Constrast
  'Style.Samples.Animation'=Moviment
  'Style.Samples.Animation'=None
  'Style.Samples.Animation.Shape'=Square
  'Style.Samples.Animation.Shape'=Circle
  'Style.Samples.Animation.Color'=#930000

  Done
  'Style.Samples.DragMode'=Channel
  'Style.Samples.DragMode'=Free
  'Relation'= 'A-A'|'B-B'|'C-C'|'A-B'|'A-C'| ...
  'Samples'= 1 .. 3
  'Comparisons'= 1 .. 3 // Note: Minimum equals Samples
  'DragMoveFactor'= 5 .. 100;
}
procedure WriteTrials(AOrientation : string; ATrials: integer;
  ADistance : integer; AName: string; ADragMode: string; ARelation: string;
  ASamples: string; AComparisons: string; AFactor: string;
  AUseHelpProgression : Boolean; AHasLimitedHold : Boolean; AShowMouse: Boolean);
begin
  case Writer.CurrentBloc of
    0, 1, 2, 3, 4, 5 : begin
      with Writer.TrialConfig do begin
        Values[_Name] := AName;
        if AShowMouse then begin
          Values[_Cursor] := '0';
        end else begin
          Values[_Cursor] := '-1';
        end;
        Values[_Kind] := T_DRAGDROP;
        if AHasLimitedHold then
          Values[_LimitedHold] := LimitedHold.ToString;
        Values[_ITI] := ITI.ToString;
        with DragDropKeys do begin
          Values[DragDropOrientation] := AOrientation;
          Values[UseHelpProgression] := AUseHelpProgression.ToString;
          Values[RepeatTrials] := ATrials.ToString;
          Values[Distance] := ADistance.ToString;
          Values[SamplesDragMode] := ADragMode;
          Values[Relation] := ARelation;
          Values[Samples] := ASamples;
          Values[Comparisons] := AComparisons;
          Values[DragMoveFactor] := AFactor.ToFactor.ToInteger.ToString;
        end;
      end;
      Writer.WriteTrial;
    end;
  end;
end;

procedure WriteToConfigurationFile(AOrientation : string; ATrials: integer;
  ADistance : integer; ARelation: string; ASamples: integer;
  AComparisons: integer; AMouseMoveMode: string; AFactor: string;
  AUseHelpProgression: Boolean; AHasLimitedHold: Boolean; AShowMouse: Boolean);
var
  LBloc : integer = 0;
  LName : string;
begin
  Writer := TConfigurationWriter.Create(ConfigurationFile);
  try
    LName := ARelation + #32 + AMouseMoveMode + #32 +
      'S' + ASamples.ToString + 'C' + AComparisons.ToString;

    Writer.CurrentBloc := LBloc;
    with Writer.BlocConfig do begin
      Values[_Name] := 'Bloco ' + LBloc.ToString;
    end;
    Writer.WriteBloc;
    WriteTrials(AOrientation, ATrials, ADistance, LName, AMouseMoveMode, ARelation,
      ASamples.ToString, AComparisons.ToString, AFactor,
      AUseHelpProgression, AHasLimitedHold, AShowMouse);

  finally
    Writer.Free;
  end;
end;

end.
