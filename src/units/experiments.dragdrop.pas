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
  ARelation : string;
  ASamples: integer;
  AComparisons: integer;
  AHelpType: integer;
  AFactor: integer);

implementation

uses Classes, SysUtils
   , Constants
   , StrUtils
   , LazFileUtils
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   , Session.ConfigurationFile.Writer
   , Stimuli.Image.DragDropable
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
procedure WriteTrials(AName: string; ADragMode: string; ARelation: string;
  ASamples: string; AComparisons: string; AFactor: string);
begin
  case Writer.CurrentBloc of
    0, 1, 2, 3, 4, 5 : begin
      with Writer.TrialConfig do begin
        Values[_Name] := AName;
        Values[_Cursor] := '0';
        Values[_Kind] := T_DRAGDROP;
        Values['RepeatTrial'] := (1).ToString;
        //Values[_LimitedHold] := (60000*15).ToString;
        Values[_ITI] := (1000).ToString;
        Values['Style.Samples.DragMode'] := ADragMode;
        Values['Relation'] := ARelation;
        Values['Samples'] := ASamples;
        Values['Comparisons'] := AComparisons;
        Values['DragMoveFactor'] := AFactor;
      end;
      Writer.WriteTrial;
    end;
  end;
end;

procedure WriteToConfigurationFile(ARelation: string; ASamples: integer;
  AComparisons: integer; AHelpType: integer; AFactor: integer);
var
  LBloc, Bloc: integer;
  LDragMode , LName: string;
begin
  Writer := TConfigurationWriter.Create(ConfigurationFile);
  try
    case AHelpType of
      0 : LDragMode := dragFree.ToString;
      1 : LDragMode := dragChannel.ToString;
    end;

    LName := ARelation + #32 + LDragMode + #32 +
      'S'+ ASamples.ToString + 'C'+AComparisons.ToString;

    for Bloc := 0 to 5 do begin
      Writer.CurrentBloc := Bloc;
      with Writer.BlocConfig do begin
        Values[_Name] := 'Bloco ' + Bloc.ToString;
      end;
      Writer.WriteBloc;
      WriteTrials(LName, LDragMode, ARelation,
        ASamples.ToString, AComparisons.ToString, AFactor.ToString);
    end;

  finally
    Writer.Free;
  end;
end;

end.
