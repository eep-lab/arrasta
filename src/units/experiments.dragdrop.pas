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
   , Stimuli.Image.DragDropable
   ;

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
procedure WriteTrial(ABlc : integer;
  AName: string; ADragMode: string; ARelation: string;
  ASamples: string; AComparisons: string; AFactor: string);
var
  i : integer;
begin
  i := ConfigurationFile.TrialCount[ABlc]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name, AName);
    WriteToTrial(i, ABlc, _Cursor, '0');
    WriteToTrial(i, ABlc, _Kind, T_DRAG_DROP);
    WriteToTrial(i, ABlc, 'RepeatTrial', (3).ToString);
    //WriteToTrial(i, ABlc, _LimitedHold, (60000*15).ToString);
    WriteToTrial(i, ABlc, _ITI, (6000).ToString);
    WriteToTrial(i, ABlc, 'Style.Samples.DragMode', ADragMode);
    WriteToTrial(i, ABlc, 'Relation', ARelation);
    WriteToTrial(i, ABlc, 'Samples', ASamples);
    WriteToTrial(i, ABlc, 'Comparisons', AComparisons);
    WriteToTrial(i, ABlc, 'DragMoveFactor', AFactor);
  end;
end;

procedure WriteToConfigurationFile(ARelation: string; ASamples: integer;
  AComparisons: integer; AHelpType: integer; AFactor: integer);
var
  LDragMode : string;
  LBloc     , i: integer;
  LName     : string;
begin
  case AHelpType of
    0 : LDragMode := dragFree.ToString;
    1 : LDragMode := dragChannel.ToString;
  end;

  LBloc := 1;

  LName := ARelation + #32 + LDragMode + #32 +
    'S'+ ASamples.ToString + 'C'+AComparisons.ToString;


  WriteTrial(LBloc, LName, LDragMode,
    ARelation, ASamples.ToString, AComparisons.ToString, AFactor.ToString);

end;

end.
