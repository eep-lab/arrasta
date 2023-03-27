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
  ADesign : string;
  ASamples: integer;
  AComparisons: integer;
  AHelpType: integer);

implementation

uses Classes, SysUtils
   , Constants
   , StrUtils
   , LazFileUtils
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   , Experiments.Grids
   ;

var
  StringListPositions : TStringList;


{
  'Style.Samples.Animation'=Constrast
  'Style.Samples.Animation'=Moviment
  'Style.Samples.Animation'=None
  'Style.Samples.Animation.Shape'=Square
  'Style.Samples.Animation.Shape'=Circle
  'Style.Samples.Animation.Color'=#930000
  'Style.Samples.DragMode'=Channel
  'Style.Samples.DragMode'=Free
  'Relation'=Identity

  'S1.File'=A1.jpg
  'S2.File'=A2.jpg
  'S3.File'=A3.jpg

  'C1.File'=B1.jpg
  'C2.File'=B2.jpg
  'C3.File'=B3.jpg
}
procedure WriteTrial(ABlc : integer;
  AName: string; ADelay: string; ADragMode: string;
  ARelation: string);
var
  i : integer;
begin
  i := ConfigurationFile.TrialCount[ABlc]+1;
  with ConfigurationFile do
  begin
    WriteToTrial(i, ABlc, _Name, AName);
    WriteToTrial(i, ABlc, _Cursor, '0');
    WriteToTrial(i, ABlc, _Kind, T_DRAG_DROP);
    WriteToTrial(i, ABlc, _Delay, ADelay);
    WriteToTrial(i, ABlc, 'Style.Samples.DragMode', ADragMode);
    WriteToTrial(i, ABlc, 'Relation', ARelation);
  end;
end;

procedure WriteA(var ABlc: integer);
var
  i : integer;
  j : integer;
  r : integer;
  LStringList : TStringList;
begin

end;

procedure WriteB(ABlc : integer);
var
  i : integer;
  j : integer;
  r : integer;
  LStringList : TStringList;
begin

end;

procedure WriteC(ABlc : integer);
var
  i : integer;
  j : integer;
  r : integer;
  LStringList : TStringList;
  LTrainingDelay : string;
  LDelay : string;
  LIsTestTrial : Boolean;
begin

end;



procedure WriteToConfigurationFile(ADesign: string; ASamples: integer;
  AComparisons: integer; AHelpType: integer);
var
  i : integer;
  LCondition : string = '';
  LConditionI : integer = 0;
  procedure WriteA1Condition;
  begin

  end;

  procedure WriteB1Condition;
  begin

  end;

  procedure WriteA2Condition;
  begin

  end;

  procedure WriteC1Condition;
  begin

  end;

begin
  //SetupStimuli;
  for i := 0 to WordCount(ADesign, [#32]) -1 do begin
    LCondition := ExtractDelimited(i+1, ADesign, [#32]);
    case LCondition of
      'A1': WriteA1Condition;
      'B1': WriteB1Condition;
      'A2': WriteA2Condition;
      'C1': WriteC1Condition;
    end;
  end;
end;

end.
