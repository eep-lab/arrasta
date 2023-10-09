{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Experiments.Arrasta;

{$mode objfpc}{$H+}

interface

function MakeConfigurationFile(
  AOrientation : string;
  ATrials : integer;
  AITI    : integer;
  //ALimitedHold : integer;
  ADistance : integer;
  ADesign : string;
  ASamples: integer;
  AComparisons: integer;
  //AMouseMoveMode: string;
  //AFactor: string;
  //AUseHelpProgression : Boolean;
  //AHasLimitedHold: Boolean;
  AShowMouse: Boolean) : string;

implementation

uses FileMethods
   , Experiments.DragDrop
   , Session.ConfigurationFile
   ;

function MakeConfigurationFile(AOrientation: string; ATrials : integer;
  AITI : integer; {ALimitedHold : integer;} ADistance : integer; ADesign: string; ASamples: integer;
  AComparisons: integer; {AMouseMoveMode: string; AFactor: string;
  AUseHelpProgression : Boolean; AHasLimitedHold: Boolean;}
  AShowMouse: Boolean): string;
begin
  Result := NewConfigurationFile;
  Experiments.DragDrop.ITI := AITI;
  //Experiments.DragDrop.LimitedHold:=ALimitedHold;
  Experiments.DragDrop.WriteToConfigurationFile(AOrientation, ATrials,
    ADistance, ADesign, ASamples, AComparisons, {AMouseMoveMode, AFactor,
    AUseHelpProgression, AHasLimitedHold,} AShowMouse);
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;





end.
