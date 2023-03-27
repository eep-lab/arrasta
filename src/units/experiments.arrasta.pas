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
  ADesign : string;
  ASamples: integer;
  AComparisons: integer;
  AHelpType: integer;
  AFactor: integer) : string;

implementation

uses FileMethods
   , Experiments.DragDrop
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   ;

function MakeConfigurationFile(ADesign: string; ASamples: integer;
  AComparisons: integer; AHelpType: integer; AFactor: integer): string;
begin
  Result := NewConfigurationFile;
  Experiments.DragDrop.WriteToConfigurationFile(
    ADesign, ASamples, AComparisons, AHelpType, AFactor);
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;





end.
