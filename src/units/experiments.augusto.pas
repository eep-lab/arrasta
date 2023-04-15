{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Experiments.Augusto;

{$mode objfpc}{$H+}

interface

function MakeConfigurationFile : string;

implementation

uses FileMethods
   , Experiments.Augusto.BeforeAfter
   , Experiments.Augusto.SameDifferent
   , Experiments.Augusto.Derivation
   , Session.ConfigurationFile
   ;

function MakeConfigurationFile : string;
begin
  Result := NewConfigurationFile;
  Experiments.Augusto.BeforeAfter.WriteToConfigurationFile;
  Experiments.Augusto.SameDifferent.WriteToConfigurationFile;
  Experiments.Augusto.Derivation.WriteToConfigurationFile;
  ConfigurationFile.Invalidate;
  ConfigurationFile.UpdateFile;
end;





end.
