{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Session.Trials;

{$mode ObjFPC}{$H+}

{$INTERFACES CORBA}

interface

type

   { IInterTrial }

  ITrial = interface
    ['{3652F678-82C6-4FDC-AC8B-E41CF4453138}']
    function ConsequenceDelay: Cardinal;
    function ConsequenceInterval: Cardinal;
    function InterTrialInterval : Cardinal;
    procedure Hide;
    procedure LoadMockParameters;
    procedure Play;
  end;

implementation

end.

