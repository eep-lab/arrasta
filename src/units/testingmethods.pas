{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit TestingMethods;

{$mode objfpc}{$H+}

interface

procedure ShowTrialConsole;

implementation

uses SysUtils, Dialogs, Session.Configuration.GlobalContainer;

procedure ShowTrialConsole;
var
  LNextTrial : string;
  LNextTrialI : integer;
begin
  LNextTrial := InputBox('Trial Console', 'Insert the Next Trial', '-');
  LNextTrialI := StrToIntDef(LNextTrial, -1);
  if LNextTrialI <> -1 then
  begin
    //while Trial = nil do Application.ProcessMessages;
    GlobalContainer.CounterManager.CurrentTrial := LNextTrialI;
  end;
end;

end.

