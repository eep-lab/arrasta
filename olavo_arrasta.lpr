{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
program olavo_arrasta;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces // this includes the LCL widgetset
  {$IFDEF DARWIN}
  , MachFullscreenFix
  {$ENDIF}
  , Forms
  , Forms.Main
  ;

{$R *.res}

begin
  Randomize;
  Application.Title:='arrasta';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TBackground, Background);
  {$IFDEF LINUX}
    Background.WindowState:=wsFullScreen;
  {$ENDIF}

  {$IFDEF DARWIN}
    FixWindowCollectionBehavior;
  {$ENDIF}
  Application.Run;
end.

