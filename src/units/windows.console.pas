{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Windows.Console;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Windows;

implementation

initialization
  AllocConsole;
  IsConsole := True;
  SysInitStdIO;

end.

