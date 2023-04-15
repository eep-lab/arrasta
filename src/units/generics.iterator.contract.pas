{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Generics.Iterator.Contract;

{$mode ObjFPC}{$H+}

{$INTERFACES CORBA}

interface

type

  { IIterator }

  generic IIterator<_GT> = interface
    function GetCurrent : _GT;
    function GetFirst : _GT;
    function GetNext : _GT;
    function GetPrevious : _GT;
    function GetLast : _GT;
    function IsFirst: boolean;
    function IsLast: boolean;
    function IndexOf(const Item: _GT): integer;
    procedure SetCurrent(AValue : integer);
    procedure First;
    procedure Next;
    procedure Previous;
    procedure Last;
  end;

implementation

end.

