{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Generics.Aggregator.Contract;

{$mode ObjFPC}{$H+}

{$INTERFACES CORBA}

interface

uses Classes, fgl, Generics.Iterator.Contract;

type

  { IAggregator }

  generic IAggregator<_GT> = interface
    function List: specialize TFPGList<_GT>;
    function Iterator: specialize IIterator<_GT>;
    procedure AssignCurrent(AParameters : TStringList);
    procedure AssignParameters(AParameters : TStringList);
  end;

implementation

end.

