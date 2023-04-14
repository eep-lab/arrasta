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

