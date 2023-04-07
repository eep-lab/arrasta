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

