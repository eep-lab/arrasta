unit Generics.Aggregator;

{$mode ObjFPC}{$H+}

interface

uses Classes, SysUtils, fgl
   , Generics.Aggregator.Contract
   , Generics.Iterator.Contract
   , Generics.Iterator;

type

  { TAggregator }

  generic TAggregator<_GT> = class abstract (specialize IAggregator<_GT>)
  public type
    TIteratorSpec = specialize TIterator<_GT>;
    TFPGListSpec  = specialize TFPGList<_GT>;
    IIteratorSpec = specialize IIterator<_GT>;
  private

  protected
    FList : TFPGListSpec;
    FIterator : TIteratorSpec;
    function Loaded : Boolean;
    procedure LoadFromFile(AFilename : string); virtual; abstract;
    procedure LoadFromMock; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    function List: TFPGListSpec;
    function Iterator: IIteratorSpec;
    procedure AssignCurrent(AParameters : TStringList); virtual; abstract;
    procedure AssignParameters(AParameters : TStringList); virtual; abstract;
  end;

implementation

{ TAggregator }

function TAggregator.Loaded: Boolean;
begin
  Result := FList.Count > 0;
end;

constructor TAggregator.Create;
begin
  FList := TFPGListSpec.Create;
  FIterator:=TIteratorSpec.Create(Self);
end;

destructor TAggregator.Destroy;
begin
  FIterator.Free;
  FList.Free;
  inherited Destroy;
end;

function TAggregator.List: TFPGListSpec;
begin
  Result := FList;
end;

function TAggregator.Iterator: IIteratorSpec;
begin
  Result := FIterator as IIteratorSpec;
end;


end.

