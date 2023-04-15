{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Generics.Iterator;

interface

uses Classes, SysUtils
   , Generics.Iterator.Contract
   , Generics.Aggregator.Contract;

type

  { TIterator }

  generic TIterator<_GT> = class(TComponent, specialize IIterator<_GT>)
  public type
    IAggregatorSpec = specialize IAggregator<_GT>;
  private
    FAggregator : IAggregatorSpec;
    FIndex : integer;
    function LastIndex : integer;
  public
    constructor Create(AAggregator: IAggregatorSpec);
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

{ TIterator }

function TIterator.LastIndex: integer;
begin
  Result := Pred(FAggregator.List.Count);
end;

constructor TIterator.Create(AAggregator: IAggregatorSpec);
begin
  FIndex := 0;
  FAggregator := AAggregator;
  Name := 'Iterator';
end;

function TIterator.GetCurrent: _GT;
begin
  Result := FAggregator.List.Items[FIndex];
end;

function TIterator.GetFirst: _GT;
begin
  Result := FAggregator.List.Items[0];
end;

function TIterator.GetNext: _GT;
begin
  if IsLast then begin
    Result := GetCurrent;
  end else begin
    Result := FAggregator.List.Items[FIndex+1];
  end;
end;

function TIterator.GetPrevious: _GT;
begin
  if IsFirst then begin
    Result := GetCurrent;
  end else begin
    Result := FAggregator.List.Items[FIndex-1];
  end;
end;

function TIterator.GetLast: _GT;
begin
  Result := FAggregator.List.Items[LastIndex];
end;

function TIterator.IsFirst: boolean;
begin
  Result := FIndex = 0;
end;

procedure TIterator.First;
begin
  FIndex := 0;
end;

procedure TIterator.Next;
begin
  if IsLast then begin
    { do nothing }
  end else begin
    Inc(FIndex);
  end;
end;

procedure TIterator.Previous;
begin
  if IsFirst then begin
    { do nothing }
  end else begin
    Dec(FIndex);
  end;
end;

procedure TIterator.Last;
begin
  FIndex := LastIndex;
end;

function TIterator.IsLast: boolean;
begin
  Result := FIndex = LastIndex;
end;

function TIterator.IndexOf(const Item: _GT): integer;
begin
  Result := FAggregator.List.IndexOf(Item);
end;

procedure TIterator.SetCurrent(AValue: integer);
begin
  if AValue < 0 then Exit;
  if AValue > LastIndex then Exit;
  FIndex := AValue;
end;

end.
