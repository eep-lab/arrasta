{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).
  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Sequence.DragDrop1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, fgl, Graphics
  , Stimuli1
  , Stimuli.Abstract1
  , Stimuli.Image.DragDropable1
  , Stimuli.Image.Base1
  ;

type

  TDragDropableItems = specialize TFPGList<TDragDropableItem>;

  { TDragDropStimuli }

  TDragDropStimuli = class(TStimulus, IStimuli)
  private
    FParent: TWinControl;
    FSamples : TDragDropableItems;
    procedure SetParent(AValue: TWinControl);
  public
    constructor Create(AOwner : TComponent); override; overload;
    constructor Create(AOwner : TComponent; ASamples: integer;
      AComparisons: integer); overload;
    destructor Destroy; override;
    function AsInterface : IStimuli;
    procedure DoExpectedResponse;
    procedure LoadFromParameters(AParameters : TStringList);
    procedure Start;
    procedure Stop;
    property Parent : TWinControl read FParent write SetParent;
  end;

implementation

uses
  Experiments.Grids1;

{ TDragDropStimuli }

function TDragDropStimuli.AsInterface : IStimuli;
begin
  Result := Self;
end;

procedure TDragDropStimuli.DoExpectedResponse;
begin

end;

procedure TDragDropStimuli.LoadFromParameters(AParameters: TStringList);
begin

end;

procedure TDragDropStimuli.Start;
var
  LItem : TDragDropableItem;
begin
  for LItem in FSamples do
  begin
    LItem.Show;
    LItem.BringToFront;
  end;
end;

procedure TDragDropStimuli.Stop;
begin

end;

procedure TDragDropStimuli.SetParent(AValue: TWinControl);
var
  LItem : TDragDropableItem;
begin
  if FParent=AValue then Exit;
  FParent:=AValue;

  for LItem in FSamples do
    LItem.Parent := AValue;
end;

constructor TDragDropStimuli.Create(AOwner: TComponent);
begin
  Create(AOwner, 1, 0);
end;

constructor TDragDropStimuli.Create(AOwner: TComponent; ASamples: integer;
  AComparisons: integer);
var
  LItem : TDragDropableItem;
  LComparison : TDragDropableItem;
  LComparisons : TDragDropableItems;
  i : integer;
begin
  inherited Create(AOwner);
  Grid := TGrid.Create(1, ASamples, AComparisons);

  FSamples := TDragDropableItems.Create;
  LComparisons := TDragDropableItems.Create;

  with Grid.RandomPositions do
  begin
    for i := low(Comparisons) to high(Comparisons) do
    begin
      LItem := TDragDropableItem.Create(self);
      LItem.Caption := 'B'+(i+1).ToString;
      LItem.SetOriginalBounds(
        Comparisons[i].Left,
        Comparisons[i].Top,
        Comparisons[i].SquareSide,
        Comparisons[i].SquareSide);

      Comparisons[i].Item := LItem as TObject;
      LComparisons.Add(LItem);
      if i = 0 then
        LItem.Caption := 'B'+(i+1).ToString;
    end;

    for i := low(Samples) to high(Samples) do
    begin
      LItem := TDragDropableItem.Create(Self);
      LItem.SetOriginalBounds(
        Samples[i].Left,
        Samples[i].Top,
        Samples[i].SquareSide,
        Samples[i].SquareSide);
      LItem.Caption := 'A'+(i+1).ToString;
      case i of
        0 :

        else begin
          LComparisons.Exchange(0, i);
        end;
      end;
      for LComparison in LComparisons do
        //LItem.AddTarget(LComparison);

      Samples[i].Item := LItem as TObject;
      FSamples.Add(LItem);
    end;
  end;
  LComparisons.Free;
end;

destructor TDragDropStimuli.Destroy;
begin
  FSamples.Free;
  Grid.Free;
  inherited Destroy;
end;

end.
