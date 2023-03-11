{
  Stimulus Control
  Copyright (C) 2014-2020 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Sequence.DragDrop;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, ExtCtrls, Schedules, Graphics
  , Stimuli
  , Stimuli.Abstract
  , Stimuli.Image.DragDropable
  , Stimuli.Image.Animation
  , Stimuli.Image.Base
  , Experiments.Grids
  , Forms.Main
  ;

type

  { TDragDropStimuli }

  TDragDropStimuli = class(TStimulus, IStimuli)
  private
    LComparison : TDragDropableItem;
    LItem : TDragDropableItem;
    FSample : TDragDropableItem;
    FAnimation : TAnimation;
  public
    constructor Create(AOwner : TComponent); override;
    procedure ResetGrid;
    function AsInterface : IStimuli;
    procedure DoExpectedResponse;
    procedure LoadFromParameters(AParameters : TStringList);
    procedure Start;
    procedure Stop;
  end;

implementation

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
  i : integer;
begin
  FAnimation := TAnimation.Create(self);
  FAnimation.Parent := Background;

  with Grid.RandomPositions do
  begin
    for i := low(Comparisons) to high(Comparisons) do
    begin
      LItem := TDragDropableItem.Create(self);
      Comparisons[i].Item := LItem as TObject;
      LItem.Caption := 'B';
      LItem.Parent := Background;
      LItem.SetOriginalBounds(
        Comparisons[i].Left,
        Comparisons[i].Top,
        Comparisons[i].SquareSide,
        Comparisons[i].SquareSide);
      LItem.Show;

      if i = 0 then
      begin
        LItem.Caption := 'A';
        LComparison := LItem;
      end;
    end;

    for i := low(Samples) to high(Samples) do
    begin
      LItem := TDragDropableItem.Create(Self);
      Samples[i].Item := LItem as TObject;
      LItem.Parent := Background;
      LItem.SetOriginalBounds(
        Samples[i].Left,
        Samples[i].Top,
        Samples[i].SquareSide,
        Samples[i].SquareSide);
      LItem.Show;

      if i = 0 then
      begin
        LItem.Caption := 'A';
        FSample := LItem;
        FSample.Target := LComparison;
        FAnimation.Animate(FSample);
        FAnimation.Show;
      end;
    end;
  end;

  with Grid.RandomPositions do
  begin
    for i := low(Comparisons) to high(Comparisons) do
    begin
      LItem := Comparisons[i].Item as TDragDropableItem;
      LItem.SetOriginalBounds(
        Comparisons[i].Left,
        Comparisons[i].Top,
        Comparisons[i].SquareSide,
        Comparisons[i].SquareSide);
    end;

    for i := low(Samples) to high(Samples) do
    begin
      LItem := Samples[i].Item as TDragDropableItem;
      LItem.Invalidate;
      LItem.SetOriginalBounds(
        Samples[i].Left,
        Samples[i].Top,
        Samples[i].SquareSide,
        Samples[i].SquareSide);
      FAnimation.Animate(LItem);
      FAnimation.Show;
    end;
  end;
end;

procedure TDragDropStimuli.ResetGrid;
var
  i : integer;
begin
  FSample.OriginalBounds;
  FSample.Color := clWhite;
  Grid.RandomizePositions;
  Grid.RandomizeOrientations;

  with Grid.RandomPositions do begin
    for i := low(Comparisons) to high(Comparisons) do
    begin
      LItem := Comparisons[i].Item as TDragDropableItem;
      LItem.SetOriginalBounds(
        Comparisons[i].Left,
        Comparisons[i].Top,
        Comparisons[i].SquareSide,
        Comparisons[i].SquareSide);
    end;

    for i := low(Samples) to high(Samples) do
    begin
      LItem := Samples[i].Item as TDragDropableItem;
      LItem.Invalidate;
      LItem.SetOriginalBounds(
        Samples[i].Left,
        Samples[i].Top,
        Samples[i].SquareSide,
        Samples[i].SquareSide);
      FAnimation.Animate(LItem);
      FAnimation.Show;
    end;
  end;
end;

procedure TDragDropStimuli.Stop;
var
  i : integer;
begin
  with Grid.RandomPositions do begin
    for i := low(Comparisons) to high(Comparisons) do
    begin
      LItem := Comparisons[i].Item as TDragDropableItem;
      LItem.Destroy;
    end;

    for i := low(Samples) to high(Samples) do
    begin
      LItem := Samples[i].Item as TDragDropableItem;
      LItem.Destroy;
      FAnimation.Destroy;
    end;
  end;
end;

constructor TDragDropStimuli.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

end.

