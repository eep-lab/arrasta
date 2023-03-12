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
  Classes, SysUtils, Controls, ExtCtrls, fgl, Schedules, Graphics
  , Stimuli
  , Stimuli.Abstract
  , Stimuli.Image.DragDropable
  , Stimuli.Image.Animation
  , Stimuli.Image.Base
  ;

type

  TDragDropableItems = specialize TFPGList<TDragDropableItem>;
  TAnimations = specialize TFPGList<TAnimation>;

  { TDragDropStimuli }

  TDragDropStimuli = class(TStimulus, IStimuli)
  private
    FParent: TWinControl;
    FComparisons : TDragDropableItems;
    FSamples : TDragDropableItems;
    FAnimation : TAnimation;
    FDoneAnimations : TAnimations;
    procedure OtherDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure RightDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SetFocus(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetParent(AValue: TWinControl);
    function GetRandomSample : TDragDropableItem;
    procedure WrongDragDrop(Sender, Source: TObject; X, Y: Integer);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure ResetGrid;
    function AsInterface : IStimuli;
    procedure DoExpectedResponse;
    procedure LoadFromParameters(AParameters : TStringList);
    procedure Start;
    procedure Stop;
    property Parent : TWinControl read FParent write SetParent;
  end;

implementation

uses
  Math
  , Experiments.Grids
  ;

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
  for LItem in FComparisons do
    LItem.Show;
  for LItem in FSamples do
  begin
    LItem.Show;
    LItem.BringToFront;
  end;
  FAnimation.Animate(GetRandomSample);
  FAnimation.Show;
end;

procedure TDragDropStimuli.ResetGrid;
var
  i : integer;
  LItem : TDragDropableItem;
  LAnimation : TAnimation;
begin
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
      LItem.OriginalBounds;
      LItem.Draggable := True;
      LItem.Color := clWhite;
      LItem.Invalidate;
      LItem.SetOriginalBounds(
        Samples[i].Left,
        Samples[i].Top,
        Samples[i].SquareSide,
        Samples[i].SquareSide);

    end;
    FAnimation.Animate(LItem);
    FAnimation.Show;
  end;

  for LAnimation in FDoneAnimations do
    LAnimation.Free;
  FDoneAnimations.Clear;
end;

procedure TDragDropStimuli.Stop;
var
  LItem : TDragDropableItem;
begin
  for LItem in FComparisons do
    LItem.Hide;
  for LItem in FSamples do
    LItem.Hide;

  FAnimation.Stop;
  FAnimation.Hide;
end;

procedure TDragDropStimuli.SetParent(AValue: TWinControl);
var
  LItem : TDragDropableItem;
begin
  if FParent=AValue then Exit;
  FParent:=AValue;
  FAnimation.Parent := AValue;
  for LItem in FSamples do
    LItem.Parent := AValue;

  for LItem in FComparisons do
    LItem.Parent := AValue;
end;

procedure TDragDropStimuli.OtherDragDrop(Sender, Source: TObject; X, Y: Integer
  );
var
  LItem : TDragDropableItem;
begin
  LItem := Source as TDragDropableItem;
  LItem.OriginalBounds;
  LItem.Color := clWhite;
end;

procedure TDragDropStimuli.RightDragDrop(Sender, Source: TObject; X, Y: Integer
  );
var
  Sample : TDragDropableItem;
  Comparison : TDragDropableItem;
  LAnimation : TAnimation;
  FDragDropDone : Boolean = False;
begin
  Sample := Source as TDragDropableItem;
  Comparison := Sender as TDragDropableItem;

  Sample.Color := clGreen;
  Sample.Left := Comparison.Left;
  Sample.Top := Comparison.Top - Sample.Height - 10;

  LAnimation := TAnimation.Create(Parent);
  LAnimation.Parent := Parent;
  LAnimation.Join(Sample, Comparison);
  LAnimation.SendToBack;
  LAnimation.Show;
  FDoneAnimations.Add(LAnimation);

  Sample.Draggable:=False;

  for Sample in FSamples do
    if Sample.Draggable then begin
      FDragDropDone := False;
      FAnimation.Animate(Sample);
      Break;
    end else begin
      FDragDropDone := True;
    end;
  if FDragDropDone then begin
    FAnimation.Stop;
    FAnimation.Hide;
  end;
end;

procedure TDragDropStimuli.SetFocus(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LSample : TDragDropableItem;
begin
  LSample := Sender as TDragDropableItem;
  if LSample.Draggable then
    if FAnimation.Sibling <> (LSample as TLightImage) then
      FAnimation.Animate(LSample);
end;

function TDragDropStimuli.GetRandomSample: TDragDropableItem;
begin
  Result := FSamples[RandomRange(0, FSamples.Count)];
end;

procedure TDragDropStimuli.WrongDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  Sample : TDragDropableItem;
begin
  Sample := Source as TDragDropableItem;
  Sample.Color := clRed;
  FAnimation.Animate(Sample);
end;

constructor TDragDropStimuli.Create(AOwner: TComponent);
var
  LItem : TDragDropableItem;
  LComparison : TDragDropableItem;
  LComparisons : TDragDropableItems;
  i : integer;
begin
  inherited Create(AOwner);
  FSamples := TDragDropableItems.Create;
  FComparisons := TDragDropableItems.Create;
  LComparisons := TDragDropableItems.Create;

  FAnimation := TAnimation.Create(Self);
  FDoneAnimations := TAnimations.Create;

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
      FComparisons.Add(LItem);
      LComparisons.Add(LItem);
      if i = 0 then
        LItem.Caption := 'B'+(i+1).ToString;

    end;

    for i := low(Samples) to high(Samples) do
    begin
      LItem := TDragDropableItem.Create(Self);
      LItem.OnMouseDown := @SetFocus;
      LItem.OnRightDragDrop:=@RightDragDrop;
      LItem.OnWrongDragDrop:=@WrongDragDrop;
      LItem.OnOtherDragDrop:=@OtherDragDrop;
      LItem.SetOriginalBounds(
        Samples[i].Left,
        Samples[i].Top,
        Samples[i].SquareSide,
        Samples[i].SquareSide);
      LItem.Caption := 'A'+(i+1).ToString;
      case i of
        0 : // do nothing;

        else begin
          // making sure that we have always
          // the right comparison as the first one
          // inside the sample targets
          LComparisons.Exchange(0, i);
        end;
      end;
      for LComparison in LComparisons do
        LItem.AddTarget(LComparison);

      Samples[i].Item := LItem as TObject;
      FSamples.Add(LItem);
    end;
  end;
  LComparisons.Free;
end;

destructor TDragDropStimuli.Destroy;
begin
  FDoneAnimations.Free;
  FSamples.Free;
  FComparisons.Free;
  inherited Destroy;
end;

end.
