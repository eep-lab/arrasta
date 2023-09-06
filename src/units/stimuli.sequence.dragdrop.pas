{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Sequence.DragDrop;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, fgl, Graphics
  , Stimuli
  , Stimuli.Abstract
  , Stimuli.Image.DragDropable
  , Stimuli.Image.Animation
  , Stimuli.Image.Base
  , Loggers.Reports
  , Experiments.Grids
  ;

type

  TDragDropableItems = specialize TFPGList<TDragDropableItem>;
  TAnimations = specialize TFPGList<TAnimation>;

  { TDragDropStimuli }

  TDragDropStimuli = class(TStimulus, IStimuli)
  private
    FLogEvent: TDataProcedure;
    FOnDragDropDone: TNotifyEvent;
    FOnOtherDragDrop: TDragDropEvent;
    FOnRightDragDrop: TDragDropEvent;
    FOnWrongDragDrop: TDragDropEvent;
    FParent: TWinControl;
    FComparisons : TDragDropableItems;
    FSamples : TDragDropableItems;
    FAnimation : TAnimation;
    FDoneAnimations : TAnimations;
    FGridOrientation : TGridOrientation;
    FTeste: Integer;
    procedure OtherDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure RightDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure SetFocus(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetLogEvent(AValue: TDataProcedure);
    procedure SetOnDragDropDone(AValue: TNotifyEvent);
    procedure SetOnOtherDragDrop(AValue: TDragDropEvent);
    procedure SetOnRightDragDrop(AValue: TDragDropEvent);
    procedure SetOnWrongDragDrop(AValue: TDragDropEvent);
    procedure SetParent(AValue: TWinControl);
    function GetRandomSample : TDragDropableItem;
    procedure FreeGridItems;
    procedure WrongDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure Animate(ASample : TDragDropableItem);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure ResetGrid;
    function AsInterface : IStimuli;
    procedure DoExpectedResponse;
    procedure LoadFromParameters(AParameters : TStringList);
    procedure Start;
    procedure Stop;
    procedure NewGridItems(ASamples, AComparisons : integer;
                            AGridOrientation : TGridOrientation);
    function GetTop(AItem : TDragDropableItem) : Integer;
    property Parent : TWinControl read FParent write SetParent;
    property LogEvent : TDataProcedure read FLogEvent write SetLogEvent;
    property OnDragDropDone : TNotifyEvent read FOnDragDropDone write SetOnDragDropDone;
    property OnRightDragDrop : TDragDropEvent read FOnRightDragDrop write SetOnRightDragDrop;
    property OnWrongDragDrop : TDragDropEvent read FOnWrongDragDrop write SetOnWrongDragDrop;
    property OnOtherDragDrop : TDragDropEvent read FOnOtherDragDrop write SetOnOtherDragDrop;
    property Teste : Integer read FTeste write FTeste;
  end;

implementation

uses
  Math
  , StrUtils
  , Stimuli.Helpers.DragDropChannel
  , Session.Trial.HelpSeries.DragDrop
  , Consequences.DragDrop
  , Constants.DragDrop
  ;

var Cursor : integer;

{ TDragDropStimuli }

function TDragDropStimuli.AsInterface : IStimuli;
begin
  Result := Self;
end;

procedure TDragDropStimuli.DoExpectedResponse;
begin

end;

procedure TDragDropStimuli.LoadFromParameters(AParameters: TStringList);
var
  S1           : string;
  DragMouseMoveMode : TDragMouseMoveMode;
  SampleLetter : string;
  ComparLetter : string;
  LSamples      : integer;
  LComparisons  : integer;
  LItem : TDragDropableItem;
  i: Integer;

  function DragDropToGridOrientation(
      DragDropOrientation : TDragDropOrientation) : TGridOrientation;
  begin
    case DragDropOrientation of
      goTopToBottom : Result := TGridOrientation.goTopToBottom;
      goBottomToTop : Result := TGridOrientation.goBottomToTop;
      goLeftToRight : Result := TGridOrientation.goLeftToRight;
      goRightToLeft : Result := TGridOrientation.goRightToLeft;
      goRandom : Result:= Grid.GetRandomGridOrientation;
    end;
  end;

begin
  if not Assigned(FParent) then
    raise Exception.Create('You must assigned a parent before loading.');
  Cursor := StrToIntDef(AParameters.Values['Cursor'], -1);

  with DragDropKeys do begin
    ChannelDragMouseMoveFactor :=
      AParameters.Values[DragMoveFactor].ToInteger;
    DragMouseMoveMode :=
      AParameters.Values[SamplesDragMode].ToDragMouseMoveMode;

    S1 := AParameters.Values[Relation];
    SampleLetter := ExtractDelimited(1,S1,['-']);
    ComparLetter := ExtractDelimited(2,S1,['-']);

    LSamples := AParameters.Values[Samples].ToInteger;
    LComparisons := AParameters.Values[Comparisons].ToInteger;
    FGridOrientation := DragDropToGridOrientation(
      AParameters.Values[DragDropOrientation].ToDragDropOrientation);
  end;


  NewGridItems(LSamples, LComparisons, FGridOrientation);
  with Grid.RandomPositions do begin
    for i := low(Comparisons) to high(Comparisons) do
    begin
      LItem := Comparisons[i].Item as TDragDropableItem;
      LItem.Cursor := Cursor;
      LItem.LoadFromFile(ComparLetter+(i+1).ToString);
      LItem.Parent := Parent;
      WriteLn('- B -');
      WriteLn('Top: ', LItem.Top);
      WriteLn('Left: ', LItem.Left);
      Teste := LItem.Top;
      LItem.Invalidate;
    end;

    for i := low(Samples) to high(Samples) do
    begin
      LItem := Samples[i].Item as TDragDropableItem;
      LItem.Cursor := Cursor;
      LItem.LoadFromFile(SampleLetter+(i+1).ToString);
      LItem.DragMouseMoveMode:=DragMouseMoveMode;
      LItem.Parent := Parent;
      with DragDropKeys do begin
        LItem.MoveToPoint(AParameters.Values[Distance].ToInteger);
      end;
      LItem.SetOriginalBounds(LItem.Left, LItem.Top, LItem.Width, LItem.Height);
      WriteLn('- A -');
      WriteLn('Top: ', LItem.Top);
      WriteLn('Left: ', LItem.Left);
      LItem.Invalidate;
    end;
  end;
end;

procedure TDragDropStimuli.Start;
var
  LItem : TDragDropableItem;
begin
  for LItem in FComparisons do
    LItem.Show;

  Animate(GetRandomSample);
  FAnimation.BringToFront;

  for LItem in FSamples do
  begin
    LItem.Show;
    LItem.BringToFront;
  end;
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
  end;

  for LAnimation in FDoneAnimations do
    LAnimation.Free;
  FDoneAnimations.Clear;
end;

procedure TDragDropStimuli.Stop;
var
  LItem : TDragDropableItem;
  LAnimation: TAnimation;
begin
  for LItem in FComparisons do
    LItem.Hide;
  for LItem in FSamples do
    LItem.Hide;

  FAnimation.Stop;
  FAnimation.Hide;
  for LAnimation in FDoneAnimations do
    LAnimation.Hide;
end;

procedure TDragDropStimuli.NewGridItems(ASamples, AComparisons: integer;
                                         AGridOrientation : TGridOrientation);
var
  LItem : TDragDropableItem;
  LComparison : TDragDropableItem;
  LComparisons : TDragDropableItems;
  i : integer;
begin
  Grid.UpdatePositions(ASamples, AComparisons, AGridOrientation);
  with Grid.RandomPositions do begin
    LComparisons := TDragDropableItems.Create;
    for i := low(Comparisons) to high(Comparisons) do
    begin
      LItem := TDragDropableItem.Create(self);
      LItem.SetOriginalBounds(
        Comparisons[i].Left,
        Comparisons[i].Top,
        Comparisons[i].SquareSide,
        Comparisons[i].SquareSide);

      Comparisons[i].Item := LItem as TObject;
      FComparisons.Add(LItem);
      LComparisons.Add(LItem);
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
      case i of
        0 : // do nothing;

        else begin                     // making sure that we have always
          LComparisons.Exchange(0, i); // the right comparison as the first one
        end;                           // inside the sample targets
      end;
      for LComparison in LComparisons do
        LItem.AddTarget(LComparison);

      Samples[i].Item := LItem as TObject;
      FSamples.Add(LItem);
    end;
    LComparisons.Free;
  end;
end;

function TDragDropStimuli.GetTop(AItem: TDragDropableItem): Integer;
begin
     Result := AItem.Top;
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

procedure TDragDropStimuli.OtherDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  LItem : TDragDropableItem;
begin
  LItem := Source as TDragDropableItem;
  LItem.Color := clWhite;
  if Assigned(OnOtherDragDrop) then
    OnOtherDragDrop(Sender, Source, X, Y);
end;

procedure TDragDropStimuli.RightDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  Sample : TDragDropableItem;
  Comparison : TDragDropableItem;
  LAnimation : TAnimation;
  FDragDropDone : Boolean = False;
  S1 : string;
begin
  SoundRight.Play;
  Sample := Source as TDragDropableItem;
  Comparison := Sender as TDragDropableItem;

  Sample.Color := clGreen;
  case FGridOrientation of
    TGridOrientation.goTopToBottom : begin
      Sample.Left := Comparison.Left;
      Sample.Top := Comparison.Top - Sample.Height - 10;
    end;
    TGridOrientation.goBottomToTop : begin
      Sample.Left := Comparison.Left;
      Sample.Top := Comparison.Top + Sample.Height + 10;
    end;
    TGridOrientation.goLeftToRight : begin
      Sample.Left := Comparison.Left - Sample.Width - 10;
      Sample.Top := Comparison.Top;
    end;
    TGridOrientation.goRightToLeft : begin
      Sample.Left := Comparison.Left + Sample.Width + 10;
      Sample.Top := Comparison.Top;
    end;
    else
    begin
      WriteStr(S1, FGridOrientation);
      raise Exception.Create(
        'TDragDropStimuli.RightDragDrop: ' + S1);
    end;
  end;

  LAnimation := TAnimation.Create(Self);
  LAnimation.Cursor:=Cursor;
  LAnimation.Parent := Parent;
  LAnimation.Join(Sample, Comparison, FGridOrientation);
  LAnimation.SendToBack;
  LAnimation.Show;
  FDoneAnimations.Add(LAnimation);

  Sample.Draggable:=False;

  for Sample in FSamples do
    if Sample.Draggable then begin
      FDragDropDone := False;
      Animate(Sample);
      Break;
    end else begin
      Sample.EdgeColor:=clInactiveCaption;
      FDragDropDone := True;
    end;

  if Assigned(OnRightDragDrop) then
    OnRightDragDrop(Sender, Source, X, Y);

  if FDragDropDone then begin
    FAnimation.Stop;
    FAnimation.Hide;
    if Assigned(OnDragDropDone) then
      OnDragDropDone(Self);
  end;
end;

procedure TDragDropStimuli.SetFocus(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  LSample : TDragDropableItem;
begin
  LSample := Sender as TDragDropableItem;
  if Assigned(OnResponse) then
    OnResponse(LSample);
  if LSample.Draggable then begin
    if FAnimation.Sibling <> (LSample as TLightImage) then
    begin
      if Assigned(FAnimation.Sibling) then
        FAnimation.Sibling.EdgeColor:=clInactiveCaption;
      Animate(LSample);
    end;
  end;
end;

procedure TDragDropStimuli.SetLogEvent(AValue: TDataProcedure);
begin
  if FLogEvent=AValue then Exit;
  FLogEvent:=AValue;
end;

procedure TDragDropStimuli.SetOnDragDropDone(AValue: TNotifyEvent);
begin
  if FOnDragDropDone=AValue then Exit;
  FOnDragDropDone:=AValue;
end;

procedure TDragDropStimuli.SetOnOtherDragDrop(AValue: TDragDropEvent);
begin
  if FOnOtherDragDrop=AValue then Exit;
  FOnOtherDragDrop:=AValue;
end;

procedure TDragDropStimuli.SetOnRightDragDrop(AValue: TDragDropEvent);
begin
  if FOnRightDragDrop=AValue then Exit;
  FOnRightDragDrop:=AValue;
end;

procedure TDragDropStimuli.SetOnWrongDragDrop(AValue: TDragDropEvent);
begin
  if FOnWrongDragDrop=AValue then Exit;
  FOnWrongDragDrop:=AValue;
end;

function TDragDropStimuli.GetRandomSample: TDragDropableItem;
begin
  Result := FSamples[RandomRange(0, FSamples.Count)];
end;

procedure TDragDropStimuli.FreeGridItems;
var
  i: Integer;
begin
  with Grid.RandomPositions do begin
    for i := low(Comparisons) to high(Comparisons) do
      Comparisons[i].Item.Free;

    for i := low(Samples) to high(Samples) do
      Samples[i].Item.Free
  end;
end;

procedure TDragDropStimuli.WrongDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  Sample : TDragDropableItem;
begin
  SoundWrong.Play;
  Sample := Source as TDragDropableItem;
  Sample.OriginalBounds;
  //FAnimation.Animate(Sample);
  if Assigned(OnWrongDragDrop) then
    OnWrongDragDrop(Sender, Source, X, Y);
end;

procedure TDragDropStimuli.Animate(ASample: TDragDropableItem);
begin
  FAnimation.Cursor := Cursor;
  FAnimation.BringToFront;
  ASample.BringToFront;
  ASample.UpdateDragMouseMoveMode;
  FAnimation.Animate(ASample);
  FAnimation.Show;
  Parent.Invalidate;
end;

constructor TDragDropStimuli.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DragDropChannel := TDragDropChannel.Create;
  FSamples := TDragDropableItems.Create;
  FComparisons := TDragDropableItems.Create;
  FAnimation := TAnimation.Create(Self);
  FDoneAnimations := TAnimations.Create;
  FParent := nil;
end;

destructor TDragDropStimuli.Destroy;
begin
  FDoneAnimations.Free;
  FSamples.Free;
  FComparisons.Free;
  inherited Destroy;
end;

end.
