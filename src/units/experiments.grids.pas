{todo: Transformar em classe}

unit Experiments.Grids;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF DEBUG}{$IFDEF WINDOWS}
  Windows.Console,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, fgl;

type
  TGridStyle =
    (gtCircle, gtSquare, gtDistributed);

  TGridOrientation =
    (goNone, goLeftToRight, goRightToLeft, goTopToBottom, goBottomToTop);

  TCell = array [0..1] of Integer;

  TGridList = specialize TFPGList<Integer>;
  TGridItem = record
    Index : integer;
    Top : integer;
    Left : integer;
    SquareSide : integer;
    Item : TObject;
  end;

  TGridItems = array of TGridItem;
  TMatrix = array of array of TGridItem;

  TRandomPositions = record
    Samples: TGridItems;
    Comparisons: TGridItems;
  end;

  { TGrid }

  TGrid = class
    private
      FSeed : integer;
      FCellsCount: integer;
      FCellsSize: real;
      FComparisonsCount: integer;
      FGrid : TMatrix;
      FGridStyle : TGridStyle;
      FGridOrientation : TGridOrientation;
      FRandomPositions : TRandomPositions;
      FSamplesCount: integer;
      procedure SetCellsCount(AValue: integer);
      procedure SetCellsSize(AValue: real);
      procedure SetComparisonsCount(AValue: integer);
      procedure SetGridStyle(AGridStyle: TGridStyle);
      procedure RandomizeGridList(AGridList: TGridList);
      function InvalidateGridList(IsSample: Boolean=true): TGridList;
      function DispersionStyle : Boolean;
      procedure SetSamplesCount(AValue: integer);
      procedure CreatePositions;
    public
      constructor Create(ASeed : integer);
      destructor Destroy; override;
      property GridStyle : TGridStyle read FGridStyle write SetGridStyle;
      property CellsCount : integer read FCellsCount write SetCellsCount;
      property CellsSize : real read FCellsSize write SetCellsSize;
      property RandomPositions : TRandomPositions read FRandomPositions;
      property SamplesCount : integer read FSamplesCount write SetSamplesCount;
      property ComparisonsCount : integer read FComparisonsCount write SetComparisonsCount;
      property Seed : integer read FSeed write FSeed;
      {Cria seleção randômica de modelos e comparações em posições diferentes no AGrid}
      procedure RandomizePositions;
      function RectFromPosition(APosition: integer) : TRect;
  end;

var
  ScreenInCentimeters : real = 39.624;
  Grid : TGrid;

implementation

uses Math, Forms, GUI.Helpers.Grids;

{
  GetPositionFromSegment returns Left or Top position based on:
    ASegment = Width or Height from which get the Left or Top position.
    ASteps = Desired number os columns or rows.
    AStep = Target column or row.
    AStimulusSide = Width or height of the target stimulus.
    AInterStimulusSpace = Desired horizontal or vertical space from one stimulus to another.
}
function GetPositionFromSegment(ASegment, AStep, ASteps,
  AStimulusSide, AInterStimulusSpace : integer):integer;
var
  LSize : integer;
begin
  LSize := AStimulusSide + AInterStimulusSpace;
  Result := Round((LSize*AStep)-((LSize*ASteps)/2)+((ASegment+AInterStimulusSpace)/2));
end;

function CmToScreenPixels(AMeasure : real) : integer;
begin
  Result := Round(AMeasure*(Screen.Width/ScreenInCentimeters));
end;

{Cria grade quadrada como uma matriz AN x AN. Quando ADistribute = true, a
distância horizontal e vertical entre os estímulos é diferente, e quando false
é igual}
function GetCentralGrid(AN: integer; ASquareSide: real;
  ADistribute: Boolean): TMatrix;
var
  LIndex      : integer = 0;
  //LSegment    : integer = 0;
  //LSteps      : integer = 0;
  //LStep       : integer = 0;
  LSquareSide : integer = 0;
  LInterSpaceW : integer = 0;
  LInterSpaceH : integer = 0;
  j : integer = 0;
  i : integer = 0;
begin
  Result := Default(TMatrix);
  SetLength(Result, AN, AN);
  LSquareSide := CmToScreenPixels(ASquareSide);
  if ADistribute then begin
    LInterSpaceW := (Screen.Width -  (LSquareSide * AN)) div AN;
    LInterSpaceH := (Screen.Height - (LSquareSide * AN)) div AN;
  end else begin
    if Screen.Width > Screen.Height then begin
      LInterSpaceH := (Screen.Height - (LSquareSide * AN)) div AN;
      LInterSpaceW := LInterSpaceH;
    end else begin
      LInterSpaceW := (Screen.Width -  (LSquareSide * AN)) div AN;
      LInterSpaceH := LInterSpaceW;
    end;
  end;
  for j := Low(Result) to High(Result) do begin
    for i := Low(Result[j]) to High(Result[j]) do begin
      with Result[j][i] do begin
        Index := LIndex;
        Top := GetPositionFromSegment(
          Screen.Height, j, AN, LSquareSide, LInterSpaceH);
        Left := GetPositionFromSegment(
          Screen.Width, i, AN, LSquareSide, LInterSpaceW);
        SquareSide := LSquareSide;
      end;
      Inc(LIndex);
    end;
  end;
end;

{Cria grade circular considerando j como modelo central e i como comparações em
torno de um diâmetro. AN = número de estímulos i; ASquareSide = lado do quadrado
dos estímulos}
function GetCircularCentralGrid(AN: integer; ASquareSide: real): TMatrix;
var
  LIndex      : integer = 0;
  //LSegment    : integer = 0;
  //LSteps      : integer = 0;
  //LStep       : integer = 0;
  LSquareSide : integer = 0;
  LDegree : integer = 0;
  LDegreeI : integer = 0;
  LPoint : TPoint;
  LRect  : TRect;
  j : integer = 0;
  i : integer = 0;
const
  BaseDegree : integer = 360;
begin
  Result := Default(TMatrix);
  SetLength(Result, 2);
  SetLength(Result[0], AN);
  SetLength(Result[1], 1);
  LSquareSide := CmToScreenPixels(ASquareSide);
  LDegree := BaseDegree;
  LDegreeI := BaseDegree div AN;
  LRect := GetCentralRect(Screen.Width, Screen.Height, LSquareSide div 2);
  for j := Low(Result) to High(Result) do begin
    for i := Low(Result[j]) to High(Result[j]) do begin
      with Result[j][i] do begin
        case j of
          0:  begin
            Index := LIndex;
            LPoint := GetPointFromAngle(LDegree, LRect);
            Top := LPoint.Y - (LSquareSide div 2);
            Left := LPoint.X - (LSquareSide div 2);
            SquareSide := LSquareSide;
            Inc(LDegree, LDegreeI);
          end;

          1: begin
            Index := LIndex;
            Top := (Screen.Height div 2) - (LSquareSide div 2);
            Left := (Screen.Width div 2) - (LSquareSide div 2);
            SquareSide := LSquareSide;
          end;
        end;
      end;
      Inc(LIndex);
    end;
  end;
end;


{
  3x3
  0..1..2
  3..4..5
  6..7..8
}
function IntToCell(AN: Integer) : TCell;
begin
  case AN of
    0 : Result := [0, 0];
    1 : Result := [0, 1];
    2 : Result := [0, 2];
    3 : Result := [1, 0];
    4 : Result := [1, 1];
    5 : Result := [1, 2];
    6 : Result := [2, 0];
    7 : Result := [2, 1];
    8 : Result := [2, 2];
  end;
end;

{ TGrid }

procedure TGrid.SetGridStyle(AGridStyle: TGridStyle);
begin
  if FGridStyle = AGridStyle then Exit;
  FGridStyle := AGridStyle;
  case AGridStyle of
    gtCircle : FGrid := GetCircularCentralGrid(FSeed, FCellsSize);
    gtSquare : FGrid := GetCentralGrid(FSeed, FCellsSize, DispersionStyle);
    gtDistributed: FGrid := GetCentralGrid(FSeed, FCellsSize, DispersionStyle);
  end;
end;

procedure TGrid.RandomizeGridList(AGridList: TGridList);
var
  i : integer;
begin
  for i := AGridList.Count - 1 downto 0 do
    AGridList.Exchange(i, RandomRange(0, i + 1));
  {$IFDEF DEBUG}
  //for i in FGridList do WriteLn(i);
  {$ENDIF}
end;

function TGrid.InvalidateGridList(IsSample: Boolean): TGridList;
var
  i : integer;
begin
  Result:= TGridList.Create;
  {
    3x3
    0..1..2
    3..4..5
    6..7..8
  }
  case FGridOrientation of
      goNone: begin
        for i := 0 to FCellsCount - 1 do Result.Add(i);
      end;
      goLeftToRight: begin
        if IsSample then begin
          Result.Add(0);
          Result.Add(3);
          Result.Add(6);
        end else begin
          Result.Add(2);
          Result.Add(5);
          Result.Add(8);
        end;
      end;
      goRightToLeft: begin
        if IsSample then begin
          Result.Add(2);
          Result.Add(5);
          Result.Add(8);
        end else begin
          Result.Add(0);
          Result.Add(3);
          Result.Add(6);
        end;
      end;
      goBottomToTop: begin
        if IsSample then begin
          Result.Add(6);
          Result.Add(7);
          Result.Add(8);
        end else begin
          Result.Add(0);
          Result.Add(1);
          Result.Add(2);
        end;
      end;
      goTopToBottom: begin
        if IsSample then begin
          Result.Add(0);
          Result.Add(1);
          Result.Add(2);
        end else begin
          Result.Add(6);
          Result.Add(7);
          Result.Add(8);
        end;
      end;
    end;
end;

function TGrid.DispersionStyle: Boolean;
begin
  case FGridStyle of
    gtCircle : Result := False; // it is ignored
    gtSquare : Result := False;
    gtDistributed : Result := True;
  end;
end;

procedure TGrid.SetSamplesCount(AValue: integer);
begin
  if FSamplesCount=AValue then Exit;
  FSamplesCount:=AValue;
end;

procedure TGrid.CreatePositions;
var
  i : integer;
begin
  with FRandomPositions do begin
      SetLength(Samples, SamplesCount);
      SetLength(Comparisons, ComparisonsCount);

      for i := low(Samples) to high(Samples) do
          Samples[i].Index := -1;
      for i := low(Comparisons) to high(Comparisons) do
          Comparisons[i].Index := -1;
  end;
end;

procedure TGrid.RandomizePositions;
var
  Cell : TCell;
  i : integer;
  LGridList : TGridList;

  {Change positions only}
  procedure SecureCopy(var A: TGridItem; B : TGridItem);
  begin
    A.Index := B.Index;
    A.Top := B.Top;
    A.Left := B.Left;
    A.SquareSide := B.SquareSide;
    // A.Item := B.Item; // do not override Item Pointer
  end;

begin
  with FRandomPositions do begin
    case FGridOrientation of
      goNone: begin
        LGridList:= InvalidateGridList;
        RandomizeGridList(LGridList);
        for i := low(Samples) to high(Samples) do
        begin
          Cell := IntToCell(LGridList.First);
          SecureCopy(Samples[i], FGrid[Cell[0], Cell[1]]);
          LGridList.Delete(0);
        end;

        for i := low(Comparisons) to high(Comparisons) do
        begin
          Cell := IntToCell(LGridList.First);
          SecureCopy(Comparisons[i], FGrid[Cell[0], Cell[1]]);
          LGridList.Delete(0);
        end;
        LGridList.Free;
      end;
      else begin
        LGridList:= InvalidateGridList(true);
        RandomizeGridList(LGridList);
        for i := low(Samples) to high(Samples) do
        begin
          Cell := IntToCell(LGridList.First);
          SecureCopy(Samples[i], FGrid[Cell[0], Cell[1]]);
          LGridList.Delete(0);
        end;
        LGridList.Free;

        LGridList:= InvalidateGridList(false);
        RandomizeGridList(LGridList);
        for i := low(Comparisons) to high(Comparisons) do
        begin
          Cell := IntToCell(LGridList.First);
          SecureCopy(Comparisons[i], FGrid[Cell[0], Cell[1]]);
          LGridList.Delete(0);
        end;
        LGridList.Free;
      end;
    end;
  end;
end;

function TGrid.RectFromPosition(APosition: integer): TRect;
var
  j, i: Integer;
begin
  for j := Low(FGrid) to High(FGrid) do begin
    for i := Low(FGrid[j]) to High(FGrid[j]) do begin
      with FGrid[j][i] do begin
        if Index = APosition then begin
          Result := Rect(Left, Top, Left+SquareSide, Top+SquareSide);
        end;
      end;
    end;
  end;
end;

procedure TGrid.SetCellsCount(AValue: integer);
begin
  if FCellsCount=AValue then Exit;
  FCellsCount:=AValue;
end;

procedure TGrid.SetCellsSize(AValue: real);
begin
  if FCellsSize=AValue then Exit;
  FCellsSize:=AValue;
end;

procedure TGrid.SetComparisonsCount(AValue: integer);
begin
  if FComparisonsCount=AValue then Exit;
  FComparisonsCount:=AValue;
end;

constructor TGrid.Create(ASeed: integer);
begin
  FSeed := ASeed;
  FCellsCount:=ASeed*ASeed;
  FCellsSize := 3.0;
  FGridStyle := gtSquare;
  FSamplesCount := 1;
  FComparisonsCount := 3;
  FGridOrientation:= goRightToLeft;
  FGrid := GetCentralGrid(FSeed, FCellsSize, DispersionStyle);

  CreatePositions;
  RandomizePositions;
end;

destructor TGrid.Destroy;
begin

end;

initialization
  Grid := TGrid.Create(3);

finalization
  Grid.Free;


end.

