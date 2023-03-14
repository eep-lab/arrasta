unit Stimuli.Helpers.DragDropChannel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Canvas.Helpers;

type

  { TDragDropChannel }

  TDragDropChannel = class(TObject)
  private
    FDragArea     : TPoints;
    FBresenhamLine : TPoints;
    FIndex        : integer;
  public
    constructor Create; overload;
    constructor Create(AOrigin, ADestin : TRect); overload;
    destructor Destroy; override;
    procedure Update(AOrigin, ADestin : TRect);
    function IntersectWith(ARect : TRect) : Boolean;
    function NextPoint : TPoint;
    property DragArea : TPoints read FBresenhamLine;
    property Line : TPoints read FBresenhamLine;
  end;


var
  DragDropChannel : TDragDropChannel;

implementation

{ TDragDropChannel }

constructor TDragDropChannel.Create;
begin
  FIndex := 0;
end;

constructor TDragDropChannel.Create(AOrigin, ADestin: TRect);
begin
  Update(AOrigin, ADestin);
end;

destructor TDragDropChannel.Destroy;
begin
  SetLength(FBresenhamLine, 0);
  inherited Destroy;
end;

procedure TDragDropChannel.Update(AOrigin, ADestin: TRect);
begin
  FIndex := 0;
  FBresenhamLine := BresenhamLine(
    AOrigin.Left,
    ADestin.Left,
    AOrigin.Top,
    ADestin.Top);
end;

function TDragDropChannel.IntersectWith(ARect: TRect): Boolean;
begin

end;

function TDragDropChannel.NextPoint: TPoint;
var
  LLength : integer;
begin
  LLength := Length(FBresenhamLine);
  if LLength > 0 then
    Result := FBresenhamLine[FIndex];
  if FIndex < LLength-1 then
    Inc(FIndex, 5);

  if FIndex >= LLength then
    FIndex := LLength-1;
end;

//function TDragDropChannel.NearMe(X, Y: Integer): Boolean;
//const
//  Tolerance : integer = 10;
//var
//  Point : TPoint;
//begin
//  Result := False;
//  if Length(FLine) < 1 then Exit;
//  for Point in FLine do
//    Result := Result or
//      ((X >= (Point.X-Tolerance)) and
//       (X <= (Point.X+Tolerance))
//       {(Y >= (Point.Y-Tolerance)) and
//       (Y <= (Point.Y+Tolerance))});
//end;

end.

