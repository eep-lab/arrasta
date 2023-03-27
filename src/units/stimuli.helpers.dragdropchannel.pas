unit Stimuli.Helpers.DragDropChannel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Canvas.Helpers;

type

  { TDragDropChannel }

  TDragDropChannel = class(TObject)
  private
    FParent : TCanvas;
    FDragArea     : TPoints;
    FBresenhamLine : TPoints;
    FIndex        : integer;
  public
    constructor Create; overload;
    constructor Create(AOrigin, ADestin : TRect); overload;
    destructor Destroy; override;
    procedure Paint; virtual;
    procedure Update(AOrigin, ADestin : TRect);
    function NextPoint : TPoint;
    property Line : TPoints read FBresenhamLine;
    property Canvas : TCanvas read FParent write FParent;
  end;


var
  DragDropChannel : TDragDropChannel;
  ChannelDragMouseMoveFactor : integer;

implementation

uses FPImage;

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

procedure TDragDropChannel.Paint;
var
  Point : TPoint;
  LColor: TFPColor;
begin
  if Length(Line) > 0 then begin
    LColor.Blue  := 0;
    LColor.Green := 0;
    LColor.Red   := 255;
    LColor.Alpha := 255;
    for Point in Line do
      Canvas.DrawPixel(Point.X, Point.Y, LColor);
  end;
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

function TDragDropChannel.NextPoint: TPoint;
var
  LLength : integer;
begin
  LLength := Length(FBresenhamLine);
  if LLength > 0 then
    Result := FBresenhamLine[FIndex];
  if FIndex < LLength-1 then
    Inc(FIndex, ChannelDragMouseMoveFactor);

  if FIndex >= LLength then
    FIndex := LLength-1;
end;

end.

