{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
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
    FBresenhamLine : TPoints;
    FIndex        : integer;
  public
    constructor Create; overload;
    constructor Create(AOrigin, ADestin : TRect); overload;
    destructor Destroy; override;
    procedure Paint; virtual;
    procedure Update(AOrigin, ADestin : TRect);
    function NextPoint : TPoint;
    function GetPoint(AValue : real) : TPoint;
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

function TDragDropChannel.GetPoint(AValue: real): TPoint;
var
  LLength: real;
  LLineIndex : real;
begin
  LLength := Length(FBresenhamLine);
  LLineIndex := AValue * LLength / 100;
  Result := FBresenhamLine[round(LLineIndex)];
end;

end.

