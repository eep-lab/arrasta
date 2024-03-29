{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Image.Animation;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, Graphics, Controls, ExtCtrls, Stimuli.Image.Base,
     Experiments.Grids;

type

  TAnimationStyle = (asCircle, asSquare, asGif);

  { TAnimation }

  TAnimation = class(TLightImage)
  private
    FAcum: double;
    //Quanto menor, a velocidade é menor, e vice-versa
    //Valores ótimos: 0.05 < x < 0.2
    FStep: double;
    FHeight: integer;
    //FWidth: integer;
    FTimer: TTimer;
    FShow: boolean;
    FGrowing: boolean;
    FSibling : TLightImage;
    FAnimationStyle: TAnimationStyle;
    function easeInOutQuad(t: double): double;
    procedure OnTimer(Sender: TObject);
    procedure OnStopTimer(Sender: TObject);
    procedure OnStartTimer(Sender: TObject);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Animate(ASibling : TLightImage);
    procedure Join(ASample, AComparison : TLightImage;
      AGridOrientation : TGridOrientation);
    procedure Stop;
    property Sibling : TLightImage read FSibling;
  end;

implementation

uses Forms, LazFileUtils, Dialogs
     , Types
     ;

{ TAnimation }

procedure TAnimation.Paint;
  procedure PaintSquare(Color: TColor);
  begin
    with Canvas do
      begin
        Pen.Width := FPenWidth;
        Pen.Color := Color;
        Brush.Style:= bsClear;
        Rectangle(ClientRect);
        TextRect(ClientRect,
          (((ClientRect.Right-ClientRect.Left) div 2) - (TextWidth(Caption)div 2)),
          (((ClientRect.Bottom-ClientRect.Top) div 2) - (TextHeight(Caption)div 2)),
          Caption);
      end;
  end;

  procedure PaintCircle(Color : TColor);
  var
    LCenter : TPoint;
    LSize : integer;
  begin
    LSize := (Width div 2) - 10;
    LCenter.X := ClientRect.Right - (Width div 2);
    LCenter.Y := ClientRect.Bottom - (Width div 2);

    //Canvas.TextStyle := LTextStyle;
    with Canvas do
      begin
        Pen.Width := FPenWidth;
        Pen.Color := Color;
        //Brush.Color:= Color;
        with LCenter do
          Ellipse(X - LSize, Y - LSize, X + LSize, Y + LSize);
      end;
  end;

begin
  case FAnimationStyle of
    asSquare: PaintSquare(Color);
    asCircle: PaintCircle(Color);
    asGif: ;
  end;
end;

procedure TAnimation.OnStartTimer(Sender: TObject);
begin
  FShow := true;
  FAcum := 0;
  Color := clRed
end;

procedure TAnimation.OnStopTimer(Sender: TObject);
begin
  Color := clDkGray;
end;

procedure TAnimation.OnTimer(Sender: TObject);
var
  temp: double;
begin
  FAcum := FAcum + FStep;
  if FStep > 1 then
    FStep := 1;
  temp := easeInOutQuad(FAcum);
  if FGrowing then
  begin
    Height := round(FHeight * temp);
    Width := Height;
    if Height >= FHeight then
    begin
      Height := FHeight;
      Width := Height;
      FGrowing := false;
      FAcum:= 0;
    end;
  end else begin
    temp := FHeight - round(FHeight * temp);
    if temp <= Constraints.MinHeight then
    begin
      Height := Constraints.MinHeight;
      Width := Constraints.MinWidth;
      FGrowing := true;
      FAcum:= 0;
    end else begin
      Height := trunc(temp);
      Width := Height;
    end;
  end;
end;

function TAnimation.easeInOutQuad(t: double): double;
begin
  if t < 0.5 then
    Result := 2 * t * t
  else
    Result := -1 + (4 - 2 * t) * t;
end;

procedure TAnimation.Animate(ASibling : TLightImage);
var
  R: TRect;
begin
  ASibling.EdgeColor := clNone;
  FSibling := ASibling;
  //Kind := ikAnimate;
  R := ASibling.ClientRect;
  InflateRect(R, 10, 10);
  Self.SetOriginalBounds(ASibling.Left, ASibling.Top, R.Width, R.Height);
  Anchors := [akLeft, akTop];
  AnchorSideLeft.Control := ASibling;
  AnchorSideLeft.Side := asrCenter;
  AnchorSideTop.Control := ASibling;
  AnchorSideTop.Side := asrCenter;
  FTimer.Enabled:= true;
  Constraints.MinHeight := R.Height;
  Constraints.MinWidth := R.Width;
end;

procedure TAnimation.Join(ASample, AComparison: TLightImage;
  AGridOrientation : TGridOrientation);
begin
  Stop;
  AnchorSideTop.Control := nil;
  ASample.EdgeColor:=clInactiveCaption;
  case AGridOrientation of
    goTopToBottom : begin
      Top := ASample.Top -10;
      Left := ASample.Left -15;
      Height := ASample.Height + AComparison.Height + 30;
      Width := ASample.Width + 30;
    end;
    goBottomToTop : begin
      Top := AComparison.Top -10;
      Left := AComparison.Left -15;
      Height := AComparison.Height + ASample.Height + 30;
      Width := AComparison.Width + 30;
    end;
    goLeftToRight : begin
      Top := ASample.Top -15;
      Left := ASample.Left -10;
      Width := ASample.Width + AComparison.Width + 30;
      Height := ASample.Height + 30;
    end;
    goRightToLeft : begin
      Top := AComparison.Top -15;
      Left := AComparison.Left -10;
      Width := AComparison.Width + ASample.Width + 30;
      Height := AComparison.Height + 30;
    end;
  end;
end;

procedure TAnimation.Stop;
begin
  FTimer.Enabled := False;
end;

constructor TAnimation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FStep := 0.09;
  FTimer.Interval := 100;
  FGrowing := false;
  FTimer.Enabled := False;
  FTimer.OnTimer := @OnTimer;
  FTimer.OnStartTimer := @OnStartTimer;
  FTimer.OnStopTimer := @OnStopTimer;
  FPenWidth := 6;
  FAnimationStyle := asSquare;
  Visible := False;
  FHeight:= 220;
  Height:= 200;
  Width:= 300;
  Color:= clDkGray;
  Canvas.Font.Size := 20;
  FSibling := nil;
end;

destructor TAnimation.Destroy;
begin
  //Destrua os objetos criados aqui
  inherited Destroy;
end;

end.
