{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Image.Animation;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, Graphics, Controls, ExtCtrls, Stimuli.Image.Base;

type

  { TAnimation }

  TAnimation = class(TLightImage)
  private
    FAcum, FStep: double;
    FHeight: integer;
    FWidth: integer;
    FTimer: TTimer;
    FShow: boolean;
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
  end;

implementation

uses Forms, FileUtil, LazFileUtils, Session.Configuration.GlobalContainer
     , Dialogs
     , Types
     ;

{ TAnimation }

procedure TAnimation.Paint;
var
  LTextStyle : TTextStyle;

  procedure PaintText(Color : TColor);
  begin
    LTextStyle := Canvas.TextStyle;
    LTextStyle.SingleLine:=False;
    LTextStyle.Wordbreak:=True;
    LTextStyle.Clipping:=False;
    LTextStyle.Alignment:=taCenter;
    LTextStyle.Layout:=tlCenter;
    Canvas.TextStyle := LTextStyle;
    with Canvas do
      begin
        Font.Color:= clWhite xor Color;
        Pen.Width := FPenWidth;
        Pen.Color := EdgeColor;
        Brush.Color:= Color;//FBorderColor;
        //FillRect(Rect(0, 0, Width, Height));
        if Caption = '' then Rectangle(ClientRect);
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

    Canvas.TextStyle := LTextStyle;
    with Canvas do
      begin
        Pen.Width := FPenWidth;
        Pen.Color := Color;
        Brush.Color:= Color;
        with LCenter do
          Ellipse(X - LSize, Y - LSize, X + LSize, Y + LSize);
      end;
  end;

  procedure PaintSquare(Color : TColor);
  begin
    with Canvas do
      begin
        Pen.Width := FPenWidth;
        Pen.Color := EdgeColor;
        Brush.Color:= Color;
        Rectangle(ClientRect);
      end;
  end;

  procedure PaintLetterRect(Color : TColor);
  var
    R : TRect;
  begin
    R := Rect(ClientRect.Left,
          ClientRect.Top,
          ClientRect.Right,
          (ClientRect.Bottom - ClientRect.Top) div 2);
    LTextStyle := Canvas.TextStyle;
    LTextStyle.SingleLine:=False;
    LTextStyle.Wordbreak:=True;
    LTextStyle.Clipping:=False;
    LTextStyle.Alignment:=taCenter;
    LTextStyle.Layout:=tlCenter;
    Canvas.TextStyle := LTextStyle;
    Canvas.Font.Size := 15;
    with Canvas do
      begin
        Font.Color:= clWhite xor Color;
        Pen.Width := FPenWidth;
        Pen.Color := EdgeColor;
        Brush.Color := Color; //FBorderColor;

        Rectangle(R);
        TextRect(R,
          (((R.Right-R.Left) div 2) - (TextWidth(Caption)div 2)),
          (((R.Bottom-R.Top) div 2) - (TextHeight(Caption)div 2)),
          Caption);
      end;
  end;

  procedure PaintAnimate(Color: TColor);
  begin
    with Canvas do
      begin
        Pen.Width := FPenWidth;
        Pen.Color := clRed;
        //Brush.Color:= clRed;
        Rectangle(ClientRect);
        TextRect(ClientRect,
          (((ClientRect.Right-ClientRect.Left) div 2) - (TextWidth(Caption)div 2)),
          (((ClientRect.Bottom-ClientRect.Top) div 2) - (TextHeight(Caption)div 2)),
          Caption);
      end;
  end;

begin
  case FImageKind of
    ikLetterRect : PaintLetterRect(Color);
    ikLetter: PaintText(Color);
    ikSquare: PaintSquare(Color);
    ikCircle: PaintCircle(Color);
    ikBitmap: Canvas.StretchDraw(ClientRect, FBitmap);
    ikAnimate: PaintAnimate(Color);
  end;
end;

procedure TAnimation.OnStartTimer(Sender: TObject);
begin
  if false then
  begin
    CalculatePreferredSize(FWidth, FHeight, False);
    AutoSize := False;
  end;
  if true then
    FShow := FHeight <> Height;
  FAcum := 0;
end;

procedure TAnimation.OnStopTimer(Sender: TObject);
begin
  if FShow and false then
    AutoSize := True;
end;

procedure TAnimation.OnTimer(Sender: TObject);
var
  temp: double;
begin
  FAcum := FAcum + FStep;
  if FStep > 1 then
    FStep := 1;
  temp := easeInOutQuad(FAcum);

  if true then
  begin
    if FShow then
    begin
      Height := round(FHeight * temp);
      if Height >= FHeight then
      begin
        Height := FHeight;
        FTimer.Enabled := False;
      end;
    end
    else
    begin
      temp := FHeight - round(FHeight * temp);
      if temp <= Constraints.MinHeight then
      begin
        Height := Constraints.MinHeight;
        FTimer.Enabled := False;
      end
      else
        Height := trunc(temp);
    end;
  end;
end;

function TAnimation.easeInOutQuad(t: double): double;
begin
  {$ifndef darwin}
  if t < 0.5 then
    Result := 2 * t * t
  else
    Result := -1 + (4 - 2 * t) * t;
  {$else}
    Result := 1;
  {$endif}
end;

procedure TAnimation.Animate(ASibling : TLightImage);
var
  R: TRect;
begin
  Self.SendToBack;
  Kind := ikAnimate;
  R := ASibling.ClientRect;
  InflateRect(R, 20, 20);
  Self.SetOriginalBounds(ASibling.Left, ASibling.Top, R.Width, R.Height);
  Anchors := [akLeft, akTop];
  AnchorSideLeft.Control := ASibling ;
  AnchorSideLeft.Side := asrCenter;
  AnchorSideTop.Control := ASibling;
  AnchorSideTop.Side := asrCenter;
  FTimer.Enabled:= true;
end;

constructor TAnimation.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FStep := 0.1;
  FTimer.Interval := 15;
  FTimer.Enabled := False;
  FTimer.OnTimer := @OnTimer;
  FTimer.OnStartTimer := @OnStartTimer;
  FTimer.OnStopTimer := @OnStopTimer;
  FPenWidth := 10;
  FImageKind:=ikAnimate;
  Visible := False;
  FHeight:= 300;
  Height:= 200;
  Width:= 300;
  EdgeColor:= clInactiveCaption;
  Canvas.Font.Size := 20;
end;

destructor TAnimation.Destroy;
begin
  //Destrua os objetos criados aqui
  inherited Destroy;
end;

end.
