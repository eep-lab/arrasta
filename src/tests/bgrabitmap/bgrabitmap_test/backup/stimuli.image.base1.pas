{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).
  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Image.Base1;

{$mode objfpc}{$H+}

interface

uses SysUtils, Classes, Graphics, Controls;

type

  TImageKind = (ikLetter);

  { TLightImage }

  TLightImage = class(TGraphicControl)
  private
    FOriginalBounds : TRect;
    FEdge : TColor;
    FImageKind: TImageKind;
    procedure SetKind(AValue: TImageKind);
  protected
    FPenWidth: integer;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetOriginalBounds(aLeft, aTop, aWidth, aHeight: integer);
    property EdgeColor : TColor read FEdge write FEdge;
    property Kind: TImageKind read FImageKind write SetKind;
  end;

implementation

uses
  Forms
  , FileUtil
  , LazFileUtils
  , Session.Configuration.GlobalContainer1
  , Dialogs
  , Types
  , ExtCtrls;

{ TLightImage }

procedure TLightImage.SetKind(AValue: TImageKind);
begin
  if FImageKind=AValue then Exit;
  FImageKind:=AValue
end;

procedure TLightImage.Paint;
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
        Brush.Color:= Color;
        if Caption = '' then Rectangle(ClientRect);
        Rectangle(ClientRect);
        TextRect(ClientRect,
          (((ClientRect.Right-ClientRect.Left) div 2) - (TextWidth(Caption)div 2)),
          (((ClientRect.Bottom-ClientRect.Top) div 2) - (TextHeight(Caption)div 2)),
          Caption);
      end;
  end;

begin
  case FImageKind of
    ikLetter: PaintText(Color);
  end;
end;

constructor TLightImage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPenWidth := 10;
  EdgeColor:= clInactiveCaption;
  Canvas.Font.Size := 20;
end;

destructor TLightImage.Destroy;
begin
  inherited Destroy;
end;

procedure TLightImage.SetOriginalBounds(aLeft, aTop, aWidth, aHeight: integer);
var
  LRect : TRect;
begin
  LRect.Left := aLeft + 500;
  LRect.Top := aTop;
  LRect.Width := aWidth;
  LRect.Height := aHeight;
  FOriginalBounds := LRect;
  SetBounds(aLeft, aTop, aWidth, aHeight);
end;

end.
