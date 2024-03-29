{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Image.MovingSquare;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls,  ExtCtrls
  , Stimuli
  , Stimuli.Image.Base
  ;

type

  TDirection = (sdTop, sdRight, sdBottom, sdLeft,
                sdTopLeft, sdTopRight, sdBottomLeft, sdBottomRight);

  { TMovingSquare }

  TMovingSquare = class sealed(TLightImage, IStimuli)
  private
    //FGranularity : Cardinal;
    //FScreenSize : real;
    //FSize : real;
    FFreezed : Boolean;
    FDirection: TDirection;
    FMovementSize: integer;
    FTimer : TTimer;
    FTimerConsequence : TTimer;
    procedure SetDirection(AValue: TDirection);
    procedure Move(Sender : TObject);
    procedure ChangeColor(Sender : TObject);
  protected
    procedure MouseDown(Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    constructor Create(AOwner : TComponent); override;
    function AsInterface : IStimuli;
    procedure Freeze;
    procedure LoadFromParameters(AParameters : TStringList);
    procedure Start;
    procedure Stop;
    property Direction : TDirection read FDirection write SetDirection;
    property MovementSize : integer read FMovementSize write FMovementSize;
  end;

var
  ConsequenceTime : Cardinal = 250;
  Granularity : Cardinal = 100;
  ScreenInCentimeters : real = 39.624;
  SquareSize : real = 0.8;
  SquareMovementSize : real = 0.53;

implementation

uses Forms, Cheats, Constants;

procedure TMovingSquare.SetDirection(AValue: TDirection);
begin
  if FDirection=AValue then Exit;
  FDirection:=AValue;
end;

constructor TMovingSquare.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if CheatsModeOn then begin
    ConsequenceTime := ConsequenceTime div VelocityFactor;
    //Granularity := Granularity div VelocityFactor;
  end;
  FFreezed := False;
  Width := Round(SquareSize*(Screen.Width/ScreenInCentimeters));
  Height:= Width;
  Color := clGray;
  MovementSize := Round(SquareMovementSize*(Screen.Width/ScreenInCentimeters));
  Direction := sdBottom;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled:=False;
  FTimer.Interval := Granularity;
  FTimer.OnTimer := @Move;
  FTimerConsequence := TTimer.Create(Self);
  FTimerConsequence.Enabled:=False;
  FTimerConsequence.Interval := ConsequenceTime;
  FTimerConsequence.OnTimer := @ChangeColor;
end;

function TMovingSquare.AsInterface : IStimuli;
begin
  Result := Self;
end;

procedure TMovingSquare.Move(Sender: TObject);
var
  R : integer;
  procedure MoveLeft;
  begin
    Left := Left-MovementSize;
  end;

  procedure MoveTop;
  begin
    Top := Top-MovementSize;
  end;

  procedure MoveRight;
  begin
    Left := Left+MovementSize;
  end;

  procedure MoveBottom;
  begin
    Top := Top+MovementSize
  end;

begin
  if FFreezed then Exit;
  R := Random(8);
  case R of
    0: Direction := sdLeft;
    1: Direction := sdTop;
    2: Direction := sdRight;
    3: Direction := sdBottom;
    4: Direction := sdTopLeft;
    5: Direction := sdTopRight;
    6: Direction := sdBottomLeft;
    7: Direction := sdBottomRight;
  end;

  case Direction of
    sdLeft: if BoundsRect.Left <= 0 then Direction := sdRight;
    sdTop: if BoundsRect.Top <= 0 then Direction := sdBottom;
    sdRight: if BoundsRect.Right >= Parent.Width then Direction := sdLeft;
    sdBottom: if BoundsRect.Bottom >= Parent.Height then Direction := sdTop;
    sdTopLeft:
        if (BoundsRect.Top <= 0) and (BoundsRect.Left <= 0) then
          Direction := sdBottomRight
        else
          begin
            if BoundsRect.Top <= 0 then Direction := sdBottomLeft;
            if BoundsRect.Left <= 0 then Direction := sdTopRight;
          end;

    sdTopRight:
        if (BoundsRect.Top <= 0) and (BoundsRect.Right >= Parent.Width) then
          Direction := sdBottomLeft
        else
          begin
            if BoundsRect.Top <= 0 then Direction := sdBottomRight;
            if BoundsRect.Right >= Parent.Width then Direction := sdTopLeft;
          end;

    sdBottomLeft:
        if (BoundsRect.Bottom >= Parent.Height) and (BoundsRect.Left <= 0) then
          Direction := sdBottomRight
        else
          begin
            if BoundsRect.Bottom >= Parent.Height then Direction := sdTopLeft;
            if BoundsRect.Left <= 0 then Direction := sdBottomRight;
          end;

    sdBottomRight:
        if (BoundsRect.Bottom >= Parent.Height) and
           (BoundsRect.Right  >= Parent.Width)  then
          Direction := sdTopLeft
        else
          begin
            if BoundsRect.Bottom >= Parent.Height then Direction := sdTopRight;
            if BoundsRect.Right >= Parent.Width then Direction := sdBottomLeft;
          end;
  end;

  case Direction of
    sdLeft: MoveLeft;
    sdTop: MoveTop;
    sdRight: MoveRight;
    sdBottom: MoveBottom;
    sdTopLeft:begin
      MoveTop;
      MoveLeft;
    end;

    sdTopRight:begin
      MoveTop;
      MoveRight;
    end;

    sdBottomLeft:begin
      MoveBottom;
      MoveLeft;
    end;

    sdBottomRight:begin
      MoveBottom;
      MoveRight;
    end;
  end;
end;

procedure TMovingSquare.ChangeColor(Sender: TObject);
begin
  if FTimerConsequence.Enabled then begin
    FTimerConsequence.Enabled := False;
    Color := clGray;
    if Assigned(FSchedule) then FSchedule.DoResponse;
  end;
end;

procedure TMovingSquare.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if not FTimerConsequence.Enabled then begin
    //inherited MouseDown(Button, Shift, X, Y);
    Color := clYellow;
    FTimerConsequence.Enabled := True;
    if Assigned(OnMouseDown) then OnMouseDown(Self, Button, Shift, X, Y);
  end;
end;

procedure TMovingSquare.Paint;
begin
  with Canvas do
  begin
    Pen.Width := 0;
    Brush.Color:= Color;
    Rectangle(ClientRect);
  end;
end;

procedure TMovingSquare.Start;
begin
  if FFreezed then Exit;
  FTimer.Enabled := True;
end;

procedure TMovingSquare.Stop;
begin
  FTimer.Enabled := False;
end;

procedure TMovingSquare.LoadFromParameters(AParameters : TStringList);
begin
  if Assigned(AParameters) then begin
    { TODO: load parameters as needed}
  end;
end;

procedure TMovingSquare.Freeze;
begin
  FFreezed := True;
end;

end.

