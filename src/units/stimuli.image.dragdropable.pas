{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).
  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Image.DragDropable;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, Controls, fgl, Stimuli, Stimuli.Image.Base;

type

  { TDragDropableItem }

  TDragDropTargets = specialize TFPGList<TObject>;

  TDropMode = (dropRect, dropCircle);

  TDragDropableItem = class (TLightImage, IDragDropable)
  private
    FDropMode: TDropMode;
    FIsDragging : Boolean;
    FCanDrag : Boolean;
    FOnOtherDragDrop: TDragDropEvent;
    FOnRightDragDrop: TDragDropEvent;
    FOnWrongDragDrop: TDragDropEvent;
    FStartPosition : TPoint;
    FStartMouseDown : TPoint;
    FTargets: TDragDropTargets;
    function GetDraggable: Boolean;
    function GetTarget: TDragDropableItem;
    procedure SetDropMode(AValue: TDropMode);
    procedure SetOnOtherDragDrop(AValue: TDragDropEvent);
    procedure SetOnRightDragDrop(AValue: TDragDropEvent);
    function IntersectsWith(Sender : TDragDropableItem) : Boolean;
    procedure SetOnWrongDragDrop(AValue: TDragDropEvent);
    procedure BorderColision;
  protected
    procedure DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure MouseDown(Button: TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;
    procedure AddTarget(ATarget : TObject);
    property Targets : TDragDropTargets read FTargets;
    property Target : TDragDropableItem read GetTarget;
    property Draggable : Boolean read GetDraggable write FCanDrag;
    property DropMode : TDropMode read FDropMode write SetDropMode;
    property OnRightDragDrop : TDragDropEvent read FOnRightDragDrop write SetOnRightDragDrop;
    property OnWrongDragDrop : TDragDropEvent read FOnWrongDragDrop write SetOnWrongDragDrop;
    property OnOtherDragDrop : TDragDropEvent read FOnOtherDragDrop write SetOnOtherDragDrop;
  end;

implementation

uses Graphics, Experiments.Grids, Stimuli.Helpers.DragDropChannel;

{ TDragDropableItem }

function TDragDropableItem.GetDraggable: Boolean;
begin
  Result := (FTargets.Count > 0) and FCanDrag;
end;

function TDragDropableItem.GetTarget: TDragDropableItem;
begin
  if Targets.Count > 0 then
    Result := Targets[0] as TDragDropableItem
  else
    Result := nil;
end;

procedure TDragDropableItem.SetDropMode(AValue: TDropMode);
begin
  if FDropMode = AValue then Exit;
  FDropMode := AValue;
end;

procedure TDragDropableItem.SetOnOtherDragDrop(AValue: TDragDropEvent);
begin
  if FOnOtherDragDrop=AValue then Exit;
  FOnOtherDragDrop:=AValue;
end;

procedure TDragDropableItem.SetOnRightDragDrop(AValue: TDragDropEvent);
begin
  if FOnRightDragDrop = AValue then Exit;
  FOnRightDragDrop := AValue;
end;

function TDragDropableItem.IntersectsWith(Sender: TDragDropableItem): Boolean;
var
  LControl : TControl;

  function InsideCircle(ACenterX, ACenterY, ARadius, AX, AY : integer): Boolean;
  var Delta : integer;
  begin
    Delta := ((AX - ACenterX) * (AX - ACenterX)) +
             ((AY - ACenterY) * (AY - ACenterY));
    if (Delta <= (ARadius * ARadius)) then
        Result := True
    else
        Result := False;
  end;
begin
  if Sender is TDragDropableItem then begin
    LControl := TDragDropableItem(Sender);
    case DropMode of
      dropRect : begin
        Result := BoundsRect.IntersectsWith(LControl.BoundsRect);
      end;
      dropCircle : begin
        //Result := InsideCircle(
        //  LControl.BoundsRect.CenterPoint.X,
        //  LControl.BoundsRect.CenterPoint.Y,
      end;
    end;
  end;
end;

procedure TDragDropableItem.SetOnWrongDragDrop(AValue: TDragDropEvent);
begin
  if FOnWrongDragDrop=AValue then Exit;
  FOnWrongDragDrop:=AValue;
end;

procedure TDragDropableItem.BorderColision;
var
  Point : TPoint;
begin
  if BoundsRect.IntersectsWith(BorderTop) then begin
    Top := BorderTop.Bottom + 1;
  end;

  if BoundsRect.IntersectsWith(BorderBottom) then begin
    Top := BorderBottom.Top - Height - 1;
  end;

  if BoundsRect.IntersectsWith(BorderLeft) then begin
    Left := BorderLeft.Right + 1;
  end;

  if BoundsRect.IntersectsWith(BorderRight) then begin
    Left := BorderRight.Left - Width - 1;
  end;
end;

procedure TDragDropableItem.DragDrop(Sender, Source: TObject; X, Y: Integer);
begin

end;

procedure TDragDropableItem.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if Draggable then begin
    if not FIsDragging then begin
      if Button in [mbLeft] then begin
        FStartPosition.X := Left;
        FStartPosition.Y := Top;
        FStartMouseDown.X := X;
        FStartMouseDown.Y := Y;
        FIsDragging := True;
      end;
    end;
  end;
  if Assigned(OnMouseDown) then OnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TDragDropableItem.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  Point : TPoint;
begin
  if FIsDragging then begin
    //Left := Left - (FStartMouseDown.X -X);
    //Top := Top - (FStartMouseDown.Y -Y);
    Point := DragDropChannel.NextPoint;
    Top  := Point.Y;
    Left := Point.X;
    BorderColision;
  end;
end;

procedure TDragDropableItem.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  i : integer;
  LTarget : TDragDropableItem;
  LIntersected : Boolean;
begin
  if Draggable then begin
    if Button in [mbLeft] then begin
      FIsDragging := False;
      for i := 0 to FTargets.Count -1 do begin
        if Targets[i] is TDragDropableItem then begin
          LTarget := Targets[i] as TDragDropableItem;
          LIntersected := IntersectsWith(LTarget);
          if LIntersected then Break;
        end;
      end;

      if LIntersected then begin
        case i of
          0 :
            if Assigned(OnRightDragDrop) then
              OnRightDragDrop(LTarget, Self, X, Y);

          else
            if Assigned(OnWrongDragDrop) then
              OnWrongDragDrop(LTarget, Self, X, Y);
        end
      end else begin
        if Assigned(OnOtherDragDrop) then
          OnOtherDragDrop(nil, Self, X, Y);
      end;
    end;
  end;
end;

constructor TDragDropableItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTargets := TDragDropTargets.Create;
  FCanDrag := True;
  Kind := ikLetter;
end;

destructor TDragDropableItem.Destroy;
begin
  FTargets.Clear;
  FTargets.Free;
end;

procedure TDragDropableItem.AddTarget(ATarget: TObject);
begin
  FTargets.Add(ATarget);
end;

end.

