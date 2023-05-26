{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Image.DragDropable;

{$mode ObjFPC}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, LCLIntf, Controls, fgl, Stimuli, Stimuli.Image.Base;

type

  { TDragDropableItem }

  TDragDropTargets = specialize TFPGList<TObject>;

  TDragMouseMoveMode = (dragFree, dragChannel);

  TDropShape = (dropRect, dropCircle);

  TDragDropableItem = class (TLightImage, IDragDropable)
  private
    FDragMouseMoveMode: TDragMouseMoveMode;
    FDropShape: TDropShape;
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
    procedure SetDragMouseMoveMode(AValue: TDragMouseMoveMode);
    procedure SetDropShape(AValue: TDropShape);
    procedure SetOnOtherDragDrop(AValue: TDragDropEvent);
    procedure SetOnRightDragDrop(AValue: TDragDropEvent);
    function IntersectsWith(Sender : TDragDropableItem) : Boolean;
    procedure SetOnWrongDragDrop(AValue: TDragDropEvent);
    procedure BorderColision;
  protected
    procedure DragDrop(Sender, Source: TObject; X, Y: Integer); reintroduce;
    procedure MouseDown(Button: TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
    procedure MouseUp(Button: TMouseButton;
      Shift:TShiftState; X,Y:Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddTarget(ATarget : TObject);
    procedure UpdateDragMouseMoveMode;
    property Targets : TDragDropTargets read FTargets;
    property Target : TDragDropableItem read GetTarget;
    property Draggable : Boolean read GetDraggable write FCanDrag;
    property DragMouseMoveMode  : TDragMouseMoveMode read FDragMouseMoveMode write SetDragMouseMoveMode;
    property DropShape : TDropShape read FDropShape write SetDropShape;
    property OnRightDragDrop : TDragDropEvent read FOnRightDragDrop write SetOnRightDragDrop;
    property OnWrongDragDrop : TDragDropEvent read FOnWrongDragDrop write SetOnWrongDragDrop;
    property OnOtherDragDrop : TDragDropEvent read FOnOtherDragDrop write SetOnOtherDragDrop;
  end;

type

  { TDragMouseMoveModeHelper }

  TDragMouseMoveModeHelper = type helper for TDragMouseMoveMode
    function ToString : string;
  end;

  { TCustomStringHelper }

  TCustomStringHelper = type helper(TStringHelper) for string
    function ToDragMouseMoveMode : TDragMouseMoveMode;
  end;

var
  DefaultDragMouveMoveMode : TDragMouseMoveMode;

implementation

uses Graphics, Experiments.Grids, Stimuli.Helpers.DragDropChannel;

{ TCustomStringHelper }

function TCustomStringHelper.ToDragMouseMoveMode: TDragMouseMoveMode;
begin
  case UpperCase(Self) of
    'DRAGFREE', 'FREE' : Result := dragFree;
    'DRAGCHANNEL', 'CHANNEL' : Result := dragChannel;
    'DEFAULT' : Result := DefaultDragMouveMoveMode;
    else
      RunError(107);
  end;
end;

{ TDragMouseMoveModeHelper }

function TDragMouseMoveModeHelper.ToString: string;
begin
  WriteStr(Result, Self);
  Result := Result.Replace('drag', '');
end;

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

procedure TDragDropableItem.SetDragMouseMoveMode(AValue: TDragMouseMoveMode);
begin
  if FDragMouseMoveMode=AValue then Exit;
  FDragMouseMoveMode:=AValue;
  UpdateDragMouseMoveMode;
end;

procedure TDragDropableItem.SetDropShape(AValue: TDropShape);
begin
  if FDropShape = AValue then Exit;
  FDropShape := AValue;
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

  function IntersectsWithCircle(ABoundsRect : TRect): Boolean;
  begin
    // todo: implement me
  end;
begin
  Result := False;
  if Sender is TDragDropableItem then begin
    LControl := TDragDropableItem(Sender);
    case DropShape of
      dropRect : begin
        Result := BoundsRect.IntersectsWith(LControl.BoundsRect);
      end;
      dropCircle : begin
        Result := IntersectsWithCircle(LControl.BoundsRect);
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
  inherited DragDrop(Sender, X, Y);
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
  DeltaX : integer;
  DeltaY : integer;
begin
  if FIsDragging then begin
    case DragMouseMoveMode of
      dragFree : begin
        DeltaX := FStartMouseDown.X -X;
        DeltaY := FStartMouseDown.Y -Y;
        if (Abs(DeltaX) > (Self.Width div 2)) or
           (Abs(DeltaY) > (Self.Height div 2)) then begin
           Exit;
           end;
        Point.X := Left - DeltaX;
        Point.Y := Top - DeltaY;
      end;
      dragChannel : begin
        Point := DragDropChannel.NextPoint;
      end;
    end;
    Left := Point.X;
    Top  := Point.Y;
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
  FDragMouseMoveMode := dragChannel;
  FTargets := TDragDropTargets.Create;
  FCanDrag := True;
  //Kind := ikLetter;
end;

destructor TDragDropableItem.Destroy;
begin
  FTargets.Clear;
  FTargets.Free;
  inherited Destroy;
end;

procedure TDragDropableItem.AddTarget(ATarget: TObject);
begin
  FTargets.Add(ATarget);
end;

procedure TDragDropableItem.UpdateDragMouseMoveMode;
begin
  case DragMouseMoveMode of
    dragFree : begin
      // do nothing
    end;
    dragChannel : begin
      if Targets.Count > 0 then
        DragDropChannel.Update(BoundsRect, Target.BoundsRect);
    end;
  end;
end;

end.

