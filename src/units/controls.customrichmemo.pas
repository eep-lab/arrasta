{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.CustomRichMemo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RichMemo, LCLType, LMessages;

type

  { TARichMemo }

  TARichMemo = class(TRichMemo)
  protected
    procedure EraseBackground(DC: HDC); override;
    procedure WndProc(var Message: TLMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses Controls, Graphics, LCLIntf, StdCtrls;

{ TARichMemo }

procedure TARichMemo.EraseBackground(DC: HDC);
begin
  inherited EraseBackground(DC);
end;

procedure TARichMemo.WndProc(var Message: TLMessage);
  procedure ColorBackground;
  begin
    SetTextColor(Message.WParam, ColorToRGB(Font.Color));
    SetBkColor(Message.WParam, ColorToRGB(Brush.Color));
    Message.Result := LRESULT(Brush.Handle);
  end;
begin
  case Message.Msg of
    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
      ColorBackground;

    CM_ENABLEDCHANGED:
      ColorBackground;
  else;
    inherited WndProc(Message);
  end;
end;

constructor TARichMemo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Color := clWhite;
  Brush.Color := clWhite;
  //Enabled := False;
  BorderStyle := bsNone;
  ScrollBars := ssAutoVertical;
  ReadOnly := True;
end;

end.

