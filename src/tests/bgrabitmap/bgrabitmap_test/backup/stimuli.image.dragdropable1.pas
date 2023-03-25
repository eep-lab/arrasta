{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).
  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Stimuli.Image.DragDropable1;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, LCLIntf, Controls, fgl, Stimuli1, Stimuli.Image.Base1;

type

  { TDragDropableItem }

  TDragDropableItem = class(TLightImage, IDragDropable)
  private
  protected
    procedure DragDrop(Sender, Source: TObject; X, Y: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;
  end;

implementation

uses
  Graphics;

{ TDragDropableItem }

procedure TDragDropableItem.DragDrop(Sender, Source: TObject; X, Y: Integer);
begin

end;

constructor TDragDropableItem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Kind := ikLetter;
end;

destructor TDragDropableItem.Destroy;
begin

end;

end.

