{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Stimuli.Text;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JvHtControls, Controls;

type

  { TLabelStimulus }

  TLabelStimulus = class(TJvHTLabel)
  private

  public
    constructor Create(AOwner: TComponent; AParent : TWinControl); reintroduce;
    procedure LoadFromFile(AFilename : string);
    procedure CentralizeLeft;
    procedure CentralizeTopMiddle;
    procedure CentralizeBottom;
    procedure CentralizeTopRight;
    procedure CentralizeMiddleRight;
    procedure CentralizeOnTopOfControl(AControl : TGraphicControl);
  end;

implementation

uses Graphics;

{ TLabelStimulus }

constructor TLabelStimulus.Create(AOwner: TComponent; AParent: TWinControl);
begin
  inherited Create(AOwner);
  Visible := False;
  Alignment := taCenter;
  Layout := tlCenter;
  Anchors := [akLeft,akRight];
  Font.Name := 'Arial';
  Font.Color:= clBlack;
  Font.Size := 14;
  Parent := AParent;
end;

procedure TLabelStimulus.LoadFromFile(AFilename: string);
var LStringList : TStringList;
begin
  LStringList := TStringList.Create;
  try
    LStringList.LoadFromFile(AFilename);
    Caption := WrapText(LStringList.Text, '<br>', [#32], 50);
    //WordWrap := True; // not supported by TJvHTLabel
  finally
    LStringList.Free;
  end;
end;

procedure TLabelStimulus.CentralizeLeft;
var
  LParent : TCustomControl;
begin
  LParent := TCustomControl(Parent);
  Width := (LParent.Width div 2)-150;
  Left := (LParent.Width div 4) - (Width div 2);
  Top := (LParent.Height div 2) - (Height div 2);
end;

procedure TLabelStimulus.CentralizeTopMiddle;
var
  LParent : TCustomControl;
begin
  LParent := TCustomControl(Parent);
  Width := (LParent.Width div 2)-150;
  Left := (LParent.Width div 2) - (Width div 2);
  Top := (LParent.Height div 2) - (LParent.Height div 4) - (Height div 2);

  if Top < 250 then Top := 250;
end;

procedure TLabelStimulus.CentralizeBottom;
var
  LParent : TCustomControl;
begin
  LParent := TCustomControl(Parent);
  Width := (LParent.Width div 2)-150;
  Left := (LParent.Width div 2) - (Width div 2);
  Top := (LParent.Height div 2) + (LParent.Height div 4) - (Height div 2);
end;

procedure TLabelStimulus.CentralizeTopRight;
var
  LParent : TCustomControl;
begin
  LParent := TCustomControl(Parent);
  Width := (LParent.Width div 2)-150;
  Left := LParent.Width - (LParent.Width div 4) - (Width div 2);
  Top := (LParent.Height div 4) - (Height div 2);
end;

procedure TLabelStimulus.CentralizeMiddleRight;
var
  LParent : TCustomControl;
begin
  LParent := TCustomControl(Parent);
  Width := (LParent.Width div 2)-150;
  Left := LParent.Width - (LParent.Width div 4) - (Width div 2);
  Top := LParent.Height - (LParent.Height div 2) - (Height div 2);
end;

procedure TLabelStimulus.CentralizeOnTopOfControl(AControl: TGraphicControl);
var
  LParent : TCustomControl;
begin
  LParent := TCustomControl(Parent);
  Width := (LParent.Width div 2)-150;
  Left := AControl.Left + (AControl.Width div 2) - (Width div 2);
  Top := AControl.Top - Height - 25;
end;

end.

