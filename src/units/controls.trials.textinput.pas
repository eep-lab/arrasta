{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.TextInput;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, StdCtrls, Graphics, IpHtml

  , Controls.Trials.Abstract
  , Controls.Trials.Helpers
  ;

type

  { TTextInput }

  TTextInput = class(TTrial)
    procedure EndButtonClick(Sender : TObject);
  private
    FResponse : string;
    FEndButton : TButton;
    FDataSupport : TDataSupport;
    FMessage : TLabel;
    FEdit : TEdit;
    procedure TrialBeforeEnd(Sender: TObject);
    procedure TrialStart(Sender: TObject);
  protected
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    destructor Destroy;override;
    function AsString : string; override;
    procedure Play(ACorrection : Boolean); override;
  end;

implementation

uses
  constants
  , Timestamps
  ;

constructor TTextInput.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  //OnTrialKeyUp := @TrialKeyUp;
  OnTrialStart := @TrialStart;

  FMessage := TLabel.Create(Self);
  with FMessage do begin
    Visible := False;
    Alignment := taCenter;
    Anchors := [akLeft,akRight];
    //Layout := tlCenter;
    WordWrap := True;
    Font.Name := 'Times New Roman';
    //OnMouseUp := @MessageMouseUp;
    Font.Size:=22;
    Width := 640;
    Parent := TCustomControl(AOwner);
  end;

  FEdit := TEdit.Create(Self);
  with FEdit do begin
    Visible := False;
    Font.Size := 20;
    Font.Name := 'Times New Roman';
    Width := 200;
    Height := 50;
    Parent := TCustomControl(AOwner);
  end;

  FEndButton := TButton.Create(Self);
  with FEndButton do begin
    Visible := False;
    Caption := 'Continuar';
    AutoSize := True;
    Font.Name:='Times New Roman';
    Font.Size := 15;
    OnClick := @EndButtonClick;
    Parent := TCustomControl(AOwner);
  end;

  Header := Header + #9 +
            rsReportStmBeg + #9 +
            rsReportStmDur + #9 +
            'Resposta';

  Result := T_NONE;
  IETConsequence := T_NONE;
  Result := T_NONE;
  FResponse := 'Sem Resposta';
end;

destructor TTextInput.Destroy;
begin
  inherited Destroy;
end;

function TTextInput.AsString: string;
begin
  Result := '';
end;

procedure TTextInput.EndButtonClick(Sender: TObject);
begin
  FResponse := FEdit.Text;
  EndTrial(Sender);
end;

procedure TTextInput.TrialBeforeEnd(Sender: TObject);
begin
  FDataSupport.StmEnd := TickCount;
  WriteData(Self);
end;


procedure TTextInput.Play(ACorrection : Boolean);
var
  LParameters : TStringList;
begin
  inherited Play(ACorrection);
  LParameters := Configurations.SList;
  FMessage.Caption := LParameters.Values[_Msg];

  if Self.ClassType = TTextInput then Config(Self);
end;

procedure TTextInput.TrialStart(Sender: TObject);
begin
  with FMessage do begin
    SetBounds((Self.Width - Width) div 2, (Self.Height - Height) div 2, Width, Height);
    Show;
  end;
  with FEdit do begin
    Top := FMessage.BoundsRect.Bottom + 20;
    Left := (Self.Width - Width) div 2;
    Show;
  end;
  with FEndButton do begin
    Top := FEdit.BoundsRect.Bottom + 20;
    Left := (Self.Width - Width) div 2;
  end;

  Self.SetFocus;
  FDataSupport.StmBegin := TickCount;
end;

procedure TTextInput.WriteData(Sender: TObject);
var aStart, aDuration : string;
begin
  inherited WriteData(Sender);
  aStart := TimestampToStr(FDataSupport.StmBegin - TimeStart);
  aDuration := TimestampToStr(FDataSupport.StmEnd - TimeStart);

  Data := Data + aStart + #9 + aDuration + #9 + FResponse;
end;

end.
