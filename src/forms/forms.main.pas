{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).
  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, IniPropStorage, ComCtrls, Spin;

type

  { TBackground }

  TBackground = class(TForm)
    Button1: TButton;
    ButtonStartAll: TButton;
    CheckBoxCheatsMode : TCheckBox;
    EditParticipant: TEdit;
    FloatSpinEditScreenWidth: TFloatSpinEdit;
    IniPropStorage: TIniPropStorage;
    LabelScreenWidth: TLabel;
    PageControl1: TPageControl;
    PanelConfigurations: TPanel;
    RadioGroupDesign1: TRadioGroup;
    TabSheet1: TTabSheet;
    procedure FormClick(Sender: TObject);
    procedure SampleDblClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ButtonStartAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EndSession(Sender: TObject);
    procedure BeforeStartSession(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private

  public

  end;

var
  Background: TBackground;

implementation

{$R *.lfm}

uses
   FileUtil
   , Constants
   , Session.Backgrounds
   , Session.Configuration.GlobalContainer
   , SessionSimple
   , Experiments.Grids
   , Experiments.Arrasta
   , Stimuli.Sequence.DragDrop
   , Cheats
   ;

{ TBackground }

var
  GSession : TSession;
  ConfigurationFilename : string;

procedure TBackground.ButtonStartAllClick(Sender: TObject);
var
  LName : string;
  LDesign : string;
begin
  GlobalContainer.RootData :=
    GlobalContainer.RootData +
    EditParticipant.Text +
    DirectorySeparator;
  ForceDirectories(GlobalContainer.RootData);
  CheatsModeOn := CheckBoxCheatsMode.Checked;
  PanelConfigurations.Hide;
  Session.Backgrounds.Background := Self;

  case PageControl1.TabIndex of
    0:begin
      LDesign := RadioGroupDesign1.Items[RadioGroupDesign1.ItemIndex];
    end;
  end;

  ConfigurationFilename :=
    Experiments.Arrasta.MakeConfigurationFile(LDesign, PageControl1.TabIndex);

  LName := 'Experimento ' + (PageControl1.TabIndex+1).ToString +
           ' Delineamento:' + LDesign;

  GSession.Play(LName, EditParticipant.Text);
end;

var
  LStimuli: TDragDropStimuli;

procedure TBackground.SampleDblClick(Sender: TObject);
begin

end;

procedure TBackground.FormClick(Sender: TObject);
begin
  LStimuli.ResetGrid;
end;

procedure TBackground.Button1Click(Sender: TObject);
begin
  LStimuli := TDragDropStimuli.Create(self);
  LStimuli.Parent := Background;
  LStimuli.Start;
  PanelConfigurations.Hide;
end;

procedure TBackground.FormCreate(Sender: TObject);
begin
  GSession := TSession.Create(Self);
  GSession.OnEndSession:=@EndSession;
  GSession.OnBeforeStart:=@BeforeStartSession;
end;

procedure TBackground.EndSession(Sender: TObject);
begin
  ShowMessage('Fim.');
  Close;
end;

procedure TBackground.BeforeStartSession(Sender: TObject);
begin
  CopyFile(ConfigurationFilename, GSession.BaseFilename+'.ini');
end;

procedure TBackground.FormPaint(Sender: TObject);
//var
//  j, i: Integer;
//  R : TRect;
begin
  //Canvas.Pen.Color := clBlack;
  //if DoDraw then
  //for j := Low(Grid) to High(Grid) do begin
  //  for i := Low(Grid[j]) to High(Grid[j]) do begin
  //    R := Rect(
  //      Grid[j][i].Left,
  //      Grid[j][i].Top,
  //      Grid[j][i].Left+Grid[j][i].SquareSide,
  //      Grid[j][i].Top +Grid[j][i].SquareSide);
  //    Canvas.Rectangle(R);
  //    Canvas.TextOut(R.Left, R.Top, Grid[j][i].Index.ToString);
  //  end;
  //end;
end;


end.

