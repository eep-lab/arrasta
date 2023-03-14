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
    ButtonStart: TButton;
    ButtonStartAll: TButton;
    CheckBoxCheatsMode : TCheckBox;
    EditParticipant: TEdit;
    FloatSpinEditScreenWidth: TFloatSpinEdit;
    IniPropStorage: TIniPropStorage;
    LabelSamples: TLabel;
    LabelComparisons: TLabel;
    LabelScreenWidth: TLabel;
    PageControl1: TPageControl;
    PanelConfigurations: TPanel;
    RadioGroupDesign1: TRadioGroup;
    SpinEditSamples: TSpinEdit;
    SpinEditComparisons: TSpinEdit;
    TabSheet1: TTabSheet;
    procedure FormClick(Sender: TObject);
    procedure SampleDblClick(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
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
   , Stimuli.Helpers.DragDropChannel
   , Cheats
   , FPImage
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

procedure TBackground.ButtonStartClick(Sender: TObject);
begin
  LStimuli := TDragDropStimuli.Create(
    Self, SpinEditSamples.Value, SpinEditComparisons.Value);
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
var
  Point : TPoint;
  LColor: TFPColor;
begin
  LColor.Blue  := 0;
  LColor.Green := 0;
  LColor.Red   := 255;
  LColor.Alpha := 255;
  //Canvas.Pen.Color := clRed;
  //Canvas.Pen.Style := psDash;
  if Assigned(DragDropChannel) then
    if Length(DragDropChannel.Line) > 0 then begin
      for Point in DragDropChannel.Line do
        Canvas.DrawPixel(Point.X, Point.Y, LColor);
    end;
end;


end.

