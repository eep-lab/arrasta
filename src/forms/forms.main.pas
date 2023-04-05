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
    ButtonStartAll: TButton;
    ButtonStartTrial: TButton;
    EditParticipant: TEdit;
    FloatSpinEditScreenWidth: TFloatSpinEdit;
    GroupBoxDesign: TGroupBox;
    GroupBoxComplexity: TGroupBox;
    IniPropStorage: TIniPropStorage;
    LabelDragMoveFactor: TLabel;
    LabelComparisons: TLabel;
    LabelSamples: TLabel;
    LabelScreenWidth: TLabel;
    PanelConfigurations: TPanel;
    RadioGroupHelpType: TRadioGroup;
    RadioGroupRelation: TRadioGroup;
    SpinEditDragMoveFactor: TSpinEdit;
    SpinEditComparisons: TSpinEdit;
    SpinEditSamples: TSpinEdit;
    TabControlDesign: TTabControl;
    procedure ButtonStartTrialClick(Sender: TObject);
    procedure RadioGroupHelpTypeClick(Sender: TObject);
    procedure ButtonStartAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EndSession(Sender: TObject);
    procedure BeforeStartSession(Sender: TObject);
    procedure SpinEditSamplesChange(Sender: TObject);
    procedure TabControlDesignChange(Sender: TObject);
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
   , Session
   , Experiments.Grids
   , Experiments.Arrasta
   , Controls.Trials.DragDrop
   , Stimuli.Image.DragDropable
   , Cheats
   ;

{ TBackground }

var
  GSession : TSession;
  ConfigurationFilename : string;

procedure TBackground.ButtonStartAllClick(Sender: TObject);
var
  LName       : string;
  LDesign     : string;
  LHelpTp     : string;
  LExperiment : string;
begin
  GlobalContainer.RootData := GlobalContainer.RootData +
    EditParticipant.Text + DirectorySeparator;
  ForceDirectories(GlobalContainer.RootData);
  CheatsModeOn := False;
  PanelConfigurations.Hide;
  Session.Backgrounds.Background := Self;

  LExperiment := TabControlDesign.Tabs[TabControlDesign.TabIndex];
  LDesign     := RadioGroupRelation.Items[RadioGroupRelation.ItemIndex];
  LHelpTp     := RadioGroupHelpType.Items[RadioGroupHelpType.ItemIndex];

  ConfigurationFilename :=
    Experiments.Arrasta.MakeConfigurationFile(
      LDesign,
      SpinEditSamples.Value,
      SpinEditComparisons.Value,
      RadioGroupHelpType.ItemIndex,
      SpinEditDragMoveFactor.Value);

  LName := LExperiment + #32 + LDesign + #32 + LHelpTp;
  GSession.Play(LName, EditParticipant.Text);
end;

procedure TBackground.RadioGroupHelpTypeClick(Sender: TObject);
var
  LVisible : Boolean;
begin
  case RadioGroupHelpType.ItemIndex of
    0 : LVisible:=False;
    1 : LVisible:=True;
  end;
  SpinEditDragMoveFactor.Visible := LVisible;
  LabelDragMoveFactor.Visible:=LVisible;
end;

var
  LTrial  : TDragDrop;

procedure TBackground.ButtonStartTrialClick(Sender: TObject);
var
  DragMouseMoveMode : TDragMouseMoveMode;
begin
  case RadioGroupHelpType.ItemIndex of
    0 : DragMouseMoveMode := dragFree;
    1 : DragMouseMoveMode := dragChannel;
  end;
  LTrial := TDragDrop.Create(Self);
  with LTrial.Configurations.Parameters do begin
    Values['Style.Samples.DragMode'] := DragMouseMoveMode.ToString;
    Values['Relation'] := RadioGroupRelation.Items[RadioGroupRelation.ItemIndex];
    Values['Samples'] := SpinEditSamples.Value.ToString;
    Values['Comparisons'] := SpinEditComparisons.Value.ToString;
    Values['DragMoveFactor'] := SpinEditDragMoveFactor.Value.ToString;
  end;
  LTrial.OnTrialEnd:=@EndSession;
  LTrial.Play;
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

procedure TBackground.SpinEditSamplesChange(Sender: TObject);
begin
  SpinEditComparisons.Enabled := SpinEditSamples.Value <> 3;
  SpinEditComparisons.MinValue := SpinEditSamples.Value;
  SpinEditComparisons.Value := SpinEditSamples.Value;
  SpinEditComparisons.Invalidate;
end;

procedure TBackground.TabControlDesignChange(Sender: TObject);
begin
  RadioGroupRelation.Items.Clear;
  case TabControlDesign.TabIndex of
    0 : begin
      RadioGroupRelation.Items.Append('A-A');
      RadioGroupRelation.Items.Append('B-B');
      RadioGroupRelation.Items.Append('C-C');
    end;
    1 : begin
      RadioGroupRelation.Items.Append('A-B');
      RadioGroupRelation.Items.Append('B-A');
      RadioGroupRelation.Items.Append('A-C');
      RadioGroupRelation.Items.Append('C-A');
      RadioGroupRelation.Items.Append('C-B');
      RadioGroupRelation.Items.Append('B-C');
    end;
  end;
end;


end.

