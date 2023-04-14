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
  ExtCtrls, StdCtrls, IniPropStorage, ComCtrls, Spin
  , Stimuli.Image.DragDropable
  , Session.Trial.HelpSeries.DragDrop
  , Types;

type

  { TBackground }

  TBackground = class(TForm)
    ButtonStartAll: TButton;
    ButtonStartTrial: TButton;
    CheckBoxHelpRegression: TCheckBox;
    CheckBoxHelpProgression: TCheckBox;
    CheckBoxMouseModeMode: TCheckBox;
    ComboBoxFactor: TComboBox;
    ComboBoxParticipants: TComboBox;
    FloatSpinEditScreenWidth: TFloatSpinEdit;
    GroupBoxComplexity: TGroupBox;
    GroupBoxDesign: TGroupBox;
    IniPropStorage: TIniPropStorage;
    LabelScreenWidthUnit: TLabel;
    LabelLimitedHoldTime: TLabel;
    LabelITITime: TLabel;
    LabelLimitedHold: TLabel;
    LabelTrials: TLabel;
    LabelSessionTimeUnit: TLabel;
    LabelConfigurations: TLabel;
    LabelSessionTime: TLabel;
    LabelComparisons: TLabel;
    LabelDragMoveFactor: TLabel;
    LabelSamples: TLabel;
    LabelScreenWidth: TLabel;
    LabelITI: TLabel;
    PageControlConfigurations: TPageControl;
    PanelConfigurations: TPanel;
    RadioGroupRelation: TRadioGroup;
    SpinEditTrials: TSpinEdit;
    SpinEditSessionTime: TSpinEdit;
    SpinEditComparisons: TSpinEdit;
    SpinEditSamples: TSpinEdit;
    SpinEditITI: TSpinEdit;
    SpinEditLimitedHold: TSpinEdit;
    TabControlDesign: TTabControl;
    TabSheetComplexity: TTabSheet;
    TabSheet2: TTabSheet;
    procedure ButtonStartTrialClick(Sender: TObject);
    procedure CheckBoxHelpRegressionChange(Sender: TObject);
    procedure CheckBoxMouseModeModeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonStartAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EndSession(Sender: TObject);
    procedure BeforeStartSession(Sender: TObject);
    procedure SpinEditSamplesChange(Sender: TObject);
    procedure TabControlDesignChange(Sender: TObject);
    procedure TabSheet2ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    function GetDragMouseMoveMode : TDragMouseMoveMode;
    function GetRelation : string;
    function GetComparValue : TComparValue;
    function GetSampleValue : TSampleValue;
    function GetMouseMoveFactor : TFactor;
    function GetSessionName : string;
  public

  end;

var
  Background: TBackground;

implementation

{$R *.lfm}

uses
   FileUtil
   , Constants
   , Session
   , Session.Backgrounds
   , Session.Configuration.GlobalContainer
   , Experiments.Arrasta
   , Controls.Trials.DragDrop
   , Cheats
   ;

{ TBackground }

var
  GSession : TSession;
  ConfigurationFilename : string;

procedure TBackground.ButtonStartAllClick(Sender: TObject);
begin
  GlobalContainer.RootData := GlobalContainer.RootData +
    ComboBoxParticipants.Text + DirectorySeparator;
  ForceDirectories(GlobalContainer.RootData);
  CheatsModeOn := False;
  PanelConfigurations.Hide;
  Session.Backgrounds.Background := Self;

  ConfigurationFilename :=
    Experiments.Arrasta.MakeConfigurationFile(
      SpinEditTrials.Value,
      SpinEditITI.Value * 1000,
      SpinEditLimitedHold.Value * 60000,
      GetRelation,
      SpinEditSamples.Value,
      SpinEditComparisons.Value,
      GetDragMouseMoveMode.ToString,
      GetMouseMoveFactor.ToString,
      CheckBoxHelpProgression.Checked,
      CheckBoxHelpRegression.Checked);

  DefaultDragMouveMoveMode := GetDragMouseMoveMode;
  with DefaultDragDropData do begin
    Relation := GetRelation.ToEquivalenceRelation;
    Comparisons := GetComparValue;
    Samples := GetSampleValue;
    HelpType := DefaultDragMouveMoveMode;
    Factor := GetMouseMoveFactor;
  end;
  if FileExists(DefaultComplexityFilename) then begin
    DragDropHelpSerie := TDragDropHelpSerie.Create(DefaultComplexityFilename);
  end else begin
    DragDropHelpSerie := TDragDropHelpSerie.Create(hsDefault);
  end;
  IDragDropHelpSerie := DragDropHelpSerie;

  GSession.Timer.Interval := SpinEditSessionTime.Value * 60000;
  GSession.Play(GetSessionName, ComboBoxParticipants.Text);
end;

var
  LTrial  : TDragDrop;

procedure TBackground.ButtonStartTrialClick(Sender: TObject);
var
  DragMouseMoveMode : TDragMouseMoveMode;
begin
  DragMouseMoveMode := GetDragMouseMoveMode;
  LTrial := TDragDrop.Create(Self);
  with LTrial.Configurations.Parameters do begin
    Values['Style.Samples.DragMode'] := DragMouseMoveMode.ToString;
    Values['Relation'] := RadioGroupRelation.Items[RadioGroupRelation.ItemIndex];
    Values['Samples'] := SpinEditSamples.Value.ToString;
    Values['Comparisons'] := SpinEditComparisons.Value.ToString;
    Values['DragMoveFactor'] := GetMouseMoveFactor.ToString;
    Values['UseHelpSerie'] := 'False';
  end;
  LTrial.OnTrialEnd:=@EndSession;
  LTrial.Play;
  PanelConfigurations.Hide;
end;

procedure TBackground.CheckBoxHelpRegressionChange(Sender: TObject);
begin
  SpinEditLimitedHold.Visible:=CheckBoxHelpRegression.Checked;
  LabelLimitedHold.Visible:=CheckBoxHelpRegression.Checked;
  LabelLimitedHoldTime.Visible:=CheckBoxHelpRegression.Checked;
end;

procedure TBackground.CheckBoxMouseModeModeChange(Sender: TObject);
begin
  ComboBoxFactor.Visible := CheckBoxMouseModeMode.Checked;
  LabelDragMoveFactor.Visible:=CheckBoxMouseModeMode.Checked;
end;

procedure TBackground.FormDestroy(Sender: TObject);
begin
  DragDropHelpSerie.Free;
end;

// uses BGRABitmap, BGRABitmapTypes;
//procedure TBackground.FormPaint(Sender: TObject);
//const
//  BorderSize : single = 50;
//var
//  BGRABitmap : TBGRABitmap;
//  LColor: TBGRAPixel;
//  Pos : integer = 0;
//  i: Integer;
//begin
//  BGRABitmap := TBGRABitmap.Create(ClientWidth, ClientHeight, BGRABlack);
//  try
//    BGRABitmap.PenStyle := psSolid;
//    for i := 1 to 4 do begin
//      LColor := BGRA(255, 0, 0, 255 div i);
//      BGRABitmap.RectangleAntialias(
//        Pos, Pos, ClientWidth-Pos, ClientHeight-Pos, LColor, 50);
//      Inc(Pos, trunc(BorderSize)+10);
//    end;
//    BGRABitmap.Draw(Canvas, 0, 0);
//  finally
//    BGRABitmap.Free;
//  end;
//end;

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

procedure TBackground.TabSheet2ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

function TBackground.GetDragMouseMoveMode: TDragMouseMoveMode;
begin
  if CheckBoxMouseModeMode.Checked then begin
    Result := dragChannel;
  end else begin
    Result := dragFree;
  end;
end;

function TBackground.GetRelation: string;
begin
  Result := RadioGroupRelation.Items[RadioGroupRelation.ItemIndex];
end;

function TBackground.GetComparValue: TComparValue;
begin
  case SpinEditComparisons.Value of
    1 : Result := compOne;
    2 : Result := compTwo;
    3 : Result := compThree;
  end;
end;

function TBackground.GetSampleValue: TSampleValue;
begin
  case SpinEditSamples.Value of
    1 : Result := sampOne;
    2 : Result := sampTwo;
    3 : Result := sampThree;
  end;
end;

function TBackground.GetMouseMoveFactor: TFactor;
begin
  case ComboBoxFactor.ItemIndex of
    0 : Result := facVeryEasy;
    1 : Result := facEasy;
    2 : Result := facNormal;
    3 : Result := facHard;
    4 : Result := facVeryHard;
  end;
end;

function TBackground.GetSessionName: string;
begin
  Result :=
    TabControlDesign.Tabs[TabControlDesign.TabIndex] + #32 +
    GetRelation + #32 +
    GetMouseMoveFactor.ToString;

  if CheckBoxMouseModeMode.Checked then begin
    Result := Result +
      ', com movimentação retrita ao arrastar, na direção da comparação correta';
  end else begin
    Result := Result + ', com movimentação livre ao arrastar';
  end;

  if CheckBoxHelpProgression.Checked then begin
    Result := Result + ', com progressão da complexidade ao acertar';
  end;

  if CheckBoxHelpRegression.Checked then begin
    Result := Result + ', com regressão da complexidade após 1 min sem acertar';
  end;
end;


end.

