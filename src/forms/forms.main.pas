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
  Classes, SysUtils, Forms, Controls, Dialogs,
  ExtCtrls, StdCtrls, IniPropStorage, ComCtrls, Spin, Menus
  , Session.Trial.HelpSeries.DragDrop
  , Types;

type

  { TBackground }

  TBackground = class(TForm)
    ButtonAddParticipant: TButton;
    ButtonTestDispenser: TButton;
    ButtonStartAll: TButton;
    ButtonStartTrial: TButton;
    CheckBoxDistance: TCheckBox;
    CheckBoxShowMouse: TCheckBox;
    ComboBoxOrientations: TComboBox;
    ComboBoxParticipants1: TComboBox;
    FloatSpinEditSize: TFloatSpinEdit;
    FloatSpinEditScreenWidth: TFloatSpinEdit;
    GroupBoxComplexity: TGroupBox;
    GroupBoxDesign: TGroupBox;
    IniPropStorage: TIniPropStorage;
    LabelStimulusSizeUnit: TLabel;
    LabelStimulusSize: TLabel;
    LabelCredits: TLabel;
    LabelTitle: TLabel;
    LabelVersion: TLabel;
    LabelNameParticipants: TLabel;
    LabelDistance: TLabel;
    LabelDistancePercentage: TLabel;
    LabelDragDropOrientation: TLabel;
    LabelITITime: TLabel;
    LabelScreenWidth: TLabel;
    LabelScreenWidthUnit: TLabel;
    LabelTrials: TLabel;
    LabelSessionTimeUnit: TLabel;
    LabelConfigurations: TLabel;
    LabelSessionTime: TLabel;
    LabelComparisons: TLabel;
    LabelSamples: TLabel;
    LabelITI: TLabel;
    MenuItemRemoveParticipant: TMenuItem;
    PageControlConfigurations: TPageControl;
    PanelConfigurations: TPanel;
    PopupMenuParticipants: TPopupMenu;
    RadioGroupDispenser: TRadioGroup;
    RadioGroupRelation: TRadioGroup;
    SpinEditDistance: TSpinEdit;
    SpinEditTrials: TSpinEdit;
    SpinEditSessionTime: TSpinEdit;
    SpinEditComparisons: TSpinEdit;
    SpinEditSamples: TSpinEdit;
    SpinEditITI: TSpinEdit;
    TabControlDesign: TTabControl;
    TabSheetAbout: TTabSheet;
    TabSheetMisc: TTabSheet;
    TabSheetComplexity: TTabSheet;
    TabSheetSession: TTabSheet;
    procedure ButtonAddParticipantClick(Sender: TObject);
    procedure ButtonStartTrialClick(Sender: TObject);
    procedure ButtonTestDispenserClick(Sender: TObject);
    procedure CheckBoxDistanceChange(Sender: TObject);
    //procedure FloatSpinEditSizeChange(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ButtonStartAllClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EndSession(Sender: TObject);
    procedure BeforeStartSession(Sender: TObject);
    procedure MenuItemRemoveParticipantClick(Sender: TObject);
    procedure SpinEditSamplesChange(Sender: TObject);
    procedure TabControlDesignChange(Sender: TObject);
    procedure TabSheet2ContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure TabSheetSessionContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
  private
    function GetRelation : string;
    function GetComparValue : TComparValue;
    function GetSampleValue : TSampleValue;
    function GetSessionName : string;
    function GetOrientation : TDragDropOrientation;
    function GetDistance : TDistanceValue;
    function GetStimulusSize : TStimulusSizeValue;
    function Validated : Boolean;
    procedure SaveParticipants(const AFileName: string);
    procedure LoadParticipants(const AFileName: string);
  public
  end;

var
  Background: TBackground;

implementation

{$R *.lfm}

uses
   FileUtil
   , Session
   , Session.Backgrounds
   , Session.Configuration.GlobalContainer
   , Experiments.Arrasta
   , Controls.Trials.DragDrop
   , Cheats
   , Devices.RS232i
   , Constants.DragDrop
   ;

{ TBackground }

var
  GSession : TSession;
  ConfigurationFilename : string;

procedure TBackground.ButtonStartAllClick(Sender: TObject);
begin
  if not Validated then Exit;
  case RadioGroupDispenser.ItemIndex of
    0 : RS232.DefaultDispenser := disp1;
    1 : RS232.DefaultDispenser := disp2;
    2 : RS232.DefaultDispenser := disp3;
    3 : RS232.DefaultDispenser := disp4;
  end;
  GlobalContainer.RootData := GlobalContainer.RootData +
    ComboBoxParticipants1.Text + DirectorySeparator;
  ForceDirectories(GlobalContainer.RootData);
  CheatsModeOn := False;
  PanelConfigurations.Hide;
  Session.Backgrounds.Background := Self;

  ConfigurationFilename :=
    Experiments.Arrasta.MakeConfigurationFile(
      GetOrientation.ToString,
      SpinEditTrials.Value,
      SpinEditITI.Value * 1000,
      SpinEditDistance.Value,
      GetRelation,
      SpinEditSamples.Value,
      SpinEditComparisons.Value,
      CheckBoxShowMouse.Checked,
      GetStimulusSize.ToString);

  {Existem duas formas de se criar uma configuração padrão do arquivo de
  configuração. Uma forma é o hard coding. A segunda forma é usando o
  DefaultDragDropData, juntamente com a extração de valores da GUI,
  essa variável global deve conter todos os parâmetros necessários para o
  experimento. O valor padrão de um parâmetro selecionado na GUI será utilizado]
  na progressão de ajuda quando o valor não for encontrado no arquivo
  ComplexityGradient.ini}
  with DefaultDragDropData do begin
    Relation := GetRelation.ToEquivalenceRelation;
    Comparisons := GetComparValue;
    Samples := GetSampleValue;
    Orientation := GetOrientation;
    Distance := GetDistance;
    StimulusSize := GetStimulusSize;
  end;
  if FileExists(DefaultComplexityFilename) then begin
    DragDropHelpSerie := TDragDropHelpSerie.Create(DefaultComplexityFilename);
  end else begin
    DragDropHelpSerie := TDragDropHelpSerie.Create(hsDefault);
  end;
  IDragDropHelpSerie := DragDropHelpSerie;

  GSession.Timer.Interval := SpinEditSessionTime.Value * 60000;
  GSession.Play(GetSessionName, ComboBoxParticipants1.Text);
end;

var
  LTrial  : TDragDrop;

procedure TBackground.ButtonStartTrialClick(Sender: TObject);
begin
  LTrial := TDragDrop.Create(Self);
  with LTrial.Configurations.Parameters do begin
    with DragDropKeys do begin
      Values[RepeatTrials] := SpinEditTrials.Value.ToString;
      Values[Relation] := GetRelation;
      Values[Samples] := SpinEditSamples.Value.ToString;
      Values[Comparisons] := SpinEditComparisons.Value.ToString;
      Values[DragDropOrientation] := GetOrientation.ToString;
      Values[Distance] := SpinEditDistance.Value.ToString;
      Values[StimulusSize] := GetStimulusSize.ToString;
    end;
  end;
  LTrial.OnTrialEnd:=@EndSession;
  LTrial.Play;
  PanelConfigurations.Hide;
end;

procedure TBackground.ButtonAddParticipantClick(Sender: TObject);
var
  LNewParticipant: string;
  LParticipantsList: TStringList;
begin
  with ComboBoxParticipants1 do
  begin
    LNewParticipant := InputBox('Arrasta', 'Nome: mínimo de 3 caracteres', '');
    if LNewParticipant.IsEmpty or (Length(LNewParticipant) < 3) then Exit;
    LParticipantsList := TStringList.Create;
    try
      if FileExists('participants.txt') then
        LParticipantsList.LoadFromFile('participants.txt');
      if LParticipantsList.IndexOf(LNewParticipant) <> -1 then
      begin
        ShowMessage('Este participante já existe.');
        Exit;
      end;
      Items.Append(LNewParticipant);
      LParticipantsList.Add(LNewParticipant);
      LParticipantsList.SaveToFile('participants.txt');
    finally
      LParticipantsList.Free;
    end;
  end;
end;

procedure TBackground.ButtonTestDispenserClick(Sender: TObject);
begin
  case RadioGroupDispenser.ItemIndex of
    0 : RS232.Dispenser(disp1);
    1 : RS232.Dispenser(disp2);
    2 : RS232.Dispenser(disp3);
    3 : RS232.Dispenser(disp4);
  end;
end;

procedure TBackground.CheckBoxDistanceChange(Sender: TObject);
begin
  SpinEditDistance.Enabled := CheckBoxDistance.Checked;
  LabelDistance.Enabled := CheckBoxDistance.Checked;
  LabelDistancePercentage.Enabled := CheckBoxDistance.Checked;
end;

//procedure TBackground.FloatSpinEditSizeChange(Sender: TObject);
//begin
//  if FloatSpinEditSize.Value = 6.5 then begin
//    CheckBoxDistance.Enabled := False;
//    CheckBoxDistance.Checked := False;
//    SpinEditDistance.Value := 0;
//    SpinEditSamples.Value := 1;
//    SpinEditComparisons.Value := 1;
//    SpinEditSamples.Enabled := False;
//    SpinEditComparisons.Enabled := False;
//  end
//  else begin
//    SpinEditSamples.Enabled := True;
//    SpinEditComparisons.Enabled := True;
//  end;
//end;

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
  LoadParticipants('participants.txt');
end;

procedure TBackground.EndSession(Sender: TObject);
begin
  ShowMessage('Fim.');
end;

procedure TBackground.BeforeStartSession(Sender: TObject);
begin
  CopyFile(ConfigurationFilename, GSession.BaseFilename+'.ini');
end;

procedure TBackground.MenuItemRemoveParticipantClick(Sender: TObject);
begin
  with ComboBoxParticipants1 do
    Items.Delete(ItemIndex);
  SaveParticipants('participants.txt');
end;

procedure TBackground.SpinEditSamplesChange(Sender: TObject);
begin
  if SpinEditSamples.Value = 1 then begin
    if FloatSpinEditSize.Value <> 6.5 then begin
      CheckBoxDistance.Enabled := True;
    end;
  end
  else begin
    CheckBoxDistance.Checked := False;
    CheckBoxDistance.Enabled := False;
    SpinEditDistance.Enabled := False;
    LabelDistance.Enabled := False;
    LabelDistancePercentage.Enabled := False;
    SpinEditDistance.Value := 0;
  end;
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

procedure TBackground.TabSheetSessionContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin

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

function TBackground.GetSessionName: string;
begin
  Result :=
    TabControlDesign.Tabs[TabControlDesign.TabIndex] + #32 +
    GetRelation;
  if CheckBoxDistance.Checked then begin
    Result := Result + ', com distância de ' +
              IntToStr(SpinEditDistance.Value) + '% entre estímulos';
  end
  else begin
    Result := Result + ', sem distância entre estímulos';
  end;
end;

function TBackground.GetOrientation : TDragDropOrientation;
begin
  case ComboBoxOrientations.ItemIndex of
    0 : Result := goTopToBottom;
    1 : Result := goBottomToTop;
    2 : Result := goLeftToRight;
    3 : Result := goRightToLeft;
    4 : Result := goRandom;
  end;
end;

function TBackground.GetDistance: TDistanceValue;
begin
  case SpinEditDistance.Value of
    1 : Result := distZero;
    2 : Result := distTen;
    3 : Result := distTwenty;
    4 : Result := distThirty;
    5 : Result := distForty;
    6 : Result := distFifty;
    7 : Result := distSixty;
  end;
end;

function TBackground.GetStimulusSize: TStimulusSizeValue;
var
  LValue: Double;
begin
  LValue := FloatSpinEditSize.Value;
  case Round((LValue - 2.5) / 2) of
    0: Result := sizeSmall;
    1: Result := sizeNormal;
    2: Result := sizeBig;
  end;
end;

function TBackground.Validated: Boolean;
begin
  Result := False;
  if ComboBoxParticipants1.Items.Count = 0 then begin
    ShowMessage('ATENÇÃO! Adicione um novo participante.');
    Exit;
  end;
  if ComboBoxParticipants1.ItemIndex < 0 then begin
    ShowMessage('ATENÇÃO! Escolha um participante.');
    Exit;
  end;
  Result := True;
end;

procedure TBackground.SaveParticipants(const AFileName: string);
begin
  ComboBoxParticipants1.Items.SaveToFile(AFileName);
end;

procedure TBackground.LoadParticipants(const AFileName: string);
begin
  if FileExists(AFileName) then
    ComboBoxParticipants1.Items.LoadFromFile(AFileName);
end;

end.

