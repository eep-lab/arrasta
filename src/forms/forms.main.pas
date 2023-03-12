{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.
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
    procedure JoinAnimation(Sender, Source: TObject; X,Y: Integer);
    procedure Wrong(Sender, Source: TObject; X,Y: Integer);
    procedure Other(Sender, Source: TObject; X,Y: Integer);
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
   , Stimuli.Image.DragDropable
   , Stimuli.Image.Animation
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
  //LSample : TDragDropableItem;
  //LAnimation: TAnimation;
  LStimuli: TDragDropStimuli;

procedure TBackground.SampleDblClick(Sender: TObject);
begin

end;

procedure TBackground.FormClick(Sender: TObject);
//var
  //Item: TDragDropableItem;
begin
  //LSample.OriginalBounds;
  //LSample.Color := clWhite;
  //Grid.RandomizePositions;
  //Grid.RandomizeOrientations;
  //Button1Click(Self);
  //Item := TDragDropableItem(Components[i]);
  LStimuli.ResetGrid;
end;

procedure TBackground.Button1Click(Sender: TObject);
//var
//  Item : TDragDropableItem;
//  i: Integer;
begin
  LStimuli := TDragDropStimuli.Create(self);
  LStimuli.Start;
  PanelConfigurations.Hide;

  //ShowMessage(Sender.ClassName);
  //if Sender is TButton then
  //begin
  //  LAnimation := TAnimation.Create(Self);
  //  LAnimation.Parent := Self;
  //  with Grid.RandomPositions do begin
  //    for i := Low(Samples) to High(Samples) do
  //    begin
  //      Item := TDragDropableItem.Create(Self);
  //      Samples[i].Item := Item as TObject;
  //      Item.Parent := Self;
  //      //Item.EdgeColor := clBlue;
  //      Item.SetOriginalBounds(
  //        Samples[i].Left,
  //        Samples[i].Top,
  //        Samples[i].SquareSide,
  //        Samples[i].SquareSide);
  //      Item.Show;
  //      if i = 0 then
  //      begin
  //        Item.Caption := 'A'+(i+1).ToString;
  //        LSample := Item;
  //        LSample.OnRightDragDrop := @JoinAnimation;
  //        LSample.OnWrongDragDrop := @Wrong;
  //        LSample.OnOtherDragDrop := @Other;
  //        //LSample.Animate;
  //        LAnimation.Animate(LSample);
  //        LAnimation.Show;
  //      end;
  //    end;
  //
  //    for i := Low(Comparisons) to High(Comparisons) do
  //      begin
  //        Item := TDragDropableItem.Create(Self);
  //        Comparisons[i].Item := Item as TObject;
  //        LSample.AddTarget(Comparisons[i].Item);
  //        Item.Caption := 'B'+(i+1).ToString;
  //        Item.Parent := Self;
  //        Item.SetOriginalBounds(
  //          Comparisons[i].Left,
  //          Comparisons[i].Top,
  //          Comparisons[i].SquareSide,
  //          Comparisons[i].SquareSide);
  //        Item.Show;
  //
  //
  //        if i = 0 then
  //        begin
  //          Item.Caption := 'B'+(i+1).ToString;
  //        end;
  //      end;
  //    LSample.BringToFront;
  //    PanelConfigurations.Hide;
  //  end;
  //end;
  //
  //if Sender is TBackground then
  //begin
  //  with Grid.RandomPositions do begin
  //    for i := Low(Comparisons) to High(Comparisons) do
  //    begin
  //      Item := Comparisons[i].Item as TDragDropableItem;
  //      Item.SetOriginalBounds(
  //        Comparisons[i].Left,
  //        Comparisons[i].Top,
  //        Comparisons[i].SquareSide,
  //        Comparisons[i].SquareSide);
  //    end;
  //
  //    for i := Low(Samples) to High(Samples) do
  //    begin
  //      Item := Samples[i].Item as TDragDropableItem;
  //      Item.Invalidate;
  //      Item.SetOriginalBounds(
  //        Samples[i].Left,
  //        Samples[i].Top,
  //        Samples[i].SquareSide,
  //        Samples[i].SquareSide);
  //      LAnimation.Animate(Item);
  //      LAnimation.Show;
  //    end;
  //  end;
  //end;
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

procedure TBackground.JoinAnimation(Sender, Source: TObject; X,Y: Integer);
//var
//  Sample : TDragDropableItem;
//  Comparison : TDragDropableItem;
begin
  //Sample := Source as TDragDropableItem;
  //Comparison := Sender as TDragDropableItem;
  //
  //Sample.Color := clGreen;
  //Sample.Left := Comparison.Left;
  //Sample.Top := Comparison.Top - Sample.Height - 10;
  //LAnimation.Join(Comparison);
end;

procedure TBackground.Wrong(Sender, Source: TObject; X, Y: Integer);
//var
//  Sample : TDragDropableItem;
begin
  //Sample := Source as TDragDropableItem;
  //Sample.Color := clRed;
  //LAnimation.Animate(Sample);
end;

procedure TBackground.Other(Sender, Source: TObject; X, Y: Integer);
//var
//  Item : TDragDropableItem;
begin
  //Item := Source as TDragDropableItem;
  //Item.OriginalBounds;
  //Item.Color := clWhite;
end;


end.

