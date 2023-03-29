{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.DragDrop;

{$mode objfpc}{$H+}

interface

uses LCLIntf, LCLType, Controls, Classes, SysUtils, ExtCtrls

  , Controls.Trials.Abstract
  , Stimuli.Sequence.DragDrop
  , Consequences
  , Timestamps
  ;

type

  TReportData = record
    SampleBegin   : string;
    DelayBegin    : string;
    ComparisonBegin   : string;
    ComparisonLatency : string;
    ComparisonChosen  : string;
  end;

  { TDragDrop }

  {
    Implements Drag Drop trials
  }
  TDragDrop = class(TTrial)
  private
    FHasConsequenceInterval : Boolean;
    FTimer : TTimer;
    FReportData : TReportData;
    FStimuli : TDragDropStimuli;
    //FParticipantResponse : TTBExpectedResponse;
    procedure Consequence(Sender, Source: TObject; X, Y: Integer);
    procedure WrongDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure StopInterval(Sender : TObject);
    procedure DragDropDone(Sender : TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    function GetHeader: string;
    //procedure WhiteScreen(Sender: TObject);
  protected
    procedure Paint; override;
    procedure TrialStart(Sender: TObject); virtual;
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    function AsString : string; override;
    function HasVisualConsequence: Boolean; override;
    function IsTestTrial : Boolean;
    function ConsequenceInterval: integer; override;

    // must load parameters os mock parameters before play
    procedure Play(ACorrection : Boolean); override;
    procedure LoadMockParameters; override;
  end;

implementation

uses Forms, Graphics
  , Constants, Cheats
  , Stimuli.Image.DragDropable
  //, Experiments.Grids
  , Session.Configuration.GlobalContainer
  , Devices.RS232i;

constructor TDragDrop.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;

  if Self.ClassType = TDragDrop then
    Header := Header + #9 + GetHeader;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 500;
  FTimer.OnTimer := @StopInterval;
  FStimuli := TDragDropStimuli.Create(Self);
  FStimuli.Parent := Self.Parent;
  FStimuli.OnRightDragDrop:=@Consequence;
  FStimuli.OnWrongDragDrop:=@WrongDragDrop;
  FStimuli.OnDragDropDone:=@DragDropDone;

  //FStimuli.LogEvent := @LogEvent;
  FResponseEnabled := False;
end;

function TDragDrop.AsString: string;
var
  LTrial : TStringList;
begin
  LTrial := TStringList.Create;
  LTrial.BeginUpdate;
  { implement me }
  LTrial.EndUpdate;
  Result := LTrial.Text;
  LTrial.Free;
end;

function TDragDrop.HasVisualConsequence: Boolean;
begin
  Result := (Self.Result <> T_NONE);
end;

function TDragDrop.IsTestTrial: Boolean;
begin
  Result := Configurations.Parameters.Values[_Consequence].ToBoolean;
end;

function TDragDrop.ConsequenceInterval: integer;
begin
  if FHasConsequenceInterval then
    Result := ConsequenceDuration
  else
    Result := 0;
end;

procedure TDragDrop.Play(ACorrection: Boolean);
var
  LParameters : TStringList;
begin
  inherited Play(ACorrection);
  FHasConsequenceInterval := True;
  FCounterType := ctNone;
  LParameters := Configurations.Parameters;
  FStimuli.LoadFromParameters(LParameters);

  if Self.ClassType = TDragDrop then Config(Self);
end;

procedure TDragDrop.LoadMockParameters;
begin
  with Configurations.Parameters do begin
    Values['Style.Samples.DragMode'] := dragFree.ToString;
    Values['Relation'] := 'A-A';
    Values['Samples'] := '3';
    Values['Comparisons'] := '3';
    Values['DragMoveFactor'] := '10';
  end;
end;

procedure TDragDrop.TrialStart(Sender: TObject);
begin
  FResponseEnabled:=False;
  FStimuli.Start;
  FReportData.SampleBegin := TimestampToStr(LogEvent(rsReportStmModBeg));

  if CheatsModeOn then begin
    //ParticipantBot.Start(FStimuli.AsInterface);
  end;
end;

procedure TDragDrop.WriteData(Sender: TObject);
var
  LSampleDuration: String;
begin
  inherited WriteData(Sender);
  //LSampleDuration := FStimuli.SampleDuration.ToString;
  Data := Data +
    LSampleDuration + HeaderTabs +
    FReportData.SampleBegin + HeaderTabs +
    FReportData.DelayBegin + HeaderTabs +
    FReportData.ComparisonBegin + HeaderTabs +
    FReportData.ComparisonLatency + HeaderTabs +
    FReportData.ComparisonChosen  + HeaderTabs;
end;

function TDragDrop.GetHeader: string;
begin
  Result :=
    'S.Modelo.DuracaoProgramada' + HeaderTabs +
    rsReportStmModBeg + HeaderTabs +
    'S.Atraso.Inicio' + HeaderTabs +
    rsReportStmCmpBeg + HeaderTabs +
    rsReportRspCmpLat + HeaderTabs +
    rsReportRspCmp    + HeaderTabs +
    'S.C1.Media' + HeaderTabs + 'S.C1.Posicao'    + HeaderTabs +
    'S.C2.Media' + HeaderTabs + 'S.C2.Posicao'    + HeaderTabs +
    'S.C3.Media' + HeaderTabs + 'S.C3.Posicao';
end;

procedure TDragDrop.Paint;
begin
  inherited Paint;
  if CheatsModeOn then begin

  end;
end;

procedure TDragDrop.Consequence(Sender, Source: TObject; X, Y: Integer);
begin
  RS232.Dispenser('3');

  //EndTrial(Sender);
end;

procedure TDragDrop.WrongDragDrop(Sender, Source: TObject; X, Y: Integer);
begin

end;

procedure TDragDrop.StopInterval(Sender: TObject);
begin
  if FTimer.Interval = 6000 then begin
    FStimuli.Start;
    FTimer.Enabled:=False;
    FTimer.Interval := 500;
  end else begin
    FTimer.Enabled:=False;
    FStimuli.Stop;
    FStimuli.ResetGrid;
    FTimer.Enabled:=True;
    FTimer.Interval := 6000;
    FTimer.Enabled:=True;
  end;
end;

procedure TDragDrop.DragDropDone(Sender: TObject);
begin
  FTimer.Enabled:=True;
end;

procedure TDragDrop.TrialBeforeEnd(Sender: TObject);
begin
  WriteData(Self);
end;


end.

