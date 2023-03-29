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
    WrongDragDrops   : integer;
    Latency          : Extended;
  end;

  { TDragDrop }

  {
    Implements Drag Drop trials
  }
  TDragDrop = class(TTrial)
  private
    FTimer : TTimer;
    FReportData : TReportData;
    FStimuli : TDragDropStimuli;
    procedure Response(Sender: TObject);
    procedure OtherDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure RightDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure WrongDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure StopInterval(Sender : TObject);
    procedure DragDropDone(Sender : TObject);
    procedure TrialBeforeEnd(Sender: TObject);
    function GetHeader: string;
  protected
    procedure Paint; override;
    procedure TrialStart(Sender: TObject); virtual;
    procedure WriteData(Sender: TObject); override;
  public
    constructor Create(AOwner: TCustomControl); override;
    function AsString : string; override;
    function IsTestTrial : Boolean;

    // must load parameters or mock parameters before play
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
  FStimuli.OnOtherDragDrop:=@OtherDragDrop;
  FStimuli.OnRightDragDrop:=@RightDragDrop;
  FStimuli.OnWrongDragDrop:=@WrongDragDrop;
  FStimuli.OnDragDropDone:=@DragDropDone;

  //FStimuli.LogEvent := @LogEvent;
  FReportData.WrongDragDrops := 0;
  FReportData.Latency := -1;
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

function TDragDrop.IsTestTrial: Boolean;
begin
  Result := Configurations.Parameters.Values[_Consequence].ToBoolean;
end;

procedure TDragDrop.Play(ACorrection: Boolean);
var
  LParameters : TStringList;
begin
  inherited Play(ACorrection);
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
  FStimuli.Start;
  FReportData.Latency := LogEvent(rsReportStmModBeg);
  if CheatsModeOn then begin
    //ParticipantBot.Start(FStimuli.AsInterface);
  end;
end;

procedure TDragDrop.WriteData(Sender: TObject);
begin
  inherited WriteData(Sender);
  Data := Data +
    TimestampToStr(FTrialStart - FReportData.Latency);
end;

function TDragDrop.GetHeader: string;
begin
  Result :=
    rsReportRspLat // + HeaderTabs +
    ;
end;

procedure TDragDrop.Paint;
begin
  inherited Paint;
  if CheatsModeOn then begin

  end;
end;

procedure TDragDrop.Response(Sender: TObject);
begin
  if FReportData.Latency < 0 then
    FReportData.Latency := LogEvent(rsReportRspLat);
end;

procedure TDragDrop.OtherDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Sample : TDragDropableItem;
begin
  RS232.Dispenser('3');
  Sample := Source as TDragDropableItem;
  LogEvent('OtherDragDrop' + HeaderTabs +
    Sample.ShortName + '-' + X.ToString + #32 + Y.ToString);
end;

procedure TDragDrop.RightDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Comparison : TDragDropableItem;
  Sample : TDragDropableItem;
begin
  RS232.Dispenser('3');
  Comparison := Sender as TDragDropableItem;
  Sample := Source as TDragDropableItem;
  LogEvent('RightDragDrop' + HeaderTabs +
    Sample.ShortName + '-' + Comparison.ShortName);
end;

procedure TDragDrop.WrongDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Comparison : TDragDropableItem;
  Sample : TDragDropableItem;
begin
  Inc(FReportData.WrongDragDrops);
  Comparison := Sender as TDragDropableItem;
  Sample := Source as TDragDropableItem;
  LogEvent('WrongDragDrop' + HeaderTabs +
    Sample.ShortName + '-' + Comparison.ShortName);
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
  if FReportData.WrongDragDrops = 0 then begin
    LogEvent('HIT1' + HeaderTabs +
      TimestampToStr(TickCount - FTrialStart));
  end else begin
    LogEvent('HIT2' + HeaderTabs +
      FReportData.WrongDragDrops.ToString);
  end;
  //EndTrial(Sender);
end;

procedure TDragDrop.TrialBeforeEnd(Sender: TObject);
begin
  WriteData(Self);
end;


end.

