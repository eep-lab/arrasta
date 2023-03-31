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

  , Session.Trials
  , Controls.Trials.Abstract
  , Stimuli.Sequence.DragDrop
  , Consequences
  , Timestamps
  ;

type

  TReportData = record
    WrongDragDrops   : integer;
    Latency          : Extended;
    StimuliStart     : Extended;
  end;

  { TDragDrop }

  {
    Implements Drag Drop trials
  }
  TDragDrop = class(TTrial, ITrial)
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
    function ConsequenceInterval: Cardinal; override;
    function ConsequenceDelay: Cardinal; override;
    function InterTrialInterval: Cardinal; override;

    // must load parameters or mock parameters before play
    procedure Play; override;
    procedure LoadMockParameters; override;
  end;

implementation

uses Forms, Graphics, Session.Backgrounds
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
  FStimuli.OnResponse:=@Response;
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

function TDragDrop.ConsequenceInterval: Cardinal;
begin
  Result:=0;
end;

function TDragDrop.ConsequenceDelay: Cardinal;
begin
  Result:=0;
end;

function TDragDrop.InterTrialInterval: Cardinal;
begin
  Result:=Configurations.Parameters.Values['ITI'].ToInteger;
end;

procedure TDragDrop.Play;
var
  LParameters : TStringList;
begin
  inherited Play;
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
  FReportData.StimuliStart := LogEvent('Stimuli.Start');
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
  LogEvent('Outro' + HeaderTabs +
    Sample.ShortName + HeaderTabs + X.ToString + #32 + Y.ToString);
end;

procedure TDragDrop.RightDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Comparison : TDragDropableItem;
  Sample : TDragDropableItem;
begin
  RS232.Dispenser('3');
  Comparison := Sender as TDragDropableItem;
  Sample := Source as TDragDropableItem;
  LogEvent('Correto' + HeaderTabs +
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
  LogEvent('Errado' + HeaderTabs +
    Sample.ShortName + '-' + Comparison.ShortName);
end;

procedure TDragDrop.StopInterval(Sender: TObject);
begin
  FTimer.Enabled:=False;
  FStimuli.Stop;
  EndTrial(Self);
end;

procedure TDragDrop.DragDropDone(Sender: TObject);
begin
  FTimer.Enabled:=True;
  if FReportData.WrongDragDrops = 0 then begin
    Result := 'Acerto1';
    LogEvent(Result);
  end else begin
    Result := 'Acerto2';
    LogEvent(Result + HeaderTabs +
      FReportData.WrongDragDrops.ToString);
  end;
end;

procedure TDragDrop.TrialBeforeEnd(Sender: TObject);
begin
  WriteData(Self);
end;


end.

