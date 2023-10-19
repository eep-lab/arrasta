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

uses LCLIntf, Controls, Classes, SysUtils, ExtCtrls

  , Session.Trials
  , Stimuli.Image.DragDropable
  , Session.Trial.HelpSeries.DragDrop
  , Controls.Trials.Abstract
  , Stimuli.Sequence.DragDrop
  , Forms.Main
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
    FUseHelpProgression : Boolean;
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
    procedure TrialLimitedHold(Sender: TObject);
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

uses Constants, Cheats
  //, Experiments.Grids
  , Session.Configuration.GlobalContainer
  , Devices.RS232i
  , Constants.DragDrop
  ;

constructor TDragDrop.Create(AOwner: TCustomControl);
begin
  inherited Create(AOwner);
  OnTrialBeforeEnd := @TrialBeforeEnd;
  OnTrialStart := @TrialStart;
  OnLimitedHold:= @TrialLimitedHold;

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

  FUseHelpProgression := False;
end;

function TDragDrop.AsString: string;
var
  LTrial : TStringList;
begin
  LTrial := TStringList.Create;
  LTrial.BeginUpdate;
  LTrial.Text := ToString + LineEnding + Configurations.Parameters.Text;
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
  Result:=Configurations.Parameters.Values[_ITI].ToInteger;
end;

procedure TDragDrop.Play;
var
  LParameters : TStringList;
  HasLimitedHold : Boolean;
begin
  inherited Play;
  FCounterType := ctNone;
  LParameters := Configurations.Parameters;
  //with DragDropKeys do
  //  FUseHelpProgression := LParameters.Values[UseHelpProgression].ToBoolean;
  HasLimitedHold := StrToIntDef(LParameters.Values[_LimitedHold], -1) > 0;
  if FUseHelpProgression or HasLimitedHold then begin
    if Counters.BlcTrials = 0 then begin
      IDragDropHelpSerie.AssignCurrent(LParameters);
    end;
    IDragDropHelpSerie.AssignParameters(LParameters);
  end;
  FStimuli.LoadFromParameters(LParameters);
  if Self.ClassType = TDragDrop then Config(Self);
end;

procedure TDragDrop.LoadMockParameters;
begin
  with Configurations.Parameters do begin
    with DragDropKeys do begin
      //Values[UseHelpProgression] := 'False';
      Values[RepeatTrials] := '1';
      //Values[SamplesDragMode] := dragFree.ToString;
      Values[Distance] := '0';
      Values[Relation] := 'A-A';
      Values[Samples] := '3';
      Values[Comparisons] := '3';
      //Values[DragMoveFactor] := '10';
      Values[DragDropOrientation] := goRandom.ToString;
    end;
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
var
  i : integer;
begin
  inherited WriteData(Sender);
  with FReportData do begin
    with FStimuli do begin
      for i := 0 to Background.SpinEditSamples.Value - 1 do begin
        Data := Data + Samples[i].GetOriginalBoundsAsString + HeaderTabs;
      end;
      for i := 0 to Background.SpinEditComparisons.Value - 1 do begin
        Data := Data + Comparisons[i].GetOriginalBoundsAsString + HeaderTabs;
      end;
      Data := Data + GetLatency(StimuliStart, Latency);
    end;
  end;
end;

procedure TDragDrop.TrialLimitedHold(Sender: TObject);
begin
  FStimuli.Stop;
  IDragDropHelpSerie.Iterator.Previous;
  Result := 'Espera';
end;

function TDragDrop.GetHeader: string;
var
  LSamplesCount : integer;
  LComparisonsCount: integer;
begin
  LSamplesCount := Background.SpinEditSamples.Value;
  LComparisonsCount := Background.SpinEditComparisons.Value;
  if LSamplesCount = 1 then begin
    case LComparisonsCount of
      1 : Result := rsReportA1PosInitial + HeaderTabs +
                    rsReportB1PosInitial + HeaderTabs +
                    rsReportRspLat;
      2 : Result := rsReportA1PosInitial + HeaderTabs +
                    rsReportB1PosInitial + HeaderTabs +
                    rsReportB2PosInitial + HeaderTabs +
                    rsReportRspLat;
      else
        Result := rsReportA1PosInitial + HeaderTabs +
                  rsReportB1PosInitial + HeaderTabs +
                  rsReportB2PosInitial + HeaderTabs +
                  rsReportB3PosInitial + HeaderTabs +
                  rsReportRspLat;
    end;
  end
  else if LSamplesCount = 2 then begin
    case LComparisonsCount of
      2: Result := rsReportA1PosInitial + HeaderTabs +
                   rsReportA2PosInitial + HeaderTabs +
                   rsReportB1PosInitial + HeaderTabs +
                   rsReportB2PosInitial + HeaderTabs +
                   rsReportRspLat;
      else
        Result := rsReportA1PosInitial + HeaderTabs +
                  rsReportA2PosInitial + HeaderTabs +
                  rsReportB1PosInitial + HeaderTabs +
                  rsReportB2PosInitial + HeaderTabs +
                  rsReportB3PosInitial + HeaderTabs +
                  rsReportRspLat;
    end;
  end
  else begin
    Result := rsReportA1PosInitial + HeaderTabs +
              rsReportA2PosInitial + HeaderTabs +
              rsReportA3PosInitial + HeaderTabs +
              rsReportB1PosInitial + HeaderTabs +
              rsReportB2PosInitial + HeaderTabs +
              rsReportB3PosInitial + HeaderTabs +
              rsReportRspLat;
  end;
end;

procedure TDragDrop.Paint;
begin
  inherited Paint;
  if CheatsModeOn then begin

  end;
end;

procedure TDragDrop.Response(Sender: TObject);
begin
  ResetLimitedHold;
  if FReportData.Latency < 0 then begin
    FReportData.Latency := LogEvent(rsReportRspLat);
  end else begin
    LogEvent('R');
  end;
end;

procedure TDragDrop.OtherDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Sample : TDragDropableItem;
begin
  Sample := Source as TDragDropableItem;
  LogEvent('Outro' + HeaderTabs +
    Sample.ShortName + HeaderTabs + X.ToString + #32 + Y.ToString);
end;

procedure TDragDrop.RightDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  Comparison : TDragDropableItem;
  Sample : TDragDropableItem;
begin
  RS232.Dispenser;
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
  LimitedHold := 0;
  //if FUseHelpProgression then
  //  IDragDropHelpSerie.Iterator.Next;
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

