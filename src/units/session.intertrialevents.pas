{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Session.InterTrialEvents;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, StdCtrls, Controls
  , SerialTimer
  , Session.Trials
  ;

type

  { TInterTrialEvents }

  TInterTrialEvents = class(TComponent)
  private
    FSerialTimer : TSerialTimer;
    FInterTrial : TTimerItem;
    FDelay : TTimerItem;
    FConsequenceDuration : TTimerItem;
    FNextTrial : string;
    //FWaitLabel : TLabel;
    //FConsequence : TStimulusFigure;
    FLastTrialHeader : string;
    procedure InterTrialConsequenceBegin;
    procedure InterTrialIntervalBegin;
    function HasDelay : Boolean;
    function HasConsequenceDuration : Boolean;
    function HasInterTrialTime : Boolean;
    procedure TrialEnd(Sender: TObject);
  private
    FOnInterTrialEnd: TNotifyEvent;
    FOnTrialEnd: TNotifyEvent;
    procedure InterTrialConsequenceEnd(Sender: TObject);
    procedure DelayEnd(Sender: TObject);
    procedure InterTrialEnd(Sender: TObject);
    procedure SetOnInterTrialEnd(AValue: TNotifyEvent);
    procedure SetOnTrialEnd(AValue: TNotifyEvent);
  public
    constructor Create(AOwner : TComponent); override;
    property OnInterTrialEnd : TNotifyEvent read FOnInterTrialEnd write SetOnInterTrialEnd;
    property OnTrialEnd : TNotifyEvent read FOnTrialEnd write SetOnTrialEnd;
  end;

var
  InterTrialEvents : TInterTrialEvents;

implementation

uses Constants
   , Timestamps
   , Loggers
   , Session.Backgrounds
   , Session.Configuration.GlobalContainer
   , Controls.Trials.Abstract
   ;

{ TInterTrialEvents }

procedure TInterTrialEvents.SetOnTrialEnd(AValue: TNotifyEvent);
begin
  if FOnTrialEnd=AValue then Exit;
  FOnTrialEnd:=AValue;
end;

procedure TInterTrialEvents.TrialEnd(Sender: TObject);
var
  LTrial : TTrial;
begin
  LTrial := TTrial(Sender);
  LTrial.Hide;

  Background.Cursor := -1;
  FDelay.Interval := LTrial.ConsequenceDelay;
  FConsequenceDuration.Interval := LTrial.ConsequenceInterval;
  FInterTrial.Interval := LTrial.InterTrialInterval;

  if HasDelay then begin
    FSerialTimer.Append(FDelay);
  end;

  if HasConsequenceDuration then begin
    FSerialTimer.Append(FConsequenceDuration);
  end;

  if HasInterTrialTime then begin
    FSerialTimer.Append(FInterTrial);
  end;

  if HasDelay then begin
    FSerialTimer.Start;
    Exit;
  end;

  if HasConsequenceDuration then begin
    InterTrialConsequenceBegin;
    FSerialTimer.Start;
    Exit;
  end;

  if HasInterTrialTime then begin
    InterTrialIntervalBegin;
    FSerialTimer.Start;
    Exit;
  end;

  InterTrialEnd(Sender);
end;

function TInterTrialEvents.HasDelay: Boolean;
begin
  Result := FDelay.Interval > 0;
end;

function TInterTrialEvents.HasConsequenceDuration: Boolean;
begin
  Result := FConsequenceDuration.Interval > 0;
end;

function TInterTrialEvents.HasInterTrialTime: Boolean;
begin
  Result := FInterTrial.Interval > 0;
end;

procedure TInterTrialEvents.DelayEnd(Sender: TObject);
begin
  if HasConsequenceDuration then begin
    InterTrialConsequenceBegin;
    Exit;
  end;

  if HasInterTrialTime then begin
    InterTrialIntervalBegin;
  end;
end;

procedure TInterTrialEvents.InterTrialConsequenceBegin;
begin
  case TrialResult of
    'HIT+BLACKOUT':
      begin

      end;
    'HIT' :
      begin

      end;

    'MISS':
      begin

      end;
  end;
  TrialResult := '';
end;

procedure TInterTrialEvents.InterTrialConsequenceEnd(Sender: TObject);
begin
  if HasInterTrialTime then begin
    InterTrialIntervalBegin;
  end;
end;

procedure TInterTrialEvents.InterTrialIntervalBegin;
begin
  ITIBegin := TickCount - GlobalContainer.TimeStart;
end;

procedure TInterTrialEvents.InterTrialEnd(Sender: TObject);
begin
  if Sender is TSerialTimer then begin
    FSerialTimer.Stop;
    FSerialTimer.Clear;
  end;

  FDelay.Interval := 0;
  FConsequenceDuration.Interval := 0;
  FInterTrial.Interval := 0;

  ITIEnd := TickCount - GlobalContainer.TimeStart;
  WriteDataRow;

  if Assigned(OnInterTrialEnd) then
    OnInterTrialEnd(Self);
end;

procedure TInterTrialEvents.SetOnInterTrialEnd(AValue: TNotifyEvent);
begin
  if FOnInterTrialEnd=AValue then Exit;
  FOnInterTrialEnd:=AValue;
end;

constructor TInterTrialEvents.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOnTrialEnd := @TrialEnd;
  FSerialTimer := TSerialTimer.Create(Self);

  FDelay.OnTimerEvent := @DelayEnd;
  FConsequenceDuration.OnTimerEvent := @InterTrialConsequenceEnd;
  FSerialTimer.OnEndTimeSerie := @InterTrialEnd;

  FLastTrialHeader := '';
  ITIBegin := 0;
  ITIEnd := 0;
end;

end.
