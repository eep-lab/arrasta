{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Session;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Session.Blocs;

type

  { TSession }

  TSession = class(TComponent)
  private
    FTimer : TTimer;
    FBloc : TBloc;
    FOnBeforeStart: TNotifyEvent;
    FOnEndSession: TNotifyEvent;
    function GetBaseFilename: string;
    procedure PlayBloc;
    procedure TimerOnTimer(Sender : TObject);
    procedure EndBloc(Sender : TObject);
    procedure EndSession;
    procedure SetOnBeforeStart(AValue: TNotifyEvent);
    procedure SetOnEndSession(AValue: TNotifyEvent);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
    procedure Play(ASessionName : string; AParticipantName: string);
    property OnEndSession : TNotifyEvent read FOnEndSession write SetOnEndSession;
    property OnBeforeStart : TNotifyEvent read FOnBeforeStart write SetOnBeforeStart;
    property BaseFilename : string read GetBaseFilename;
    property Timer : TTimer read FTimer;
  end;

implementation

uses
  Timestamps
, FileUtil
, LazFileUtils
, Loggers.Reports
, Loggers
, Session.ConfigurationFile
, Session.Configuration.GlobalContainer
;

{ TSession }

var
  Header : string;
  FirstFilename: string = '001';

procedure TSession.PlayBloc;
begin
  EndCriteria.Invalidate;
  if EndCriteria.OfSession then begin
    EndSession;
  end else begin
    FBloc.BeforePlay;
    Counters.SetVirtualTrialValue(
      ConfigurationFile.Bloc[Counters.CurrentBlc+1].VirtualTrialValue);
    FBloc.Play;
  end;
end;

procedure TSession.TimerOnTimer(Sender: TObject);
begin
  EndSession;
end;

function TSession.GetBaseFilename: string;
begin
  if DataFilename = '' then
    Result := FirstFilename
  else
    Result := ExtractFileNameWithoutExt(DataFilename);
end;

procedure TSession.EndBloc(Sender: TObject);
begin
  Counters.OnEndBlc(Self);
  PlayBloc;
end;

procedure TSession.EndSession;
var
  Footer : string;
begin
  Footer := HEND_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time)+ LineEnding;
  FreeLogger(LGTimestamps,Footer);
  FreeLogger(LGData, Footer);
  if Assigned(OnEndSession) then OnEndSession(Self);
end;

procedure TSession.SetOnBeforeStart(AValue: TNotifyEvent);
begin
  if FOnBeforeStart=AValue then Exit;
  FOnBeforeStart:=AValue;
end;

procedure TSession.SetOnEndSession(AValue: TNotifyEvent);
begin
  if FOnEndSession=AValue then Exit;
  FOnEndSession:=AValue;
end;

constructor TSession.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(Self);
  FTimer.Enabled:=False;
  FTimer.OnTimer:=@TimerOnTimer;
  FBloc := TBloc.Create(Self);
  FBloc.OnEndBloc := @EndBloc;
  //FBloc.OnInterTrialEnd := @InterTrialStop;
end;

destructor TSession.Destroy;
begin
  FTimer.Free;
  ConfigurationFile.Free;
  inherited Destroy;
end;

procedure TSession.Play(ASessionName: string; AParticipantName: string);
begin
  FirstFilename := GlobalContainer.RootData + FirstFilename;
  Header := HSUBJECT_NAME + #9 + AParticipantName + LineEnding +
            HSESSION_NAME + #9 + ASessionName + LineEnding +
            HBEGIN_TIME + #9 + DateTimeToStr(Date) + #9 + TimeToStr(Time) + LineEnding;
  DataFilename := CreateLogger(LGData, FirstFilename, Header);
  TimestampsFilename := CreateLogger(LGTimestamps, FirstFilename, Header);
  if Assigned(OnBeforeStart) then OnBeforeStart(Self);

  GlobalContainer.TimeStart := TickCount;
  GlobalContainer.BaseFilename := BaseFilename;
  FTimer.Enabled:=True;
  PlayBloc;
end;

end.

