{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Session.Blocs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils
  , Session.Trials
  ;

type

  { TBloc }

  {
    Controls Trials Create/Destroy cycle
  }
  TBloc = class(TComponent)
  private
    FTrial : ITrial;
    FOnEndBloc: TNotifyEvent;
    FOnInterTrialEnd: TNextTrialEvent;
    procedure SetOnEndBloc(AValue: TNotifyEvent);
    procedure InterTrialEventsEnd(NextTrialCode: string);
    procedure SetOnInterTrialEnd(AValue: TNextTrialEvent);
  public
    constructor Create(AOwner : TComponent); override;
    procedure BeforePlay;
    procedure Play;
    property OnEndBloc : TNotifyEvent read FOnEndBloc write SetOnEndBloc;
    property OnInterTrialEnd : TNextTrialEvent read FOnInterTrialEnd write SetOnInterTrialEnd;
  end;

implementation

uses Session.Backgrounds
   , Session.EndCriteria
   , Session.Trials.Factory
   , Session.InterTrialEvents
   , Session.Configuration.GlobalContainer
   ;

{ TBloc }

procedure TBloc.SetOnEndBloc(AValue: TNotifyEvent);
begin
  if FOnEndBloc=AValue then Exit;
  FOnEndBloc:=AValue;
end;

procedure TBloc.InterTrialEventsEnd(NextTrialCode: string);
var
  LNextTrial: Integer;
begin
  LNextTrial := StrToIntDef(NextTrialCode, 1);
  Counters.CurrentTrial := Counters.CurrentTrial+LNextTrial;
  if Counters.CurrentTrial < 0 then
    Exception.Create('Exception. CurrentTrial cannot be less than zero.');
  Play;
end;

procedure TBloc.SetOnInterTrialEnd(AValue: TNextTrialEvent);
begin
  if FOnInterTrialEnd=AValue then Exit;
  FOnInterTrialEnd:=AValue;
end;

constructor TBloc.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  InterTrialEvents := TInterTrialEvents.Create(Self);
  InterTrialEvents.OnInterTrialEnd:=@InterTrialEventsEnd;
end;

procedure TBloc.BeforePlay;
begin
  Background.Cursor := -1;
end;

procedure TBloc.Play;
begin
  EndCriteria.Invalidate;
  if EndCriteria.OfBloc then begin
    if Assigned(OnEndBloc) then OnEndBloc(Self);
  end else begin
    FTrial := TTrialFactory.NewTrial;
    FTrial.Play;
  end;
end;

end.
