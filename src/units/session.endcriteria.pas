{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Session.EndCriteria;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils
  , Session.Configuration
  ;

type

  { TEndCriteria }

  TEndCriteria = class
  private
    FCurrentBloc : TCfgBlc;
    //FTrial : TCfgTrial;
    procedure EndBlocOnEndTrial;
    procedure EndSessionOnEndBloc;
    function HitPorcentageInBloc : real;
  public
    constructor Create;
    procedure Invalidate;
    function OfSession : Boolean;
    function OfBloc : Boolean;
    function OfTrial : Boolean;
  end;

var
  EndCriteria : TEndCriteria;

implementation

uses
  Session.Configuration.GlobalContainer,
  Session.ConfigurationFile;

{ TEndCriteria }

constructor TEndCriteria.Create;
begin
  Counters.OnBeginSess(Self);
end;

procedure TEndCriteria.Invalidate;
begin
  FCurrentBloc := ConfigurationFile.CurrentBloc;
  //LCurrentTrial := Counters.CurrentTrial;
end;

function TEndCriteria.OfSession: Boolean;
begin
  EndSessionOnEndBloc;
  Result := Counters.CurrentBlc >= ConfigurationFile.BlocCount;
end;

function TEndCriteria.OfBloc: Boolean;
begin
  EndBlocOnEndTrial;
  Result := Counters.CurrentTrial >= FCurrentBloc.TotalTrials;
end;

function TEndCriteria.OfTrial: Boolean;
var
  RepeatTrial : integer;
  S1 : string;
begin
  Result := True;
  if Assigned(ConfigurationFile) then begin
    S1 := ConfigurationFile.CurrentTrial.Parameters.Values['RepeatTrial'];
  end else begin
    S1 := '0';
  end;
  RepeatTrial := StrToIntDef(S1, 0) -1;
  if RepeatTrial > 0 then begin
    if Counters.RepeatedTrials < RepeatTrial then begin
      Result := False;
      Counters.RepeatedTrials := Counters.RepeatedTrials +1;
    end else begin
      Counters.RepeatedTrials := 0;
    end;
  end;
end;

procedure TEndCriteria.EndBlocOnEndTrial;
  procedure EndBloc;
  begin
    Counters.CurrentTrial := FCurrentBloc.TotalTrials;
  end;

begin
  if FCurrentBloc.CrtConsecutiveHit > 0 then begin
    if Counters.BlcCscHits >= FCurrentBloc.CrtConsecutiveHit then begin
      EndBloc;
      Exit;
    end;
  end;

  if FCurrentBloc.CrtMaxTrials > 0 then begin
    if Counters.BlcTrials >= FCurrentBloc.CrtMaxTrials then begin
      EndBloc;
      Exit;
    end;
  end;
end;

procedure TEndCriteria.EndSessionOnEndBloc;
  procedure EndSession;
  begin
    if FCurrentBloc.MaxBlcRepetition > 0 then begin
      if (Counters.BlcRepetitions < FCurrentBloc.MaxBlcRepetition) then begin
        Counters.OnRepeatBlc(Self);
        Exit;
      end;
    end;

    if FCurrentBloc.AutoEndSession then begin
      { End session }
    end else begin
      Exit;
    end;

    Counters.CurrentBlc := ConfigurationFile.BlocCount;
  end;
  procedure NextBlocOnCriteria;
  begin
    if FCurrentBloc.NextBlocOnCriteria > 0 then begin
      Counters.CurrentBlc := FCurrentBloc.NextBlocOnCriteria-1;
    end;
  end;
begin
  if (FCurrentBloc.CrtHitValue > 0) then begin
    if (Counters.BlcHits < FCurrentBloc.CrtHitValue) then begin
      EndSession;
    end else begin
      NextBlocOnCriteria;
    end;
  end;

  if (FCurrentBloc.CrtHitPorcentage > 0) and
     (FCurrentBloc.CrtHitPorcentage <= 100) then begin
    if (HitPorcentageInBloc < FCurrentBloc.CrtHitPorcentage) then begin
      EndSession;
    end else begin
      NextBlocOnCriteria;
    end;
  end;
end;


function TEndCriteria.HitPorcentageInBloc: real;
begin
  Result := (Counters.BlcHits * 100)/FCurrentBloc.TotalTrials;
end;


end.

