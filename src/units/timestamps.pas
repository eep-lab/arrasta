{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Timestamps;

{$mode objfpc}{$H+}

interface

uses  SysUtils;

procedure StartEpikTimer;
function GetLatency(AStart, ALatency : Extended) : string;
function Elapsed : Extended;
function GetTimeStampF : string; overload;
function GetTimeStampF(ATimestamp:Extended): string; overload;
function TickCount : Extended;
function TimestampToStr(ATimestamp: Extended) : string;
function TimestampToStrDelta(ATimestamp: Extended) : string;

implementation

uses
    Session.Configuration.GlobalContainer
    , Timestamps.Helpers;

procedure StartEpikTimer;
begin
  {$IFDEF WINDOWS}
  Timestamps.Helpers.StartEpiktimer;
  {$ENDIF}
  GlobalContainer.TimeStart := TickCount;
end;

function GetLatency(AStart, ALatency: Extended): string;
begin
  if ALatency > 0 then begin
      Result := TimestampToStr(ALatency - AStart);
    end else begin
      Result := 'NA';
    end;
end;

function Elapsed: Extended;
begin
  Result := GetCustomTick - GlobalContainer.TimeStart;
end;

function GetTimeStampF: string;
begin
  Result:=FloatToStrF(GetCustomTick,ffFixed,0,9)
end;

function GetTimeStampF(ATimestamp: Extended): string;
begin
  Result:=FloatToStrF(GetCustomTick-ATimestamp,ffFixed,0,9)
end;

function TickCount: Extended;
begin
  Result := GetCustomTick;
end;

function TimestampToStr(ATimestamp: Extended): string;
begin
  Result:=FloatToStrF(ATimestamp,ffFixed,0,9)
end;

function TimestampToStrDelta(ATimestamp : Extended) : string;
begin
  Result:=FloatToStrF(GlobalContainer.TimeStart - ATimestamp,ffFixed,0,9)
end;


end.

