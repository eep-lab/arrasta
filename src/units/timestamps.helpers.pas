{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Timestamps.Helpers;

{$mode objfpc}{$H+}

interface

uses
  SysUtils

{$ifdef LINUX}
  , Linux
  , UnixType
{$endif}

{$ifdef WINDOWS}
  , epiktimer
{$endif}

{$ifdef DARWIN}
  , ctypes
  , MachTime
{$endif}
  ;

  function GetCustomTick : Extended;

{$ifdef LINUX}
  function GetMonotonicTime : timespec;
  function GetMonotonicTimeRaw : timespec;
  function GetClockResolution : string; // granularity
{$endif}

{$ifdef WINDOWS}
  procedure StartEpiktimer;
{$endif}

implementation

{$ifdef LINUX}
function GetCustomTick: Extended;
var
  tp: timespec;
  a, b : Extended;
begin
  clock_gettime(CLOCK_MONOTONIC, @tp);
  a := Extended(tp.tv_sec);
  b := Extended(tp.tv_nsec) * 1e-9;
  Result := a+b;
end;


function GetMonotonicTime: timespec;
var
  tp: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC, @tp);
  Result := tp;
end;

function GetMonotonicTimeRaw: timespec;
var
  tp: timespec;
begin
  clock_gettime(CLOCK_MONOTONIC_RAW, @tp);
  Result := tp;
end;

function GetClockResolution: string;
var
  tp: timespec;
begin
  clock_getres(CLOCK_MONOTONIC, @tp);
  Result := IntToStr(tp.tv_sec) + '.' + FloatToStr(tp.tv_nsec * 1e-9);
end;
{$endif}

{$ifdef WINDOWS}
var
  ET: TEpikTimer;

procedure StartEpiktimer;
begin
  ET.Start;
end;


function GetCustomTick: Extended;
begin
  Result := ET.GetSystemTicks * 1e-7;
end;

initialization
  ET := TEpikTimer.Create(nil);
  ET.Clear;

finalization
  ET.Free;
{$endif}

{$ifdef DARWIN}
{credits: https://github.com/pupil-labs/pyuvc/blob/master/pyuvc-source/darwin_time.pxi}

var
  timeConvert: Extended = 0.0;

//function get_sys_time_monotonic: Extended;
function GetCustomTick : Extended;
var
  timeBase: mach_timebase_info_data_t;
begin
  if timeConvert = 0.0 then begin
    mach_timebase_info(@timeBase);
    timeConvert := (timeBase.numer / timeBase.denom) / 1000000000.0;
  end;
  Result := mach_absolute_time() * timeConvert;
end;

{$endif}
end.

