{
  Stimulus Control
  Copyright (C) 2014-2017 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Devices.PLPi;

{$mode objfpc}{$H+}

interface

// this unit was not finished yet

uses Classes, SysUtils

    , custom_timer
    ;

type

  {
    alternative
    http://wiki.freepascal.org/Hardware_Access#Using_inpout32.dll_for_Windows
  }

  { TPLP }

  TPLP = class (TComponent)
  private
    FTimer : TClockThread;
    FAutoOff : Boolean;
  public
    constructor Create (AOwner : TComponent); override;
    destructor Destroy; override;
    procedure OutPortOn (Data : BYTE);
    procedure OutPortOff (Sender: TObject);
    procedure OutPortNone (Sender: TObject);
    procedure OnTimerMethod(AutoOff: Boolean);

  end;

{$IFDEF Linux}
  //program need to be executed under/have root privileges
  function ioperm(from: Cardinal; num: Cardinal; turn_on: Integer): Integer; cdecl; external 'libc';
{$ENDIF}

{$IFDEF WINDOWS}
  function Inp32(EndPorta: integer): BYTE stdcall; external 'inpout32';
  procedure Out32(EndPorta: integer; Valor:BYTE); stdcall; external 'inpout32';
{$ENDIF}

var PLP : TPLP;

implementation

{TPLP}

constructor TPLP.Create(AOwner : TComponent);
begin

// $378 $278 $3BC
// Usual addresses of the PC Parallel Port
// In linux $3BC for /dev/lp0, $378 for /dev/lp1, and $278 for /dev/lp2
// For now, need to check the bios settings
// of the target machine before compile.

{$IFDEF Linux}
  if ioperm($378, 8, 1) = -1 then Showmessage('Error ioperm $378 on') else;
  //if ioperm($278, 8, 1) = -1 then Showmessage('Error ioperm $278') else;
  //if ioperm($3BC, 8, 1) = -1 then Showmessage('Error ioperm $3BC') else;
{$ENDIF}
end;

destructor TPLP.Destroy;
begin

{$IFDEF Linux}
  if ioperm($378, 8, 0) = -1 then Showmessage('Error ioperm $378 off') else;
  //if ioperm($278, 8, 1) = -1 then Showmessage('Error ioperm $278') else;
  //if ioperm($3BC, 8, 1) = -1 then Showmessage('Error ioperm $3BC') else;
{$ENDIF}
  inherited Destroy;
end;

procedure TPLP.OnTimerMethod(AutoOff: Boolean);
begin
  FAutoOff := AutoOff;
end;

procedure TPLP.OutPortNone (Sender: TObject);
begin

end;

procedure TPLP.OutPortOff (Sender: TObject);
begin
  FTimer.Terminate;

{$IFDEF WINDOWS}
  Out32($378, 0);
  //FOut32($278, 0);
  //FOut32($3BC, 0);
{$ENDIF}

{$IFDEF Linux}
  port[$378] := 0;
  //port[$278] := 0;
  //port[$3BC] := Data;
{$ENDIF}
end;

procedure TPLP.OutPortOn(Data: BYTE);
begin
{$IFDEF WINDOWS}
  Out32($378, Data);
  //FOut32($278, Data);
  //FOut32($3BC, Data);
{$ENDIF}

{$IFDEF Linux}
  port[$378] := Data;
  //port[$278] := Data;
  //port[$3BC] := Data;
{$ENDIF}

  FTimer := TClockThread.Create(True);
  FTimer.Interval := 50;
  if FAutoOff then
    FTimer.OnTimer := @OutPortOff
  else FTimer.OnTimer := @OutPortNone;
  FTimer.Start;
end;



end.
