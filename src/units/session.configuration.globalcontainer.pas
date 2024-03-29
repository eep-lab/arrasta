{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Session.Configuration.GlobalContainer;

{$mode objfpc}{$H+}

interface

uses
  Session.EndCriteria, CounterManager;

type

  { TGlobalContainer }

  TGlobalContainer = class
    {$IFNDEF NO_LIBZMQ}
    PupilClient: TPupilClient;
    {$ENDIF}
    PupilEnabled : Boolean;
    RootData : string;
    RootMedia : string;
    BaseFileName : string;
    ExeName : string;
    TimeStart : Extended;
    TestMode : Boolean;
    MonitorToShow : Byte;
  end;
var
  GlobalContainer : TGlobalContainer;
  Counters : TCounterManager;
  EndCriteria : TEndCriteria;

implementation

uses SysUtils, Forms;

initialization
  Counters := TCounterManager.Create(nil);
  GlobalContainer := TGlobalContainer.Create;
  EndCriteria := TEndCriteria.Create;

  with GlobalContainer do
  begin
    BaseFileName := '';
    ExeName := Application.ExeName;
    RootData := ExtractFilePath(ExeName) + 'data' + DirectorySeparator;
    RootMedia := ExtractFilePath(ExeName) + 'media' + DirectorySeparator;
    ForceDirectories(RootData);
    ForceDirectories(RootMedia);
    MonitorToShow := 0;
    PupilEnabled := False;
    TestMode := False;
  end

finalization
  GlobalContainer.Free;
  Counters.Free;
  EndCriteria.Free;

end.

