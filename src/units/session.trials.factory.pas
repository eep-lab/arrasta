{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Session.Trials.Factory;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, fgl, Session.Trials, Controls.Trials.Abstract;

type

  TTrialClass = class of TTrial;

  TTrialRegistry = specialize TFPGMap<string, TTrialClass>;

  { TTrialFactory }

  TTrialFactory = class
    private
      class var FCurrentTrial: TTrial;
      class var FRegistry: TTrialRegistry;
      class function GetRegistry: TTrialRegistry; static;
      class constructor Create;
      class destructor Destroy;
    public
      class procedure RegisterTrialClass(
        ATrialKind: string; ATrialClass: TTrialClass); static;
      class function GetNewTrial : ITrial; static;
  end;

implementation

uses Constants
   , Session.InterTrialEvents
   , Session.Backgrounds
   , Session.Configuration
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   , Controls.Trials.HTMLMessage
   , Controls.Trials.DragDrop
   ;

{ TTrialFactory }

class function TTrialFactory.GetRegistry: TTrialRegistry;
begin
  if not Assigned(FRegistry) then
    FRegistry := TTrialRegistry.Create;
  Result := FRegistry;
end;

class constructor TTrialFactory.Create;
begin
  FCurrentTrial := nil;
  FRegistry := nil;
end;

class destructor TTrialFactory.Destroy;
begin
  if Assigned(FRegistry) then
    FRegistry.Free;
end;

class procedure TTrialFactory.RegisterTrialClass(ATrialKind: string;
  ATrialClass: TTrialClass);
begin
  GetRegistry[ATrialKind] := ATrialClass;
end;

class function TTrialFactory.GetNewTrial: ITrial;
var
  Configurations : TCfgTrial;
  TrialClass : TTrialClass;
begin
  if Assigned(FCurrentTrial) then
    FreeAndNil(FCurrentTrial);

  Configurations :=
    ConfigurationFile.Trial[Counters.CurrentBlc+1, Counters.CurrentTrial+1];

  if not FRegistry.TryGetData(Configurations.Kind, TrialClass) then
    raise EArgumentException.CreateFmt(
      'Trial kind is not registered: %s %s', [Configurations.Kind, TrialClass]);

  FCurrentTrial := TrialClass.Create(Background);
  FCurrentTrial.Configurations := Configurations;
  FCurrentTrial.OnTrialEnd := InterTrialEvents.OnTrialEnd;
  Result := FCurrentTrial as ITrial;
end;

initialization
  TTrialFactory.RegisterTrialClass(T_HTMLMESSAGE, THTMLMessage);
  TTrialFactory.RegisterTrialClass(T_DRAGDROP, TDragDrop);

end.

