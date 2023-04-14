{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Session.Configurationfile.Writer;

interface

uses
  Classes, Session.ConfigurationFile;

type

  { TConfigurationWriter }

  TConfigurationWriter = class
  private
    FBlocConfig: TStringList;
    FTrialConfig: TStringList;
    FCurrentBloc : integer;
    FConfigurationFile: TConfigurationFile;
    function GetCurrentTrial: integer;
  public
    constructor Create(AConfigurationFile: TConfigurationFile); reintroduce;
    destructor Destroy; override;
    procedure WriteBloc;
    procedure WriteTrial;
    property BlocConfig : TStringList read FBlocConfig;
    property TrialConfig: TStringList read FTrialConfig;
    property CurrentBloc  : integer read FCurrentBloc write FCurrentBloc;
    property CurrentTrial : integer read GetCurrentTrial;
  end;

implementation

{ TConfigurationWriter }

function TConfigurationWriter.GetCurrentTrial: integer;
begin
  Result := FConfigurationFile.TrialCount[CurrentBloc+1];
end;

constructor TConfigurationWriter.Create(AConfigurationFile: TConfigurationFile);
begin
  if not Assigned(ConfigurationFile) then
    raise EFilerError.Create('Configuration file not assigned.');

  FConfigurationFile := AConfigurationFile;
  FBlocConfig := TStringList.Create;
  FTrialConfig:= TStringList.Create;
  FCurrentBloc := 0;
end;

destructor TConfigurationWriter.Destroy;
begin
  FTrialConfig.Free;
  FBlocConfig.Free;
  FConfigurationFile := nil;
  inherited Destroy;
end;

procedure TConfigurationWriter.WriteBloc;
var
  i: integer;
  LName , LValue: string;
begin
  with FConfigurationFile do begin
    for i := 0 to FBlocConfig.Count -1 do begin
      FBlocConfig.GetNameValue(i, LName, LValue);
      WriteToBloc(Self.CurrentBloc+1, LName, LValue);
    end;
  end;
end;

procedure TConfigurationWriter.WriteTrial;
var
  i: integer;
  LName, LValue : string;
  LCurrentTrial : integer;
  LCurrentBloc  : integer;
begin
  LCurrentTrial := CurrentTrial+1;
  LCurrentBloc  := CurrentBloc+1;
  with FConfigurationFile do begin
    for i := 0 to FTrialConfig.Count -1 do begin
      FTrialConfig.GetNameValue(i, LName, LValue);
      WriteToTrial(LCurrentTrial, LCurrentBloc, LName, LValue);
    end;
  end;
end;

end.
