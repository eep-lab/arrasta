{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Experiments.Eduardo.Experimento2.Tables;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Loggers.Tables;

type

  { TExperiment2Table }

  TExperiment2Table = class(TComponent)
  private
    FSampleDurations : TStringColumn;
    FResults : TIntegerColumn;
    FSampleDurationsTest : TStringColumn;
    FResultsTest : TIntegerColumn;
    function GetFilename : string;
    procedure WriteTable;
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;
  public
    procedure AddRow(
      ASampleDuration: string; AResult: integer; IsTestTrial: Boolean);
  end;

implementation

uses Session.Configuration.GlobalContainer;

{ TExperiment1Table }

constructor TExperiment2Table.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSampleDurations := TStringColumn.Create;
  FResults := TIntegerColumn.Create;
  FSampleDurationsTest := TStringColumn.Create;
  FResultsTest := TIntegerColumn.Create;
end;

destructor TExperiment2Table.Destroy;
begin
  WriteTable;
  FSampleDurations.Free;
  FResults.Free;
  FSampleDurationsTest.Free;
  FResultsTest.Free;
  inherited Destroy;
end;

procedure TExperiment2Table.AddRow(ASampleDuration: string; AResult: integer;
  IsTestTrial: Boolean);
begin
  if IsTestTrial then begin
    FSampleDurationsTest.Add(ASampleDuration);
    FResultsTest.Add(AResult);
  end else begin
    FSampleDurations.Add(ASampleDuration);
    FResults.Add(AResult);
  end;
end;

function TExperiment2Table.GetFilename: string;
begin
  Result := GlobalContainer.BaseFilename + #95 + Self.name + '.data';
end;

procedure TExperiment2Table.WriteTable;
const
  LeftLong = 0;
  RightShort = 1;
var
  S : string;
  S4000 : string;
  S3170 : string;
  S2520 : string;
  S2000 : string;
  S1590 : string;
  S1260 : string;
  S1000 : string;

  SP4000C : integer = 0;
  SP3170C : integer = 0;
  SP2520C : integer = 0;
  SP2000C : integer = 0;
  SP1590C : integer = 0;
  SP1260C : integer = 0;
  SP1000C : integer = 0;

  SP4000T : integer = 0;
  SP3170T : integer = 0;
  SP2520T : integer = 0;
  SP2000T : integer = 0;
  SP1590T : integer = 0;
  SP1260T : integer = 0;
  SP1000T : integer = 0;
  Table : TTabDelimitedReport;
  i: Integer;
  function CalculatePorcentage(A, B : integer):string;
  begin
    Result := ((A*100)/B).ToString;
  end;
begin
  S4000 := SP4000.ToString;
  S3170 := SP3170.ToString;
  S2520 := SP2520.ToString;
  S2000 := SP2000.ToString;
  S1590 := SP1590.ToString;
  S1260 := SP1260.ToString;
  S1000 := SP1000.ToString;
  Table := TTabDelimitedReport.Create;
  Table.Filename := GetFilename;
  try
    Table.WriteRow(['Treino']);
    Table.WriteRow(
      ['Duração do Modelo', 'Número de Tentativas', 'Número de Acertos', 'Porcentagem de Acertos']);
    for i := 0 to FResults.Count -1 do begin
      S := FSampleDurations[i];
      if S = S4000 then begin
        Inc(SP4000T);
        case FResults[i] of
          LeftLong : Inc(SP4000C);
          RightShort : { do nothing };
        end
      end;

      if S = S1000 then begin
        Inc(SP1000T);
        case FResults[i] of
          LeftLong : { do nothing};
          RightShort : Inc(SP1000C);
        end
      end;
    end;
    with Table do begin
      WriteRow([S4000, SP4000T.ToString, SP4000C.ToString, CalculatePorcentage(SP4000C, SP4000T)]);
      WriteRow([S1000, SP1000T.ToString, SP1000C.ToString, CalculatePorcentage(SP1000C, SP1000T)]);
    end;

    SP4000C := 0;
    SP4000T := 0;
    SP3170C := 0;
    SP3170T := 0;
    SP2520C := 0;
    SP2520T := 0;
    SP2000C := 0;
    SP2000T := 0;
    SP1590C := 0;
    SP1590T := 0;
    SP1260C := 0;
    SP1260T := 0;
    SP1000C := 0;
    SP1000T := 0;
    for i := 0 to FResultsTest.Count -1 do begin
      S := FSampleDurationsTest[i];
      if S = S4000 then begin
        Inc(SP4000T);
        case FResultsTest[i] of
          LeftLong : Inc(SP4000C);
          RightShort : { do nothing };
        end
      end;

      if S = S3170 then begin
        Inc(SP3170T);
        case FResultsTest[i] of
          LeftLong : Inc(SP3170C);
          RightShort : { do nothing };
        end
      end;

      if S = S2520 then begin
        Inc(SP2520T);
        case FResultsTest[i] of
          LeftLong : Inc(SP2520C);
          RightShort : { do nothing };
        end
      end;

      if S = S2000 then begin
        Inc(SP2000T);
        case FResultsTest[i] of
          LeftLong : Inc(SP2000C);
          RightShort : { do nothing };
        end
      end;

      if S = S1590 then begin
        Inc(SP1590T);
        case FResultsTest[i] of
          LeftLong : Inc(SP1590C);
          RightShort : { do nothing };
        end
      end;

      if S = S1260 then begin
        Inc(SP1260T);
        case FResultsTest[i] of
          LeftLong : Inc(SP1260C);
          RightShort : { do nothing };
        end
      end;

      if S = S1000 then begin
        Inc(SP1000T);
        case FResultsTest[i] of
          LeftLong : Inc(SP1000C);
          RightShort : { do nothing };
        end
      end;
    end;
    Table.WriteRow(['']);
    Table.WriteRow(['Teste']);
    Table.WriteRow(
      ['Duração do Modelo', 'Número de Tentativas', 'Número de R (Longo)', 'Porcentagem (Longo)']);
    with Table do begin
      WriteRow([S4000, SP4000T.ToString, SP4000C.ToString, CalculatePorcentage(SP4000C, SP4000T)]);
      WriteRow([S3170, SP3170T.ToString, SP3170C.ToString, CalculatePorcentage(SP3170C, SP3170T)]);
      WriteRow([S2520, SP2520T.ToString, SP2520C.ToString, CalculatePorcentage(SP2520C, SP2520T)]);
      WriteRow([S2000, SP2000T.ToString, SP2000C.ToString, CalculatePorcentage(SP2000C, SP2000T)]);
      WriteRow([S1590, SP1590T.ToString, SP1590C.ToString, CalculatePorcentage(SP1590C, SP1590T)]);
      WriteRow([S1260, SP1260T.ToString, SP1260C.ToString, CalculatePorcentage(SP1260C, SP1260T)]);
      WriteRow([S1000, SP1000T.ToString, SP1000C.ToString, CalculatePorcentage(SP1000C, SP1000T)]);
    end;
  finally
    Table.Free;
  end;
end;

end.

