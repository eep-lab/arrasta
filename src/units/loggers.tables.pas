{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Loggers.Tables;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Loggers.Helpers;

type

  TStringColumn = specialize TGenericList<string>;
  TFloatColumn = specialize TGenericList<Extended>;
  TIntegerColumn = specialize TGenericList<Integer>;
  TDemandDataColumn = specialize TGenericList<TDemandDatum>;

  { TTabDelimitedReport }


  TTabDelimitedReport = class
    private
      FFilename : string;
      FTextFile : TextFile;
      procedure SetFilename(AFilename: string);
    public
      destructor Destroy; override;
      procedure WriteRow(Cols : array of string);
      property Filename : string read FFilename write SetFilename;
    end;

implementation

uses SysUtils, LazFileUtils;

procedure TTabDelimitedReport.WriteRow(Cols: array of string);
const
  TAB = #9;
var
  i : Integer;
  LastColumn : Integer;
begin
  LastColumn := High(Cols);
  for i := 0 to LastColumn do
    if i < LastColumn then
      Write(FTextFile, Cols[i]+TAB)
    else
      WriteLn(FTextFile, Cols[i]);
  System.Flush(FTextFile);
end;

procedure TTabDelimitedReport.SetFilename(AFilename: string);
var
  LFilePath, LExtension, LBasename: string;
  i : Integer;
begin
  LFilePath := ExtractFilePath(AFilename);
  LBasename := ExtractFileNameOnly(AFilename);
  LExtension := ExtractFileExt(AFilename);

  case LExtension of
  '', '.exe' : LExtension:='.txt';
  end;

  i := 0;
  while FileExists(AFilename) do begin
    Inc(i);
    AFilename := LFilePath+LBasename+Format('%.3d', [i])+LExtension;
  end;
  System.Assign(FTextFile, AFilename);
  System.Rewrite(FTextFile);
  FFilename:=AFilename;
end;

destructor TTabDelimitedReport.Destroy;
begin
  System.Close(FTextFile);
  inherited Destroy;
end;

end.

