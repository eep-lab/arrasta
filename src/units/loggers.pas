{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Loggers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

procedure WriteDataRow;

var
  LastTrialHeader,
  TrialHeader,
  TrialName,
  TrialData,
  TrialResult: string;
  ITIBegin,
  ITIEnd: Extended;

implementation

uses Constants
   , Timestamps
   , Loggers.DataFile
   , Loggers.Reports
   , Session.ConfigurationFile
   , Session.Configuration.GlobalContainer;

procedure WriteDataRow;
var
  LSaveData : TDataProcedure;
  i, j : integer;
  LTrialNo, LBlocID, LBlocName,
  LTrialID, ITIData, LData : string;
const
  DoNotApply = #32#32#32#32#32#32 + 'NA';
begin
  if TrialHeader <> LastTrialHeader then begin
    LData := TRegData.Row([
      rsReportTrialNO,
      rsReportBlocID,
      rsReportBlocName,
      rsReportTrialID,
      rsReportTrialName,
      rsReportITIBeg,
      rsReportITIEnd,
      TrialHeader]);
  end;
  LastTrialHeader := TrialHeader;

  i := Counters.CurrentTrial;
  j := Counters.CurrentBlc;
  LTrialNo := (Counters.SessionTrials + 1).ToString;
  LBlocID := (j + 1).ToString;
  LBlocName := ConfigurationFile.Bloc[j+1].Name;
  LTrialID := (i + 1).ToString;

  // FTrial Name
  if TrialName = '' then
    TrialName := '--------';

  // iti
  if Counters.SessionTrials = 0 then
    ITIData := DoNotApply + #9 + TimestampToStr(0)
  else
    ITIData :=
      TimestampToStr(ITIBegin) + #9 +
      TimestampToStr(ITIEnd);

  // write data
  LSaveData := GetSaveDataProc(LGData);
  LData := TRegData.Row([LData +
    LTrialNo,
    LBlocID,
    LBlocName,
    LTrialID,
    TrialName,
    ITIData,
    TrialData]);
  LSaveData(LData);
end;

end.

