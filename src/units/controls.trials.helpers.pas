{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Controls.Trials.Helpers;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TDataSupport }

  TDataSupport = record
    BackgroundResponses,
    Responses : integer;
    Latency,
    StmBegin,
    StmEnd : Extended;
  end;

  TCircle = record
    OuterRect : TRect;  //Left/Top
    InnerRect : TRect;
    gap : Boolean;
    gap_degree : integer;
    gap_length : integer;
  end;

  TGoNoGoStyles = (
     {
       Limited hold > 0 is assumed.
       Play HIT or MISS sounds OnConsequence.
       Sounds OnConsequence are always contiguous to TSchedule.OnConsequence triggered by responses.
     }
     gngPlayGo,

     {
       Limited hold > 0 is assumed.
       Play HIT or MISS sounds OnBeforeEndTrial.
       Sounds OnBeforeEndTrial may not be contiguous to TSchedule.OnConsequence triggered by responses.
     }
     gngPlayNoGo,

     {
       Limited hold > 0 is assumed.
       If a sound would be presented OnConsequence, it is presented OnBeforeTrial instead.
     }
     gngPlayGoOnBeforeEnd

   );

  TGoNoGoStyle = set of TGoNoGoStyles;


  function StringToStyle(S : string) : TGoNoGoStyles;

implementation

function StringToStyle(S : string) : TGoNoGoStyles;
begin
  case UpperCase(S) of
    'GO': Result := gngPlayGo;
    'NOGO': Result := gngPlayNoGo;
    'END': Result := gngPlayGoOnBeforeEnd;
  end;
end;

end.

