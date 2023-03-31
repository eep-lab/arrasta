unit Session.Trials.Factory;

{$mode ObjFPC}{$H+}
{$modeswitch TypeHelpers}

interface

uses
  Classes, SysUtils, Session.Trials;

type

  TTrialKind = (TrialHtmlMessage, TrialDragDrop);

  { TTrialFactory }

  TTrialFactory = class
    public
      class function NewTrial : ITrial;
  end;

type

  { TTrialKindHelper }

  TTrialKindHelper = type helper for TTrialKind
    function ToString : string;
  end;

  { TTrialKindStringHelper }

  TTrialKindStringHelper = type helper(TStringHelper) for string
    function ToTrialKind : TTrialKind;
  end;

implementation

uses Constants
   , Timestamps
   , Loggers.Reports
   , Session.InterTrialEvents
   , Session.Backgrounds
   , Session.Configuration
   , Session.Configuration.GlobalContainer
   , Session.ConfigurationFile
   , Controls.Trials.Abstract
   , Controls.Trials.HTMLMessage
   , Controls.Trials.DragDrop
   ;

{ TTrialKindHelper }

function TTrialKindHelper.ToString: string;
begin
  WriteStr(Result, Self);
end;

{ TTrialKindStringHelper }

function TTrialKindStringHelper.ToTrialKind: TTrialKind;
begin
  case UpperCase(Self) of
    T_HTM       : Result := TrialHtmlMessage;
    T_DRAG_DROP : Result := TrialDragDrop;
    else
      RunError(107);
  end;
end;

var
  Trial : TTrial;

{ TTrialFactory }

class function TTrialFactory.NewTrial: ITrial;
var
  Configurations : TCfgTrial;
begin
  if Assigned(Trial) then
    FreeAndNil(Trial);

  Configurations :=
    ConfigurationFile.Trial[Counters.CurrentBlc+1, Counters.CurrentTrial+1];

  try
    case Configurations.Kind.ToTrialKind of
      TrialHtmlMessage : Trial := THTMLMessage.Create(Background);
      TrialDragDrop : Trial := TDragDrop.Create(Background);
    end;
    if Counters.SessionTrials = 0 then
    begin
      Trial.SaveData(
        HFIRST_TIMESTAMP + #9 +
        TimestampToStr(GlobalContainer.TimeStart) + LineEnding + LineEnding);
      Trial.SaveData(Trial.HeaderTimestamps + LineEnding);
    end;
    Trial.Configurations := Configurations;
    Trial.OnTrialEnd := InterTrialEvents.OnTrialEnd;
  finally
    Configurations.Parameters.Free;
  end;
  Result := Trial as ITrial;
end;

initialization
  Trial := nil;

end.

