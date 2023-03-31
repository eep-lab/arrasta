unit Session.Trials;

{$mode ObjFPC}{$H+}

{$INTERFACES CORBA}

interface

uses
  Classes, SysUtils;

type

  TNextTrialEvent = procedure(NextTrialCode: string) of object;

   { IInterTrial }

  ITrial = interface
    ['{3652F678-82C6-4FDC-AC8B-E41CF4453138}']
    function ConsequenceDelay: Cardinal;
    function ConsequenceInterval: Cardinal;
    function InterTrialInterval : Cardinal;
    procedure Hide;
    procedure LoadMockParameters;
    procedure Play;
  end;

implementation

end.
