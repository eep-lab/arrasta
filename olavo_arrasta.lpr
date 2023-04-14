program olavo_arrasta;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces // this includes the LCL widgetset
  {$IFDEF DARWIN}
  , MachFullscreenFix
  {$ENDIF}
  , Forms
  , Forms.Main
  ;

{$R *.res}

begin
  Randomize;
  Application.Title:='arrasta';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TBackground, Background);
  {$IFDEF LINUX}
    Background.WindowState:=wsFullScreen;
  {$ENDIF}

  {$IFDEF DARWIN}
    FixWindowCollectionBehavior;
  {$ENDIF}
  Application.Run;
end.

