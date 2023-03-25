unit Forms.Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

uses
  Stimuli.Sequence.DragDrop1;

var
  LStimuli : TDragDropStimuli;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  LStimuli := TDragDropStimuli.Create(self);
  LStimuli.Parent := Form1;
  LStimuli.Start;
  Button1.Hide;
end;

end.
