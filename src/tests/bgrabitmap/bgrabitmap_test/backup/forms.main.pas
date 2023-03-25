//unit Forms.Main;
//
//{$mode objfpc}{$H+}
//
//interface
//
//uses
//  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;
//
//type
//
//  { TForm1 }
//
//  TForm1 = class(TForm)
//    Button1: TButton;
//    procedure Button1Click(Sender: TObject);
//  private
//
//  public
//
//  end;
//
//var
//  Form1: TForm1;
//
//implementation
//
//uses
//  Stimuli.Sequence.DragDrop1;
//
//var
//  LStimuli : TDragDropStimuli;
//
//{$R *.lfm}
//
//{ TForm1 }
//
//procedure TForm1.Button1Click(Sender: TObject);
//begin
//  LStimuli := TDragDropStimuli.Create(self);
//  LStimuli.Parent := Form1;
//  LStimuli.Start;
//  Button1.Hide;
//end;
//
//end.


program Forms.Main;

uses
  BGRABitmap, Graphics, BGRABitmapTypes;

var
  bmp: TBGRABitmap;

begin
  bmp := TBGRABitmap.Create(800, 600); // cria um novo bitmap

  bmp.Fill(BGRAPixelTransparent); // preenche o bitmap com transparência

  bmp.FillEllipseAntialias(400, 300, 200, 200, BGRA(255, 0, 0, 255)); // desenha um círculo vermelho no centro do bitmap

  bmp.SaveToFile('circulo.bmp'); // salva o bitmap em um arquivo

  bmp.Free; // libera a memória alocada pelo bitmap
end.
