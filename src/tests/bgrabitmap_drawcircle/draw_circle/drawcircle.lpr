program drawcircle;

uses
  BGRABitmap, Graphics, BGRABitmapTypes, Interfaces;

var
  bmp: TBGRABitmap;

begin
  bmp := TBGRABitmap.Create(800, 600); // cria um novo bitmap

  bmp.Fill(BGRAPixelTransparent); // preenche o bitmap com transparência

  bmp.FillEllipseAntialias(400, 300, 200, 200, BGRA(255, 0, 0, 255)); // desenha um círculo vermelho no centro do bitmap

  bmp.SaveToFile('circulo.bmp'); // salva o bitmap em um arquivo

  bmp.Free; // libera a memória alocada pelo bitmap
end.
