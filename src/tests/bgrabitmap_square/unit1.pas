unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, StdCtrls, BGRABitmap,
  BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FBitmaps : array [0..5] of TBGRABitmap;
    FBorderColors : array [0..5] of TBGRAPixel;
    procedure DrawBitmaps;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to 5 do begin
    FBitmaps[i] := TBGRABitmap.Create(200, 200);
    FBorderColors[i] := BGRA(255, 0, 0, 255 div (i + 1));
  end;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  DrawBitmaps;
end;

procedure TForm1.DrawBitmaps;
var
  ACenterHeight: integer;
  ASquareY : integer;
  i : integer;
begin
  ACenterHeight := ClientHeight div 2;
  ASquareY := ACenterHeight - FBitmaps[0].Height div 2;

  for i := 0 to 5 do begin
    FBitmaps[i].PenStyle := psSolid;
    FBitmaps[i].RectangleAntialias(0, 0, FBitmaps[i].Width, FBitmaps[i].Height,
                                      FBorderColors[i], 15);
    FBitmaps[i].Draw(Canvas, 240 * (i + 1), ASquareY, False);
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  i : integer;
begin
  for i := 0 to 5 do begin
    FBitmaps[i].Free;
  end;
end;

end.
