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
    FBitmap : TBGRABitmap;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FBitmap := TBGRABitmap.Create(200, 200);
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  ACenterWidth : integer;
  ACenterHeight: integer;
  ASquareX : integer;
  ASquareY : integer;
  ABorderColor : TBGRAPixel;
begin
  ACenterWidth := ClientWidth div 2;
  ACenterHeight := ClientHeight div 2;
  ASquareX := ACenterWidth - FBitmap.Width div 2;
  ASquareY := ACenterHeight - FBitmap.Height div 2;
  ABorderColor := BGRA(255, 0, 0, 255);
  FBitmap.PenStyle := psSolid;
  FBitmap.RectangleAntialias(0, 0, FBitmap.Width, FBitmap.Height, ABorderColor,
                             15);
  FBitmap.Draw(Canvas, ASquareX, ASquareY, False);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FBitmap.Free;
end;

end.

