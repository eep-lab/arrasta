unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, BGRABitmap,
  BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    bmp : TBGRABitmap;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private

  protected
    procedure Paint; override;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  Width := Screen.Width;
  Height := Screen.Height;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

end;

procedure TForm1.Paint;
begin
  inherited Paint;
  bmp := TBGRABitmap.Create(Width, Height);
  bmp.Fill(VGAWhite);
  bmp.FillRectAntialias(300, 200, 100, 100, BGRA(255, 0, 0, 255));
  bmp.Draw(Canvas, 0, 0, True);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  bmp.Free;
end;

end.

