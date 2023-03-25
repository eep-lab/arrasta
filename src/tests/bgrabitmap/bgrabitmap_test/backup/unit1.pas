unit unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  BGRABitmap, BGRABitmapTypes;

type
  { TForm1 }

  TForm1 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    { private declarations }
    image: TBGRABitmap;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  image := TBGRABitmap.Create('Figura1.png');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  image.free;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  image.Draw(Canvas,0,0,True);
end;

initialization
  {$I unit1.lrs}

end.
