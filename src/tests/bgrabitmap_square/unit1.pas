unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, BGRABitmap,
  BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FBitmap : TBGRABitmap;
    //CenterWidth : integer;
    //CenterHeight : integer;
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
  FBitmap := TBGRABitmap.Create(ClientWidth, ClientHeight);
  //CenterWidth := ClientWidth div 2;
  //CenterHeight := ClientHeight div 2;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin

end;

procedure TForm1.Paint;
var
  R : TRect;
  CenterWidth : integer;
  CenterHeight: integer;
  SquareWidth: integer;
  SquareHeight: integer;
  SquareX : integer;
  SquareY : integer;
begin
  inherited Paint;
  CenterWidth := ClientWidth div 2;
  CenterHeight := ClientHeight div 2;
  SquareWidth := 150;
  SquareHeight := 150;
  SquareX := CenterWidth - SquareWidth div 2;
  SquareY := CenterHeight - SquareHeight div 2;
  R := Rect(SquareX, SquareY, SquareX + SquareWidth, SquareY + SquareHeight);
  FBitmap.Fill(VGARed);
  FBitmap.Draw(Canvas, R);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FBitmap.Free;
end;

end.

