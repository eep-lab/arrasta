unit LatinSquares;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fgl;

type
{
procedure TfRandPosition.RandLatinSquareBalanced;
var   jumbled, sequence, rotateS, signs: array of integer;
      i, j, k, size : integer;
//      s1 : string;
//      a1 : integer;

  procedure SetArrayLength (aSize : integer);
  begin
    SetLength(signs, aSize);
    SetLength(rotateS, aSize);
    SetLength(jumbled, aSize);
    SetLength(sequence, aSize);
    SetLength(FLatinSquare, aSize, aSize);
  end;

  procedure Shuffle(var array1 : array of integer ; aSize : integer);   //embaralhar lista
  var v, aTemp, aRandom : integer;
  begin
    for v := 0 to aSize - 1 do array1[v] := v + 1;

    for v := 0 to aSize - 1 do
      begin
        aRandom := Round(Random * (aSize - 1));
        aTemp := array1[aRandom];
        array1[aRandom] := array1[v];
        array1[v] := aTemp;
      end;
  end;

  procedure Rotate(var array2 : array of integer; aSize, aTimes : integer);   //primeiro elemento torna-se último, elementos restantes para esquerda n vezes
  var aTemp, v, x : integer;
  begin
    for x := 0 to aTimes - 1 do
      begin
        aTemp := array2[0];
        for v := Low(array2) to High(array2) do array2[v] := array2[v + 1];
        array2[aSize - 1] := aTemp;
      end;
  end;

begin
  //how many?: 1 for each NumPos cicle on seSeqToWrite.
  Size := Escriba.NumPos;
  SetArrayLength(Size);

  shuffle(jumbled, size);                      //gerar lista de referência; aleatória
  shuffle(rotateS, size);                      //gerar lista de rotações; aleatória
  for i := 0 to size - 1 do signs[i] := i + 1; //gerar lista de elementos; ordenada

  for i := 0 to size - 1 do
    begin
      for k := 0 to size - 1 do sequence[k] := jumbled[k]; //gerar lista de trabalho a partir da lista de referência
      rotate(sequence, size, rotateS[i]);                  //mover elementos da lista de trabalho
      for j := 0 to size - 1 do FLatinSquare[j, sequence[j] - 1] := signs[i]; //preencher Latin Square
    end;
end;
}
  TLatinRow = specialize TFPGList<Integer>;
  TLatinMatrix = array of TLatinRow;

  { TLatinSquare }

  TLatinSquare = class
    private
      FSigns: TLatinRow;
      FSize : integer;
      FMatrix : TLatinMatrix;
      FCurrentRow : integer;
      procedure NewLatinSquare;
    public
      constructor Create(ASize: integer);
      destructor Destroy; override;
      property Signs : TLatinRow read FSigns write FSigns;
      procedure Invalidate;
      function NextRow : TLatinRow;
      function AsString : string;
  end;

implementation

uses Math;

{ TLatinSquare }

procedure TLatinSquare.NewLatinSquare;
var
  LJumbled, LSequence, LRotateS: TLatinRow;
  i, j, k : integer;

  procedure Shuffle(var Array1 : TLatinRow);
  var i : integer;
  begin
    Array1.Clear;
    for i := 0 to FSize - 1 do Array1.Add(i + 1);

    for i := Array1.Count - 1 downto 0 do
      Array1.Exchange(i, RandomRange(0, i + 1));
  end;

  //primeiro elemento torna-se último, elementos restantes para esquerda n vezes
  procedure Rotate(var Array2 : TLatinRow; ATimes : integer);
  var LTemp, j, i : integer;
  begin
    for i := 0 to ATimes - 1 do
      begin
        LTemp := Array2[0];
        for j := 0 to FSize-2 do
          Array2[j] := Array2[j + 1];
        Array2[FSize - 1] := LTemp;
      end;
  end;

begin
  //LSigns := TLatinRow.Create;
  LRotateS := TLatinRow.Create;
  LJumbled := TLatinRow.Create;
  LSequence := TLatinRow.Create;

  Shuffle(LJumbled); //gerar lista de referência; aleatória
  Shuffle(LRotateS); //gerar lista de rotações; aleatória

  //gerar lista de elementos; ordenada
  for i := 0 to FSize - 1 do FSigns.Add(i);
  for i := 0 to FSize - 1 do LSequence.Add(i);

  for i := 0 to FSize - 1 do
    begin
      //gerar lista de trabalho a partir da lista de referência
      for k := 0 to FSize - 1 do LSequence[k] := LJumbled[k];

      //mover elementos da lista de trabalho
      Rotate(LSequence, LRotateS[i]);

      //preencher Latin Square
      for j := Low(FMatrix) to High(FMatrix) do
        FMatrix[j][LSequence[j]-1] := FSigns[i];
    end;

  //LSigns.Free;
  LRotateS.Free;
  LJumbled.Free;
  LSequence.Free;
  FCurrentRow := 0;
end;

constructor TLatinSquare.Create(ASize: integer);
var
  i , j: integer;
begin
  FSize := ASize;
  SetLength(FMatrix, ASize);
  FSigns := TLatinRow.Create;
  for j := Low(FMatrix) to High(FMatrix) do begin
    FMatrix[j] := TLatinRow.Create;
    for i := 0 to ASize -1 do begin
      FMatrix[j].Add(i);
    end;
  end;
  NewLatinSquare;
end;

destructor TLatinSquare.Destroy;
var
  i: Integer;
begin
  FSigns.Free;
  for i := Low(FMatrix) to High(FMatrix) do begin
    FMatrix[i].Free;
  end;
  inherited Destroy;
end;

procedure TLatinSquare.Invalidate;
begin
  NewLatinSquare;
end;

function TLatinSquare.NextRow: TLatinRow;
begin
  if FCurrentRow <= High(FMatrix) then begin
    Result := FMatrix[FCurrentRow];
  end else begin
    NewLatinSquare;
    Result := FMatrix[FCurrentRow];
  end;
  Inc(FCurrentRow);
end;

function TLatinSquare.AsString: string;
var
  i, j : integer;
begin
  Result := '';
  for j := Low(FMatrix) to High(FMatrix) do begin
    for i in FMatrix[j] do Result := Result + i.ToString+#32;
    Result := Result + LineEnding;
  end;
end;

end.

