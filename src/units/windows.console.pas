unit Windows.Console;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils, Windows;

implementation

initialization
  AllocConsole;
  IsConsole := True;
  SysInitStdIO;

end.

