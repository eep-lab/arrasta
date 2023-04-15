{
  Stimulus Control
  Copyright (C) 2014-2021 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit Consequences.DragDrop;

{$mode objfpc}{$H+}

// linux dependencies: sudo apt install zlib1g-dev libopenal-dev libvorbis-dev

interface

uses
  Classes, SysUtils, Audio.CastleSound;

type

  { TConsequence }

  TConsequence = class(TSound)
  public
    constructor Create(AFilename: string); reintroduce;
    destructor Destroy; override;
  end;

var
  SoundRight : TConsequence;
  SoundWrong : TConsequence;

implementation

uses
  LazFileUtils;

constructor TConsequence.Create(AFilename: string);
begin
  inherited Create(nil);
  Name := 'Consequence_'+ExtractFileNameWithoutExt(AFilename);
  LoadFromFile(AFilename);
end;

destructor TConsequence.Destroy;
begin
  { do stuff here }
  inherited Destroy;
end;


initialization
  SoundRight := TConsequence.Create('acerto.wav');
  SoundWrong := TConsequence.Create('erro.wav');

finalization
  SoundRight.Free;
  SoundWrong.Free;

end.

