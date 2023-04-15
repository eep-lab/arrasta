{
  Stimulus Control
  Copyright (C) 2014-2023 Carlos Rafael Fernandes Picanço, Universidade Federal do Pará.

  The present file is distributed under the terms of the GNU General Public License (GPL v3.0).

  You should have received a copy of the GNU General Public License
  along with this program. If not, see <http://www.gnu.org/licenses/>.
}
unit MachFullscreenFix;

{$mode ObjFPC}{$H+}
{$modeswitch OBJECTIVEC1}

interface

procedure FixWindowCollectionBehavior;

implementation

uses CocoaAll;

procedure FixWindowCollectionBehavior;
var
  Options : NSApplicationPresentationOptions;
begin
  // get the current collection behavior options
  Options := NSApplication.sharedApplication.presentationOptions;

  // add fullscreen auxiliary option
  Options := Options + NSWindowCollectionBehaviorFullScreenAuxiliary;

  // set the current collection behavior options
  NSApplication.sharedApplication.setPresentationOptions(Options);
end;

end.

