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

