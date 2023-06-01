unit Constants.DragDrop;

{$mode ObjFPC}{$H+}

interface

type
  TDragDropKeys = record
    DragDropOrientation : string;
    UseHelpProgression : string;
    RepeatTrials : string;
    SamplesDragMode : string;
    Relation : string;
    Samples : string;
    Comparisons : string;
    DragMoveFactor : string;
  end;

const
  DragDropKeys : TDragDropKeys = (
    DragDropOrientation : 'Orientation';
    UseHelpProgression : 'UseHelpProgression';
    RepeatTrials : 'RepeatTrial';
    SamplesDragMode : 'Style.Samples.DragMode';
    Relation : 'Relation';
    Samples : 'Samples';
    Comparisons : 'Comparisons';
    DragMoveFactor : 'DragMoveFactor');

implementation

end.

