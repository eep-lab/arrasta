unit Constants.DragDrop;

{$mode ObjFPC}{$H+}

interface

type
  TDragDropKeys = record
    DragDropOrientation : string;
    RepeatTrials : string;
    Distance : string;
    Relation : string;
    Samples : string;
    Comparisons : string;
    StimulusSize : string;
  end;

const
  DragDropKeys : TDragDropKeys = (
    DragDropOrientation : 'Orientation';
    RepeatTrials : 'RepeatTrial';
    Distance : 'Distance';
    Relation : 'Relation';
    Samples : 'Samples';
    Comparisons : 'Comparisons';
    StimulusSize : 'StimulusSize');

implementation

end.

