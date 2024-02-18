unit session.constants.dragdrop;

{$mode ObjFPC}{$H+}

interface

type
  TDragDropKeys = record
    DragDropOrientationKey : string;
    UseHelpProgressionKey : string;
    DistanceKey : string;
    DragModeKey : string;
    DragMoveFactorKey : string;
    DragableAnimationKey : string;
    GridSizeKey : string;
    StimuliFolderKey: string;
  end;

const
  DragDropKeys : TDragDropKeys = (
    DragDropOrientationKey : 'Orientation';
    UseHelpProgressionKey : 'UseHelpProgression';
    DistanceKey : 'Distance';
    DragModeKey : 'DragMode';
    DragMoveFactorKey : 'DragMoveFactor';
    DragableAnimationKey : 'DragableAnimation';
    GridSizeKey : 'GridSize';
    StimuliFolderKey : 'StimuliFolder');

implementation

end.


