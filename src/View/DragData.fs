namespace Tactix

open Browser.Types

/// Data carried during an HTML drag/drop operation.
type private DragData =
    {
        /// Tactic type being dragged.
        TacticType : TacticType
    }

module DragData =

    // https://stackoverflow.com/questions/40940288/drag-datatransfer-data-unavailable-in-ondragover-event
    // https://stackoverflow.com/questions/31915653/how-to-get-data-from-datatransfer-getdata-in-event-dragover-or-dragenter
    let mutable private shared : Option<DragData> = None

    /// Sets drag data for the given event.
    let private setData dragData (evt : DragEvent) =
        // evt.dataTransfer.setData
        shared <- Some dragData

    /// Gets drag data for the given event.
    let private getData (evt : DragEvent) =
        // evt.dataTransfer.getData
        shared

    /// Sets the tactic type being dragged.
    let setTacticType tacticType evt =
        setData { TacticType = tacticType } evt

    /// Gets the tactic type being dragged.
    let getTacticType evt =
        match getData evt with
            | Some dragData -> dragData.TacticType
            | None -> failwith "Unexpected"
