namespace SageSerpent.Infrastructure

    open System
    open OptionWorkflow

    type Chunk<'Element when 'Element: equality> =
        Contiguous of array<'Element>
      | RunLength of 'Element * Int32
      | Slice of array<'Element> * Int32 * Int32
        member this.Length =
            match this with
                Contiguous backingArray ->
                    backingArray.Length
              | RunLength (duplicatedItem
                           , numberOfRepeats) ->
                    numberOfRepeats
              | Slice (_
                       , startIndex
                       , endIndex) ->
                    1 + endIndex - startIndex

        member this.IsEmpty =
            match this with
                Contiguous backingArray ->
                    Array.isEmpty backingArray
              | RunLength (_
                           , numberOfRepeats) ->
                    0 = numberOfRepeats
              | Slice (_
                       , startIndex
                       , endIndex) ->
                    1 + endIndex = startIndex

        member this.Reversed =
            match this with
                Contiguous backingArray ->
                    backingArray
                    |> Array.rev
                    |> Contiguous
              | RunLength _ ->
                    this
              | Slice(backingArray
                      , startIndex
                      , endIndex) ->
                    [|
                        for index in endIndex .. -1 .. startIndex do
                            yield backingArray.[index]
                    |]
                    |> Contiguous

    and ChunkedList<'Element when 'Element: equality> (representation: List<Chunk<'Element>>) =
        do
            if representation
                |> List.exists (fun (chunk: Chunk<'Element>) ->
                                    chunk.IsEmpty)
            then
                raise (InvariantViolationException "All chunks must be non-empty.")
            if representation
               |> Seq.pairwise
               |> Seq.exists (fun leaderAndFollower ->
                                match leaderAndFollower with
                                    RunLength (leaderDuplicatedItem
                                               , numberOfRepeatsInLeader)
                                    , RunLength (followerDuplicatedItem
                                                 , numberOfRepeatsInFollower) when leaderDuplicatedItem = followerDuplicatedItem ->
                                        true
                                  | Contiguous _
                                    , Contiguous _ ->
                                        true
                                  | _ ->
                                        false)
            then
                raise (InvariantViolationException "Fusible adjacent chunks found.")

        interface System.Collections.Generic.IEnumerable<'Element> with
            member this.GetEnumerator (): System.Collections.Generic.IEnumerator<'Element> =
                (ChunkedList.RepresentationToList representation :> seq<'Element>).GetEnumerator ()

        interface System.Collections.IEnumerable with
            member this.GetEnumerator () =
                (this:> seq<'Element>).GetEnumerator () :> System.Collections.IEnumerator

        new () =
            ChunkedList (List.Empty)

        new (arrayOfElements: array<'Element>) =
            if Array.isEmpty arrayOfElements
            then
                ChunkedList ()
            else
                ChunkedList ([Contiguous arrayOfElements])

        new ((head: 'Element),
             (tail: ChunkedList<'Element>)) =
            let representation =
                match tail.Representation with
                    [] ->
                        [RunLength (head
                                    , 1 )]
                  | RunLength (duplicatedItem
                               , numberOfRepeats) :: remainder when head = duplicatedItem ->
                        RunLength (duplicatedItem
                                   , 1 + numberOfRepeats) :: remainder
                  | tailRepresentation ->
                        RunLength (head
                                   , 1) :: tailRepresentation
            ChunkedList representation

        member private this.Representation =
            representation

        override this.Equals another =
            match another with
                :? ChunkedList<'Element> as another ->
                    Seq.forall2 (fun lhsElement
                                     rhsElement ->
                                    lhsElement
                                     = rhsElement)
                                this
                                another
              | _ ->
                    false

        override this.GetHashCode () =
            (~-) (hash this.ToArray)

        member this.Length: Int32 =
            representation
            |> List.fold (fun length
                              chunk ->
                            length + chunk.Length)
                         0

        member this.IsEmpty: Boolean =
            representation.IsEmpty  // The invariant makes this work; see above.

        member this.Item
            with get (index: Int32): 'Element =
                let rec chunkForIndex (representation: List<Chunk<'Element>>)
                                      cumulativeOffset =
                    // NOTE: assume that the number of chunks is small, so a linear search will suffice.
                    match representation with
                        [] ->
                            if 0 = cumulativeOffset
                            then
                                raise (PreconditionViolationException "Cannot index into an empty list.")
                            else
                                raise (PreconditionViolationException "The zero-relative index is greater than or equal to the length.")
                      | headChunk :: tail ->
                            let cumulativeOffsetIncludingHeadChunk =
                                headChunk.Length + cumulativeOffset
                            if index < cumulativeOffsetIncludingHeadChunk
                            then
                                let intraChunkIndex =
                                    index - cumulativeOffset
                                match headChunk with
                                    Contiguous backingArray ->
                                        backingArray.[intraChunkIndex]
                                  | RunLength (duplicatedItem
                                               , numberOfRepeats) ->
                                        duplicatedItem
                                  | Slice (backingArray
                                           , startIndex
                                           , endIndex) ->
                                        let offsetIndex =
                                            startIndex + intraChunkIndex
                                        backingArray.[offsetIndex]                                
                            else
                                chunkForIndex tail
                                              cumulativeOffsetIncludingHeadChunk
                chunkForIndex representation
                              0

        member this.GetSlice (startIndex: Option<Int32>,
                              endIndex: Option<Int32>): ChunkedList<'Element> =
            let length =
                this.Length
            let startIndex =
                defaultArg startIndex
                           0
            let endIndex =
                defaultArg endIndex
                           (length - 1)
            if startIndex < 0
            then
                raise (PreconditionViolationException "'startIndex' cannot be negative.")
            if startIndex > length
            then
                raise (PreconditionViolationException "'startIndex' cannot be greater than the length.")
            if endIndex < -1
            then
                raise (PreconditionViolationException "'endIndex' cannot be less than -1.")
            if endIndex >= length
            then
                raise (PreconditionViolationException "'endIndex' cannot be greater than or equal to the length.")
            if startIndex > 1 + endIndex
            then
                raise (PreconditionViolationException "'endIndex' may only lag by at most one position behind 'startIndex'.")
            let sliceLength =
                1 + endIndex - startIndex
            if 0 = sliceLength
            then
                ChunkedList ()
            else
                let rec chunksForSlice (representation: List<Chunk<'Element>>)
                                       cumulativeOffset =
                    // NOTE: assume that the number of chunks is small, so a linear search will suffice.
                    match representation with
                        [] ->
                            raise (LogicErrorException "The should have been guarded against by the precondition checks above.")
                      | headChunk :: tail ->
                            let cumulativeOffsetIncludingHeadChunk =
                                headChunk.Length + cumulativeOffset
                            if startIndex < cumulativeOffsetIncludingHeadChunk
                            then
                                let intraChunkStartIndex =
                                    startIndex - cumulativeOffset
                                let startOfSlice =
                                    if 0 = intraChunkStartIndex
                                    then
                                        headChunk
                                    else
                                        match headChunk with
                                            Contiguous backingArray ->
                                                Slice (backingArray
                                                       , intraChunkStartIndex
                                                       , backingArray.Length - 1)
                                          | RunLength (duplicatedItem
                                                       , numberOfRepeats) ->
                                                RunLength (duplicatedItem
                                                           , numberOfRepeats - intraChunkStartIndex)
                                          | Slice (backingArray
                                                   , startIndexForSlice
                                                   , endIndexForSlice) ->
                                                Slice (backingArray
                                                       , startIndexForSlice + intraChunkStartIndex
                                                       , endIndexForSlice)
                                let rec chunksForSlice (representation: List<Chunk<'Element>>)
                                                       cumulativeOffset
                                                       chunksForSliceInReverseOrder =
                                     match representation with
                                        [] ->
                                            raise (LogicErrorException "The should have been guarded against by the precondition checks above.")
                                      | headChunk :: tail ->
                                            let headChunkLength =
                                                headChunk.Length
                                            let cumulativeOffsetIncludingHeadChunk =
                                                headChunkLength + cumulativeOffset
                                            if endIndex < cumulativeOffsetIncludingHeadChunk
                                            then
                                                let intraChunkEndIndex =
                                                    endIndex - cumulativeOffset
                                                let endOfSlice =
                                                    if headChunkLength = 1 + intraChunkEndIndex
                                                    then
                                                        headChunk
                                                    else
                                                        match headChunk with
                                                            Contiguous backingArray ->
                                                                Slice (backingArray
                                                                       , 0
                                                                       , intraChunkEndIndex)
                                                          | RunLength (duplicatedItem
                                                                       , numberOfRepeats) ->
                                                                RunLength (duplicatedItem
                                                                           , 1 + intraChunkEndIndex)
                                                          | Slice (backingArray
                                                                   , startIndexForSlice
                                                                   , _) ->
                                                                Slice (backingArray
                                                                       , startIndexForSlice
                                                                       , startIndexForSlice + intraChunkEndIndex)
                                                let sliceRepresentation =
                                                    (endOfSlice :: chunksForSliceInReverseOrder)
                                                    |> List.rev
                                                ChunkedList sliceRepresentation
                                            else
                                                chunksForSlice tail
                                                               cumulativeOffsetIncludingHeadChunk
                                                               (headChunk :: chunksForSliceInReverseOrder)
                                chunksForSlice (startOfSlice :: tail)
                                               (cumulativeOffset + intraChunkStartIndex)
                                               List.Empty
                            else
                                chunksForSlice tail
                                               cumulativeOffsetIncludingHeadChunk
                chunksForSlice representation
                               0

        member this.DecomposeToHeadAndTail: 'Element * ChunkedList<'Element> =
            match representation with
                [] ->
                    raise (PreconditionViolationException "The chunked list must be non-empty.")
              | headChunk :: tail ->
                    match headChunk with
                        Contiguous backingArray ->
                            let length =
                                backingArray.Length
                            backingArray.[0]
                            , if 1 = length
                              then
                                ChunkedList tail
                              else
                                ChunkedList (Slice (backingArray
                                                    , 1
                                                    , length - 1) :: tail)
                      | RunLength (duplicatedItem
                                   , numberOfRepeats) ->
                            duplicatedItem
                            , if 1 = numberOfRepeats
                              then
                                ChunkedList tail
                              else
                                ChunkedList (RunLength (duplicatedItem
                                                        , numberOfRepeats - 1) :: tail)
                      | Slice (backingArray
                               , startIndex
                               , endIndex) ->
                            backingArray.[startIndex]
                            , if endIndex = startIndex
                              then
                                ChunkedList tail
                              else
                                ChunkedList (Slice(backingArray
                                                   , 1 + startIndex
                                                   , endIndex) :: tail)

        member this.ConcatenateWith (rhs: ChunkedList<'Element>): ChunkedList<'Element> =
            let concatenatedRepresentation =
                List.append representation
                            rhs.Representation
            ChunkedList<'Element>.Fuse concatenatedRepresentation

        static member private Fuse representation =
            let rec fuseRunLengths representation
                                   reversedPrefixOfResult =
                match representation with
                    [] ->
                        reversedPrefixOfResult
                  | RunLength (duplicatedItemFromFirst
                               , numberOfRepeatsFromFirst)
                    :: RunLength (duplicatedItemFromSecond
                                  , numberOfRepeatsFromSecond)
                    :: tail when duplicatedItemFromFirst = duplicatedItemFromSecond ->
                        fuseRunLengths (RunLength (duplicatedItemFromFirst
                                                   , numberOfRepeatsFromFirst + numberOfRepeatsFromSecond) :: tail)
                                       reversedPrefixOfResult
                  | head :: tail ->
                        fuseRunLengths tail
                                       (head :: reversedPrefixOfResult)
            let fuseOddPieces representation =
                let rec group representation
                              groupsAlreadyFormedInReverse =
                    match representation with
                        [] ->
                            groupsAlreadyFormedInReverse
                      | headChunk :: tail ->
                            match groupsAlreadyFormedInReverse with
                                [] ->
                                    group tail
                                          [[headChunk]]
                              | groupInForce :: otherGroups ->
                                    match headChunk
                                          , groupInForce with
                                        RunLength (_
                                                   , numberOfRepeatsFromHeadChunk)
                                        , _ when 1 < numberOfRepeatsFromHeadChunk ->
                                            group tail
                                                  ([headChunk] :: groupsAlreadyFormedInReverse)
                                      | _
                                        , [RunLength (_
                                                      , numberOfRepeatsFromSingletonGroupInForce)] when 1 < numberOfRepeatsFromSingletonGroupInForce ->
                                            group tail
                                                  ([headChunk] :: groupsAlreadyFormedInReverse)
                                      | _ ->
                                            group tail
                                                  ((headChunk :: groupInForce) :: otherGroups)
                let groups =
                    group representation
                          List.empty
                groups
                |> List.map (fun group ->
                                match group with
                                    [singletonToPreserve] ->
                                        singletonToPreserve
                                  | _ ->    // NOTE: 'group' must never be empty - otherwise
                                            // it would cause an invariant violation in the
                                            // chunk being created.
                                        ChunkedList<'Element>.RepresentationToList group
                                        |> Array.ofList
                                        |> Contiguous)
            ChunkedList (fuseRunLengths representation
                                        List.empty
                         |> fuseOddPieces)  // NOTE: the innate reversals from both 'fuseRunLengths' and 'fuseOddPieces' cancel
                                            // each other out, even though the latter reverses at both group and intra-group level.

        member private this.BlitInto (destination: array<'Element>)
                                     (destinationOffset: Int32): unit =
            let mutable destinationIndex =
                destinationOffset   // This method is imperative anyway, so forget about purity.
            for item in this do
                destination.[destinationIndex] <- item
                destinationIndex <- 1 + destinationIndex

        member this.ToArray =
            let flattenedBackingArray =
                Array.zeroCreate this.Length
            this.BlitInto flattenedBackingArray
                          0
            flattenedBackingArray

        member this.ToList =
            ChunkedList<'Element>.RepresentationToList representation

        static member OfList(listOfElements: List<'Element>) =
            let inefficientRepresentation =
                listOfElements
                |> List.map (fun item ->
                                (item
                                 , 1) |> RunLength)
            ChunkedList.Fuse inefficientRepresentation

        static member private RepresentationToList representation =
                let listFrom chunk =
                    match chunk with
                        Contiguous backingArray ->
                            backingArray :> seq<'Element>
                      | RunLength (duplicatedItem
                                   , numberOfRepeats) ->
                            seq
                                {
                                    yield! List.replicate numberOfRepeats
                                                          duplicatedItem
                                }
                      | Slice (backingArray
                               , startIndex
                               , endIndex) ->
                            seq
                                {
                                    for index in startIndex .. endIndex do
                                        yield backingArray.[index]
                                }   // I'm assuming that the alternative of an array slice would eagerly
                                    // allocate new storage for the slice, which isn't desirable.
                [
                    for chunk in representation do
                        yield! listFrom chunk
                ]

        member this.Reversed =
            let reversedRepresentation =
                representation
                |> List.rev
                |> List.map (fun chunk ->
                                chunk.Reversed)
            ChunkedList reversedRepresentation

    module ChunkedListExtensions =
        let inline (|Cons|Nil|) (chunkedList: ChunkedList<'Element>) =
            if chunkedList.IsEmpty
            then
                Nil
            else
                Cons chunkedList.DecomposeToHeadAndTail

        let inline Cons ((head: 'Element)
                         , (tail: ChunkedList<'Element>)): ChunkedList<'Element> =
            ChunkedList(head,
                        tail)

        [<GeneralizableValue>]
        let Nil<'Element when 'Element: equality> =
            ChunkedList<'Element> ()

    open ChunkedListExtensions

    module ChunkedList =
        let inline length (listOfElements: ChunkedList<'Element>) =
            listOfElements.Length

        let inline isEmpty (listOfElements: ChunkedList<'Element>) =
            listOfElements.IsEmpty

        let inline ofList (listOfElements: List<'Element>) =
            ChunkedList.OfList listOfElements

        let inline ofArray (arrayOfElement: array<'Element>) =
            ChunkedList arrayOfElement

        let inline append (lhs: ChunkedList<_>)
                           rhs =
            lhs.ConcatenateWith rhs

        let inline fold (binaryOperation: 'State -> 'Element -> 'State)
                        (initialState: 'State)
                        (chunkedList: ChunkedList<'Element>): 'State =
                 List.fold binaryOperation
                           initialState
                           chunkedList.ToList

        let inline zip (lhs: ChunkedList<'LhsElement>)
                       (rhs: ChunkedList<'RhsElement>): ChunkedList<'LhsElement * 'RhsElement> =
            List.zip lhs.ToList
                     rhs.ToList
            |> ChunkedList.OfList

        let inline map (transform: 'InputElement -> 'OutputElement)
                       (chunkedList: ChunkedList<'InputElement>): ChunkedList<'OutputElement> =
            ChunkedList.OfList (chunkedList.ToList
                                |> List.map transform)

        let inline toList (chunkedList: ChunkedList<'Element>): List<'Element> =
            chunkedList.ToList

        let inline toArray (chunkedList: ChunkedList<'Element>): array<'Element> =
            chunkedList.ToArray

        let inline rev (chunkedList: ChunkedList<'Element>): ChunkedList<'Element> =
            chunkedList.Reversed
