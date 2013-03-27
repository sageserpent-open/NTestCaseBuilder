namespace SageSerpent.Infrastructure

    open System
    open OptionWorkflow

    type ChunkedList<'Element when 'Element: equality> (representation: ChunkedListRepresentation<'Element>) =
        interface System.Collections.Generic.IEnumerable<'Element> with
            member this.GetEnumerator (): System.Collections.Generic.IEnumerator<'Element> =
                let sequence representation =
                    match representation with
                        Contiguous backingArray ->
                            backingArray :> seq<'Element>
                      | Slice (backingArray
                               , startIndex
                               , endIndex) ->
                            seq
                                {
                                    for index in startIndex .. endIndex do
                                        yield backingArray.[index]
                                }   // I'm assuming that the alternative of an array slice would eagerly
                                    // allocate new storage for the slice, which isn't desirable.
                      | RunLength (duplicatedItem
                                   , numberOfRepeats
                                   , suffix) ->
                            seq
                                {
                                    yield! Seq.init numberOfRepeats
                                                    (fun _ ->
                                                        duplicatedItem)
                                    yield! suffix
                                }
                      | Empty ->
                            Seq.empty
                (sequence representation).GetEnumerator ()

        interface System.Collections.IEnumerable with
            member this.GetEnumerator () =
                (this:> seq<'Element>).GetEnumerator () :> System.Collections.IEnumerator

        new () =
            ChunkedList (Array.empty)

        new (arrayOfElements: array<'Element>) =
            ChunkedList (arrayOfElements
                         |> Contiguous)

        new ((head: 'Element),
             (tail: ChunkedList<'Element>)) =
            match tail.Representation with
                RunLength (duplicatedItem
                           , numberOfRepeats
                           , suffix) when head = duplicatedItem ->
                    ChunkedList((duplicatedItem
                                 , 1 + numberOfRepeats
                                 , suffix)
                                |> RunLength)
              | _ ->
                   ChunkedList((head
                                 , 1
                                 , tail)
                                |> RunLength)

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
            let rec length representation
                           prefixLength =
                match representation with
                    Contiguous backingArray ->
                        prefixLength + backingArray.Length
                  | Slice (backingArray
                           , startIndex
                           , endIndex) ->
                        prefixLength + 1 + endIndex - startIndex
                  | RunLength (duplicatedItem
                               , numberOfRepeats
                               , suffix) ->
                        length suffix.Representation
                               (prefixLength
                                + numberOfRepeats)
                  | Empty ->
                        prefixLength
            length representation
                   0

        member this.IsEmpty: Boolean =
            match representation with
                Contiguous backingArray ->
                    Array.isEmpty backingArray
              | Slice (backingArray
                       , startIndex
                       , endIndex) ->
                    startIndex = 1 + endIndex
              | RunLength (duplicatedItem
                           , numberOfRepeats
                           , suffix) ->
                    false
              | Empty ->
                    true

        member this.Item
            with get (index: Int32): 'Element =
                match representation with
                    Contiguous backingArray ->
                        backingArray.[index]
                  | Slice (backingArray
                           , startIndex
                           , endIndex) ->
                        let offsetIndex =
                            startIndex + index
                        if endIndex < offsetIndex
                        then
                            raise (PreconditionViolationException "The zero-relative index is greater than or equal to the length.")
                        backingArray.[offsetIndex]
                  | RunLength (duplicatedItem
                               , numberOfRepeats
                               , suffix) ->
                        if numberOfRepeats > index
                        then
                            duplicatedItem
                        else
                            suffix.[index - numberOfRepeats]
                  | Empty ->
                        raise (PreconditionViolationException "Cannot index into an empty list.")

        member this.GetSlice (startIndex: Option<Int32>,
                              endIndex: Option<Int32>): ChunkedList<'Element> =
            let startIndex =
                defaultArg startIndex
                           0
            let endIndex =
                defaultArg endIndex
                           (this.Length - 1)
            let length =
                this.Length
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
                endIndex - startIndex + 1
            if 0 = sliceLength
            then
                ChunkedList Empty
            else
                match representation with
                    Contiguous backingArray ->
                        ChunkedList((backingArray
                                     , startIndex
                                     , endIndex)
                                    |> Slice)
                  | Slice (backingArray
                           , startIndexInExistingSlice
                           , _) ->
                        ChunkedList((backingArray
                                     , startIndex + startIndexInExistingSlice
                                     , endIndex + startIndexInExistingSlice)
                                    |> Slice)
                  | RunLength (duplicatedItem
                               , numberOfRepeats
                               , suffix) ->
                        // TODO: consider *all* cases.
                        if startIndex >= numberOfRepeats
                        then
                            suffix.[startIndex - numberOfRepeats .. endIndex - numberOfRepeats]
                        else if endIndex < numberOfRepeats
                             then
                                ChunkedList ((duplicatedItem
                                              , sliceLength
                                              , ChunkedList())
                                             |> RunLength)
                             else
                                let flattenedBackingArray =
                                    this.ToArray
                                ChunkedList(flattenedBackingArray: 'Element[]).[startIndex .. endIndex]
                  | Empty ->
                        raise (LogicErrorException "This case is guarded against by the catch-all for empty lists above.")
                           // Note that a non-trivial slice is a precondition failure and
                           // should have been dealt with by the preceeding guard code.

        member this.DecomposeToHeadAndTail: 'Element * ChunkedList<'Element> =
            if this.IsEmpty
            then
                raise (PreconditionViolationException "The chunked list must be non-empty.")
            match representation with
                Contiguous backingArray ->
                    backingArray.[0]
                    , ChunkedList((backingArray
                                   , 1
                                   , backingArray.Length - 1)
                                  |> Slice)
              | Slice (backingArray
                       , startIndex
                       , endIndex) ->
                    backingArray.[startIndex]
                    , ChunkedList((backingArray
                                   , 1 + startIndex
                                   , endIndex)
                                  |> Slice)
              | RunLength (duplicatedItem
                           , numberOfRepeats
                           , suffix) ->
                    if 1 = numberOfRepeats
                    then
                        duplicatedItem
                        , suffix
                    else
                        duplicatedItem
                        , ChunkedList ((duplicatedItem
                                        , numberOfRepeats - 1
                                        , suffix)
                                       |> RunLength)
              | Empty ->
                    raise (LogicErrorException "This case is guarded against by the catch-all for empty lists above.")

        member this.ConcatenateWith (rhs: ChunkedList<'Element>): ChunkedList<'Element> =
            match representation
                  , rhs.Representation with
                Empty
                , _ ->
                    rhs
              | _
                , Empty ->
                    this
              | RunLength (duplicatedItem
                           , numberOfRepeats
                           , suffix)
                , RunLength (duplicatedItemFromRhs
                             , numberOfRepeatsFromRhs
                             , suffixFromRhs) when suffix.IsEmpty
                                                   && duplicatedItem = duplicatedItemFromRhs ->
                    ChunkedList((duplicatedItem
                                 , numberOfRepeats + numberOfRepeatsFromRhs
                                 , suffixFromRhs)
                                |> RunLength)
              | _ ->
                    let thisLength =
                        this.Length
                    let flattenedBackingArray =
                        Array.zeroCreate (thisLength + rhs.Length)
                    this.BlitInto flattenedBackingArray
                                  0
                    rhs.BlitInto flattenedBackingArray
                                 thisLength
                    ChunkedList(flattenedBackingArray)

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

    and ChunkedListRepresentation<'Element when 'Element: equality> =
        Contiguous of array<'Element>
      | Slice of array<'Element> * Int32 * Int32
      | RunLength of 'Element * Int32 * ChunkedList<'Element>   // NOTE: this case is not meant to be efficient for single-character runs
                                                                // - single character runs should only be used to represent the result of a
                                                                // *couple* of 'Cons' operations - using it many times to build a cons-cascade
                                                                // is technically correct but is not the right style - use 'List' for that
                                                                // and convert later to a 'ChunkedList'.
      | Empty

    module ChunkedListDetail =
        let rec runLengthEncode listOfElements =
            match listOfElements with
                [] ->
                    Empty
                | exemplar :: remainingElements ->
                    let rec pastRunLength listOfElements
                                            count =
                        match listOfElements with
                            head :: tail when exemplar = head ->
                                pastRunLength tail
                                              (1 + count)
                            | _ ->
                                if 0 = count
                                then
                                    None
                                else
                                    Some (exemplar
                                          , 1 + count   // Don't forget to count 'exemplar' itself as well as its duplicates.
                                          , listOfElements)
                    let runLengthEncoded =
                            optionWorkflow
                                {
                                    let! exemplar
                                         , multiplicity
                                         , remainderStartingWithDissimilarElementOrEmpty =
                                        pastRunLength remainingElements
                                                      0
                                    return (exemplar
                                            , multiplicity
                                            , ChunkedList (runLengthEncode remainderStartingWithDissimilarElementOrEmpty))
                                            |> RunLength
                                }
                    defaultArg runLengthEncoded
                                ((exemplar
                                  , 1
                                  , ChunkedList (runLengthEncode remainingElements))
                                 |> RunLength)

    open ChunkedListDetail

    type ChunkedList<'Element when 'Element: equality> with
        new (listOfElements: List<'Element>) =
            ChunkedList (listOfElements
                         |> runLengthEncode)

    module ChunkedListExtensions =
        let (|Cons|Nil|) (chunkedList: ChunkedList<'Element>) =
            if chunkedList.IsEmpty
            then
                Nil
            else
                Cons chunkedList.DecomposeToHeadAndTail

        let Cons ((head: 'Element)
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
            ChunkedList listOfElements

        let append (lhs: ChunkedList<_>)
                   rhs =
            lhs.ConcatenateWith rhs

        let fold (binaryOperation: 'State -> 'Element -> 'State)
                 (initialState: 'State)
                 (chunkedList: ChunkedList<'Element>): 'State =
                 Seq.fold binaryOperation
                          initialState
                          chunkedList

        let zip (lhs: ChunkedList<'LhsElement>)
                (rhs: ChunkedList<'RhsElement>): ChunkedList<'LhsElement * 'RhsElement> =
            ChunkedList (Seq.zip lhs
                                 rhs
                         |> Array.ofSeq)

        let map (transform: 'InputElement -> 'OutputElement)
                (chunkedList: ChunkedList<'InputElement>): ChunkedList<'OutputElement> =
            ChunkedList (chunkedList.ToArray
                         |> Array.map transform)

        let rec toList (chunkedList: ChunkedList<'Element>): List<'Element> =
            match chunkedList with
                Cons (head
                      , tail) ->
                    head :: toList tail
              | Nil ->
                    List.Empty

        let toArray (chunkedList: ChunkedList<'Element>): array<'Element> =
            chunkedList.ToArray