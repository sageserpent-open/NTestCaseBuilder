namespace SageSerpent.Infrastructure

    open System

    type ChunkedList<[<EqualityConditionalOn>]'Element> (representation: ChunkedListRepresentation<'Element>) =
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
                      | ListEmulation (listPrefix
                                       , suffix) ->
                            seq
                                {
                                    yield! listPrefix
                                    yield! suffix
                                }
                (sequence representation).GetEnumerator ()

        interface System.Collections.IEnumerable with
            member this.GetEnumerator () =
                (this:> seq<'Element>).GetEnumerator () :> System.Collections.IEnumerator

        new () =
            ChunkedList (Array.empty)

        new (listOfElements: List<'Element>) =
            ChunkedList (listOfElements
                         |> Array.ofList)

        new (arrayOfElements: array<'Element>) =
            ChunkedList (arrayOfElements
                         |> Contiguous)

        new ((head: 'Element),
             (tail: ChunkedList<'Element>)) =
            match tail.Representation with
                ListEmulation (listPrefix
                               , suffix) ->
                    ChunkedList((head :: listPrefix
                                 , suffix)
                                |> ListEmulation)
              | _ ->
                    ChunkedList(([head]
                                 , tail)
                                |> ListEmulation)

        member private this.Representation =
            representation

        override this.Equals another =
            match another with
                :? ChunkedList<'Element> as another ->
                    Seq.forall2 (fun lhsElement
                                     rhsElement ->
                                    Unchecked.equals lhsElement
                                                        rhsElement)
                                this
                                another
              | _ ->
                    false

        override this.GetHashCode () =
            (~-) (Unchecked.hash this.ToArray)

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
                  | ListEmulation (listPrefix
                                   , suffix) ->
                        length suffix.Representation
                               (prefixLength
                                + listPrefix.Length)
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
              | ListEmulation (listPrefix
                               , suffix) ->
                    listPrefix.IsEmpty
                    && suffix.IsEmpty

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
                  | ListEmulation (listPrefix
                                   , suffix) ->
                        let prefixLength =
                            listPrefix.Length
                        if prefixLength > index
                        then
                            List.nth listPrefix
                                     index
                        else
                            suffix.[index - prefixLength]

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
              | ListEmulation (listPrefix
                               , suffix) ->
                    let listPrefixLength =
                        listPrefix.Length
                    if startIndex >= listPrefixLength
                    then
                        suffix.[startIndex - listPrefixLength .. endIndex - listPrefixLength]
                    else
                        let flattenedBackingArray =
                            this.ToArray
                        ChunkedList(flattenedBackingArray: 'Element[]).[startIndex .. endIndex]

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
              | ListEmulation (listPrefix
                               , suffix) ->
                    match listPrefix with
                        prefixHead :: [] ->
                            prefixHead
                            , suffix
                      | prefixHead :: nonEmptyPrefixTail     ->
                            prefixHead
                            , ChunkedList ((nonEmptyPrefixTail
                                            , suffix)
                                           |> ListEmulation)
                      | [] ->
                            suffix.DecomposeToHeadAndTail

        member this.ConcatenateWith (suffix: ChunkedList<'Element>): ChunkedList<'Element> =
            let thisLength =
                this.Length
            let flattenedBackingArray =
                Array.zeroCreate (thisLength + suffix.Length)
            this.BlitInto flattenedBackingArray
                          0
            suffix.BlitInto flattenedBackingArray
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

    and ChunkedListRepresentation<'Element> =
        Contiguous of array<'Element>
      | Slice of array<'Element> * Int32 * Int32
      | ListEmulation of List<'Element> * ChunkedList<'Element> // NOTE: this case is not meant to be efficient - the idea is to use it
                                                                // to represent the result of a *couple* of 'Cons' operations - using it
                                                                // many times to build a cons-cascade is technically correct but is not the
                                                                // right style - use 'List' for that and convert later to a 'ChunkedList'.

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
        let Nil<'Element> =
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