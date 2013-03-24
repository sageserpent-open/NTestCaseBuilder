namespace SageSerpent.Infrastructure

    open System

    type ChunkedList<'Element> (representation: ChunkedListRepresentation<'Element>) =
        new () =
            ChunkedList (Array.empty
                         |> Contiguous)

        new (listOfElements: List<'Element>) =
            ChunkedList (listOfElements
                         |> Array.ofList
                         |> Contiguous)

        member inline private this.Representation =
            representation

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

        member inline this.GetSlice (startIndex: Option<Int32>,
                                     endIndex: Option<Int32>): ChunkedList<'Element> =
            failwith "Not implemented."

        member inline this.DecomposeToHeadAndTail: 'Element * ChunkedList<'Element> =
            failwith "Not implemented."

        member inline this.ConcatenateWith (suffix: ChunkedList<'Element>): ChunkedList<'Element> =
            failwith "Not implemented."

    and ChunkedListRepresentation<'Element> =
        Contiguous of array<'Element>
      | Slice of array<'Element> * Int32 * Int32
      | ListEmulation of List<'Element> * ChunkedList<'Element> // NOTE: this case is not meant to be efficient - the idea is to use it
                                                                // to represent the result of a *couple* of 'Cons' operations - using it
                                                                // many times to build a cons-cascade is technically correct but is not the
                                                                // right style - use 'List' for that and convert later to a 'ChunkedList'.

    module ChunkedList =
        let inline length (listOfElements: ChunkedList<'Element>) =
            listOfElements.Length

        let inline ofList (listOfElements: List<'Element>) =
            ChunkedList listOfElements

        let inline append (lhs: ChunkedList<_>)
                   rhs =
            lhs.ConcatenateWith rhs

        let fold (binaryOperation: 'State -> 'Element -> 'State)
                 (initialState: 'State)
                 (chunkedList: ChunkedList<'Element>): 'State =
                 failwith "Not implemented."

        let zip (lhs: ChunkedList<'LhsElement>)
                (rhs: ChunkedList<'RhsElement>): ChunkedList<'LhsElement * 'RhsElement> =
            failwith "Not implemented."

        let map (transform: 'InputElement -> 'OutputElement)
                (chunkedList: ChunkedList<'InputElement>): ChunkedList<'OutputElement> =
            failwith "Not implemented."

        let toList (chunkedList: ChunkedList<'Element>): List<'Element> =
            failwith "Not implemented."

    module ChunkedListExtensions =
        let (|Cons|Nil|) (chunkedList: ChunkedList<'Element>) =
            match true with
                true ->
                    Cons (Unchecked.defaultof<'Element>
                          , chunkedList)
              | false ->
                    Nil

        let Cons ((head: 'Element)
                  , (tail: ChunkedList<'Element>)): ChunkedList<'Element> =
            failwith "Not implemented."

        [<GeneralizableValue>]
        let Nil<'Element> =
            ChunkedList<'Element> ()

