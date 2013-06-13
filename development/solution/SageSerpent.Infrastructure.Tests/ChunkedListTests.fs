namespace SageSerpent.Infrastructure.Tests

    open NUnit.Framework
    open System

    open SageSerpent.Infrastructure
    open SageSerpent.Infrastructure.RandomExtensions
    open SageSerpent.Infrastructure.ChunkedListExtensions

    type ContenderTriumvirate =
        {
            ChunkedList: ChunkedList<Int32>
            List: List<Int32>
            Array: array<Int32>
        }

    type Decision =
        Cons
      | ConsWithPotentialDuplicate
      | Append
      | StartANewDoubleton
      | StartABigChunk
      | StartABigArrayChunk
      | StartASingletonArrayChunk
      | StartAnEmptyArrayChunk
      | StartANewDuplicateDoubleton
      | Halve
      | SliceOneOff

    [<TestFixture>]
    type ChunkedListTestFixture () =
        [<Test>]
        member this.AnythingListCanDoICanDoBetterICanDoAnythingBetterThanArray () =
            let seed =
                74339105
            let numberOfTrials =
                1000
            let random =
                Random seed
            let choices =
                [| Cons; ConsWithPotentialDuplicate; Append; StartANewDoubleton; StartABigChunk; StartABigArrayChunk; StartASingletonArrayChunk; StartAnEmptyArrayChunk; StartANewDuplicateDoubleton; Halve; SliceOneOff |]
            let decisions =
                List.init numberOfTrials
                          (fun _ ->
                                random.ChooseOneOf choices)
            let checkEquivalenceOfContenders {
                                                ChunkedList = chunkedList
                                                List = list
                                                Array = array
                                             } =
                printf "Checking: %A\n" [ yield! chunkedList ]
                let shouldBeTrue =
                    chunkedList
                    |> ChunkedList.toArray
                    |> Array.rev
                    |> BargainBasement.IsSorted // This is by construction of each new triumvirate - what
                                                // we are really checking is that the order of elements is
                                                // preserved via 'Cons' and 'append'.
                Assert.IsTrue shouldBeTrue
                let tripleResultsAgree (first
                                        , second
                                        , third) =
                    first = second
                    && second = third
                let shouldBeTrue =
                    (ChunkedList.length chunkedList
                    , List.length list
                    , Array.length array)
                    |> tripleResultsAgree
                Assert.IsTrue shouldBeTrue
                let shouldBeTrue =
                    (ChunkedList.isEmpty chunkedList
                    , List.isEmpty list
                    , Array.isEmpty array)
                    |> tripleResultsAgree
                Assert.IsTrue shouldBeTrue
                match chunkedList
                      , list with
                    Nil
                    , [] ->
                        ()
                  | ChunkedListExtensions.Cons(headFromChunkedList
                                               , tailFromChunkedList)
                    , headFromList :: tailFromList ->
                        let shouldBeTrue =
                            headFromChunkedList = headFromList
                        Assert.IsTrue shouldBeTrue
                        let shouldBeTrue =
                            tailFromChunkedList
                             = (tailFromList
                                |> ChunkedList.ofList)
                        Assert.IsTrue shouldBeTrue
                  | _ ->
                        Assert.Fail "Mismatch between 'ChunkedList' and 'List' decompositions."
                let shouldBeTrue =
                    (ChunkedList.toList chunkedList
                    , list
                    , Array.toList array)
                    |> tripleResultsAgree
                Assert.IsTrue shouldBeTrue
                let sharedLength =
                    ChunkedList.length chunkedList
                let shouldBeTrue =
                    Nil = chunkedList.[.. -1]
                    && Array.empty = array.[.. -1]
                    && Nil = chunkedList.[sharedLength ..]
                    && Array.empty = array.[sharedLength ..]
                Assert.IsTrue shouldBeTrue
                if 0 < sharedLength
                then
                    let shouldBeTrue =
                        let oneElementSliceIsOk index =
                            1 = chunkedList.[index .. index].Length
                            && 1 = array.[index .. index].Length
                            && chunkedList.[index .. index].[0] = array.[index .. index].[0]
                            && chunkedList.[index] = array.[index]
                            && chunkedList.[index .. index].[0] = array.[index]
                            && array.[index .. index].[0] = chunkedList.[index]
                        oneElementSliceIsOk 0
                        && oneElementSliceIsOk (sharedLength - 1)
                        && (chunkedList.[0 .. sharedLength - 1]
                            |> ChunkedList.fold (+)
                                                1)
                            = (array.[0 .. sharedLength - 1]
                               |> Array.fold (+)
                                             1)
                    Assert.IsTrue shouldBeTrue
                Assert.IsTrue shouldBeTrue
                let transform x =
                    2 * x
                let (foldResult
                     , _
                     , _) as foldResultsTriple =
                    ChunkedList.fold (+)
                                     0
                                     chunkedList
                    , List.fold (+)
                                0
                                list
                    , Array.fold (+)
                                 0
                                 array
                let shouldBeTrue =
                    foldResultsTriple
                    |> tripleResultsAgree
                Assert.IsTrue shouldBeTrue
                let mappedChunkedList
                    , mappedList
                    , mappedArray =
                    ChunkedList.map transform
                                    chunkedList
                    , List.map transform
                               list
                    , Array.map transform
                                array
                let (mapAndFoldResult
                     , _
                     , _) as mapAndfoldResultsTriple =
                    ChunkedList.fold (+)
                                     0
                                     mappedChunkedList
                    , List.fold (+)
                                0
                                mappedList
                    , Array.fold (+)
                                 0
                                 mappedArray
                let shouldBeTrue =
                    mapAndfoldResultsTriple
                    |> tripleResultsAgree
                Assert.IsTrue shouldBeTrue
                // This next line simultaneously checks the 'map' for 'ChunkedList' - the transforming the fold result should
                // be the same as folding the transformed results as well 'fold' - distribution of multiplication over addition.
                let shouldBeTrue =
                    transform foldResult
                     = mapAndFoldResult
                Assert.IsTrue shouldBeTrue
                let zipAndFoldResultsTriple =
                    let binaryOperation lhs
                                        (x
                                         , y) =
                        lhs + x * y
                    ChunkedList.zip chunkedList
                                    mappedChunkedList
                    |> ChunkedList.fold binaryOperation
                                        1
                    , List.zip list
                               mappedList
                    |> List.fold binaryOperation
                                 1
                    , Array.zip array
                                mappedArray
                    |> Array.fold binaryOperation
                                  1
                let shouldBeTrue =
                    zipAndFoldResultsTriple
                    |> tripleResultsAgree
                Assert.IsTrue shouldBeTrue
                let reverseResultsTriple =
                    ChunkedList.rev chunkedList
                    |> ChunkedList.toArray
                    , List.rev list
                      |> List.toArray
                    , Array.rev array
                let shouldBeTrue =
                    reverseResultsTriple
                    |> tripleResultsAgree
                Assert.IsTrue shouldBeTrue
            let carryOutDecision (contenderTriumvirates
                                  , nextUniqueElement)
                                 decision =
                match contenderTriumvirates with
                    head :: _ ->
                        checkEquivalenceOfContenders head
                  | _ ->
                        ()
                match contenderTriumvirates
                      , decision with
                    headTriumvirate :: tail
                    , Cons ->
                        {
                            ChunkedList =
                                ChunkedListExtensions.Cons (nextUniqueElement
                                                            , headTriumvirate.ChunkedList)
                            List =
                                nextUniqueElement
                                :: headTriumvirate.List
                            Array =
                                [| yield nextUniqueElement
                                   yield! headTriumvirate.Array |]
                        } :: tail
                        , 1 + nextUniqueElement
                  | headTriumvirate :: tail
                    , ConsWithPotentialDuplicate ->
                        {
                            ChunkedList =
                                ChunkedListExtensions.Cons (nextUniqueElement
                                                            , headTriumvirate.ChunkedList)
                            List =
                                nextUniqueElement
                                :: headTriumvirate.List
                            Array =
                                [| yield nextUniqueElement
                                   yield! headTriumvirate.Array |]
                        } :: tail
                        , nextUniqueElement
                  | headTriumvirate :: nextTriumvirate :: tail
                    , Append ->
                        {
                            ChunkedList =
                                ChunkedList.append headTriumvirate.ChunkedList
                                                   nextTriumvirate.ChunkedList
                            List =
                                List.append headTriumvirate.List
                                            nextTriumvirate.List
                            Array =
                                Array.append headTriumvirate.Array
                                             nextTriumvirate.Array
                        } :: tail
                        , nextUniqueElement
                  | triumvirates
                    , StartANewDoubleton ->
                        let newItems =
                            [(1 + nextUniqueElement); nextUniqueElement]
                        {
                            ChunkedList =
                                newItems
                                |> ChunkedList.ofList
                            List =
                                newItems
                            Array =
                                newItems
                                |> Array.ofList
                        } :: triumvirates
                        , 2 + nextUniqueElement
                  | triumvirates
                    , StartABigChunk ->
                        let chunkSize =
                            38
                        let newItems =
                            List.init chunkSize
                                      (fun index ->
                                        nextUniqueElement + index)
                            |> List.rev
                        {
                            ChunkedList =
                                newItems
                                |> ChunkedList.ofList
                            List =
                                newItems
                            Array =
                                newItems
                                |> Array.ofList
                        } :: triumvirates
                        , chunkSize + nextUniqueElement
                  | triumvirates
                    , StartABigArrayChunk ->
                        let chunkSize =
                            38
                        let newItems =
                            Array.init chunkSize
                                      (fun index ->
                                        nextUniqueElement + index)
                            |> Array.rev
                        {
                            ChunkedList =
                                newItems
                                |> ChunkedList.ofArray
                            List =
                                newItems
                                |> List.ofArray
                            Array =
                                newItems
                        } :: triumvirates
                        , chunkSize + nextUniqueElement
                  | triumvirates
                    , StartASingletonArrayChunk ->
                        let singleton =
                            [|nextUniqueElement|]
                        {
                            ChunkedList =
                                singleton
                                |> ChunkedList.ofArray
                            List =
                                singleton
                                |> List.ofArray
                            Array =
                                singleton
                        } :: triumvirates
                        , 1 + nextUniqueElement
                  | triumvirates
                    , StartASingletonArrayChunk ->
                        let empty =
                            [||]
                        {
                            ChunkedList =
                                empty
                                |> ChunkedList.ofArray
                            List =
                                empty
                                |> List.ofArray
                            Array =
                                empty
                        } :: triumvirates
                        , nextUniqueElement
                  | triumvirates
                    , StartANewDuplicateDoubleton ->
                        let newItems =
                            [nextUniqueElement; nextUniqueElement]
                        {
                            ChunkedList =
                                newItems
                                |> ChunkedList.ofList
                            List =
                                newItems
                            Array =
                                newItems
                                |> Array.ofList
                        } :: triumvirates
                        , 1 + nextUniqueElement
                  | headTriumvirate :: tail
                    , Halve ->
                        let endIndex =
                            headTriumvirate.ChunkedList.Length / 2
                        {
                            ChunkedList =
                                headTriumvirate.ChunkedList.[0 .. endIndex - 1]
                            List =
                                (headTriumvirate.List
                                 |> Array.ofList).[0 .. endIndex - 1]
                                |> List.ofArray
                            Array =
                                headTriumvirate.Array.[0 .. endIndex - 1]
                        } :: tail
                        , nextUniqueElement
                  | headTriumvirate :: tail
                    , SliceOneOff when not headTriumvirate.ChunkedList.IsEmpty ->
                        {
                            ChunkedList =
                                headTriumvirate.ChunkedList.[1 .. ]
                            List =
                                (headTriumvirate.List
                                 |> Array.ofList).[1 ..]
                                |> List.ofArray
                            Array =
                                headTriumvirate.Array.[1 ..]
                        } :: tail
                        , nextUniqueElement
                  | triumvirates
                    , _ ->
                        {
                            ChunkedList =
                                Nil
                            List =
                                []
                            Array =
                                Array.empty
                        } :: triumvirates
                        , nextUniqueElement
            let triumvirates
                , _ =
                decisions
                |> List.fold carryOutDecision
                             ([]
                              , 0)
            for triumvirate in triumvirates do
                checkEquivalenceOfContenders triumvirate