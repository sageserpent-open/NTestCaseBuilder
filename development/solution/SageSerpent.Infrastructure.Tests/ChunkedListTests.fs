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
      | Append
      | StartANewDoubleton

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
                [| Cons; Append; StartANewDoubleton |]
            let decisions =
                List.init numberOfTrials
                          (fun _ ->
                                random.ChooseOneOf choices)
            let checkEquivalenceOfContenders {
                                                ChunkedList = chunkedList
                                                List = list
                                                Array = array
                                             } =
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
                        1 = chunkedList.[0 .. 0].Length
                        && 1 = array.[0 .. 0].Length
                        && chunkedList.[0 .. 0].[0] = array.[0 .. 0].[0]
                        && chunkedList.[0] = array.[0]
                        && chunkedList.[0 .. 0].[0] = array.[0]
                        && array.[0 .. 0].[0] = chunkedList.[0]
                        && 1 = chunkedList.[sharedLength - 1 .. sharedLength - 1].Length
                        && 1 = array.[sharedLength - 1 .. sharedLength - 1].Length
                        && chunkedList.[sharedLength - 1 .. sharedLength - 1].[0] = array.[sharedLength - 1 .. sharedLength - 1].[0]
                        && chunkedList.[sharedLength - 1] = array.[sharedLength - 1]
                        && chunkedList.[sharedLength - 1 .. sharedLength - 1].[0] = array.[sharedLength - 1]
                        && array.[sharedLength - 1 .. sharedLength - 1].[0] = chunkedList.[sharedLength - 1]
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
                                     1
                                     chunkedList
                    , List.fold (+)
                                1
                                list
                    , Array.fold (+)
                                 1
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
                                     1
                                     mappedChunkedList
                    , List.fold (+)
                                1
                                mappedList
                    , Array.fold (+)
                                 1
                                 mappedArray
                let shouldBeTrue =
                    mapAndfoldResultsTriple
                    |> tripleResultsAgree
                Assert.IsTrue shouldBeTrue
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
                            [nextUniqueElement; (1 + nextUniqueElement)]
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