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

    type CompetitionState =
        List<ContenderTriumvirate> * Int32

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
                    (ChunkedList.toList chunkedList
                    , list
                    , Array.toList array)
                    |> tripleResultsAgree
                Assert.IsTrue shouldBeTrue
                let transform x =
                    2 * x
                let binaryOperation lhs
                                    rhs =
                    lhs +  rhs
                let (foldResult
                     , _
                     , _) as foldResultsTriple =
                    ChunkedList.fold binaryOperation
                                     1
                                     chunkedList
                    , List.fold binaryOperation
                                1
                                list
                    , Array.fold binaryOperation
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
                    ChunkedList.fold binaryOperation
                                     1
                                     mappedChunkedList
                    , List.fold binaryOperation
                                1
                                mappedList
                    , Array.fold binaryOperation
                                 1
                                 mappedArray
                let shouldBeTrue =
                    mapAndfoldResultsTriple
                    |> tripleResultsAgree
                let shouldBeTrue =
                    2 * foldResult
                     = mapAndFoldResult
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