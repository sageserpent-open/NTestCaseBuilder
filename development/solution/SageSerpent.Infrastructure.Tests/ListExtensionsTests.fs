namespace SageSerpent.Infrastructure.Tests

    open NUnit.Framework

    open SageSerpent.Infrastructure
    open SageSerpent.Infrastructure.ListExtensions
    open SageSerpent.Infrastructure.RandomExtensions
    open SageSerpent.Infrastructure.OptionExtensions
    open System

    [<TestFixture>]
    type ListExtensionsTestFixture() =
        let randomSeed =
            9273
        let exclusiveUpperBoundOnNumberOfUnitEntries =
            3u
        let exclusiveUpperBoundOnNumberOfZeroEntries =
            2u
        let inclusiveUpperBoundOnNumberOfBinaryChoiceEntries =
            7u
        let numberOfRepeatsToInvestigateZeroesAndUnits =
            10u
        let createBinaryCrossProductsAndHandOffToEachTest testHandoff =
            let randomBehaviour =
                Random randomSeed
            for numberOfBinaryChoiceEntries in 0u .. inclusiveUpperBoundOnNumberOfBinaryChoiceEntries do
                let binaryChoiceEntries =
                    List.init (numberOfBinaryChoiceEntries
                               |> int32)
                              (fun index ->
                                seq
                                    {
                                        yield Some (2 * index)
                                        yield Some (2 * index + 1)
                                    })
                for _ in 1u .. numberOfRepeatsToInvestigateZeroesAndUnits do
                    let unitEntries =
                        List.init (randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan exclusiveUpperBoundOnNumberOfUnitEntries
                                   |> int32)
                                  (fun _ ->
                                    Seq.singleton None)
                    let zeroEntries =
                        List.init (randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan exclusiveUpperBoundOnNumberOfZeroEntries
                                   |> int32)
                                  (fun _ ->
                                    Seq.empty)
                    let shuffledZeroAndUnitEntries =
                        [yield! unitEntries
                         yield! zeroEntries]
                        |> randomBehaviour.Shuffle
                        |> List.ofArray
                    let rec interspersePreservingOriginalOrders lhs
                                                                rhs =
                        match lhs
                              , rhs with
                              []
                              , [] ->
                                    []
                            | lhs
                              , [] ->
                                    lhs
                            | []
                              , rhs ->
                                    rhs
                            | lhsHead :: lhsTail
                              , rhsHead :: rhsTail ->
                                    if randomBehaviour.HeadsItIs ()
                                    then
                                        lhsHead
                                        :: interspersePreservingOriginalOrders lhsTail rhs
                                    else
                                        rhsHead
                                        :: interspersePreservingOriginalOrders lhs rhsTail
                    let inputList =
                        interspersePreservingOriginalOrders binaryChoiceEntries
                                                            shuffledZeroAndUnitEntries
                    testHandoff inputList
                                numberOfBinaryChoiceEntries
                                (not zeroEntries.IsEmpty)

        [<Test>]
        member this.TestNumberOfItemsGeneratedFromACrossProductOfBinaryChoices() =
            let testHandoff inputList
                            numberOfBinaryChoiceEntries
                            hasZeroes =
                let expectedNumberOfItems =
                    if hasZeroes
                    then
                        0u
                    else
                        1u <<< (int32 numberOfBinaryChoiceEntries)
                let crossProduct =
                    List.CrossProduct inputList
                printf "Expecting %A number of items in cross product %A, taken from input list %A\n" expectedNumberOfItems crossProduct inputList
                let shouldBeTrue =
                    crossProduct
                    |> Seq.length
                    |> uint32
                     = expectedNumberOfItems
                Assert.IsTrue shouldBeTrue
                let shouldBeTrue =
                    crossProduct
                    |> Set.ofSeq
                    |> Set.count
                    |> uint32
                     = expectedNumberOfItems
                Assert.IsTrue shouldBeTrue
            createBinaryCrossProductsAndHandOffToEachTest testHandoff

        [<Test>]
        member this.TestItemsInCrossProductListsReferOnlyToCorrespondingItemsInInputLists() =
            let testHandoff inputList
                            numberOfBinaryChoiceEntries
                            _ =
                for crossProductList in List.CrossProduct inputList do
                    crossProductList
                    |> Option<_>.GetFromMany
                    |> List.iteri (fun index
                                       item ->
                                        let shouldBeTrue =
                                            item >= 2 * index
                                            && item <= 2 * index + 1
                                        Assert.IsTrue shouldBeTrue)
            createBinaryCrossProductsAndHandOffToEachTest testHandoff

        [<Test>]
        member this.TestFrequencyOfItemsInCrossProductOfBinaryChoicesIsAlwaysTheSame() =
            let testHandoff inputList
                            numberOfBinaryChoiceEntries
                            _ =
                let expectedCount =
                    (1u <<< (int32 numberOfBinaryChoiceEntries)) / 2u   // NOTE: zeroes make this value irrelevant, as does any input list consisting of only unit entries.
                let crossProduct
                    = List.CrossProduct inputList
                printf "Expecting each item to occur %A times in cross product %A, taken from input list %A\n" expectedCount crossProduct inputList
                let itemToCountAssociation =
                    [for crossProductList in crossProduct do
                        yield! crossProductList]
                    |> Option<_>.GetFromMany
                    |> List.fold (fun itemToCountAssociation
                                      item ->
                                        match itemToCountAssociation
                                              |> Map.tryFind item with
                                            Some count ->
                                                itemToCountAssociation
                                                |> Map.add item
                                                           (count + 1u)
                                          | None ->
                                                itemToCountAssociation
                                                |> Map.add item
                                                           1u)
                                 Map.empty
                let shouldBeTrue =
                    itemToCountAssociation
                    |> Map.forall (fun _
                                       count ->
                                       expectedCount = count)
                Assert.IsTrue shouldBeTrue
            createBinaryCrossProductsAndHandOffToEachTest testHandoff

        [<Test>]
        member this.TestThatTheDecorrelatedCrossProductProducesTheSameResultsAsTheConventionalImplementation() =
            let testHandoff inputList
                            numberOfBinaryChoiceEntries
                             _ =
                let crossProduct =
                    List.CrossProduct inputList
                    |> Set.ofSeq
                let randomisationSeed =
                    89438
                let randomBehaviour =
                    Random randomisationSeed
                let decorrelatedCrossProduct =
                    List.DecorrelatedCrossProduct randomBehaviour
                                                  inputList
                    |> Set.ofSeq
                let shouldBeTrue =
                    crossProduct = decorrelatedCrossProduct
                Assert.IsTrue shouldBeTrue
            createBinaryCrossProductsAndHandOffToEachTest testHandoff

        [<Test>]
        member this.TestDistributionOfEachItemInDecorrelatedCrossProductResultsIsRandom() =
            let testHandoff inputList
                            numberOfBinaryChoiceEntries
                            _ =
                printf "Input list: %A\n" inputList
                let randomisationSeed =
                    676792
                let randomBehaviour =
                    Random randomisationSeed
                let decorrelatedCrossProduct =
                    List.DecorrelatedCrossProduct randomBehaviour
                                                  inputList
                let numberOfInputs =
                    List.length inputList
                let itemToCrossProductPositionSumAndCountAssociation
                    , _ =
                    [
                        for crossProductList in decorrelatedCrossProduct do
                            printf "%A\n" crossProductList
                            yield! List.zip crossProductList
                                            (List.init numberOfInputs
                                                       BargainBasement.Identity)
                    ]
                    |> List.fold (fun (itemToCrossProductPositionSumAndCountAssociation
                                       , itemPosition)
                                      item ->
                                        let positionOfEnclosingCrossProductTerm =
                                            itemPosition / numberOfInputs
                                        itemToCrossProductPositionSumAndCountAssociation
                                        |> match Map.tryFind item
                                                             itemToCrossProductPositionSumAndCountAssociation with
                                                    Some (positionSum
                                                            , numberOfPositions) ->
                                                        Map.add item
                                                                (positionOfEnclosingCrossProductTerm + positionSum
                                                                 , 1 + numberOfPositions)
                                                  | None ->
                                                        Map.add item
                                                                (positionOfEnclosingCrossProductTerm
                                                                 , 1)
                                        , 1 + itemPosition)
                                 (Map.empty
                                  , 0)
                let minumumRequiredNumberOfPositions
                    = 10
                let toleranceEpsilon =
                    3e-1
                let maximumPossiblePosition =
                    (decorrelatedCrossProduct
                    |> Seq.length)
                    - 1 // NOTE: if the cross product is empty, the resulting bogus value of -1 will nevertheless be ignored by the next code block.
                for item
                    , (positionSum
                       , numberOfPositions) in Map.toSeq itemToCrossProductPositionSumAndCountAssociation do
                    if minumumRequiredNumberOfPositions <= numberOfPositions
                    then
                        let meanPosition =
                            (double) positionSum / (double) numberOfPositions
                        printf "Item: %A, mean position: %A, maximum possible position: %A\n" item
                                                                                              meanPosition
                                                                                              maximumPossiblePosition
                        let shouldBeTrue =
                            Math.Abs (2.0 * meanPosition - (double) maximumPossiblePosition) < (double) maximumPossiblePosition * toleranceEpsilon
                        Assert.IsTrue shouldBeTrue
            createBinaryCrossProductsAndHandOffToEachTest testHandoff