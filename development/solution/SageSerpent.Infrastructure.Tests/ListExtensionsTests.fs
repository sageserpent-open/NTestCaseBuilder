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
            5u
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
                    List.DecorrelatedCrossProduct inputList
                                                  randomBehaviour
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
                    List.DecorrelatedCrossProduct inputList
                                                  randomBehaviour
                let numberOfResults =
                    Seq.length decorrelatedCrossProduct
                let numberOfInputs =
                    List.length inputList
                let itemToResultPositionSumAssociation
                    , _ =
                    [for crossProductList in decorrelatedCrossProduct do
                        printf "%A\n" crossProductList
                        yield! List.zip crossProductList
                                        (List.init numberOfInputs
                                                   BargainBasement.Identity)]
                    |> List.fold (fun (itemToResultPositionSumAssociation
                                       , position)
                                      item ->
                                        let positionOfEnclosingCrossProductTerm =
                                            position / numberOfInputs
                                        match itemToResultPositionSumAssociation
                                              |> Map.tryFind item with
                                            Some positionSum ->
                                                itemToResultPositionSumAssociation
                                                |> Map.add item
                                                           (positionSum + positionOfEnclosingCrossProductTerm)
                                          | None ->
                                                itemToResultPositionSumAssociation
                                                |> Map.add item
                                                           positionOfEnclosingCrossProductTerm
                                        , 1 + position)
                                 (Map.empty
                                  , 0)
                let minumumRequiredNumberOfResults
                    = 10
                if minumumRequiredNumberOfResults <= numberOfResults
                then
                    let toleranceEpsilon =
                        1e-1
                    let itemToMeanResultPositionAssociation =
                        itemToResultPositionSumAssociation
                        |> Map.map (fun _
                                        positionSum ->
                                            ((double) positionSum) / ((double) numberOfResults))
                    let shouldBeTrue =
                        itemToMeanResultPositionAssociation
                        |> Map.forall (fun item
                                           meanPosition ->
                                           printf "Item: %A, mean position: %A, number of results: %A\n" item
                                                                                                         meanPosition
                                                                                                         numberOfResults
                                           Math.Abs (2.0 * meanPosition - (double) numberOfResults) < (double) numberOfResults * toleranceEpsilon)
                    Assert.IsTrue shouldBeTrue
            createBinaryCrossProductsAndHandOffToEachTest testHandoff