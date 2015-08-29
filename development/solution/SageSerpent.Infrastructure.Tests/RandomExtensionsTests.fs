namespace SageSerpent.Infrastructure.Tests

    open NUnit.Framework

    open SageSerpent.Infrastructure
    open System
    open C5
    open RandomExtensions

    [<TestFixture>]
    type RandomExtensionsTestFixture() =
        let inclusiveUpToExclusiveRange inclusiveLimit exclusiveLimit =
            Seq.init (exclusiveLimit - inclusiveLimit) (fun x -> inclusiveLimit + x)

        let commonTestStructureForTestingOfChoosingSeveralItems testOnSuperSetAndItemsChosenFromIt =
            let  random = Random 1

            for inclusiveLowerBound in 58 .. 98 do
                for numberOfConsecutiveItems in 1 .. 50 do
                    let superSet = inclusiveUpToExclusiveRange inclusiveLowerBound (inclusiveLowerBound + numberOfConsecutiveItems) |> Set.ofSeq
                    for subsetSize in 1 .. numberOfConsecutiveItems do
                        for _ in 1 .. 10 do
                            let chosenItems = random.ChooseSeveralOf(superSet, subsetSize)
                            testOnSuperSetAndItemsChosenFromIt superSet chosenItems subsetSize

        let pig maximumUpperBound =
            let random = Random 678
            let concreteRangeOfIntegers = inclusiveUpToExclusiveRange 0 maximumUpperBound

            for _ in 1 .. 10 do
                let chosenItems = random.ChooseSeveralOf(concreteRangeOfIntegers, maximumUpperBound)
                for chosenItem in chosenItems do
                    ()

        let sampleDistributions upperBound
                                sampleSize
                                buildRandomSequenceOfDistinctIntegersOfSize =
            let numberOfTrials =
                (BargainBasement.NumberOfCombinations upperBound 
                                                      sampleSize) * 1000
    
            printf "Number of trials: %d, upperBound: %d, sampleSize: %d\n" numberOfTrials upperBound sampleSize

            let findWithDefaultValue (dictionary: TreeDictionary<_, 'Value>)
                                     key
                                     defaultValue =
                let result = ref Unchecked.defaultof<'Value>
                if dictionary.Find(ref key, result)
                then
                    !result
                else
                    defaultValue

            let sampleToCountMap =
                TreeDictionary()

            let itemToCountAndSumOfPositionsMap =
                TreeDictionary()

            for _ in 1 .. numberOfTrials do
                let sample =
                    buildRandomSequenceOfDistinctIntegersOfSize sampleSize
                    |> List.ofSeq
      
                let sampleAsSet =
                    sample
                    |> Set.ofList
      
                sampleToCountMap.[sampleAsSet]
                    <- 1 + findWithDefaultValue sampleToCountMap
                                                sampleAsSet
                                                0
                for position
                    , item in sample
                              |> List.mapi (fun index
                                                item ->
                                                index
                                                , item) do
                    let count
                        , sumOfPositions =
                        findWithDefaultValue itemToCountAndSumOfPositionsMap
                                             item
                                             (0, 0.0)
                    itemToCountAndSumOfPositionsMap.[item] <-
                        (1 + count
                         , (double position + sumOfPositions))

            let numberOfDistinctSamplesObtained =
                sampleToCountMap.Count
    
            Assert.IsTrue(sampleToCountMap.Values
                          |> Seq.filter(fun count ->
                                            let expectedCount =
                                                double numberOfTrials / double numberOfDistinctSamplesObtained
                                            let tolerance =
                                                1e-1
                                            abs(double count - expectedCount) <= tolerance * double expectedCount)
                          |> Seq.length
                          |> double
                          >= 9e-1 * double sampleToCountMap.Count)

            Assert.IsTrue(itemToCountAndSumOfPositionsMap.Count = upperBound)

            Assert.IsTrue(itemToCountAndSumOfPositionsMap.Keys
                          |> Seq.forall(fun item ->
                                            0 <= item && upperBound > item))

            Assert.IsTrue(itemToCountAndSumOfPositionsMap.Values
                          |> Seq.filter(function (count, sumOfPositions) ->
                                                    let meanPosition =
                                                        sumOfPositions / double count
                                                    let expectedMeanPosition =
                                                        double (sampleSize - 1) / 2.0
                                                    let difference =
                                                        abs(meanPosition - expectedMeanPosition)
                                                    let tolerance =
                                                        1e-1
                                                    difference <= tolerance * expectedMeanPosition)
                          |> Seq.length
                          |> double
                          >= 9e-1 * double itemToCountAndSumOfPositionsMap.Count)


        let commonTestStructureForTestingAlternatePickingFromSequences testOnSequences =
            let randomBehaviour =
                Random 232
            for numberOfSequences in 0 .. 50 do
                let maximumPossibleNumberOfItemsInASequence =
                    100
                let sequenceSizes =
                    List.init numberOfSequences
                              (fun _ ->
                                randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumPossibleNumberOfItemsInASequence)
                let sequences =
                    sequenceSizes
                    |> List.mapi (fun sequenceIndex
                                      sequenceSize ->
                                        Seq.init sequenceSize
                                                 (fun itemIndex ->
                                                    sequenceIndex + numberOfSequences * itemIndex))
                testOnSequences sequences

        [<Test>]
        member this.TestCoverageOfIntegersUpToExclusiveUpperBound() =
            let random = Random 29

            let maximumUpperBound = 30

            for upperBound in 0 .. maximumUpperBound do
                let concreteRangeOfIntegers = inclusiveUpToExclusiveRange 0 upperBound

                let chosenItems = random.ChooseSeveralOf(concreteRangeOfIntegers, upperBound)
                let expectedRange = inclusiveUpToExclusiveRange 0 upperBound
                let shouldBeTrue = (chosenItems |> Set.ofArray) = (expectedRange |> Set.ofSeq)
                Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestUniquenessOfIntegersProduced() =
            let random = Random 678

            let maximumUpperBound = 30

            for upperBound in 0 .. maximumUpperBound do
                let concreteRangeOfIntegers = inclusiveUpToExclusiveRange 0 upperBound

                let chosenItems = random.ChooseSeveralOf(concreteRangeOfIntegers, upperBound)
                let shouldBeTrue = upperBound = (chosenItems |> Set.ofArray |> Seq.length)
                Assert.IsTrue shouldBeTrue
                let shouldBeTrue = upperBound = (chosenItems |> Array.length)
                Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestDistributionOfSuccessiveSequencesWithTheSameUpperBound() =
            let random = Random 1

            let maximumUpperBound = 17

            for upperBound in 1 .. maximumUpperBound do
                let concreteRangeOfIntegers =
                    inclusiveUpToExclusiveRange 0 upperBound

                let sampleSizes =
                    Set [1; min (1 + random.Next(upperBound)) upperBound; upperBound]

                for sampleSize in sampleSizes do
                    sampleDistributions upperBound
                                        sampleSize
                                        (fun sampleSize ->
                                            random.ChooseSeveralOf(concreteRangeOfIntegers, sampleSize))


        [<Test>]
        member this.TestThatAllItemsChosenBelongToTheSourceSequence() =
            commonTestStructureForTestingOfChoosingSeveralItems (fun superSet chosenItems _ ->
                                                                    let shouldBeTrue = (chosenItems |> Set.ofArray).IsSubsetOf superSet
                                                                    Assert.IsTrue shouldBeTrue)

        [<Test>]
        member this.TestThatTheNumberOfItemsRequestedIsHonouredIfPossible() =
            commonTestStructureForTestingOfChoosingSeveralItems (fun _ chosenItems subsetSize ->
                                                                    let shouldBeTrue = (chosenItems |> Array.length) = subsetSize
                                                                    Assert.IsTrue shouldBeTrue)

        [<Test>]
        member this.TestThatUniqueItemsInTheSourceSequenceAreNotDuplicated() =
            commonTestStructureForTestingOfChoosingSeveralItems (fun _ chosenItems _ ->
                                                                    let shouldBeTrue = (chosenItems |> Set.ofArray |> Seq.length) = (chosenItems |> Array.length)
                                                                    Assert.IsTrue shouldBeTrue)

        [<Test>]
        member this.TestThatChoosingItemsRepeatedlyEventuallyCoversAllPermutations() =
            let empiricallyDeterminedMultiplicationFactorToEnsureCoverage = double 70500 / (BargainBasement.Factorial 7 |> double)

            let random = Random 1

            for inclusiveLowerBound in 58 .. 98 do
                for numberOfConsecutiveItems in 1 .. 7 do
                    let superSet = inclusiveUpToExclusiveRange inclusiveLowerBound (inclusiveLowerBound + numberOfConsecutiveItems) |> Set.ofSeq
                    for subsetSize in 1 .. numberOfConsecutiveItems do
                        let expectedNumberOfPermutations = BargainBasement.NumberOfPermutations numberOfConsecutiveItems subsetSize
                        let oversampledOutputs =
                            seq {
                                for _ in 1 .. Math.Ceiling(empiricallyDeterminedMultiplicationFactorToEnsureCoverage * double expectedNumberOfPermutations) |> int32 do
                                    yield random.ChooseSeveralOf(superSet, subsetSize) |> List.ofArray
                                }
                        let shouldBeTrue = oversampledOutputs |> Set.ofSeq |> Seq.length = expectedNumberOfPermutations
                        Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestPig0GetInTheTrough() =
            pig 64000

        [<Test>]
        member this.TestPig1() =
            pig 1000

        [<Test>]
        member this.TestPig2() =
            pig 2000

        [<Test>]
        member this.TestPig3() =
            pig 4000

        [<Test>]
        member this.TestPig4() =
            pig 8000

        [<Test>]
        member this.TestPig5() =
            pig 16000

        [<Test>]
        member this.TestPig6() =
            pig 32000

        [<Test>]
        member this.TestPig7() =
            pig 64000

        [<Test>]
        member this.TestPig8() =
            pig 50000

        [<Test>]
        member this.TestThatPickingAlternatelyFromSequencesPreservesTheItemsInTheOriginalSequences () =
            let randomBehaviour =
                Random 89734873
            let testHandoff sequences =
                let alternatelyPickedSequence =
                    randomBehaviour.PickAlternatelyFrom sequences
                let setOfAllItemsPickedFrom =
                    sequences
                    |> List.map Set.ofSeq
                    |> Set.unionMany
                let setofAllItemsActuallyPicked =
                    alternatelyPickedSequence
                    |> Set.ofSeq
                let shouldBeTrue =
                    setOfAllItemsPickedFrom = setofAllItemsActuallyPicked
                Assert.IsTrue shouldBeTrue
                let shouldBeTrue =
                    (sequences
                     |> List.map Seq.length
                     |> List.fold (+)
                                  0)
                     = (alternatelyPickedSequence
                        |> Seq.length)
                Assert.IsTrue shouldBeTrue
            commonTestStructureForTestingAlternatePickingFromSequences testHandoff

        [<Test>]
        member this.TestThatPickingAlternatelyFromSequencesPreservesTheOrderOfItemsInTheOriginalSequences () =
            let randomBehaviour =
                Random 2317667
            let testHandoff sequences =
                let alternatelyPickedSequence =
                    randomBehaviour.PickAlternatelyFrom sequences
                let numberOfSequences =
                    sequences
                    |> List.length
                let disentangledPickedSubsequences =
                    let sequenceIndexToDisentangledPickedSubsequenceMap =
                        alternatelyPickedSequence
                        |> Seq.fold (fun sequenceIndexToDisentangledPickedSubsequenceMap
                                         item ->
                                        let sequenceIndex =
                                            item % numberOfSequences
                                        let disentangledSubsequence =
                                            match Map.tryFind sequenceIndex
                                                              sequenceIndexToDisentangledPickedSubsequenceMap with
                                                Some disentangledSubsequence ->
                                                    item :: disentangledSubsequence
                                              | None ->
                                                    [item]
                                        Map.add sequenceIndex
                                                disentangledSubsequence
                                                sequenceIndexToDisentangledPickedSubsequenceMap)
                                    Map.empty
                    sequenceIndexToDisentangledPickedSubsequenceMap
                    |> Map.toList
                    |> List.map (snd >> List.rev)
                let shouldBeTrue =
                    printf "Expected: %A\n" (sequences
                                             |> List.filter (Seq.isEmpty >> not))
                    printf "Got: %A\n" disentangledPickedSubsequences
                    List.zip (sequences
                              |> List.filter (Seq.isEmpty >> not))
                             disentangledPickedSubsequences
                    |> List.forall (fun (sequence
                                         , disentangledPickedSubsequence) ->
                                         0 = Seq.compareWith compare
                                                             sequence
                                                             disentangledPickedSubsequence)
                Assert.IsTrue shouldBeTrue
            commonTestStructureForTestingAlternatePickingFromSequences testHandoff

        [<Test>]
        member this.TestThatPickingAlternatelyFromSequencesChoosesRandomlyFromTheSequences () =
            let randomBehaviour =
                Random 2317667
            let testHandoff sequences =
                let alternatelyPickedSequence =
                    randomBehaviour.PickAlternatelyFrom sequences
                let numberOfSequences =
                    sequences
                    |> List.length
                let sequenceIndexToPositionSumAndCount
                    , pickedSequenceLength =
                    alternatelyPickedSequence
                    |> Seq.fold (fun (sequenceIndexToPositionSumAndCount
                                      , itemPosition)
                                     item ->
                                    let sequenceIndex =
                                        item % numberOfSequences
                                    sequenceIndexToPositionSumAndCount
                                    |> match Map.tryFind sequenceIndex
                                                         sequenceIndexToPositionSumAndCount with
                                        Some (positionSum
                                              , numberOfPositions) ->
                                            Map.add sequenceIndex
                                                    (itemPosition + positionSum
                                                     , 1 + numberOfPositions)
                                                
                                      | None ->
                                            Map.add sequenceIndex
                                                    (itemPosition
                                                     , 1)
                                    , 1 + itemPosition)
                                (Map.empty
                                 , 0)

                let minumumRequiredNumberOfPositions
                    = 50
                let toleranceEpsilon =
                    6e-1
                for item
                    , (positionSum
                       , numberOfPositions) in Map.toSeq sequenceIndexToPositionSumAndCount do
                    if minumumRequiredNumberOfPositions <= numberOfPositions
                    then
                        let meanPosition =
                            (double) positionSum / (double) numberOfPositions
                        printf "Item: %A, mean position: %A, picked sequence length: %A\n" item
                                                                                           meanPosition
                                                                                           pickedSequenceLength
                        let shouldBeTrue =
                            Math.Abs (2.0 * meanPosition - (double) pickedSequenceLength) < (double) pickedSequenceLength * toleranceEpsilon
                        Assert.IsTrue shouldBeTrue
            commonTestStructureForTestingAlternatePickingFromSequences testHandoff
