namespace SageSerpent.Infrastructure.Tests

    open NUnit.Framework

    open SageSerpent.Infrastructure
    open System
    open RandomExtensions

    [<TestFixture>]
    type RandomExtensionsTestFixture() =
        let inclusiveUpToExclusiveRange inclusiveLimit exclusiveLimit =
            Seq.init (int32 (exclusiveLimit - inclusiveLimit)) (fun x -> inclusiveLimit + uint32 x)

        let commonTestStructureForTestingOfChoosingSeveralItems testOnSuperSetAndItemsChosenFromIt =
            let  random = Random 1

            for inclusiveLowerBound in 58u .. 98u do
                for numberOfConsecutiveItems in 1u .. 50u do
                    let superSet = inclusiveUpToExclusiveRange inclusiveLowerBound (inclusiveLowerBound + numberOfConsecutiveItems) |> Set.ofSeq
                    for subsetSize in 1u .. numberOfConsecutiveItems do
                        for _ in 1 .. 10 do
                            let chosenItems = random.ChooseSeveralOf(superSet, subsetSize)
                            testOnSuperSetAndItemsChosenFromIt superSet chosenItems subsetSize

        let pig maximumUpperBound =
            let random = Random 678
            let concreteRangeOfIntegers = inclusiveUpToExclusiveRange 0u maximumUpperBound
    
            for _ in 1 .. 10 do
                let chosenItems = random.ChooseSeveralOf(concreteRangeOfIntegers, maximumUpperBound)
                for chosenItem in chosenItems do
                    ()  

        [<Test>]
        member this.TestCoverageOfIntegersUpToExclusiveUpperBound() =
            let random = Random 29

            let maximumUpperBound = 30u

            for upperBound in 0u .. maximumUpperBound do
                let concreteRangeOfIntegers = inclusiveUpToExclusiveRange 0u upperBound

                let chosenItems = random.ChooseSeveralOf(concreteRangeOfIntegers, upperBound)
                let expectedRange = inclusiveUpToExclusiveRange 0u upperBound
                let shouldBeTrue = (chosenItems |> Set.ofArray) = (expectedRange |> Set.ofSeq)
                Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestUniquenessOfIntegersProduced() =
            let random = Random 678

            let maximumUpperBound = 30u

            for upperBound in 0u .. maximumUpperBound do
                let concreteRangeOfIntegers = inclusiveUpToExclusiveRange 0u upperBound

                let chosenItems = random.ChooseSeveralOf(concreteRangeOfIntegers, upperBound)
                let shouldBeTrue = upperBound = (chosenItems |> Set.ofArray |> Seq.length |> uint32)
                Assert.IsTrue shouldBeTrue
                let shouldBeTrue = upperBound = (chosenItems |> Array.length |> uint32)
                Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestDistributionOfSuccessiveSequencesWithTheSameUpperBound() =
            let random = Random 1

            let maximumUpperBound = 30u

            for upperBound in 0u .. maximumUpperBound do
                let concreteRangeOfIntegers = inclusiveUpToExclusiveRange 0u upperBound

                let numberOfTrials = 100000

                let itemToCountAndSumOfPositionsMap = Array.create (int32 upperBound) (0, 0.0)

                for _ in 1 .. numberOfTrials do
                    for position, item in random.ChooseSeveralOf(concreteRangeOfIntegers, upperBound) |> Seq.mapi (fun position item -> position, item) do
                        let count, sumOfPositions = itemToCountAndSumOfPositionsMap.[int32 item]
                        itemToCountAndSumOfPositionsMap.[int32 item] <- 1 + count, (float position + sumOfPositions)

                let toleranceEpsilon = 1e-1

                let shouldBeTrue =
                    itemToCountAndSumOfPositionsMap
                    |> Seq.forall (fun (count, sumOfPositions)
                                    -> let difference = (sumOfPositions / (float count) - float (0u + upperBound - 1u) / 2.0)
                                       difference < toleranceEpsilon)     

                Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestThatAllItemsChosenBelongToTheSourceSequence() =
            commonTestStructureForTestingOfChoosingSeveralItems (fun superSet chosenItems _ ->
                                                                    let shouldBeTrue = (chosenItems |> Set.ofArray).IsSubsetOf superSet
                                                                    Assert.IsTrue shouldBeTrue)

        [<Test>]
        member this.TestThatTheNumberOfItemsRequestedIsHonouredIfPossible() =
            commonTestStructureForTestingOfChoosingSeveralItems (fun _ chosenItems subsetSize ->
                                                                    let shouldBeTrue = (chosenItems |> Array.length) = (subsetSize |> int32)
                                                                    Assert.IsTrue shouldBeTrue)

        [<Test>]
        member this.TestThatUniqueItemsInTheSourceSequenceAreNotDuplicated() =
            commonTestStructureForTestingOfChoosingSeveralItems (fun _ chosenItems _ ->
                                                                    let shouldBeTrue = (chosenItems |> Set.ofArray |> Seq.length) = (chosenItems |> Array.length)
                                                                    Assert.IsTrue shouldBeTrue)

        [<Test>]
        member this.TestThatChoosingItemsRepeatedlyEventuallyCoversAllPermutations() =
            let empiricallyDeterminedMultiplicationFactorToEnsureCoverage = double 70500 / (BargainBasement.Factorial 7u |> double)

            let random = Random 1

            for inclusiveLowerBound in 58u .. 98u do
                for numberOfConsecutiveItems in 1u .. 7u do
                    let superSet = inclusiveUpToExclusiveRange inclusiveLowerBound (inclusiveLowerBound + numberOfConsecutiveItems) |> Set.ofSeq
                    for subsetSize in 1u .. numberOfConsecutiveItems do
                        let expectedNumberOfPermutations = BargainBasement.NumberOfPermutations numberOfConsecutiveItems subsetSize
                        let oversampledOutputs =
                            seq {
                                for _ in 1 .. Math.Ceiling(empiricallyDeterminedMultiplicationFactorToEnsureCoverage * double expectedNumberOfPermutations) |> int32 do
                                    yield random.ChooseSeveralOf(superSet, subsetSize) |> List.ofArray
                                }
                        let shouldBeTrue = oversampledOutputs |> Set.ofSeq |> Seq.length = (expectedNumberOfPermutations |> int32)
                        Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestPig0GetInTheTrough() =
            pig 64000u

        [<Test>]
        member this.TestPig1() =
            pig 1000u

        [<Test>]
        member this.TestPig2() =
            pig 2000u

        [<Test>]
        member this.TestPig3() =
            pig 4000u

        [<Test>]
        member this.TestPig4() =
            pig 8000u

        [<Test>]
        member this.TestPig5() =
            pig 16000u

        [<Test>]
        member this.TestPig6() =
            pig 32000u

        [<Test>]
        member this.TestPig7() =
            pig 64000u

        [<Test>]
        member this.TestPig8() =
            pig 50000u

