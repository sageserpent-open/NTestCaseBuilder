namespace SageSerpent.Infrastructure.Tests

    open NUnit.Framework

    open scala.util
    open scala.collection.immutable
    open com.sageserpent.infrastructure
    open SageSerpent.Infrastructure

    [<TestFixture>]
    type RandomExtensionsTestFixture() =
        let inclusiveUpToExclusiveRange inclusiveLowerLimit exclusiveUpperLimit =
            Range(inclusiveLowerLimit, exclusiveUpperLimit, 1)

        let inclusiveUpToInclusiveRange inclusiveLowerLimit inclusiveUpperLimit =
            Range.Inclusive(inclusiveLowerLimit, inclusiveUpperLimit, 1)

        let jemmyScalaCollectionToFSharpSeq (scalaCollection: scala.collection.Seq): seq<'X> =
            let iterator = scalaCollection.iterator()

            seq {
                while iterator.hasNext() do
                    yield unbox<'X> (iterator.next())
            }

        let commonTestStructureForTestingOfChoosingSeveralItems testOnSuperSetAndItemsChosenFromIt =
            let  random = RichRandom (Random 1)

            for inclusiveLowerBound in 58 .. 98 do
                for numberOfConsecutiveItems in 1 .. 50 do
                    let superSet = (inclusiveUpToExclusiveRange inclusiveLowerBound (inclusiveLowerBound + numberOfConsecutiveItems)).toSet()
                    for subsetSize in 1 .. numberOfConsecutiveItems do
                        for _ in 1 .. 10 do
                            let chosenItems = random.chooseSeveralOf(superSet, subsetSize)
                            testOnSuperSetAndItemsChosenFromIt superSet chosenItems subsetSize

        let pig maximumUpperBound =
            let random = RichRandom (Random 678)
            let concreteRangeOfIntegers = inclusiveUpToExclusiveRange 0 maximumUpperBound
    
            for _ in 1 .. 10 do
                let chosenItems = random.chooseSeveralOf(concreteRangeOfIntegers, maximumUpperBound)
                for chosenItem in chosenItems |> jemmyScalaCollectionToFSharpSeq do
                    ()  

        [<Test>]
        member this.TestCoverageOfIntegersUpToExclusiveUpperBound() =
            let random = RichRandom (Random 29)

            let maximumUpperBound = 30

            for upperBound in 0 .. maximumUpperBound do
                let concreteRangeOfIntegers = inclusiveUpToExclusiveRange 0 upperBound

                let chosenItems = random.chooseSeveralOf(concreteRangeOfIntegers, upperBound)
                let expectedRange = inclusiveUpToExclusiveRange 0 upperBound
                let shouldBeTrue = chosenItems.toSet() = expectedRange.toSet()
                Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestUniquenessOfIntegersProduced() =
            let random = RichRandom (Random 678)

            let maximumUpperBound = 30

            for upperBound in 0 .. maximumUpperBound do
                let concreteRangeOfIntegers = inclusiveUpToExclusiveRange 0 upperBound

                let chosenItems = random.chooseSeveralOf(concreteRangeOfIntegers, upperBound)
                let shouldBeTrue = upperBound = chosenItems.toSet().size()
                Assert.IsTrue shouldBeTrue
                let shouldBeTrue = upperBound = chosenItems.size()
                Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestDistributionOfSuccessiveSequencesWithTheSameUpperBound() =
            let random = RichRandom (Random 1)

            let maximumUpperBound = 30

            for upperBound in 0 .. maximumUpperBound do
                let concreteRangeOfIntegers = inclusiveUpToExclusiveRange 0 upperBound

                let numberOfTrials = 100000

                let itemToCountAndSumOfPositionsMap = Array.create (int32 upperBound) (0, 0.0)

                for _ in 1 .. numberOfTrials do
                    for position, item in random.chooseSeveralOf(concreteRangeOfIntegers, upperBound) |> jemmyScalaCollectionToFSharpSeq |> Seq.mapi (fun position item -> position, item) do
                        let count, sumOfPositions = itemToCountAndSumOfPositionsMap.[int32 item]
                        itemToCountAndSumOfPositionsMap.[int32 item] <- 1 + count, (float position + sumOfPositions)

                let toleranceEpsilon = 1e-1

                let shouldBeTrue =
                    itemToCountAndSumOfPositionsMap
                    |> Seq.forall (fun (count, sumOfPositions)
                                    -> let difference = (sumOfPositions / (float count) - float (0 + upperBound - 1) / 2.0)
                                       difference < toleranceEpsilon)     

                Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestThatAllItemsChosenBelongToTheSourceSequence() =
            commonTestStructureForTestingOfChoosingSeveralItems (fun superSet chosenItems _ ->
                                                                    let shouldBeTrue = chosenItems.toSet().subsetOf(superSet)
                                                                    Assert.IsTrue shouldBeTrue)

        [<Test>]
        member this.TestThatTheNumberOfItemsRequestedIsHonouredIfPossible() =
            commonTestStructureForTestingOfChoosingSeveralItems (fun _ chosenItems subsetSize ->
                                                                    let shouldBeTrue = chosenItems.size() = subsetSize
                                                                    Assert.IsTrue shouldBeTrue)

        [<Test>]
        member this.TestThatUniqueItemsInTheSourceSequenceAreNotDuplicated() =
            commonTestStructureForTestingOfChoosingSeveralItems (fun _ chosenItems _ ->
                                                                    let shouldBeTrue = chosenItems.toSet().size() = chosenItems.size()
                                                                    Assert.IsTrue shouldBeTrue)

        [<Test>]
        member this.TestThatChoosingItemsRepeatedlyEventuallyCoversAllPermutations() =
            let empiricallyDeterminedMultiplicationFactorToEnsureCoverage = double 70500 / (BargainBasement.Factorial 7u |> double)

            let random = RichRandom (Random 1)

            for inclusiveLowerBound in 58 .. 98 do
                for numberOfConsecutiveItems in 1 .. 7 do
                    let superSet = (inclusiveUpToExclusiveRange inclusiveLowerBound (inclusiveLowerBound + numberOfConsecutiveItems)).toSet()
                    for subsetSize in 1 .. numberOfConsecutiveItems do
                        let expectedNumberOfPermutations = BargainBasement.NumberOfPermutations (numberOfConsecutiveItems |> uint32) (subsetSize |> uint32)
                        let oversampledOutputs =
                            seq {
                                for _ in 1 .. scala.math.package.ceil(empiricallyDeterminedMultiplicationFactorToEnsureCoverage * double expectedNumberOfPermutations) |> int32 do
                                    yield random.chooseSeveralOf(superSet, subsetSize) |> jemmyScalaCollectionToFSharpSeq |> List.ofSeq
                                }
                        let shouldBeTrue = oversampledOutputs |> Set.ofSeq |> Seq.length = (expectedNumberOfPermutations |> int32)
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
        member this.TestPig9() =
            pig 100000

        [<Test>]
        member this.TestPig10() =
            pig 200000

        [<Test>]
        member this.TestPig11() =
            pig 500000

        [<Test>]
        member this.TestPig12() =
            pig 1000000

