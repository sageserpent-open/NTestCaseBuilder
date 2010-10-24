namespace SageSerpent.Infrastructure.Tests

    open NUnit.Framework
    open SageSerpent.Infrastructure
    open SageSerpent.Infrastructure.IEnumerableExtensions
    open System.Collections.Generic

    [<TestFixture>]
    type CombinatoricUtilitiesTestFixture () =
        let rec contributionLimitsEquallingLimitInTotal limit maximumLengthOfContributionLimits =
            if maximumLengthOfContributionLimits = 0u
                then []
                else if maximumLengthOfContributionLimits = 1u
                     then [[limit]]
                     else let rec contributionLimitsWithFirstContributionOf firstContribution =
                            let partialResult =
                                (contributionLimitsEquallingLimitInTotal (limit - firstContribution) (maximumLengthOfContributionLimits - 1u))
                                |> List.map (fun item -> firstContribution::item)
                            if firstContribution = 0u
                            then partialResult
                            else List.append (contributionLimitsWithFirstContributionOf (firstContribution - 1u)) partialResult
                          contributionLimitsWithFirstContributionOf limit
                          
        let contributionLimitsEquallingUpToLimitInTotal limit maximumLengthOfContributionLimits =
            [0u .. limit]
            |> Seq.map (function item -> contributionLimitsEquallingLimitInTotal item maximumLengthOfContributionLimits)
            |> List.concat
             
        let sumContributions = List.reduceBack (+)
        
        let sequenceOfOrderedListsOfUniqueItems = List.init 10 (fun item -> item)
                                                  |> (BargainBasement.Flip (List.scanBack (BargainBasement.Curry List.Cons))) []
                                                          
        [<Test>]
        member this.TestThatAttemptingToChooseContributionsFromAnEmptyListResultsInAnEmptyResultList () =
            for limit in 0u .. 5u do
                let results = CombinatoricUtilities.ChooseContributionsToMeetTotal [] limit
                let shouldBeTrue = results = []
                Assert.IsTrue shouldBeTrue
                
        [<Test>]
        member this.TestThatSupplyingATotalGreaterThanTheSumOfTheInputContributionsResultsInAnEmptyList () =
            for limit in 0u .. 5u do
                for inputExample in contributionLimitsEquallingLimitInTotal limit 4u do
                    for increment in 1u .. 2u do
                        let results = CombinatoricUtilities.ChooseContributionsToMeetTotal inputExample (limit + increment)
                        let shouldBeTrue = results = []
                        Assert.IsTrue shouldBeTrue
                    
        [<Test>]
        member this.TestThatSumOfChosenContributionsInEachResultIsTheSameAsTheTotal () =
            for limit in 0u .. 3u do
                for inputExample in contributionLimitsEquallingLimitInTotal limit 4u do
                    for total in 0u .. limit do
                        let results = CombinatoricUtilities.ChooseContributionsToMeetTotal inputExample total
                        for sum in List.map sumContributions results do
                            let shouldBeTrue = sum = total
                            printf "(TestThatSumOfChosenContributionsInEachResultIsTheSameAsTheLimit) Input Example: %A\n" inputExample; Assert.IsTrue shouldBeTrue
                        let shouldBeTrue = not results.IsEmpty
                        Assert.IsTrue shouldBeTrue
                        
        [<Test>]
        member this.TestThatSumOfChosenContributionsInEachResultCannotExceedSumOfInputContributions () =
            for limit in 0u .. 3u do
                for inputExample in contributionLimitsEquallingLimitInTotal limit 4u do
                    for increment in 1u .. 3u do
                        let results = CombinatoricUtilities.ChooseContributionsToMeetTotal inputExample (limit + increment)
                        let shouldBeTrue = results.IsEmpty
                        Assert.IsTrue shouldBeTrue
                        
        [<Test>]
        member this.TestThatChosenContributionsInEachResultAreBoundedByInputContributions () =
            for limit in 0u .. 3u do
                for inputExample in contributionLimitsEquallingLimitInTotal limit 4u do
                    for total in 0u .. limit do
                        let results = CombinatoricUtilities.ChooseContributionsToMeetTotal inputExample total
                        for item in results do
                            for chosenAndInputContributionPair in List.zip item inputExample do
                                let shouldBeTrue = fst chosenAndInputContributionPair <= snd chosenAndInputContributionPair
                                Assert.IsTrue shouldBeTrue
                        
        [<Test>]
        member this.TestThatEachResultOccursOnlyOnce () =
            for limit in 0u .. 3u do
                for inputExample in contributionLimitsEquallingLimitInTotal limit 4u do
                    for total in 0u .. limit do
                        let results = CombinatoricUtilities.ChooseContributionsToMeetTotal inputExample total
                        let shouldBeTrue = results.Length = (Set.ofList results).Count
                        Assert.IsTrue shouldBeTrue
               
        [<Test>]
        member this.TestThatEachResultHasTheSameLengthAsTheInputContributionList () =
            for limit in 0u .. 3u do
                for inputExample in contributionLimitsEquallingLimitInTotal limit 4u do
                    for total in 0u .. limit do
                        let results = CombinatoricUtilities.ChooseContributionsToMeetTotal inputExample total
                        for item in results do
                            let shouldBeTrue = item.Length = inputExample.Length
                            printf "(TestThatEachResultHasTheSameLengthAsTheInputContributionList) Input Example: %A\n" inputExample; Assert.IsTrue shouldBeTrue
                        let shouldBeTrue = not results.IsEmpty
                        Assert.IsTrue shouldBeTrue
               
        [<Test>]
        member this.TestCoverageOfAllPossibleContributionsThatCanMeetTheTotal () =
            for total in 0u .. 5u do
                let inputExamples = contributionLimitsEquallingLimitInTotal total 5u
                let combinedResultsFromAllPossibleInputExamplesSummingToTotal =
                    inputExamples
                    |> List.map (fun inputExample -> CombinatoricUtilities.ChooseContributionsToMeetTotal inputExample total)
                    |> List.concat
                    |> Set.ofList
                let inputExamplesAsSet = Set.ofList inputExamples
                printf "(TestCoverageOfAllPossibleContributionsThatCanMeetTheTotal) Number of input examples: %d, number of combined results: %d\n"
                       inputExamplesAsSet.Count
                       combinedResultsFromAllPossibleInputExamplesSummingToTotal.Count
                let shouldBeTrue = combinedResultsFromAllPossibleInputExamplesSummingToTotal = inputExamplesAsSet
                Assert.IsTrue shouldBeTrue
                if combinedResultsFromAllPossibleInputExamplesSummingToTotal.IsEmpty
                then raise (InternalAssertionViolationException "Internal failure in test: should have at least one input example.")
                    
        
        [<Test>]
        member this.TestThatContributionsThatMeetUpToATotalProduceTheSameResultsAsContributionsThatEqualATotal () =
            for limit in 0u .. 3u do
                let inputExamples = contributionLimitsEquallingLimitInTotal limit 5u
                for inputExample in inputExamples do
                    let enMasseResults = CombinatoricUtilities.ChooseContributionsToMeetTotalsUpToLimit inputExample limit
                    for total in 0u .. limit do
                        let resultsFromEnMasseCalculationForTotal = enMasseResults.[total]
                        let resultsFromEnMasseCalculationForTotalAsSet = Set.ofList resultsFromEnMasseCalculationForTotal
                        let resultsFromIndividualCalculation = CombinatoricUtilities.ChooseContributionsToMeetTotal inputExample total
                        let resultsFromIndividualCalculationAsSet = Set.ofList resultsFromIndividualCalculation
                        printf "(TestThatContributionsThatMeetUpToATotalProduceTheSameResultsAsContributionsThatEqualATotal) Number of results from en-masse calculation: %d, number of results from individual calculation: %d\n"
                               resultsFromEnMasseCalculationForTotal.Length
                               resultsFromIndividualCalculation.Length
                        let shouldBeTrue = resultsFromIndividualCalculationAsSet = resultsFromEnMasseCalculationForTotalAsSet
                                           && resultsFromEnMasseCalculationForTotal.Length = resultsFromIndividualCalculation.Length
                        Assert.IsTrue shouldBeTrue
                        
        [<Test>]
        member this.TestThatCalculatingContributionsThatMeetUpToATotalNeverProduceEmptyResultsForAGivenTotal () =
            for limit in 0u .. 3u do
                let inputExamples = contributionLimitsEquallingLimitInTotal limit 5u
                for inputExample in inputExamples do
                    for increment in 1u .. 3u do
                        let unachievableTotal = limit + increment
                        let enMasseResults = CombinatoricUtilities.ChooseContributionsToMeetTotalsUpToLimit inputExample unachievableTotal
                        let shouldBeTrue = not (enMasseResults.ContainsKey unachievableTotal)
                        Assert.IsTrue shouldBeTrue
        
        
        
        
        [<Test>]
        member this.TestThatAZeroCombinationOfItemsIsAlwaysASingleEmptyList () =
            for items in sequenceOfOrderedListsOfUniqueItems do
                let combinations = CombinatoricUtilities.GenerateCombinationsOfGivenSizePreservingOrder 0u items
                let shouldBeTrue = 1 = Seq.length combinations
                Assert.IsTrue shouldBeTrue
                match Seq.head combinations with
                    [] -> ()
                  | _ -> Assert.Fail ()
                  
        [<Test>]
        member this.TestThatAnOversizedCombinationCannotBeRequested () =
            for items in sequenceOfOrderedListsOfUniqueItems do
                for increment in 1u .. 3u do
                    let unachievableSize =
                        (List.length items
                         |> uint32)
                        + increment
                    let combinations = CombinatoricUtilities.GenerateCombinationsOfGivenSizePreservingOrder unachievableSize items
                    let shouldBeTrue =
                        Seq.isEmpty combinations
                    Assert.IsTrue shouldBeTrue
                    
        [<Test>]
        member this.TestThatTheExpectedNumberOfCombinationsAreGenerated () =
            for items in sequenceOfOrderedListsOfUniqueItems do
                let maximumSize = 
                    List.length items
                    |> uint32
                for size in 1u .. maximumSize do
                    let shouldBeTrue =
                        BargainBasement.NumberOfCombinations maximumSize size
                         = (Seq.length (CombinatoricUtilities.GenerateCombinationsOfGivenSizePreservingOrder size items)
                            |> uint32)
                    Assert.IsTrue shouldBeTrue
                    
        [<Test>]
        member this.TestThatCombinationsTakenFromUniqueItemsContainUniqueItems () =
            for items in sequenceOfOrderedListsOfUniqueItems do
                let maximumSize = 
                    List.length items
                    |> uint32
                for size in 1u .. maximumSize do
                    let shouldBeTrue =
                        not (CombinatoricUtilities.GenerateCombinationsOfGivenSizePreservingOrder size items
                             |> Seq.map (Set.ofList >> Set.count >> uint32)
                             |> Seq.exists ((<>) size))
                    Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestThatCombinationsTakenFromUniqueItemsAreAllDistinct () =
            for items in sequenceOfOrderedListsOfUniqueItems do
                let maximumSize = 
                    List.length items
                    |> uint32
                for size in 1u .. maximumSize do
                    let combinations =
                        CombinatoricUtilities.GenerateCombinationsOfGivenSizePreservingOrder size items
                    let shouldBeTrue =
                        Seq.length combinations
                         = (combinations
                            |> Seq.map Set.ofList
                            |> Set.ofSeq
                            |> Set.count)
                    Assert.IsTrue shouldBeTrue

                                
        [<Test>]
        member this.TestThatCombinationsTakenTogetherIncludeAllTheItemsTheyWereTakenFrom () =
            for items in sequenceOfOrderedListsOfUniqueItems do
                let maximumSize = 
                    List.length items
                    |> uint32
                for size in 1u .. maximumSize do
                    let shouldBeTrue =
                        Set.ofList items
                         = (CombinatoricUtilities.GenerateCombinationsOfGivenSizePreservingOrder size items
                            |> Seq.map Set.ofList
                            |> Set.unionMany)
                    Assert.IsTrue shouldBeTrue
                                
        [<Test>]
        member this.TestThatCombinationsPreserveTheOriginalOrderOfItems () =
            for items in sequenceOfOrderedListsOfUniqueItems do
                let maximumSize = 
                    List.length items
                    |> uint32
                for size in 1u .. maximumSize do
                    let shouldBeTrue =
                        not (CombinatoricUtilities.GenerateCombinationsOfGivenSizePreservingOrder size items
                             |> Seq.exists (not << IEnumerable<_>.IsSorted))
                    Assert.IsTrue shouldBeTrue    
                    
        [<Test>]
        member this.TestThatChoosingASpecificCombinationYieldsTheRightOne () =
            for items in sequenceOfOrderedListsOfUniqueItems do
                let maximumSize = 
                    List.length items
                    |> uint32
                for size in 0u .. maximumSize do
                    let combinationsObtainedEnMasse =
                        CombinatoricUtilities.GenerateCombinationsOfGivenSizePreservingOrder size items
                        |> List.ofSeq
                    let numberOfCombinations =
                        List.length combinationsObtainedEnMasse
                    let combinationsPickedOutSpecifically =
                        List.init numberOfCombinations
                                 (fun indexOfCombination
                                    -> CombinatoricUtilities.GenerateCombinationOfGivenSizePreservingOrder size (List.toArray items) (uint32 indexOfCombination))
                    let shouldBeTrue =
                        combinationsObtainedEnMasse = combinationsPickedOutSpecifically
                    printf "combinationsObtainedEnMasse: %A, combinationsPickedOutSpecifically: %A\n" combinationsObtainedEnMasse combinationsPickedOutSpecifically
                    Assert.IsTrue shouldBeTrue                                                                      