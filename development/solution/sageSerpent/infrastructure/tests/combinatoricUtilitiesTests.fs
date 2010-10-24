#light

namespace SageSerpent.Infrastructure.Tests

    open NUnit.Framework
    open SageSerpent.Infrastructure

    [<TestFixture>]
    type CombinatoricUtilitiesTestFixture() =
        let rec contributionLimitsEquallingLimitInTotal limit maximumLengthOfContributionLimits =
            if maximumLengthOfContributionLimits = 0u
                then []
                else if maximumLengthOfContributionLimits = 1u
                     then [[limit]]
                     else let rec contributionLimitsWithFirstContributionOf firstContribution =
                            let partialResult = List.map (fun item -> firstContribution::item)
                                                         (contributionLimitsEquallingLimitInTotal (limit - firstContribution) (maximumLengthOfContributionLimits - 1u))
                            if firstContribution = 0u
                            then partialResult
                            else List.append (contributionLimitsWithFirstContributionOf (firstContribution - 1u)) partialResult
                          contributionLimitsWithFirstContributionOf limit
                          
        let contributionLimitsEquallingUpToLimitInTotal limit maximumLengthOfContributionLimits =
            List.flatten (Seq.map (function item -> contributionLimitsEquallingLimitInTotal item maximumLengthOfContributionLimits) [0u..limit])
            
        let sumContributions (contributionList: uint32 list) = List.reduce_right (fun x y -> x + y) contributionList
        
        [<Test>]
        member this.TestThatAttemptingToChooseContributionsFromAnEmptyListResultsInAnEmptyResultList () =
            for limit in [0u..5u] do
                let results = CombinatoricUtilities.chooseContributionsToMeetTotal [] limit
                let shouldBeTrue = results = []
                Assert.IsTrue shouldBeTrue
                
        [<Test>]
        member this.TestThatSupplyingATotalGreaterThanTheSumOfTheInputContributionsResultsInAnEmptyList () =
            for limit in [0u..5u] do
                for inputExample in contributionLimitsEquallingLimitInTotal limit 4u do
                    for increment in [1u..2u] do
                        let results = CombinatoricUtilities.chooseContributionsToMeetTotal [] (limit + increment)
                        let shouldBeTrue = results = []
                        Assert.IsTrue shouldBeTrue
                    
        [<Test>]
        member this.TestThatSumOfChosenContributionsInEachResultIsTheSameAsTheTotal () =
            for limit in [0u..3u] do
                for inputExample in contributionLimitsEquallingLimitInTotal limit 4u do
                    for total in [0u..limit] do
                        let results = CombinatoricUtilities.chooseContributionsToMeetTotal inputExample total
                        for sum in List.map sumContributions results do
                            let shouldBeTrue = sum = total
                            printf "(TestThatSumOfChosenContributionsInEachResultIsTheSameAsTheLimit) Input Example: %A\n" inputExample; Assert.IsTrue shouldBeTrue
                        let shouldBeTrue = not results.IsEmpty
                        Assert.IsTrue shouldBeTrue
                        
        [<Test>]
        member this.TestThatEachResultHasTheSameLengthAsTheInputContributionList () =
            for limit in [0u..3u] do
                for inputExample in contributionLimitsEquallingLimitInTotal limit 4u do
                    for total in [0u..limit] do
                        let results = CombinatoricUtilities.chooseContributionsToMeetTotal inputExample total
                        let shouldBeTrue = results.Length = (Set.of_list results).Count
                        Assert.IsTrue shouldBeTrue
               
        [<Test>]
        member this.TestThatEachResultOccursOnlyOnce () =
            for limit in [0u..3u] do
                for inputExample in contributionLimitsEquallingLimitInTotal limit 4u do
                    for total in [0u..limit] do
                        let results = CombinatoricUtilities.chooseContributionsToMeetTotal inputExample total
                        for item in results do
                            let shouldBeTrue = item.Length = inputExample.Length
                            printf "(TestThatEachResultHasTheSameLengthAsTheInputContributionList) Input Example: %A\n" inputExample; Assert.IsTrue shouldBeTrue
                        let shouldBeTrue = not results.IsEmpty
                        Assert.IsTrue shouldBeTrue
               
        [<Test>]
        member this.TestCoverageOfAllPossibleContributionsThatCanMeetTheTotal () =
            for total in [0u..5u] do
                let inputExamples = contributionLimitsEquallingLimitInTotal total 5u
                let combinedResultsFromAllPossibleInputExamplesSummingToTotal
                    = Set.of_list (List.flatten (List.map (fun inputExample -> CombinatoricUtilities.chooseContributionsToMeetTotal inputExample total)
                                                           inputExamples))
                let inputExamplesAsSet = Set.of_list inputExamples
                printf "(TestCoverageOfAllPossibleContributionsThatCanMeetTheTotal) Number of input examples: %d, number of combined results: %d\n"
                       inputExamplesAsSet.Count
                       combinedResultsFromAllPossibleInputExamplesSummingToTotal.Count
                let shouldBeTrue = combinedResultsFromAllPossibleInputExamplesSummingToTotal = inputExamplesAsSet
                Assert.IsTrue shouldBeTrue
                if combinedResultsFromAllPossibleInputExamplesSummingToTotal.IsEmpty
                then raise (InternalAssertionViolation "Internal failure in test: should have at least one input example.")
                    
        
        [<Test>]
        member this.TestThatContributionsThatMeetUpToATotalProduceTheSameResultsAsContributionsThatEqualATotal () =
            for limit in [0u..3u] do
                let inputExamples = contributionLimitsEquallingLimitInTotal limit 5u
                for inputExample in inputExamples do
                    let enMasseResults = CombinatoricUtilities.chooseContributionsToMeetTotalsUpToLimit inputExample limit
                    for total in [0u..limit] do
                        let resultsFromEnMasseCalculationForTotal = enMasseResults.[total]
                        let resultsFromEnMasseCalculationForTotalAsSet = Set.of_list resultsFromEnMasseCalculationForTotal
                        let resultsFromIndividualCalculation = CombinatoricUtilities.chooseContributionsToMeetTotal inputExample total
                        let resultsFromIndividualCalculationAsSet = Set.of_list resultsFromIndividualCalculation
                        printf "(TestThatContributionsThatMeetUpToATotalProduceTheSameResultsAsContributionsThatEqualATotal) Number of results from en-masse calculation: %d, number of results from individual calculation: %d\n"
                               resultsFromEnMasseCalculationForTotal.Length
                               resultsFromIndividualCalculation.Length
                        let shouldBeTrue = resultsFromIndividualCalculationAsSet = resultsFromEnMasseCalculationForTotalAsSet
                                           && resultsFromEnMasseCalculationForTotal.Length = resultsFromIndividualCalculation.Length
                        Assert.IsTrue shouldBeTrue