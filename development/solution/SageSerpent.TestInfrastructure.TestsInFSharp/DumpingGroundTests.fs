#light

namespace SageSerpent.TestInfrastructure.Tests

    open NUnit.Framework
    
    open SageSerpent.Infrastructure
    open SageSerpent.TestInfrastructure
    open System
    open System.Windows.Forms
    open System.Drawing
    open System.Collections.Generic
    open Wintellect.PowerCollections
    
    type private TestLevel =
            Untracked of UInt32 * UInt32   // level, number of tracked variables that interleave
          | Tracked of UInt32 * UInt32          // tracked variable index, level
          
    type DistributionModeWrtInterleavingNode =
            BetweenSiblingSubtrees
          | WithinOnlyASingleSubtree
    
    [<TestFixture>]
    type DumpingGroundTestFixture () =
        let dumpTree tree =
            let form = new Form ()
            form.AutoSizeMode <- AutoSizeMode.GrowOnly
            form.AutoSize <- true
            let treeView = new TreeView ()
            form.Controls.Add treeView
            treeView.Dock <- DockStyle.Fill
            let treeGuiNode = TreeNode ()
            treeView.Nodes.Add treeGuiNode |> ignore
            let rec dumpNode node (treeGuiNode: TreeNode) =
                match node with
                    TestVariableNode levels -> let subtreeGuiNode = TreeNode ("TreeNode\n" + any_to_string levels)
                                               treeGuiNode.Nodes.Add subtreeGuiNode |> ignore
                  | InterleavingNode subtrees -> let subtreeGuiNode = TreeNode ("InterleavingNode\n")
                                                 treeGuiNode.Nodes.Add subtreeGuiNode |> ignore
                                                 for subtree in subtrees do
                                                    dumpNode subtree subtreeGuiNode
                  | SynthesizingNode subtrees -> let subtreeGuiNode = TreeNode ("SynthesizingNode\n")
                                                 treeGuiNode.Nodes.Add subtreeGuiNode |> ignore
                                                 for subtree in subtrees do
                                                    dumpNode subtree subtreeGuiNode
            dumpNode tree treeGuiNode
            treeView.ExpandAll ()
            form.ShowDialog () |> ignore

        let maximumNumberOfTrackedTestVariables = 4u
        let maximumNumberOfTestLevelsForATestVariable = 3u
        let maximumNumberOfSubtreeHeadsPerAncestorNode = 5u
        let maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables = 1u
        let randomSeed = 23
        
        let createTestCasesAndHandEachOffToTest distributionModeWrtInterleavingNode testHandoff =
            let randomBehaviour = Random randomSeed
            let chooseAnyNumberFromZeroToOneLessThan = int32 >> randomBehaviour.Next >> uint32
            let chooseAnyNumberFromOneTo = chooseAnyNumberFromZeroToOneLessThan >> (+) 1u
            let headsItIs () = chooseAnyNumberFromZeroToOneLessThan 2u = 0u
            let didTheSingleTestVariableEdgeCase = ref false
            let timeAtStart = DateTime.Now
            for _ in 0u .. 90u do
                let numberOfTrackedTestVariables = chooseAnyNumberFromOneTo maximumNumberOfTrackedTestVariables
                if numberOfTrackedTestVariables = 1u
                then didTheSingleTestVariableEdgeCase := true
                let trackedTestVariableToNumberOfLevelsMap =
                    Map.of_list (List.init (int32 numberOfTrackedTestVariables)
                                           (fun testVariable -> testVariable, chooseAnyNumberFromOneTo maximumNumberOfTestLevelsForATestVariable))
                let rec createTree distributionModeWrtInterleavingNode
                                   indexForLeftmostTrackedTestVariable
                                   numberOfTrackedTestVariables
                                   maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables
                                   interleavedTrackedVariablesFromSiblingsOfParents =
                    let thinkAboutTerminatingRecursion = numberOfTrackedTestVariables <= 1u
                    if headsItIs () && thinkAboutTerminatingRecursion
                       || maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables = 0u
                    then if numberOfTrackedTestVariables = 1u
                         then TestVariableNode ([for level in 1u .. trackedTestVariableToNumberOfLevelsMap.[int32 indexForLeftmostTrackedTestVariable] do
                                                    yield box (Tracked (indexForLeftmostTrackedTestVariable, level))])
                              , indexForLeftmostTrackedTestVariable + 1u
                              , interleavedTrackedVariablesFromSiblingsOfParents  
                         else TestVariableNode ([for level in 1u .. chooseAnyNumberFromOneTo maximumNumberOfTestLevelsForATestVariable do
                                                    yield box (Untracked (level, uint32 (List.length interleavedTrackedVariablesFromSiblingsOfParents)))])
                              , indexForLeftmostTrackedTestVariable
                              , interleavedTrackedVariablesFromSiblingsOfParents
                    else let allOnOneSubtreeDistributionMaker numberOfSubtrees numberOfTrackedTestVariables =
                            let choice =
                                chooseAnyNumberFromOneTo numberOfSubtrees
                            [for counter in 1u .. numberOfSubtrees do
                                if counter = choice
                                then yield numberOfTrackedTestVariables
                                else yield 0u]
                         let arbitrarilySpreadAcrossSubtreesDistributionMaker numberOfSubtrees numberOfTrackedTestVariables =
                            if numberOfSubtrees = 1u
                            then [numberOfTrackedTestVariables]
                            else let rec generateAsManyCumulativeSumsAs numberOfSums cumulativeSum partialResult =
                                    if numberOfSums = 0u
                                    then partialResult
                                    else let nextLowerCumulativeSum =
                                            if cumulativeSum > 0u
                                            then cumulativeSum - 1u
                                            else 0u
                                         generateAsManyCumulativeSumsAs (numberOfSums - 1u)
                                                                        nextLowerCumulativeSum
                                                                        (nextLowerCumulativeSum :: partialResult)
                                 let cumulativeSumsUpToOneLessThanNumberOfTrackedTestVariables =
                                    generateAsManyCumulativeSumsAs (max numberOfSubtrees numberOfTrackedTestVariables
                                                                    - 1u) numberOfTrackedTestVariables []
                                 let pickRandomlyAllowingRepetitionOfChoices sums =
                                    let sums
                                        = List.to_array sums
                                    List.of_array [|for _ in 1u .. numberOfSubtrees - 1u do
                                                        yield sums.[int32 (chooseAnyNumberFromZeroToOneLessThan (uint32 sums.Length))]|]
                                 let selectedSumsIncludingNumberOfTrackedTestVariables =
                                    List.append (pickRandomlyAllowingRepetitionOfChoices [0u .. numberOfTrackedTestVariables]
                                                 |> List.sort compare) [numberOfTrackedTestVariables]
                                 let firstSum
                                    = List.hd selectedSumsIncludingNumberOfTrackedTestVariables
                                 let leadingSumAndSubsequentDifferences =
                                    firstSum :: ((Seq.pairwise selectedSumsIncludingNumberOfTrackedTestVariables)
                                                 |> Seq.map (function first, second -> second - first)
                                                 |> List.of_seq)
                                 Algorithms.RandomShuffle (leadingSumAndSubsequentDifferences, randomBehaviour)
                                 |> List.of_array
                         let distributionMakerForSynthesizingNodes =
                            // The following definition looks as if it is transposed - but it is not: the
                            // modes being matched refer to *interleaving* nodes. The crucial point here
                            // is that we want to avoid accidently separating test variables down subtrees
                            // of synthesizing nodes when we are trying to force them to be spread across
                            // subtrees of interleaving nodes.
                            match distributionModeWrtInterleavingNode with
                                BetweenSiblingSubtrees -> allOnOneSubtreeDistributionMaker
                              | WithinOnlyASingleSubtree -> arbitrarilySpreadAcrossSubtreesDistributionMaker
                         let distributionMakerForInterleavedNodes numberOfSubtrees numberOfTrackedTestVariables =
                            match distributionModeWrtInterleavingNode with
                                BetweenSiblingSubtrees when numberOfSubtrees > 1u ->
                                    if numberOfTrackedTestVariables > 1u
                                    then let rec incrementOneOfTheEntriesWithoutPuttingAllOfTheTrackedVariablesInOneSubtree distribution =
                                            match distribution with
                                                [] -> raise (InternalAssertionViolationException "Either given an empty distribution or one with too high a limit.")
                                              | head :: tail -> if head + 1u = numberOfTrackedTestVariables
                                                                then head :: incrementOneOfTheEntriesWithoutPuttingAllOfTheTrackedVariablesInOneSubtree tail
                                                                else head + 1u :: tail
                                         incrementOneOfTheEntriesWithoutPuttingAllOfTheTrackedVariablesInOneSubtree
                                            (arbitrarilySpreadAcrossSubtreesDistributionMaker numberOfSubtrees (numberOfTrackedTestVariables - 1u))
                                    else arbitrarilySpreadAcrossSubtreesDistributionMaker numberOfSubtrees numberOfTrackedTestVariables
                              | _ -> allOnOneSubtreeDistributionMaker numberOfSubtrees numberOfTrackedTestVariables
                         let generateNode nodeFactory 
                                          distributionMaker
                                          interleavedTrackedVariablesFromSiblingsGroupedAccordingToSubtreesOfNode =
                            let numberOfSubtrees =
                                chooseAnyNumberFromOneTo maximumNumberOfSubtreeHeadsPerAncestorNode
                            let gatherSubtree (previouslyGatheredSubtrees
                                               , indexForLeftmostTrackedTestVariable
                                               , trackedVariablesFoundPreviouslyThatInterleaveWithUntrackedOnesWithDuplicateEntries)
                                              (numberOfTrackedVariables
                                               , interleavedTrackedVariablesFromSiblings) =
                                let subtree
                                    , maximumTrackingVariableIndexFromSubtree
                                    , trackedVariablesFoundInSubtreeThatInterleaveWithUntrackedOnesWithDuplicateEntries =
                                    createTree distributionModeWrtInterleavingNode
                                               indexForLeftmostTrackedTestVariable
                                               numberOfTrackedVariables
                                               (if thinkAboutTerminatingRecursion
                                                then maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables - 1u
                                                else maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables)
                                               interleavedTrackedVariablesFromSiblings
                                subtree :: previouslyGatheredSubtrees
                                , maximumTrackingVariableIndexFromSubtree
                                , BargainBasement.MergeSortedListsAllowingDuplicates trackedVariablesFoundInSubtreeThatInterleaveWithUntrackedOnesWithDuplicateEntries
                                                                                     trackedVariablesFoundPreviouslyThatInterleaveWithUntrackedOnesWithDuplicateEntries
                            let distributionOfNumberOfTrackedTestVariablesForEachSubtree =
                                distributionMaker numberOfSubtrees numberOfTrackedTestVariables
                            let interleavedTrackedVariablesFromSiblingsGroupedAccordingToSubtrees =
                                interleavedTrackedVariablesFromSiblingsGroupedAccordingToSubtreesOfNode distributionOfNumberOfTrackedTestVariablesForEachSubtree
                                                                                                        interleavedTrackedVariablesFromSiblingsOfParents
                            let subtrees
                                , maximumTrackingVariableIndex
                                , trackedVariablesThatInterleaveWithUntrackedOnesWithDuplicateEntries =
                                List.zip distributionOfNumberOfTrackedTestVariablesForEachSubtree interleavedTrackedVariablesFromSiblingsGroupedAccordingToSubtrees                     
                                |> List.fold_left gatherSubtree ([], indexForLeftmostTrackedTestVariable, [])
                            nodeFactory subtrees
                            , maximumTrackingVariableIndex
                            , trackedVariablesThatInterleaveWithUntrackedOnesWithDuplicateEntries
                         let interleavedTrackedVariablesFromSiblingsGroupedAccordingToSubtreesOfInterleavingNode distribution contributionFromParentNode =
                            let rec accumulateContributionsFromTheLeftAndThenJoinInContributionsFromTheRight distribution
                                                                                                             (contributionsFromTheLeft, indexForLeftmostTrackedTestVariable) =
                                match distribution with
                                    [] ->
                                        [],
                                        []
                                  | head :: tail ->
                                        let trackedVariablesFromHead =
                                            List.init (int32 head) (fun trackedVariableCount -> uint32 trackedVariableCount + indexForLeftmostTrackedTestVariable)
                                        let partialResult, contributionsFromTheRight =
                                            accumulateContributionsFromTheLeftAndThenJoinInContributionsFromTheRight tail
                                                                                                                     (List.append trackedVariablesFromHead contributionsFromTheLeft
                                                                                                                      , head + indexForLeftmostTrackedTestVariable)
                                        List.append contributionsFromTheLeft contributionsFromTheRight :: partialResult
                                        , List.append trackedVariablesFromHead contributionsFromTheRight
                            let result, _ =
                                accumulateContributionsFromTheLeftAndThenJoinInContributionsFromTheRight distribution
                                                                                                         (contributionFromParentNode, indexForLeftmostTrackedTestVariable)
                            result                                                                  
                         let interleavedTrackedVariablesFromSiblingsGroupedAccordingToSubtreesOfSynthesizingNode distribution contributionFromParentNode =
                            distribution
                            |> List.map (fun _ -> contributionFromParentNode)
                         if headsItIs ()
                         then generateNode (fun subtrees -> InterleavingNode subtrees)
                                           distributionMakerForInterleavedNodes
                                           interleavedTrackedVariablesFromSiblingsGroupedAccordingToSubtreesOfInterleavingNode
                         else generateNode (fun subtrees -> SynthesizingNode subtrees)
                                           distributionMakerForSynthesizingNodes
                                           interleavedTrackedVariablesFromSiblingsGroupedAccordingToSubtreesOfSynthesizingNode                          
                let tree
                    , _
                    , trackedVariablesThatInterleaveWithUntrackedOnesWithDuplicateEntries = 
                    createTree distributionModeWrtInterleavingNode
                               0u
                               numberOfTrackedTestVariables
                               maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables
                               []
                // TODO - add in some assertions on this - we could confirm that for each k-combination
                // of tracked test variables, we see exclusion sentinel values for N untracked test
                // variables: N should be >= the minimum of the associated values for all of the k tracked
                // test variables, and <= the sum of the associated values.
                let associationFromTrackedVariablesToNumberOfInterleavingUntrackedVariables =
                    trackedVariablesThatInterleaveWithUntrackedOnesWithDuplicateEntries
                    |> BargainBasement.CountDuplicatesInSortedList
                    |> Map.of_list
                let maximumStrengthOfTestVariableCombination =
                    tree.MaximumStrengthOfTestVariableCombination
                if maximumStrengthOfTestVariableCombination < 60u
                then //printf "**** Tree:\n%A\n\n" tree
                     //printf "Number of test variables: %d\nNumber of levels overall: %d\nMaximum strength: %d\n\n"
                     //       tree.CountTestVariables
                     //       tree.SumLevelCountsFromAllTestVariables
                     //       tree.MaximumStrengthOfTestVariableCombination
                     
                     let results =
                        tree.TestVectorRepresentationsGroupedByStrengthUpToAndIncluding numberOfTrackedTestVariables
                     let resultsWithOnlyLevelsFromTrackedTestVariablesCombinedAtDesiredStrength =
                        let resultsAtDesiredStrength =
                            if numberOfTrackedTestVariables <= (uint32 results.Length)
                            then List.nth results (int32 numberOfTrackedTestVariables - 1)
                            else Seq.empty
                        let checkPresenceOfSentinelLevelsForTrackedVariablesExcludedInAnInterleave testVectorRepresentation =
                            let numberOfSentinelLevels =
                                testVectorRepresentation
                                |> BargainBasement.AssociatedValues
                                |> Seq.fold (fun partialResult level ->
                                                match level with
                                                    None ->
                                                        partialResult + 1u
                                                  | Some _ ->
                                                        partialResult) 0u
                            testVectorRepresentation
                            |> Map.iter (fun _ level ->
                                            match level with
                                                Some actualLevel ->
                                                    match unbox actualLevel with
                                                        Tracked (trackedTestVariableIndex, _) ->
                                                            match Map.tryfind trackedTestVariableIndex
                                                                              associationFromTrackedVariablesToNumberOfInterleavingUntrackedVariables with
                                                                Some numberOfInterleavingUntrackedVariables ->
                                                                    let shouldBeTrue = numberOfInterleavingUntrackedVariables <= numberOfSentinelLevels
                                                                    Assert.IsTrue shouldBeTrue
                                                              | None ->
                                                                    ()
                                                      | Untracked (_, numberOfInterleavedTrackedVariables) ->
                                                                    let shouldBeTrue = numberOfInterleavedTrackedVariables <= numberOfSentinelLevels
                                                                    Assert.IsTrue shouldBeTrue
                                               | None -> ())
                        resultsAtDesiredStrength
                        |> Seq.iter checkPresenceOfSentinelLevelsForTrackedVariablesExcludedInAnInterleave
                        let extractLevelsForTrackedTestVariablesOnly testVectorRepresentation =
                            let testVectorRepresentationForTrackedVariablesOnly = 
                                    Map.fold_right (fun _ level partialResult ->
                                                        match level with
                                                            Some actualLevel ->
                                                                match unbox actualLevel with
                                                                    Tracked (trackedTestVariableIndex, level) ->
                                                                        (trackedTestVariableIndex, level)
                                                                         :: partialResult
                                                                    | _ ->
                                                                        partialResult
                                                          | None ->
                                                                partialResult)
                                                   testVectorRepresentation []
                            testVectorRepresentationForTrackedVariablesOnly
                            |> Map.of_list  // Sort by the tracked test variable index - hence the roundtrip from list -> map -> list!
                            |> Map.to_list
                            |> List.map (function _, level -> level)
                        resultsAtDesiredStrength
                        |> Seq.map extractLevelsForTrackedTestVariablesOnly
                        |> Seq.filter (fun levels -> uint32 levels.Length = numberOfTrackedTestVariables)
                        |> Set.of_seq
                     testHandoff tree trackedTestVariableToNumberOfLevelsMap resultsWithOnlyLevelsFromTrackedTestVariablesCombinedAtDesiredStrength
                else printf "Rejected tree of strength %d as it would take too long!\n" maximumStrengthOfTestVariableCombination
            let timeAtEnd = DateTime.Now
            printf "**** TIME FOR TEST: %A\n" (timeAtEnd - timeAtStart)
            Assert.IsTrue !didTheSingleTestVariableEdgeCase                 
        
        [<Test>]
        member this.TestThatARandomlyChosenNCombinationOfTestVariablesIsCoveredInTermsOfCrossProductOfLevels () =
            let testHandoff tree trackedTestVariableToNumberOfLevelsMap resultsWithOnlyLevelsFromTrackedTestVariablesAtDesiredStrength =
                let crossProductOfTrackedTestVariableLevels =
                    Map.fold_right (fun _ level partialResult ->
                                        [1u .. level] :: partialResult)
                                   trackedTestVariableToNumberOfLevelsMap []
                    |> BargainBasement.CrossProduct
                    |> Set.of_list
                let shouldBeTrue =
                    resultsWithOnlyLevelsFromTrackedTestVariablesAtDesiredStrength = crossProductOfTrackedTestVariableLevels
                if not shouldBeTrue
                then dumpTree tree
                Assert.IsTrue shouldBeTrue
            createTestCasesAndHandEachOffToTest WithinOnlyASingleSubtree testHandoff

        [<Test>]
        member this.TestThatARandomlyChosenNCombinationOfTestVariablesSpanningSubtreesOfAnInterleavingNodeIsForbidden () =
            let testHandoff tree trackedTestVariableToNumberOfLevelsMap resultsWithOnlyLevelsFromTrackedTestVariablesAtDesiredStrength =
                if (trackedTestVariableToNumberOfLevelsMap: Map<'Key, 'Value>).Count > 1
                then let shouldBeTrue =
                        Set.is_empty resultsWithOnlyLevelsFromTrackedTestVariablesAtDesiredStrength
                     if not shouldBeTrue
                     then dumpTree tree
                     Assert.IsTrue shouldBeTrue
            createTestCasesAndHandEachOffToTest BetweenSiblingSubtrees testHandoff
            
            
                                