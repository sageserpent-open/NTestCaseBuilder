#light

namespace SageSerpent.TestInfrastructure.Tests

    open NUnit.Framework
    
    open SageSerpent.Infrastructure
    open SageSerpent.TestInfrastructure
    open System
    open System.Windows.Forms
    open System.Drawing
    open System.Collections.Generic
    open Microsoft.FSharp.Collections
    
    type private TestLevel =
            Untracked of UInt32                 // Index to distinguish the level.
          | Tracked of UInt32 * UInt32          // Tracked variable index, index to distinguish the level.
          
    type DistributionModeWrtInterleavingNode =
            BetweenSiblingSubtrees
          | WithinOnlyASingleSubtree
          
    [<TestFixture>]
    type NodeTestFixture () =
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
                    TestVariableNode levels ->
                        let subtreeGuiNode =
                            TreeNode ("TreeNode\n" + any_to_string levels)
                        treeGuiNode.Nodes.Add subtreeGuiNode |> ignore
                  | InterleavingNode subtrees ->
                        let subtreeGuiNode =
                            TreeNode ("InterleavingNode\n")
                        treeGuiNode.Nodes.Add subtreeGuiNode |> ignore
                        for subtree in subtrees do
                            dumpNode subtree subtreeGuiNode
                  | SynthesizingNode (subtrees
                                      , _) ->
                        let subtreeGuiNode =
                            TreeNode ("SynthesizingNode\n")
                        treeGuiNode.Nodes.Add subtreeGuiNode |> ignore
                        for subtree in subtrees do
                            dumpNode subtree subtreeGuiNode
            dumpNode tree treeGuiNode
            treeView.ExpandAll ()
            form.ShowDialog () |> ignore
            
        let dumpTestVectorRepresentation testVectorRepresentation =
            printf "**************************\n"
            testVectorRepresentation
            |> Map.iter (fun testVariableIndex level ->
                            printf "Index: %d, " testVariableIndex
                            match level with
                                Some actualLevel ->
                                    printf "value: %A.\n" actualLevel
                              | None ->
                                    printf "sentinel for an interleaved situation.\n")

        let maximumNumberOfTrackedTestVariables = 4u
        let maximumNumberOfTestLevelsForATestVariable = 3u
        let maximumNumberOfSubtreeHeadsPerAncestorNode = 5u
        let maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables = 1u
        let maximumStrengthOfTestVariableCombinationAllowed = 20u
        let randomBehaviourSeed = 23
        
        let createTreesTopDownAndHandEachOffToTest distributionModeWrtInterleavingNode testHandoff =
            let randomBehaviour = RandomBehaviour randomBehaviourSeed
            let didTheSingleTestVariableEdgeCase = ref false
            let timeAtStart = DateTime.Now
            for treeNumber in 1u .. 100u do
                let numberOfTrackedTestVariables = randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTrackedTestVariables
                if numberOfTrackedTestVariables = 1u
                then didTheSingleTestVariableEdgeCase := true
                let trackedTestVariableToNumberOfLevelsMap =
                    Map.of_list (List.init (int32 numberOfTrackedTestVariables)
                                           (fun testVariable -> testVariable, randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestLevelsForATestVariable))
                let rec createTree distributionModeWrtInterleavingNode
                                   indexForRightmostTrackedTestVariable
                                   numberOfTrackedTestVariables
                                   maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables =
                    let thinkAboutTerminatingRecursion = numberOfTrackedTestVariables <= 1u
                    if randomBehaviour.HeadsItIs () && thinkAboutTerminatingRecursion
                       || maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables = 0u
                    then if numberOfTrackedTestVariables = 1u
                         then TestVariableNode ([for levelIndex in 1u .. trackedTestVariableToNumberOfLevelsMap.[int32 indexForRightmostTrackedTestVariable] do
                                                    yield box (Tracked (indexForRightmostTrackedTestVariable, levelIndex))])
                              , indexForRightmostTrackedTestVariable + 1u  
                         else // TODO: add in a synthesizing node with no subtrees as an occasional alternative here.
                              TestVariableNode ([for levelIndex in 1u .. randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestLevelsForATestVariable do
                                                    yield box (Untracked levelIndex)])
                              , indexForRightmostTrackedTestVariable
                    else let allOnOneSubtreeDistributionMaker numberOfSubtrees numberOfTrackedTestVariables =
                            let choice =
                                randomBehaviour.ChooseAnyNumberFromOneTo numberOfSubtrees
                            [for counter in 1u .. numberOfSubtrees do
                                if counter = choice
                                then yield numberOfTrackedTestVariables
                                else yield 0u]
                         let arbitrarilySpreadAcrossSubtreesDistributionMaker numberOfSubtrees numberOfTrackedTestVariables =
                            if numberOfSubtrees = 1u
                            then [numberOfTrackedTestVariables]
                            else let pickRandomlyAllowingRepetitionOfChoices sums =
                                    let sums
                                        = List.to_array sums
                                    List.of_array [|for _ in 1u .. numberOfSubtrees - 1u do
                                                        yield sums.[int32 (randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (uint32 sums.Length))]|]
                                 let selectedSumsIncludingNumberOfTrackedTestVariables =
                                    List.append (pickRandomlyAllowingRepetitionOfChoices [0u .. numberOfTrackedTestVariables]
                                                 |> List.sort compare) [numberOfTrackedTestVariables]
                                 let firstSum
                                    = List.hd selectedSumsIncludingNumberOfTrackedTestVariables
                                 let leadingSumAndSubsequentDifferences =
                                    firstSum :: ((Seq.pairwise selectedSumsIncludingNumberOfTrackedTestVariables)
                                                 |> Seq.map (function first, second -> second - first)
                                                 |> List.of_seq)
                                 randomBehaviour.Shuffle leadingSumAndSubsequentDifferences
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
                                          distributionMaker =
                            let numberOfSubtrees =
                                randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfSubtreeHeadsPerAncestorNode
                            let gatherSubtree (previouslyGatheredSubtrees
                                               , indexForRightmostTrackedTestVariable)
                                              numberOfTrackedVariables =
                                let subtree
                                    , maximumTrackingVariableIndexFromSubtree =
                                    createTree distributionModeWrtInterleavingNode
                                               indexForRightmostTrackedTestVariable
                                               numberOfTrackedVariables
                                               (if thinkAboutTerminatingRecursion
                                                then maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables - 1u
                                                else maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables)
                                subtree :: previouslyGatheredSubtrees
                                , maximumTrackingVariableIndexFromSubtree
                            let distributionOfNumberOfTrackedTestVariablesForEachSubtree =
                                distributionMaker numberOfSubtrees numberOfTrackedTestVariables
                            let subtrees
                                , maximumTrackingVariableIndex =
                                distributionOfNumberOfTrackedTestVariablesForEachSubtree                     
                                |> List.fold_left gatherSubtree ([], indexForRightmostTrackedTestVariable)
                                    // NOTE: this is why the test variable indices increase from right to left across subtrees.
                            nodeFactory subtrees
                            , maximumTrackingVariableIndex
                         if randomBehaviour.HeadsItIs ()
                         then generateNode (fun subtrees -> InterleavingNode subtrees)
                                           distributionMakerForInterleavedNodes
                         else generateNode (fun subtrees -> SynthesizingNode (subtrees
                                                                              , BargainBasement.IdentityFunctionDelegate))
                                           distributionMakerForSynthesizingNodes                          
                let tree
                    , _ = 
                    createTree distributionModeWrtInterleavingNode
                               0u
                               numberOfTrackedTestVariables
                               maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables
                let maximumStrengthOfTestVariableCombination =
                    tree.MaximumStrengthOfTestVariableCombination
                if maximumStrengthOfTestVariableCombination < maximumStrengthOfTestVariableCombinationAllowed
                then //printf "Number of test variables: %d\nNumber of levels overall: %d\nMaximum strength: %d\n\n"
                     //       tree.CountTestVariables
                     //       tree.SumLevelCountsFromAllTestVariables
                     //       tree.MaximumStrengthOfTestVariableCombination
                     //dumpTree tree
                     printf "Tree #%u\n" treeNumber
                     let results =
                        tree.PartialTestVectorRepresentationsGroupedByStrengthUpToAndIncluding numberOfTrackedTestVariables
                     let resultsWithOnlyLevelIndicesFromTrackedTestVariablesCombinedAtDesiredStrength =
                        let resultsAtDesiredStrength =
                            if numberOfTrackedTestVariables = (uint32 results.Length)
                            then List.nth results (int32 numberOfTrackedTestVariables - 1)
                            else if numberOfTrackedTestVariables > (uint32 results.Length)
                                 then Seq.empty
                                 else raise (InternalAssertionViolationException
                                                "The maximum requested strength of combination is the number of tracked variables, but there are higher strength results.")
                        let extractLevelIndicesFromTrackedTestVariablesOnly testVectorRepresentation =
                            let testVectorRepresentationForTrackedVariablesOnly = 
                                    Map.fold_right (fun _ level partialResult ->
                                                        match level with
                                                            Some actualLevel ->
                                                                match unbox actualLevel with
                                                                    Tracked (trackedTestVariableIndex, levelIndex) ->
                                                                        (trackedTestVariableIndex, levelIndex)
                                                                         :: partialResult
                                                                    | _ ->
                                                                        partialResult
                                                          | None ->
                                                                partialResult)
                                                   testVectorRepresentation []
                            testVectorRepresentationForTrackedVariablesOnly
                            |> Map.of_list  // Sort by the tracked test variable index - hence the roundtrip from list -> map -> list!
                            |> Map.to_list
                            |> List.map (function _, levelIndex -> levelIndex)
                        resultsAtDesiredStrength
                        |> Seq.map extractLevelIndicesFromTrackedTestVariablesOnly
                        |> Seq.filter (fun levelIndices -> uint32 levelIndices.Length = numberOfTrackedTestVariables)
                        |> Set.of_seq
                     testHandoff tree trackedTestVariableToNumberOfLevelsMap resultsWithOnlyLevelIndicesFromTrackedTestVariablesCombinedAtDesiredStrength
                else printf "Rejected tree of strength %d as it would take too long!\n" maximumStrengthOfTestVariableCombination
            let timeAtEnd = DateTime.Now
            printf "**** TIME FOR TEST: %A\n" (timeAtEnd - timeAtStart)
            Assert.IsTrue !didTheSingleTestVariableEdgeCase                 
        
        let maximumNumberOfTestVariables = 20u
        
        let maximumStrengthOfCombination = 6u
        
        [<Test>]
        member this.TestThatARandomlyChosenNCombinationOfTestVariablesIsCoveredInTermsOfCrossProductOfLevels () =
            let testHandoff tree 
                            trackedTestVariableToNumberOfLevelsMap
                            resultsWithOnlyLevelIndicesFromTrackedTestVariablesCombinedAtDesiredStrength =
                let crossProductOfLevelIndices =
                    Map.fold_right (fun _ numberOfLevels partialResult ->
                                        [1u .. numberOfLevels] :: partialResult)
                                   trackedTestVariableToNumberOfLevelsMap []
                    |> BargainBasement.CrossProduct
                    |> Set.of_list
                let shouldBeTrue =
                    resultsWithOnlyLevelIndicesFromTrackedTestVariablesCombinedAtDesiredStrength = crossProductOfLevelIndices
                if not shouldBeTrue
                then dumpTree tree
                Assert.IsTrue shouldBeTrue
            createTreesTopDownAndHandEachOffToTest WithinOnlyASingleSubtree testHandoff

        [<Test>]
        member this.TestThatARandomlyChosenNCombinationOfTestVariablesSpanningSubtreesOfAnInterleavingNodeIsForbidden () =
            let testHandoff tree
                            trackedTestVariableToNumberOfLevelsMap
                            resultsWithOnlyLevelIndicesFromTrackedTestVariablesCombinedAtDesiredStrength =
                if (trackedTestVariableToNumberOfLevelsMap: Map<_, _>).Count > 1
                then let shouldBeTrue =
                        Set.is_empty resultsWithOnlyLevelIndicesFromTrackedTestVariablesCombinedAtDesiredStrength
                     if not shouldBeTrue
                     then dumpTree tree
                     Assert.IsTrue shouldBeTrue
            createTreesTopDownAndHandEachOffToTest BetweenSiblingSubtrees testHandoff
            
        [<Test>]
        member this.TestCorrectnessOfTestVariableIndicesAndThatASentinelLevelValueIsCreatedForInterleavedVariableIndicesNotChosenInACombination () =
            let randomBehaviour = RandomBehaviour randomBehaviourSeed
            let didTheSingleTestVariableEdgeCase = ref false
            let timeAtStart = DateTime.Now
            for treeNumber in 1u .. 100u do
                let rec workUpToTreeRootByMakingNewSubtreeRoots accumulatingInterleavedTestVariableIndexPairs
                                                                nodeAndItsSpannedTestVariableIndicesPairs =
                    let numberOfNodes =
                        uint32 (List.length nodeAndItsSpannedTestVariableIndicesPairs)
                    match numberOfNodes with
                        0u ->
                            raise (InternalAssertionViolationException "Internal failure in test - should have at least one node.")
                      | 1u ->
                            nodeAndItsSpannedTestVariableIndicesPairs
                            , accumulatingInterleavedTestVariableIndexPairs
                      | _ ->
                            let numberOfSubtreeRoots =
                                randomBehaviour.ChooseAnyNumberFromOneTo (numberOfNodes - 1u)
                            let groupsForSubtreeRoots =
                                BargainBasement.PartitionItemsIntoSubgroupsOfRandomNonZeroLength nodeAndItsSpannedTestVariableIndicesPairs
                                                                                                 numberOfSubtreeRoots
                                                                                                 randomBehaviour
                            let whetherInterleavedNodeChoices =
                                groupsForSubtreeRoots
                                |> List.map (fun _ -> randomBehaviour.HeadsItIs ())
                            let chooseAPairOfInterleavedTestVariableIndices (nodeAndItsSpannedTestVariableIndicesPairs
                                                                             , whetherInterleavedNodeChoice) =
                                if whetherInterleavedNodeChoice
                                   && List.length nodeAndItsSpannedTestVariableIndicesPairs > 1
                                then let testVariableIndicesChosenFromSeparateSpans =
                                        nodeAndItsSpannedTestVariableIndicesPairs
                                        |> List.map (snd >> randomBehaviour.ChooseOneOf)
                                     let chosenPair =
                                        randomBehaviour.ChooseSeveralOf testVariableIndicesChosenFromSeparateSpans
                                                                        2u
                                     Some (chosenPair.[0], chosenPair.[1])
                                else None                                 
                            let chosenPairsOfInterleavedTestVariableIndices =
                                (List.zip groupsForSubtreeRoots whetherInterleavedNodeChoices)
                                |> List.map chooseAPairOfInterleavedTestVariableIndices
                                |> List.filter Option.is_some
                                |> List.map Option.get
                            let subtreeRootFromSpannedNodes (nodeAndItsSpannedTestVariableIndicesPairs
                                                             , whetherInterleavedNodeChoice) =
                                if List.length nodeAndItsSpannedTestVariableIndicesPairs = 1
                                then List.hd nodeAndItsSpannedTestVariableIndicesPairs  // Pass up 'as is' to the next level: this way we can get
                                                                                        // variable-length paths from the overall root down to
                                                                                        // the test variable leaves.
                                else let nodes =
                                        nodeAndItsSpannedTestVariableIndicesPairs
                                        |> List.map fst
                                     let spannedTestVariableIndices =
                                        nodeAndItsSpannedTestVariableIndicesPairs
                                        |> List.map snd
                                        |> List.concat
                                     (if whetherInterleavedNodeChoice
                                      then InterleavingNode nodes
                                      else SynthesizingNode (nodes
                                                             , BargainBasement.IdentityFunctionDelegate))
                                     , spannedTestVariableIndices
                            let nextLevelOfNodeAndItsSpannedTestVariableIndicesPairs =
                                (List.zip groupsForSubtreeRoots whetherInterleavedNodeChoices)
                                |> List.map subtreeRootFromSpannedNodes
                            workUpToTreeRootByMakingNewSubtreeRoots (List.append chosenPairsOfInterleavedTestVariableIndices
                                                                                 accumulatingInterleavedTestVariableIndexPairs)
                                                                    nextLevelOfNodeAndItsSpannedTestVariableIndicesPairs
                let testVariableIndices =
                       List.init (int32 (randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestVariables))
                                 (fun testVariableIndex ->
                                    uint32 testVariableIndex)
                if testVariableIndices.Length = 1
                then didTheSingleTestVariableEdgeCase := true                                    
                let testVariableNodes =
                    testVariableIndices
                    |> List.map (fun testVariableIndex ->
                                    TestVariableNode (seq {for index in [1u .. maximumNumberOfTestLevelsForATestVariable] do
                                                                yield box (testVariableIndex, index)}))
                let nodeAndItsSpannedTestVariableIndicesPairs,
                    interleavedTestVariableIndexPairs =
                        List.zip testVariableNodes
                                 (testVariableIndices
                                  |> List.map (fun testVariableIndex -> [testVariableIndex]))
                        |> workUpToTreeRootByMakingNewSubtreeRoots []
                let reversedInterleavedTestVariableIndexPairs = // Interleaving is bidrectional, so create reverse pairs in preparation
                                                                // for creation of a one-way association below.
                    interleavedTestVariableIndexPairs
                    |> List.map (function first, second -> second, first)
                let tree =
                    fst (List.hd nodeAndItsSpannedTestVariableIndicesPairs)
                let associationFromTestVariableIndexToInterleavedTestVariableIndices =
                    HashMultiMap.Create (List.append interleavedTestVariableIndexPairs reversedInterleavedTestVariableIndexPairs)
                printf "Tree #%u\n" treeNumber
                let results =
                    tree.PartialTestVectorRepresentationsGroupedByStrengthUpToAndIncluding (min tree.MaximumStrengthOfTestVariableCombination
                                                                                                maximumStrengthOfCombination)
                    |> List.reduce_left Seq.append
                for result in results do
                    for entry in result do
                        let testVariableIndex = entry.Key
                        let testVariableLevel = entry.Value
                        match testVariableLevel with
                            Some boxed ->
                                let intrinsicTestVariableIndex
                                    , (_: UInt32) = unbox boxed
                                let shouldBeTrue = testVariableIndex = intrinsicTestVariableIndex
                                Assert.IsTrue shouldBeTrue
                                if associationFromTestVariableIndexToInterleavedTestVariableIndices.ContainsKey testVariableIndex
                                then let interleavedTestVariableIndices =
                                        associationFromTestVariableIndexToInterleavedTestVariableIndices.FindAll testVariableIndex
                                     for interleavedTestVariable in interleavedTestVariableIndices do
                                        let shouldBeTrue = 
                                            result.[interleavedTestVariable] = None
                                        Assert.IsTrue shouldBeTrue 
                          | None ->
                                ()
            let timeAtEnd = DateTime.Now
            printf "**** TIME FOR TEST: %A\n" (timeAtEnd - timeAtStart)
            Assert.IsTrue !didTheSingleTestVariableEdgeCase
                
