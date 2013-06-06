namespace NTestCaseBuilder.Tests

    open NUnit.Framework

    open SageSerpent.Infrastructure
    open SageSerpent.Infrastructure.ListExtensions
    open SageSerpent.Infrastructure.RandomExtensions
    open SageSerpent.Infrastructure.OptionExtensions
    open NTestCaseBuilder
    open System
    open System.Windows.Forms
    open System.Collections.Generic
    open Microsoft.FSharp.Collections

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
                            TreeNode ("TestVariableNode\n" + sprintf "%A" levels)
                        treeGuiNode.Nodes.Add subtreeGuiNode |> ignore
                  | SingletonNode testCase ->
                        let subtreeGuiNode =
                            TreeNode ("SingletonNode\n" + sprintf "%A" testCase)
                        treeGuiNode.Nodes.Add subtreeGuiNode |> ignore
                  | InterleavingNode subtrees ->
                        let subtreeGuiNode =
                            TreeNode ("InterleavingNode\n")
                        treeGuiNode.Nodes.Add subtreeGuiNode |> ignore
                        for subtree in subtrees do
                            dumpNode subtree subtreeGuiNode
                  | SynthesizingNode fixedCombinationOfSubtreeNodesForSynthesis ->
                        let subtreeGuiNode =
                            TreeNode ("SynthesizingNode\n")
                        treeGuiNode.Nodes.Add subtreeGuiNode |> ignore
                        for subtree in fixedCombinationOfSubtreeNodesForSynthesis.Nodes do
                            dumpNode subtree subtreeGuiNode
            dumpNode tree treeGuiNode
            treeView.ExpandAll ()
            form.ShowDialog () |> ignore

        let dumpTestVectorRepresentation testVectorRepresentation =
            printf "**************************\n"
            testVectorRepresentation
            |> Map.iter (fun testVariableIndex level ->
                            printf "Level: %d, " testVariableIndex
                            match level with
                                Some actualLevel ->
                                    printf "value: %A.\n" actualLevel
                              | None ->
                                    printf "sentinel for an interleaved situation.\n")

        let maximumNumberOfTrackedTestVariables = 4u
        let maximumNumberOfTestLevelsForATestVariable = 3u
        let maximumNumberOfSubtreeHeadsPerAncestorNode = 5u
        let maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables = 1u
        let maximumStrengthOfTestVariableCombinationAllowed = 30u
        let randomBehaviourSeed = 23

        let createTreesTopDownAndHandEachOffToTest distributionModeWrtInterleavingNode testHandoff =
            let randomBehaviour = Random randomBehaviourSeed
            let didTheSingleTestVariableEdgeCase = ref false
            let timeAtStart = DateTime.Now
            for treeNumber in 1u .. 100u do
                let numberOfTrackedTestVariables = randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTrackedTestVariables
                if numberOfTrackedTestVariables = 1u
                then didTheSingleTestVariableEdgeCase := true
                let trackedTestVariableToNumberOfLevelsMap =
                    Map.ofList (List.init (int32 numberOfTrackedTestVariables)
                                           (fun testVariable -> testVariable, randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestLevelsForATestVariable))
                let rec createTree distributionModeWrtInterleavingNode
                                   previousTrackedTestVariablesIndexedByTestVariable
                                   indexForRightmostTrackedTestVariable
                                   numberOfTrackedTestVariables
                                   maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables =
                    let thinkAboutTerminatingRecursion = numberOfTrackedTestVariables <= 1u
                    if randomBehaviour.HeadsItIs () && thinkAboutTerminatingRecursion
                       || maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables = 0u
                    then if numberOfTrackedTestVariables = 1u
                         then TestVariableNode ([|for _ in 1u .. trackedTestVariableToNumberOfLevelsMap.[int32 indexForRightmostTrackedTestVariable] do
                                                    yield box ()|])
                              , Some indexForRightmostTrackedTestVariable :: previousTrackedTestVariablesIndexedByTestVariable
                              , indexForRightmostTrackedTestVariable + 1u
                         else // TODO: add in a synthesizing node with no subtrees as an occasional alternative here.
                              TestVariableNode ([|for _ in 1u .. randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestLevelsForATestVariable do
                                                    yield box ()|])
                              , None :: previousTrackedTestVariablesIndexedByTestVariable
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
                                        = List.toArray sums
                                    List.ofArray [|for _ in 1u .. numberOfSubtrees - 1u do
                                                        yield sums.[int32 (randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (uint32 sums.Length))]|]
                                 let selectedSumsIncludingNumberOfTrackedTestVariables =
                                    List.append (pickRandomlyAllowingRepetitionOfChoices [0u .. numberOfTrackedTestVariables]
                                                 |> List.sort) [numberOfTrackedTestVariables]
                                 let firstSum
                                    = List.head selectedSumsIncludingNumberOfTrackedTestVariables
                                 let leadingSumAndSubsequentDifferences =
                                    firstSum :: ((Seq.pairwise selectedSumsIncludingNumberOfTrackedTestVariables)
                                                 |> Seq.map (function first, second -> second - first)
                                                 |> List.ofSeq)
                                 randomBehaviour.Shuffle leadingSumAndSubsequentDifferences
                                 |> List.ofArray
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
                            let gatherSubtree numberOfTrackedVariables
                                              (previouslyGatheredSubtrees
                                               , previousTrackedTestVariablesIndexedByTestVariable
                                               , indexForRightmostTrackedTestVariable) =
                                let subtree
                                    , trackedTestVariablesIndexedByTestVariable
                                    , maximumTrackingVariableIndexFromSubtree =
                                    createTree distributionModeWrtInterleavingNode
                                               previousTrackedTestVariablesIndexedByTestVariable
                                               indexForRightmostTrackedTestVariable
                                               numberOfTrackedVariables
                                               (if thinkAboutTerminatingRecursion
                                                then maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables - 1u
                                                else maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables)
                                subtree :: previouslyGatheredSubtrees
                                , trackedTestVariablesIndexedByTestVariable
                                , maximumTrackingVariableIndexFromSubtree
                            let distributionOfNumberOfTrackedTestVariablesForEachSubtree =
                                distributionMaker numberOfSubtrees numberOfTrackedTestVariables
                            let subtrees
                                , trackedTestVariablesIndexedByTestVariable
                                , maximumTrackingVariableIndex =
                                    List.foldBack gatherSubtree distributionOfNumberOfTrackedTestVariablesForEachSubtree ([], previousTrackedTestVariablesIndexedByTestVariable, indexForRightmostTrackedTestVariable)
                                    // NOTE: this is why the test variable indices increase from right to left across subtrees.
                            nodeFactory subtrees
                            , trackedTestVariablesIndexedByTestVariable
                            , maximumTrackingVariableIndex
                         if randomBehaviour.HeadsItIs ()
                         then generateNode (fun subtrees -> InterleavingNode subtrees)
                                           distributionMakerForInterleavedNodes
                         else generateNode (fun subtrees -> Node.CreateSynthesizingNode subtrees
                                                                                        BargainBasement.IdentityFunctionDelegate)
                                           distributionMakerForSynthesizingNodes
                let tree
                    , trackedTestVariablesIndexedByTestVariable
                    , _ =
                    createTree distributionModeWrtInterleavingNode
                               []
                               0u
                               numberOfTrackedTestVariables
                               maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables
                let trackedTestVariablesIndexedByTestVariable =
                    trackedTestVariablesIndexedByTestVariable
                    |> Array.ofList
                let maximumStrengthOfTestVariableCombination =
                    tree.MaximumStrengthOfTestVariableCombination
                if maximumStrengthOfTestVariableCombination < maximumStrengthOfTestVariableCombinationAllowed
                then //printf "Number of test variables: %d\nNumber of levels overall: %d\nMaximum strength: %d\n\n"
                     //       tree.CountTestVariables
                     //       tree.SumLevelCountsFromAllTestVariables
                     //       tree.MaximumStrengthOfTestVariableCombination
                     //dumpTree tree
                     printf "Tree #%u\n" treeNumber
                     let results,
                         _ =
                        tree.AssociationFromStrengthToPartialTestVectorRepresentations numberOfTrackedTestVariables
                     let resultsWithOnlyLevelIndicesFromTrackedTestVariablesCombinedAtDesiredStrength =
                        let maximumStrengthFromResults =
                            results
                            |> Map.toSeq
                            |> Seq.map fst
                            |> Seq.max
                        let resultsAtDesiredStrength =
                            if numberOfTrackedTestVariables = maximumStrengthFromResults
                            then
                                results.[numberOfTrackedTestVariables]
                            else
                                if numberOfTrackedTestVariables > maximumStrengthFromResults
                                then
                                    Seq.empty
                                else
                                    raise (InternalAssertionViolationException
                                            "The maximum requested strength of combination is the number of tracked variables, but there are higher strength results.")
                        let extractLevelIndicesFromTrackedTestVariablesOnly testVectorRepresentation =
                            let testVectorRepresentationForTrackedVariablesOnly =
                                    Seq.fold (fun partialResult
                                                  (keyValuePair: KeyValuePair<_, _>) ->
                                                    let testVariableIndex =
                                                        keyValuePair.Key
                                                    let level =
                                                        keyValuePair.Value
                                                    match level with
                                                        Level testVariableLevelIndex ->
                                                            match trackedTestVariablesIndexedByTestVariable.[int32 testVariableIndex] with
                                                                Some trackedTestVariableIndex ->
                                                                    (trackedTestVariableIndex, testVariableLevelIndex)
                                                                        :: partialResult
                                                                | _ ->
                                                                    partialResult
                                                        | _ ->
                                                            partialResult)
                                              [] testVectorRepresentation
                                    |> List.rev
                            testVectorRepresentationForTrackedVariablesOnly
                            |> Map.ofList  // Sort by the tracked test variable index - hence the roundtrip from list -> map -> list!
                            |> Map.toList
                            |> List.map snd
                        resultsAtDesiredStrength
                        |> Seq.map extractLevelIndicesFromTrackedTestVariablesOnly
                        |> Seq.filter (fun levelIndices -> uint32 levelIndices.Length = numberOfTrackedTestVariables)
                        |> Set.ofSeq
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
                    Map.foldBack (fun _ numberOfLevels partialResult ->
                                    [0 .. int32 numberOfLevels - 1] :: partialResult)
                                 trackedTestVariableToNumberOfLevelsMap []
                    |> List.CrossProduct
                    |> Set.ofSeq
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
                        Set.isEmpty resultsWithOnlyLevelIndicesFromTrackedTestVariablesCombinedAtDesiredStrength
                     if not shouldBeTrue
                     then dumpTree tree
                     Assert.IsTrue shouldBeTrue
            createTreesTopDownAndHandEachOffToTest BetweenSiblingSubtrees testHandoff

        [<Test>]
        member this.TestCorrectnessOfTestVariableLevelIndicesAndThatASentinelLevelValueIsCreatedForInterleavedVariableIndicesNotChosenInACombination () =
            let randomBehaviour = Random randomBehaviourSeed
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
                                        randomBehaviour.ChooseSeveralOf (testVariableIndicesChosenFromSeparateSpans, 2u)
                                     Some (chosenPair.[0], chosenPair.[1])
                                else None
                            let chosenPairsOfInterleavedTestVariableIndices =
                                (List.zip groupsForSubtreeRoots whetherInterleavedNodeChoices)
                                |> List.map chooseAPairOfInterleavedTestVariableIndices
                                |> Option<_>.GetFromMany
                            let subtreeRootFromSpannedNodes (nodeAndItsSpannedTestVariableIndicesPairs
                                                             , whetherInterleavedNodeChoice) =
                                if List.length nodeAndItsSpannedTestVariableIndicesPairs = 1
                                then List.head nodeAndItsSpannedTestVariableIndicesPairs    // Pass up 'as is' to the next level: this way we can get
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
                                      else Node.CreateSynthesizingNode nodes
                                                                       BargainBasement.IdentityFunctionDelegate)
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
                let testVariableNodes
                    , testVariableLevelCounts =
                    testVariableIndices
                    |> List.map (fun testVariableIndex ->
                                    let testVariableLevelCount =
                                        randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestLevelsForATestVariable
                                    TestVariableNode [|for _ in [1u .. testVariableLevelCount] do
                                                                yield box ()|]
                                    , testVariableLevelCount)
                    |> List.unzip
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
                    fst (List.head nodeAndItsSpannedTestVariableIndicesPairs)
                let associationFromTestVariableIndexToInterleavedTestVariableIndices =
                    HashMultiMap (List.append interleavedTestVariableIndexPairs reversedInterleavedTestVariableIndexPairs, HashIdentity.Structural)
                printf "Tree #%u\n" treeNumber
                let results =
                    (tree.AssociationFromStrengthToPartialTestVectorRepresentations (min tree.MaximumStrengthOfTestVariableCombination
                                                                                         maximumStrengthOfCombination)
                     |> fst
                     :> IDictionary<_, _>).Values
                    |> Seq.reduce Seq.append
                let foldInMaximumLevelIndices testVariableIndexToMaximumLevelIndexMap
                                              (keyValuePair: KeyValuePair<_, _>)  =
                    let testVariableIndex =
                        keyValuePair.Key
                    let testVariableLevelIndex =
                        keyValuePair.Value
                    match testVariableLevelIndex with
                        Level testVariableLevelIndex ->
                            match Map.tryFind testVariableIndex testVariableIndexToMaximumLevelIndexMap with
                                Some previousTestVariableLevelIndex ->
                                    if previousTestVariableLevelIndex < testVariableLevelIndex
                                    then Map.add testVariableIndex testVariableLevelIndex testVariableIndexToMaximumLevelIndexMap
                                    else testVariableIndexToMaximumLevelIndexMap
                              | _ ->
                                    Map.add testVariableIndex testVariableLevelIndex testVariableIndexToMaximumLevelIndexMap
                       | _ ->
                            testVariableIndexToMaximumLevelIndexMap
                let foldInMaximumLevelIndices testVariableIndexToMaximumLevelIndexMap (result: IDictionary<_, _>) =
                    Seq.fold foldInMaximumLevelIndices testVariableIndexToMaximumLevelIndexMap result
                let testVariableIndexToMaximumLevelIndexMap =
                    Seq.fold foldInMaximumLevelIndices Map.empty results
                let observedNumberOfLevelsMap =
                    Map.map (fun _ maximumTestVariableLevelIndex ->
                                uint32 maximumTestVariableLevelIndex + 1u)
                            testVariableIndexToMaximumLevelIndexMap
                let observedNumberOfLevels =
                    observedNumberOfLevelsMap
                    |> Map.toList
                    |> List.map snd

                let shouldBeTrue =
                    observedNumberOfLevels = testVariableLevelCounts

                Assert.IsTrue shouldBeTrue

                for result in results do
                    for entry in result do
                        let testVariableIndex = entry.Key
                        let testVariableLevelIndex = entry.Value
                        match testVariableLevelIndex with
                            Level _ ->
                                if associationFromTestVariableIndexToInterleavedTestVariableIndices.ContainsKey testVariableIndex
                                then let interleavedTestVariableIndices =
                                        associationFromTestVariableIndexToInterleavedTestVariableIndices.FindAll testVariableIndex
                                     for interleavedTestVariable in interleavedTestVariableIndices do
                                        let shouldBeTrue =
                                            result.[interleavedTestVariable] = Exclusion
                                        Assert.IsTrue shouldBeTrue
                          | _ ->
                                ()
            let timeAtEnd = DateTime.Now
            printf "**** TIME FOR TEST: %A\n" (timeAtEnd - timeAtStart)
            Assert.IsTrue !didTheSingleTestVariableEdgeCase

