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

        let maximumNumberOfTrackedTestVariables = 4
        let maximumNumberOfTestLevelsForATestVariable = 3
        let maximumNumberOfSubtreeHeadsPerAncestorNode = 5
        let maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables = 1
        let maximumStrengthOfTestVariableCombinationAllowed = 30
        let randomBehaviourSeed = 23

        let createTreesTopDownAndHandEachOffToTest distributionModeWrtInterleavingNode testHandoff =
            let randomBehaviour = Random randomBehaviourSeed
            let didTheSingleTestVariableEdgeCase = ref false
            let timeAtStart = DateTime.Now
            for treeNumber in 1 .. 100 do
                let numberOfTrackedTestVariables = randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTrackedTestVariables
                if numberOfTrackedTestVariables = 1
                then didTheSingleTestVariableEdgeCase := true
                let trackedTestVariableToNumberOfLevelsMap =
                    Map.ofList (List.init numberOfTrackedTestVariables
                                          (fun testVariable -> testVariable, randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestLevelsForATestVariable))
                let rec createTree distributionModeWrtInterleavingNode
                                   previousTrackedTestVariablesIndexedByTestVariable
                                   indexForRightmostTrackedTestVariable
                                   numberOfTrackedTestVariables
                                   maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables =
                    let thinkAboutTerminatingRecursion = numberOfTrackedTestVariables <= 1
                    if randomBehaviour.HeadsItIs () && thinkAboutTerminatingRecursion
                       || maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables = 0
                    then if numberOfTrackedTestVariables = 1
                         then TestVariableNode ([|for _ in 1 .. trackedTestVariableToNumberOfLevelsMap.[indexForRightmostTrackedTestVariable] do
                                                    yield box ()|])
                              , Some indexForRightmostTrackedTestVariable :: previousTrackedTestVariablesIndexedByTestVariable
                              , indexForRightmostTrackedTestVariable + 1
                         else // TODO: add in a synthesizing node with no subtrees as an occasional alternative here.
                              TestVariableNode ([|for _ in 1 .. randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestLevelsForATestVariable do
                                                    yield box ()|])
                              , None :: previousTrackedTestVariablesIndexedByTestVariable
                              , indexForRightmostTrackedTestVariable
                    else let allOnOneSubtreeDistributionMaker numberOfSubtrees numberOfTrackedTestVariables =
                            let choice =
                                randomBehaviour.ChooseAnyNumberFromOneTo numberOfSubtrees
                            [for counter in 1 .. numberOfSubtrees do
                                if counter = choice
                                then yield numberOfTrackedTestVariables
                                else yield 0]
                         let arbitrarilySpreadAcrossSubtreesDistributionMaker numberOfSubtrees numberOfTrackedTestVariables =
                            if numberOfSubtrees = 1
                            then [numberOfTrackedTestVariables]
                            else let pickRandomlyAllowingRepetitionOfChoices sums =
                                    let sums
                                        = List.toArray sums
                                    List.ofArray [|for _ in 1 .. numberOfSubtrees - 1 do
                                                        yield sums.[randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan sums.Length]|]
                                 let selectedSumsIncludingNumberOfTrackedTestVariables =
                                    List.append (pickRandomlyAllowingRepetitionOfChoices [0 .. numberOfTrackedTestVariables]
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
                                BetweenSiblingSubtrees when numberOfSubtrees > 1 ->
                                    if numberOfTrackedTestVariables > 1
                                    then let rec incrementOneOfTheEntriesWithoutPuttingAllOfTheTrackedVariablesInOneSubtree distribution =
                                            match distribution with
                                                [] -> raise (InternalAssertionViolationException "Either given an empty distribution or one with too high a limit.")
                                              | head :: tail -> if head + 1 = numberOfTrackedTestVariables
                                                                then head :: incrementOneOfTheEntriesWithoutPuttingAllOfTheTrackedVariablesInOneSubtree tail
                                                                else head + 1 :: tail
                                         incrementOneOfTheEntriesWithoutPuttingAllOfTheTrackedVariablesInOneSubtree
                                            (arbitrarilySpreadAcrossSubtreesDistributionMaker numberOfSubtrees (numberOfTrackedTestVariables - 1))
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
                                                then maximumDepthOfSubtreeWithOneOrNoTrackedTestVariables - 1
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
                               0
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
                                (results :> IDictionary<_, _>).[numberOfTrackedTestVariables]
                            else
                                if numberOfTrackedTestVariables > maximumStrengthFromResults
                                then
                                    Seq.empty
                                else
                                    raise (InternalAssertionViolationException
                                            "The maximum requested strength of combination is the number of tracked variables, but there are higher strength results.")
                        let extractLevelIndicesFromTrackedTestVariablesOnly testVectorRepresentation =
                            let testVectorRepresentationForTrackedVariablesOnly =
                                    Map.foldBack (fun testVariableIndex level partialResult ->
                                                    match level with
                                                        Level testVariableLevelIndex ->
                                                            match trackedTestVariablesIndexedByTestVariable.[testVariableIndex] with
                                                                Some trackedTestVariableIndex ->
                                                                    (trackedTestVariableIndex, testVariableLevelIndex)
                                                                     :: partialResult
                                                              | _ ->
                                                                    partialResult
                                                      | _ ->
                                                            partialResult)
                                                 testVectorRepresentation []
                            testVectorRepresentationForTrackedVariablesOnly
                            |> Map.ofList  // Sort by the tracked test variable index - hence the roundtrip from list -> map -> list!
                            |> Map.toList
                            |> List.map snd
                        resultsAtDesiredStrength
                        |> Seq.map extractLevelIndicesFromTrackedTestVariablesOnly
                        |> Seq.filter (fun levelIndices -> levelIndices.Length = numberOfTrackedTestVariables)
                        |> Set.ofSeq
                     testHandoff tree trackedTestVariableToNumberOfLevelsMap resultsWithOnlyLevelIndicesFromTrackedTestVariablesCombinedAtDesiredStrength
                else printf "Rejected tree of strength %d as it would take too long!\n" maximumStrengthOfTestVariableCombination
            let timeAtEnd = DateTime.Now
            printf "**** TIME FOR TEST: %A\n" (timeAtEnd - timeAtStart)
            Assert.IsTrue !didTheSingleTestVariableEdgeCase

        let maximumNumberOfTestVariables = 20

        let maximumStrengthOfCombination = 6

        [<Test>]
        member this.TestThatARandomlyChosenNCombinationOfTestVariablesIsCoveredInTermsOfCrossProductOfLevels () =
            let testHandoff tree
                            trackedTestVariableToNumberOfLevelsMap
                            resultsWithOnlyLevelIndicesFromTrackedTestVariablesCombinedAtDesiredStrength =
                let crossProductOfLevelIndices =
                    Map.foldBack (fun _ numberOfLevels partialResult ->
                                    [0 .. numberOfLevels - 1] :: partialResult)
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
            for treeNumber in 1 .. 100 do
                let rec workUpToTreeRootByMakingNewSubtreeRoots accumulatingInterleavedTestVariableIndexPairs
                                                                nodeAndItsSpannedTestVariableIndicesPairs =
                    let numberOfNodes =
                        List.length nodeAndItsSpannedTestVariableIndicesPairs
                    match numberOfNodes with
                        0 ->
                            raise (InternalAssertionViolationException "Internal failure in test - should have at least one node.")
                      | 1 ->
                            nodeAndItsSpannedTestVariableIndicesPairs
                            , accumulatingInterleavedTestVariableIndexPairs
                      | _ ->
                            let numberOfSubtreeRoots =
                                randomBehaviour.ChooseAnyNumberFromOneTo (numberOfNodes - 1)
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
                                        randomBehaviour.ChooseSeveralOf (testVariableIndicesChosenFromSeparateSpans, 2)
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
                       List.init (randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestVariables)
                                 BargainBasement.Identity
                if testVariableIndices.Length = 1
                then didTheSingleTestVariableEdgeCase := true
                let testVariableNodes
                    , testVariableLevelCounts =
                    testVariableIndices
                    |> List.map (fun testVariableIndex ->
                                    let testVariableLevelCount =
                                        randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestLevelsForATestVariable
                                    TestVariableNode [|for _ in [1 .. testVariableLevelCount] do
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
                let foldInMaximumLevelIndices testVariableIndexToMaximumLevelIndexMap testVariableIndex testVariableLevelIndex =
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
                let foldInMaximumLevelIndices testVariableIndexToMaximumLevelIndexMap result =
                    Map.fold foldInMaximumLevelIndices testVariableIndexToMaximumLevelIndexMap result
                let testVariableIndexToMaximumLevelIndexMap =
                    Seq.fold foldInMaximumLevelIndices Map.empty results
                let observedNumberOfLevelsMap =
                    Map.map (fun _ maximumTestVariableLevelIndex ->
                                maximumTestVariableLevelIndex + 1)
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

