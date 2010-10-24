#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System.Collections.Generic
    open System
    open SageSerpent.Infrastructure
    
    type 'Result NodeVisitOperations = // A 'visitor' by any other name. :-)
        {
            TestVariableNodeResult: seq<Object> -> 'Result
            CombineResultsFromInterleavingNodeSubtrees: seq<'Result> -> 'Result
            CombineResultsFromSynthesizingNodeSubtrees: seq<'Result> -> 'Result
        }
    and Node =
            TestVariableNode of seq<Object>
          | InterleavingNode of seq<Node>
          | SynthesizingNode of seq<Node>
          
            member private this.TraverseTree nodeOperations =
                match this with
                    TestVariableNode levels ->
                        nodeOperations.TestVariableNodeResult levels
                  | InterleavingNode subtreeRootNodes ->
                        subtreeRootNodes
                        |> Seq.map (fun subtreeRootNode -> subtreeRootNode.TraverseTree nodeOperations)
                        |> nodeOperations.CombineResultsFromInterleavingNodeSubtrees
                  | SynthesizingNode subtreeRootNodes ->
                        subtreeRootNodes
                        |> Seq.map (fun subtreeRootNode -> subtreeRootNode.TraverseTree nodeOperations)
                        |> nodeOperations.CombineResultsFromSynthesizingNodeSubtrees
          
            member this.CountTestVariables =
                this.TraverseTree   {
                                        TestVariableNodeResult = fun _ -> 1u
                                        CombineResultsFromInterleavingNodeSubtrees = Seq.reduce (+)
                                        CombineResultsFromSynthesizingNodeSubtrees = Seq.reduce (+)
                                    }
          
            member this.SumLevelCountsFromAllTestVariables =
                this.TraverseTree   {
                                        TestVariableNodeResult = fun levels -> uint32 (Seq.length levels)
                                        CombineResultsFromInterleavingNodeSubtrees = Seq.reduce (+)
                                        CombineResultsFromSynthesizingNodeSubtrees = Seq.reduce (+)
                                    }
          
            member this.MaximumStrengthOfTestVariableCombination =
                this.TraverseTree   {
                                        TestVariableNodeResult = fun _ -> 1u
                                        CombineResultsFromInterleavingNodeSubtrees = Seq.max
                                        CombineResultsFromSynthesizingNodeSubtrees = Seq.reduce (+)
                                    }
                                    
            member this.AssociationFromTestVariableIndexToVariablesThatAreInterleavedWithIt =
                let rec walkTree node indexForLeftmostTestVariable =
                    match node with
                        TestVariableNode levels ->
                            [(indexForLeftmostTestVariable, [])]
                            , indexForLeftmostTestVariable + 1u
                      | InterleavingNode subtreeRootNodes ->
                            let association
                                , maximumTestVariableFromSubtree
                                , _ =
                                let rec accumulateContributionsFromTheLeftAndThenJoinInContributionsFromTheRight subtreeRootNodes
                                                                                                                 (contributionsFromTheLeft
                                                                                                                  , indexForLeftmostTestVariable) =
                                    match subtreeRootNodes with
                                        [] ->
                                            []
                                            , indexForLeftmostTestVariable
                                            , []
                                      | head :: tail ->
                                            let associationFromSubtreeItself
                                                , maximumTestVariableIndexFromSubtree =
                                                walkTree head indexForLeftmostTestVariable                                                    
                                            let numberOfTestVariablesFromNode =
                                                maximumTestVariableIndexFromSubtree - indexForLeftmostTestVariable
                                            let testVariableIndicesFromNode =
                                                List.init (int32 numberOfTestVariablesFromNode)
                                                          (fun variableCount -> uint32 variableCount + indexForLeftmostTestVariable)
                                            let association
                                                , maximumTestVariableIndex
                                                , contributionsFromTheRight =
                                                accumulateContributionsFromTheLeftAndThenJoinInContributionsFromTheRight tail
                                                                                                                         (List.append testVariableIndicesFromNode contributionsFromTheLeft
                                                                                                                          , maximumTestVariableIndexFromSubtree)
                                            let contributionsFromEitherSide =
                                                List.append contributionsFromTheLeft contributionsFromTheRight
                                                
                                            let associationIncludingSiblingsActingOnSubtree
                                                = associationFromSubtreeItself
                                                  |> List.map (function testVariableIndex
                                                                        , interleavedTestVariableIndices ->
                                                                            testVariableIndex
                                                                            , List.append contributionsFromEitherSide interleavedTestVariableIndices)
                                                                    
                                            List.append associationIncludingSiblingsActingOnSubtree association
                                            , maximumTestVariableIndex
                                            , List.append testVariableIndicesFromNode contributionsFromTheRight
                                accumulateContributionsFromTheLeftAndThenJoinInContributionsFromTheRight (List.of_seq subtreeRootNodes)
                                                                                                             ([], indexForLeftmostTestVariable)
                            association
                            , maximumTestVariableFromSubtree
                      | SynthesizingNode subtreeRootNodes ->
                            let mergeAssociationFromSubtree (previouslyMergedAssociationList
                                                             , indexForLeftmostTestVariable)
                                                            subtreeRootNode =
                                let associationFromSubtree
                                    , maximumTestVariableIndexFromSubtree =
                                    walkTree subtreeRootNode indexForLeftmostTestVariable
                                List.append associationFromSubtree previouslyMergedAssociationList
                                , maximumTestVariableIndexFromSubtree
                            subtreeRootNodes
                            |> Seq.fold mergeAssociationFromSubtree
                                        ([], indexForLeftmostTestVariable)
                let result,
                    _ =
                        walkTree this 0u
                result
                                    
            member this.TestVectorRepresentationsGroupedByStrengthUpToAndIncluding strength =
                if strength = 0u
                then raise (PreconditionViolationException "Strength must be non-zero.")
                else let rec walkTree node strength indexForLeftmostTestVariable =
                        match node with
                            TestVariableNode levels ->
                                [[[indexForLeftmostTestVariable]]]
                                , indexForLeftmostTestVariable + 1u
                                , [(indexForLeftmostTestVariable, levels
                                                                  |> Seq.map Some)]
                                
                          | InterleavingNode subtreeRootNodes ->
                                let rec joinPairsAtEachStrength first second =
                                    match first, second with
                                        _, [] ->
                                            first
                                      | [], _ ->
                                            second
                                      | headFromFirst :: tailFromFirst, headFromSecond :: tailFromSecond ->
                                            (List.append headFromFirst headFromSecond) :: (joinPairsAtEachStrength tailFromFirst tailFromSecond)
                                let mergeTestVariableIndexListsFromSubtree (previouslyMergedTestVariableIndexLists
                                                                            , indexForLeftmostTestVariable
                                                                            , previousAssociationFromTestVariableIndexToItsLevels)
                                                                            subtreeRootNode =
                                    let testVariableIndexListsGroupedByStrengthFromSubtree
                                        , maximumTestVariableIndexFromSubtree
                                        , associationFromTestVariableIndexToItsLevelsFromSubtree =
                                        walkTree subtreeRootNode strength indexForLeftmostTestVariable
                                    let mergedTestVariableIndexListsGroupedByStrength =
                                        joinPairsAtEachStrength previouslyMergedTestVariableIndexLists testVariableIndexListsGroupedByStrengthFromSubtree
                                    let associationFromTestVariableIndexToItsLevels =
                                        List.append associationFromTestVariableIndexToItsLevelsFromSubtree previousAssociationFromTestVariableIndexToItsLevels
                                    mergedTestVariableIndexListsGroupedByStrength
                                    , maximumTestVariableIndexFromSubtree
                                    , associationFromTestVariableIndexToItsLevels                               
                                subtreeRootNodes
                                |> Seq.fold mergeTestVariableIndexListsFromSubtree ([], indexForLeftmostTestVariable, [])
                            
                          | SynthesizingNode subtreeRootNodes ->
                                let gatherTestVariableIndexListsFromSubtree (previouslyGatheredTestVariableIndexLists
                                                                             , indexForLeftmostTestVariable
                                                                             , previousAssociationFromTestVariableIndexToItsLevels)
                                                                             subtreeRootNode =
                                    let testVariableIndexListsGroupedByStrengthFromSubtree
                                        , maximumTestVariableIndexFromSubtree
                                        , associationFromTestVariableIndexToItsLevelsFromSubtree =
                                        walkTree subtreeRootNode strength indexForLeftmostTestVariable
                                    let gatheredTestVariableIndexListsGroupedBySubtreeAndThenByStrength =
                                        testVariableIndexListsGroupedByStrengthFromSubtree :: previouslyGatheredTestVariableIndexLists
                                    let associationFromTestVariableIndexToItsLevels =
                                        List.append associationFromTestVariableIndexToItsLevelsFromSubtree previousAssociationFromTestVariableIndexToItsLevels
                                    gatheredTestVariableIndexListsGroupedBySubtreeAndThenByStrength
                                    , maximumTestVariableIndexFromSubtree
                                    , associationFromTestVariableIndexToItsLevels
                                // Using 'fold' causes 'resultsFromSubtrees' to be built up in reverse to the subtree sequence,
                                // and this reversal propagates consistently through the code below. The only way it could
                                // cause a problem would be due to the order of processing the subtrees, but because the combinations
                                // from sibling subtrees are simply placed in a list and because the test variable indices are already
                                // correctly calculated, it doesn't matter.
                                let testVariableIndexListsGroupedBySubtreeAndThenByStrength
                                    , maximumTestVariableIndex
                                    , associationFromTestVariableIndexToItsLevels =
                                    subtreeRootNodes
                                    |> Seq.fold gatherTestVariableIndexListsFromSubtree ([], indexForLeftmostTestVariable, [])
                                let maximumStrengthsFromSubtrees =
                                    testVariableIndexListsGroupedBySubtreeAndThenByStrength
                                    |> List.map (fun testVariableIndexListsForOneSubtreeGroupedByStrength ->
                                                    uint32 (List.length testVariableIndexListsForOneSubtreeGroupedByStrength))
                                let testVariableIndexListsGroupedBySubtreeAndThenByStrength =
                                    testVariableIndexListsGroupedBySubtreeAndThenByStrength
                                    |> List.map List.to_array
                                // Remove the key for zero: we are not interested in zero strength combinations!
                                // Not even if we consider the case where a synthesizing node has no subtrees,
                                // because in that case there is nothing to apply a zero-total distribution to.
                                let distributionsOfStrengthsOverSubtreesAtEachTotalStrength =
                                    Map.remove 0u (CombinatoricUtilities.ChooseContributionsToMeetTotalsUpToLimit maximumStrengthsFromSubtrees strength)
                                let addInTestVariableIndexListsForGivenStrength strength distributions partialResult =
                                    let addInTestVariableIndexListsForAGivenDistribution partialResult distribution =
                                        let testVariableIndexListsBySubtree =
                                            (List.zip distribution testVariableIndexListsGroupedBySubtreeAndThenByStrength)
                                            |> List.map (function strength
                                                                  , resultFromSubtree ->
                                                                        if strength > 0u
                                                                        then resultFromSubtree.[int32 (strength - 1u)]
                                                                        else [[]])
                                        let joinTestVariableIndexLists first second =
                                            List.append first second
                                        let testVariableIndexListsBuiltFromCrossProduct =
                                            (BargainBasement.CrossProduct testVariableIndexListsBySubtree)
                                            |> List.map (List.reduce_left joinTestVariableIndexLists)
                                        List.append testVariableIndexListsBuiltFromCrossProduct partialResult
                                    (distributions |> Seq.fold addInTestVariableIndexListsForAGivenDistribution [])::partialResult
                                let testVariableIndexListsGroupedByStrength =
                                    Map.fold_right addInTestVariableIndexListsForGivenStrength distributionsOfStrengthsOverSubtreesAtEachTotalStrength []
                                testVariableIndexListsGroupedByStrength
                                , maximumTestVariableIndex
                                , associationFromTestVariableIndexToItsLevels
                     let testVariableIndexListsGroupedByStrength
                         , _
                         , associationFromTestVariableIndexToItsLevels =
                         walkTree this strength 0u
                     let associationFromTestVariableIndexToItsLevels =
                        associationFromTestVariableIndexToItsLevels
                        |> Map.of_list 
                     let associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt =
                        this.AssociationFromTestVariableIndexToVariablesThatAreInterleavedWithIt
                        |> Map.of_list
                     let createTestVectorRepresentations testVariableIndexList =
                        let entriesForInterleavedTestVariableIndices =
                            testVariableIndexList
                            |> List.map (fun testVariableIndex ->
                                            if associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt.ContainsKey testVariableIndex
                                            then associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt.[testVariableIndex]
                                            else [])
                            |> List.concat
                            |> Set.of_list
                            |> Set.to_list
                            |> List.map (fun testVariableIndex ->
                                            (testVariableIndex, None))
                        testVariableIndexList
                        |> List.map (fun testVariableIndex ->
                                     associationFromTestVariableIndexToItsLevels.[testVariableIndex]
                                     |> Seq.map (fun level -> testVariableIndex, level)
                                     |> List.of_seq)
                        |> BargainBasement.CrossProduct
                        |> List.map (fun testVectorRepresentationAsList ->
                                        Map.of_list (List.append testVectorRepresentationAsList entriesForInterleavedTestVariableIndices))
                     testVariableIndexListsGroupedByStrength
                     |> List.map (fun testVariableIndexLists ->
                                    testVariableIndexLists
                                    |> Seq.map createTestVectorRepresentations
                                    |> Seq.concat)
            
                                