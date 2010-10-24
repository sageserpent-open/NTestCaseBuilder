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
                let rec walkTree node indexForLeftmostTestVariable interleavingTestVariableIndices association =
                    match node with
                        TestVariableNode levels ->
                            (indexForLeftmostTestVariable, interleavingTestVariableIndices)
                             :: association
                            , indexForLeftmostTestVariable + 1u
                      | InterleavingNode subtreeRootNodes ->
                            let association
                                , maximumTestVariableFromSubtree
                                , _ =
                                let rec accumulateContributionsFromTheLeftAndThenJoinInContributionsFromTheRight subtreeRootNodes
                                                                                                                 contributionsFromTheLeft
                                                                                                                 indexForLeftmostTestVariable =
                                    match subtreeRootNodes with
                                        [] ->
                                            association
                                            , indexForLeftmostTestVariable
                                            , []
                                      | head :: tail ->
                                            let numberOfTestVariablesFromNode =
                                                (head: Node).CountTestVariables
                                            let testVariableIndicesFromNode =
                                                List.init (int32 numberOfTestVariablesFromNode)
                                                          (fun variableCount -> uint32 variableCount + indexForLeftmostTestVariable)
                                            let partialAssociation
                                                , maximumTestVariableIndex
                                                , contributionsFromTheRight =
                                                accumulateContributionsFromTheLeftAndThenJoinInContributionsFromTheRight tail
                                                                                                                         (List.append testVariableIndicesFromNode contributionsFromTheLeft)
                                                                                                                         (indexForLeftmostTestVariable + numberOfTestVariablesFromNode)
                                            let contributionsFromEitherSide =
                                                List.append contributionsFromTheLeft contributionsFromTheRight
                                                
                                            let resultAssociation
                                                , _ =
                                                walkTree head indexForLeftmostTestVariable contributionsFromEitherSide partialAssociation                                                 

                                                                    
                                            resultAssociation
                                            , maximumTestVariableIndex
                                            , List.append testVariableIndicesFromNode contributionsFromTheRight
                                accumulateContributionsFromTheLeftAndThenJoinInContributionsFromTheRight (List.of_seq subtreeRootNodes)
                                                                                                         interleavingTestVariableIndices
                                                                                                         indexForLeftmostTestVariable
                            association
                            , maximumTestVariableFromSubtree
                      | SynthesizingNode subtreeRootNodes ->
                            let mergeAssociationFromSubtree (previouslyMergedAssociationList
                                                             , indexForLeftmostTestVariable)
                                                            subtreeRootNode =
                                walkTree subtreeRootNode indexForLeftmostTestVariable interleavingTestVariableIndices previouslyMergedAssociationList
                            subtreeRootNodes
                            |> Seq.fold mergeAssociationFromSubtree
                                        (association, indexForLeftmostTestVariable)
                let result,
                    _ =
                        walkTree this 0u [] []
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
                                let mergeTestVariableCombinationsFromSubtree (previouslyMergedTestVariableCombinations
                                                                              , indexForLeftmostTestVariable
                                                                              , previousAssociationFromTestVariableIndexToItsLevels)
                                                                             subtreeRootNode =
                                    let testVariableCombinationsGroupedByStrengthFromSubtree
                                        , maximumTestVariableIndexFromSubtree
                                        , associationFromTestVariableIndexToItsLevelsFromSubtree =
                                        walkTree subtreeRootNode strength indexForLeftmostTestVariable
                                    let mergedTestVariableCombinationsGroupedByStrength =
                                        joinPairsAtEachStrength previouslyMergedTestVariableCombinations testVariableCombinationsGroupedByStrengthFromSubtree
                                    let associationFromTestVariableIndexToItsLevels =
                                        List.append associationFromTestVariableIndexToItsLevelsFromSubtree previousAssociationFromTestVariableIndexToItsLevels
                                    mergedTestVariableCombinationsGroupedByStrength
                                    , maximumTestVariableIndexFromSubtree
                                    , associationFromTestVariableIndexToItsLevels                               
                                subtreeRootNodes
                                |> Seq.fold mergeTestVariableCombinationsFromSubtree ([], indexForLeftmostTestVariable, [])
                            
                          | SynthesizingNode subtreeRootNodes ->
                                let gatherTestVariableCombinationsFromSubtree (previouslyGatheredTestVariableCombinations
                                                                               , indexForLeftmostTestVariable
                                                                               , previousAssociationFromTestVariableIndexToItsLevels)
                                                                              subtreeRootNode =
                                    let testVariableCombinationsGroupedByStrengthFromSubtree
                                        , maximumTestVariableIndexFromSubtree
                                        , associationFromTestVariableIndexToItsLevelsFromSubtree =
                                        walkTree subtreeRootNode strength indexForLeftmostTestVariable
                                    let gatheredTestVariableCombinationsGroupedBySubtreeAndThenByStrength =
                                        testVariableCombinationsGroupedByStrengthFromSubtree :: previouslyGatheredTestVariableCombinations
                                    let associationFromTestVariableIndexToItsLevels =
                                        List.append associationFromTestVariableIndexToItsLevelsFromSubtree previousAssociationFromTestVariableIndexToItsLevels
                                    gatheredTestVariableCombinationsGroupedBySubtreeAndThenByStrength
                                    , maximumTestVariableIndexFromSubtree
                                    , associationFromTestVariableIndexToItsLevels
                                // Using 'fold' causes 'resultsFromSubtrees' to be built up in reverse to the subtree sequence,
                                // and this reversal propagates consistently through the code below. The only way it could
                                // cause a problem would be due to the order of processing the subtrees, but because the combinations
                                // from sibling subtrees are simply placed in a list and because the test variable indices are already
                                // correctly calculated, it doesn't matter.
                                let testVariableCombinationsGroupedBySubtreeAndThenByStrength
                                    , maximumTestVariableIndex
                                    , associationFromTestVariableIndexToItsLevels =
                                    subtreeRootNodes
                                    |> Seq.fold gatherTestVariableCombinationsFromSubtree ([], indexForLeftmostTestVariable, [])
                                let maximumStrengthsFromSubtrees =
                                    testVariableCombinationsGroupedBySubtreeAndThenByStrength
                                    |> List.map (fun testVariableCombinationsForOneSubtreeGroupedByStrength ->
                                                    uint32 (List.length testVariableCombinationsForOneSubtreeGroupedByStrength))
                                let testVariableCombinationsGroupedBySubtreeAndThenByStrength =
                                    testVariableCombinationsGroupedBySubtreeAndThenByStrength
                                    |> List.map List.to_array
                                // Remove the key for zero: we are not interested in zero strength combinations!
                                // Not even if we consider the case where a synthesizing node has no subtrees,
                                // because in that case there is nothing to apply a zero-total distribution to.
                                let distributionsOfStrengthsOverSubtreesAtEachTotalStrength =
                                    Map.remove 0u (CombinatoricUtilities.ChooseContributionsToMeetTotalsUpToLimit maximumStrengthsFromSubtrees strength)
                                let addInTestVariableCombinationsForGivenStrength strength distributions partialResult =
                                    let addInTestVariableCombinationsForAGivenDistribution partialResult distribution =
                                        let testVariableCombinationsBySubtree =
                                            (List.zip distribution testVariableCombinationsGroupedBySubtreeAndThenByStrength)
                                            |> List.map (function strength
                                                                  , resultFromSubtree ->
                                                                        if strength > 0u
                                                                        then resultFromSubtree.[int32 (strength - 1u)]
                                                                        else [[]])
                                        let joinTestVariableCombinations first second =
                                            List.append first second
                                        let testVariableCombinationsBuiltFromCrossProduct =
                                            (BargainBasement.CrossProduct testVariableCombinationsBySubtree)
                                            |> List.map (List.reduce_left joinTestVariableCombinations)
                                        List.append testVariableCombinationsBuiltFromCrossProduct partialResult
                                    (distributions |> Seq.fold addInTestVariableCombinationsForAGivenDistribution [])::partialResult
                                let testVariableCombinationsGroupedByStrength =
                                    Map.fold_right addInTestVariableCombinationsForGivenStrength distributionsOfStrengthsOverSubtreesAtEachTotalStrength []
                                testVariableCombinationsGroupedByStrength
                                , maximumTestVariableIndex
                                , associationFromTestVariableIndexToItsLevels
                     let testVariableCombinationsGroupedByStrength
                         , _
                         , associationFromTestVariableIndexToItsLevels =
                         walkTree this strength 0u
                     let associationFromTestVariableIndexToItsLevels =
                        associationFromTestVariableIndexToItsLevels
                        |> Map.of_list 
                     let associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt =
                        this.AssociationFromTestVariableIndexToVariablesThatAreInterleavedWithIt
                        |> Map.of_list
                     let createTestVectorRepresentations testVariableCombination =
                        let sentinelEntriesForInterleavedTestVariableIndices =
                            testVariableCombination
                            |> List.map (fun testVariableIndex ->
                                            if associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt.ContainsKey testVariableIndex
                                            then associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt.[testVariableIndex]
                                            else [])
                            |> List.concat
                            |> Set.of_list
                            |> Set.to_list
                            |> List.map (fun testVariableIndex ->
                                            (testVariableIndex, None))
                        let levelEntriesForTestVariableIndicesFromList =
                            testVariableCombination
                            |> List.map (fun testVariableIndex ->
                                         associationFromTestVariableIndexToItsLevels.[testVariableIndex]
                                         |> Seq.map (fun level -> testVariableIndex, level)
                                         |> List.of_seq)
                        levelEntriesForTestVariableIndicesFromList
                        |> BargainBasement.CrossProductWithCommonSuffix sentinelEntriesForInterleavedTestVariableIndices
                        |> List.map (fun testVectorRepresentationAsList ->
                                        Map.of_list testVectorRepresentationAsList)
                     testVariableCombinationsGroupedByStrength
                     |> List.map (fun testVariableCombinations ->
                                    testVariableCombinations
                                    |> Seq.map createTestVectorRepresentations
                                    |> Seq.concat)
            
                                