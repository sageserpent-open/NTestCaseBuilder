#nowarn "40"

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System.Collections.Generic
    open System
    open SageSerpent.Infrastructure
    open SageSerpent.Infrastructure.RandomExtensions
    open SageSerpent.Infrastructure.ListExtensions
    open Microsoft.FSharp.Collections
    open Wintellect.PowerCollections
    
    type NodeVisitOperations<'Result> =
        {
            TestVariableNodeResult: array<Object> -> 'Result
            SingletonNodeResult: unit -> 'Result
            CombineResultsFromInterleavingNodeSubtrees: seq<'Result> -> 'Result
            CombineResultsFromSynthesizingNodeSubtrees: seq<'Result> -> 'Result
        }
    and LevelForTestVariable =
        Index of Int32
      | SingletonPlaceholder
      | Exclusion
    and Node =
        TestVariableNode of array<Object>
      | SingletonNode of Object
      | InterleavingNode of List<Node>
      | SynthesizingNode of List<Node> * Delegate
      
    module NodeDetail =
        let traverseTree nodeOperations =
            let rec memoizedCalculation =
                BargainBasement.Memoize (fun node ->
                                            match node with
                                                TestVariableNode levels ->
                                                    nodeOperations.TestVariableNodeResult levels
                                              | SingletonNode _ ->
                                                    nodeOperations.SingletonNodeResult ()
                                              | InterleavingNode subtrees ->
                                                    subtrees
                                                    |> Seq.map (fun subtreeHead -> memoizedCalculation subtreeHead)
                                                    |> nodeOperations.CombineResultsFromInterleavingNodeSubtrees
                                              | SynthesizingNode (subtrees
                                                                  , _) ->
                                                    subtrees
                                                    |> Seq.map (fun subtreeHead -> memoizedCalculation subtreeHead)
                                                    |> nodeOperations.CombineResultsFromSynthesizingNodeSubtrees)
            memoizedCalculation
      
        let countTestVariables =
            traverseTree    {
                                TestVariableNodeResult = fun _ -> 1u
                                SingletonNodeResult = fun () -> 1u
                                CombineResultsFromInterleavingNodeSubtrees = Seq.reduce (+)
                                CombineResultsFromSynthesizingNodeSubtrees = Seq.reduce (+)
                            }
        
        let sumLevelCountsFromAllTestVariables =
            traverseTree    {
                                TestVariableNodeResult = fun levels -> uint32 (Seq.length levels)
                                SingletonNodeResult = fun () -> 0u
                                CombineResultsFromInterleavingNodeSubtrees = Seq.reduce (+)
                                CombineResultsFromSynthesizingNodeSubtrees = Seq.reduce (+)
                            }
      
        let maximumStrengthOfTestVariableCombination =
            traverseTree    {
                                TestVariableNodeResult = fun _ -> 1u
                                SingletonNodeResult = fun () -> 0u
                                CombineResultsFromInterleavingNodeSubtrees = Seq.max
                                CombineResultsFromSynthesizingNodeSubtrees = Seq.reduce (+)
                            }

    open NodeDetail
                                
    type Node with
        member this.CountTestVariables =
            countTestVariables this
            
        member this.SumLevelCountsFromAllTestVariables =
            sumLevelCountsFromAllTestVariables this
            
        member this.MaximumStrengthOfTestVariableCombination =
            maximumStrengthOfTestVariableCombination this
    
        member this.PruneTree =
            let rec walkTree node =
                match node with
                    TestVariableNode levels ->
                        if Array.isEmpty levels
                        then None
                        else Some node
                  | SingletonNode _ as node ->
                        Some node
                  | InterleavingNode subtreeRootNodes ->
                        let prunedSubtreeRootNodes =
                            subtreeRootNodes
                            |> List.map walkTree
                            |> List.filter Option.isSome
                            |> List.map Option.get
                        if Seq.isEmpty prunedSubtreeRootNodes
                            then None
                            else Some (InterleavingNode prunedSubtreeRootNodes)
                  | SynthesizingNode (subtreeRootNodes
                                      , synthesisDelegate) ->
                        let prunedSubtreeRootNodes =
                            subtreeRootNodes
                            |> List.map walkTree
                            |> List.filter Option.isSome
                            |> List.map Option.get
                        if not (Seq.isEmpty prunedSubtreeRootNodes)
                           && Seq.length prunedSubtreeRootNodes
                              = Seq.length subtreeRootNodes
                        then Some (SynthesizingNode (prunedSubtreeRootNodes
                                                     , synthesisDelegate))
                        else None
            walkTree this
                                
        member this.AssociationFromTestVariableIndexToVariablesThatAreInterleavedWithIt =
            let rec walkTree node
                             indexForLeftmostTestVariable
                             interleavingTestVariableIndices
                             previousAssociationFromTestVariableIndexToVariablesThatAreInterleavedWithIt =
                let resultsForASingleTestVariable () =
                    let forwardInterleavingPairs =
                        interleavingTestVariableIndices
                        |> List.map (function interleavingTestVariableIndex ->
                                                indexForLeftmostTestVariable
                                                , interleavingTestVariableIndex)
                    let backwardInterleavingPairs =
                        interleavingTestVariableIndices
                        |> List.map (function interleavingTestVariableIndex ->
                                                interleavingTestVariableIndex
                                                , indexForLeftmostTestVariable)
                    indexForLeftmostTestVariable + 1u
                    , previousAssociationFromTestVariableIndexToVariablesThatAreInterleavedWithIt
                      |> List.append forwardInterleavingPairs
                      |> List.append backwardInterleavingPairs   
                match node with
                    TestVariableNode _ ->
                        resultsForASingleTestVariable ()
                  | SingletonNode _ ->
                        resultsForASingleTestVariable ()
                  | InterleavingNode subtreeRootNodes ->
                        let mergeAssociationFromSubtree (indexForLeftmostTestVariable
                                                         , interleavingTestVariableIndicesFromTheLeftSiblings
                                                         , previousAssociationFromTestVariableIndexToVariablesThatAreInterleavedWithIt)
                                                        subtreeRootNode =
                            let maximumTestVariableFromSubtree
                                , associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt =
                                walkTree subtreeRootNode
                                         indexForLeftmostTestVariable
                                         interleavingTestVariableIndicesFromTheLeftSiblings
                                         previousAssociationFromTestVariableIndexToVariablesThatAreInterleavedWithIt
                            let testVariableIndicesFromNode =
                                List.init (int32 (maximumTestVariableFromSubtree - indexForLeftmostTestVariable))
                                          (fun variableCount -> uint32 variableCount + indexForLeftmostTestVariable)
                            maximumTestVariableFromSubtree
                            , List.append testVariableIndicesFromNode interleavingTestVariableIndicesFromTheLeftSiblings
                            , associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt
                        let maximumTestVariableFromSubtree
                            , _
                            , associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt =
                            subtreeRootNodes
                            |> Seq.fold mergeAssociationFromSubtree (indexForLeftmostTestVariable
                                                                     , interleavingTestVariableIndices
                                                                     , previousAssociationFromTestVariableIndexToVariablesThatAreInterleavedWithIt)
                        maximumTestVariableFromSubtree
                        , associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt
                  | SynthesizingNode (subtreeRootNodes
                                      , _) ->
                        let mergeAssociationFromSubtree (indexForLeftmostTestVariable
                                                         , previouslyMergedAssociationList)
                                                        subtreeRootNode =
                            walkTree subtreeRootNode indexForLeftmostTestVariable interleavingTestVariableIndices previouslyMergedAssociationList
                        subtreeRootNodes
                        |> Seq.fold mergeAssociationFromSubtree (indexForLeftmostTestVariable
                                                                 , previousAssociationFromTestVariableIndexToVariablesThatAreInterleavedWithIt)
            let _,
                result =
                    walkTree this 0u [] []
            HashMultiMap (result, HashIdentity.Structural)
                                
        member this.AssociationFromStrengthToPartialTestVectorRepresentations maximumDesiredStrength =
            let rec walkTree node maximumDesiredStrength indexForLeftmostTestVariable =
                match node with
                    TestVariableNode levels ->
                        if 0u = maximumDesiredStrength
                        then
                            Map.empty
                        else
                            Map.ofList [1u, [[indexForLeftmostTestVariable]]]
                        , indexForLeftmostTestVariable + 1u
                        , [indexForLeftmostTestVariable
                           , Array.length levels
                             |> uint32]
                        
                  | SingletonNode _ ->
                        Map.ofList [0u, [[indexForLeftmostTestVariable]]]
                        , indexForLeftmostTestVariable + 1u
                        , []
                  
                  | InterleavingNode subtreeRootNodes ->
                        let mergeTestVariableCombinationsFromSubtree (previousAssociationFromStrengthToTestVariableCombinations
                                                                      , indexForLeftmostTestVariable
                                                                      , previousAssociationFromTestVariableIndexToNumberOfItsLevels)
                                                                    subtreeRootNode =
                            let associationFromStrengthToTestVariableCombinationsFromSubtree
                                , maximumTestVariableIndexFromSubtree
                                , associationFromTestVariableIndexToNumberOfItsLevelsFromSubtree =
                                walkTree subtreeRootNode maximumDesiredStrength indexForLeftmostTestVariable
                            let mergedAssociationFromStrengthToTestVariableCombinations =
                                BargainBasement.MergeAssociations previousAssociationFromStrengthToTestVariableCombinations associationFromStrengthToTestVariableCombinationsFromSubtree
                            let associationFromTestVariableIndexToNumberOfItsLevels =
                                List.append associationFromTestVariableIndexToNumberOfItsLevelsFromSubtree previousAssociationFromTestVariableIndexToNumberOfItsLevels
                            mergedAssociationFromStrengthToTestVariableCombinations
                            , maximumTestVariableIndexFromSubtree
                           , associationFromTestVariableIndexToNumberOfItsLevels                               
                        subtreeRootNodes
                        |> Seq.fold mergeTestVariableCombinationsFromSubtree (Map.empty, indexForLeftmostTestVariable, [])
                    
                  | SynthesizingNode (subtreeRootNodes
                                      , _) ->
                        let gatherTestVariableCombinationsFromSubtree (previousPerSubtreeAssociationsFromStrengthToTestVariableCombinations
                                                                       , indexForLeftmostTestVariable
                                                                       , previousAssociationFromTestVariableIndexToNumberOfItsLevels)
                                                                      subtreeRootNode =
                            let associationFromStrengthToTestVariableCombinationsFromSubtree
                                , maximumTestVariableIndexFromSubtree
                                , associationFromTestVariableIndexToNumberOfItsLevelsFromSubtree =
                                walkTree subtreeRootNode maximumDesiredStrength indexForLeftmostTestVariable
                            let perSubtreeAssociationsFromStrengthToTestVariableCombinations =
                                associationFromStrengthToTestVariableCombinationsFromSubtree :: previousPerSubtreeAssociationsFromStrengthToTestVariableCombinations
                            let associationFromTestVariableIndexToNumberOfItsLevels =
                                List.append associationFromTestVariableIndexToNumberOfItsLevelsFromSubtree previousAssociationFromTestVariableIndexToNumberOfItsLevels
                            perSubtreeAssociationsFromStrengthToTestVariableCombinations
                            , maximumTestVariableIndexFromSubtree
                            , associationFromTestVariableIndexToNumberOfItsLevels
                        // Using 'fold' causes 'perSubtreeAssociationsFromStrengthToTestVariableCombinations' to be built up in
                        // reverse to the subtree sequence, and this reversal propagates consistently through the code below. The
                        // only way it could cause a problem would be due to the order of processing the subtrees, but because the
                       // combinations of the same strength from sibling subtrees are simply placed in a list and because the test
                        // variable indices are already correctly calculated, it doesn't matter.
                        let perSubtreeAssociationsFromStrengthToTestVariableCombinations
                            , maximumTestVariableIndex
                            , associationFromTestVariableIndexToNumberOfItsLevels =
                            subtreeRootNodes
                            |> Seq.fold gatherTestVariableCombinationsFromSubtree ([], indexForLeftmostTestVariable, [])
                        let maximumStrengthsFromSubtrees =
                            perSubtreeAssociationsFromStrengthToTestVariableCombinations
                            |> List.map (fun associationFromStrengthToTestVariableCombinationsForOneSubtree ->
                                            if associationFromStrengthToTestVariableCombinationsForOneSubtree.IsEmpty
                                            then
                                                0u
                                            else
                                               (associationFromStrengthToTestVariableCombinationsForOneSubtree :> IDictionary<_, _>).Keys
                                                |> Algorithms.Maximum)
                        // We have to cope with individual subtrees being requested to yield zero strength combinations: what we
                        // mean is that either the subtree doesn't have to provide any test variables for a given distribution,
                        // in which case we yield a sentinel value of [[]] (which is an identity under the cross product), or that we
                        // can get by with providing a combination of singleton test variables which don't count towards the overall
                        // desired total strength.
                        // Contrast this with the case where a subtree is asked for combinations of a positive strength that it
                        // doesn't have: in this case we yield [], which is a zero under the cross product, reflecting the fact
                       // that we can't achieve the distribution in question.
                        let distributionsOfStrengthsOverSubtreesAtEachTotalStrength =
                            CombinatoricUtilities.ChooseContributionsToMeetTotalsUpToLimit maximumStrengthsFromSubtrees maximumDesiredStrength
                        let addInTestVariableCombinationsForGivenTotalStrength totalStrength
                                                                               distributionsOfStrengthsOverSubtrees
                                                                               partialAssociationFromStrengthToTestVariableCombinations =
                            let addInTestVariableCombinationsForAGivenDistribution partialTestVariableCombinations distributionOfStrengthsOverSubtrees =
                                let perSubtreeTestVariableCombinations =
                                    List.zip distributionOfStrengthsOverSubtrees perSubtreeAssociationsFromStrengthToTestVariableCombinations
                                    |> List.map (function strength
                                                         , associationFromStrengthToTestVariableCombinationsForOneSubtree ->
                                                                match Map.tryFind strength associationFromStrengthToTestVariableCombinationsForOneSubtree with
                                                                    Some testVariableCombinations ->
                                                                        testVariableCombinations
                                                                  | None ->
                                                                        if strength = 0u
                                                                        then
                                                                            [[]]
                                                                        else
                                                                            [])
                                let joinTestVariableCombinations =
                                   List.append
                                let testVariableCombinationsBuiltFromCrossProduct =
                                    (List.CrossProduct perSubtreeTestVariableCombinations)
                                    |> List.map (List.reduce joinTestVariableCombinations)
                                List.append testVariableCombinationsBuiltFromCrossProduct partialTestVariableCombinations
                            let testVariableCombinationsWithTotalStrength =
                                distributionsOfStrengthsOverSubtrees |> List.fold addInTestVariableCombinationsForAGivenDistribution []
                            if testVariableCombinationsWithTotalStrength.IsEmpty
                            then
                                partialAssociationFromStrengthToTestVariableCombinations
                            else
                                Map.add totalStrength 
                                        testVariableCombinationsWithTotalStrength
                                        partialAssociationFromStrengthToTestVariableCombinations
                        let associationFromStrengthToTestVariableCombinations =
                            Map.foldBack addInTestVariableCombinationsForGivenTotalStrength distributionsOfStrengthsOverSubtreesAtEachTotalStrength Map.empty
                        associationFromStrengthToTestVariableCombinations
                        , maximumTestVariableIndex
                        , associationFromTestVariableIndexToNumberOfItsLevels
            let associationFromStrengthToTestVariableCombinations
                , _
                , associationFromTestVariableIndexToNumberOfItsLevels =
                walkTree this maximumDesiredStrength 0u
            let associationFromTestVariableIndexToNumberOfItsLevels =
                associationFromTestVariableIndexToNumberOfItsLevels
                |> Map.ofList 
            let associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt =
                this.AssociationFromTestVariableIndexToVariablesThatAreInterleavedWithIt
            let createTestVectorRepresentations testVariableCombination =
                let sentinelEntriesForInterleavedTestVariableIndices =
                   testVariableCombination
                    |> List.map (fun testVariableIndex ->
                                    if associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt.ContainsKey testVariableIndex
                                    then associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt.FindAll testVariableIndex
                                    else [])
                    |> List.concat
                    |> Set.ofList
                    |> Set.toList
                    |> List.map (fun testVariableIndex ->
                                    (testVariableIndex, Exclusion))
                let testVariableCombinationSortedByDecreasingNumberOfLevels = // Using this sort order optimizes the cross product later on.
                    let numberOfLevelsForTestVariable testVariableIndex =
                        match Map.tryFind testVariableIndex associationFromTestVariableIndexToNumberOfItsLevels with
                            Some numberOfLevels ->
                                numberOfLevels
                          | None ->
                                1u    
                    testVariableCombination
                    |> List.sortWith (fun first second ->
                                        compare (numberOfLevelsForTestVariable second)
                                                (numberOfLevelsForTestVariable first))
                let levelEntriesForTestVariableIndicesFromList =
                    testVariableCombinationSortedByDecreasingNumberOfLevels
                    |> List.map (fun testVariableIndex ->
                                    match Map.tryFind testVariableIndex associationFromTestVariableIndexToNumberOfItsLevels with
                                        Some numberOfLevels ->
                                            numberOfLevels
                                            |> int32
                                            |> (BargainBasement.Flip List.init) (fun levelIndex -> testVariableIndex, Index levelIndex)
                                      | None ->
                                            [(testVariableIndex, SingletonPlaceholder)])
                levelEntriesForTestVariableIndicesFromList
                |> List.CrossProductWithCommonSuffix sentinelEntriesForInterleavedTestVariableIndices
                |> List.map (fun testVectorRepresentationAsList ->
                                Map.ofList testVectorRepresentationAsList)
            associationFromStrengthToTestVariableCombinations
            |> Map.map (fun strength testVariableCombinations ->
                            testVariableCombinations
                            |> Seq.map createTestVectorRepresentations
                            |> Seq.concat)
            , associationFromTestVariableIndexToNumberOfItsLevels
                                
        member this.FillOutPartialTestVectorRepresentation associationFromTestVariableIndexToNumberOfItsLevels
                                                           partialTestVectorRepresentation
                                                           randomBehaviour =
            let associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt =
                this.AssociationFromTestVariableIndexToVariablesThatAreInterleavedWithIt
            let testVariableIndices =
                ((partialTestVectorRepresentation: Map<_, _>):> IDictionary<_, _>).Keys
                |> Set.ofSeq
            let missingTestVariableIndices =
                (List.init (int32 this.CountTestVariables)
                           (fun count ->
                             uint32 count)
                 |> Set.ofList)
                - testVariableIndices
            let rec fillInRandomTestVariablesMarkingExcludedOnesAsWell missingTestVariableIndices =
                if Set.count missingTestVariableIndices = 0
                then
                    []
                else
                    let chosenTestVariableIndex =
                        (randomBehaviour: Random).ChooseOneOf missingTestVariableIndices
                    let levelForChosenTestVariable =
                        match Map.tryFind chosenTestVariableIndex associationFromTestVariableIndexToNumberOfItsLevels with
                            Some numberOfLevels ->
                                let chosenLevel =
                                    (randomBehaviour: Random).ChooseAnyNumberFromZeroToOneLessThan numberOfLevels
                                chosenLevel
                                |> int32
                                |> Index
                          | None ->
                                // This case picks up a test variable index for a singleton test case:
                                // the map is built so that it doesn't have entries for these.
                                SingletonPlaceholder  
                    let entryForChosenTestVariable =
                        chosenTestVariableIndex
                        , levelForChosenTestVariable
                    let excludedTestVariableIndices =
                        associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt.FindAll chosenTestVariableIndex
                    let entriesForExcludedTestVariableIndices =
                        excludedTestVariableIndices
                        |> List.map (fun excludedTestVariableIndex ->
                                        excludedTestVariableIndex
                                        , Exclusion)
                    let missingTestVariableIndicesExcludingTheChosenOneAndItsExclusions =
                        missingTestVariableIndices - Set.ofList (chosenTestVariableIndex
                                                                   :: excludedTestVariableIndices)
                    let resultFromRecursiveCase =
                        fillInRandomTestVariablesMarkingExcludedOnesAsWell missingTestVariableIndicesExcludingTheChosenOneAndItsExclusions
                    List.append (entryForChosenTestVariable
                                 :: entriesForExcludedTestVariableIndices)
                                resultFromRecursiveCase
            let filledAndExcludedTestVariables = fillInRandomTestVariablesMarkingExcludedOnesAsWell missingTestVariableIndices
            List.append filledAndExcludedTestVariables
                        (partialTestVectorRepresentation
                         |> Map.toList)
                   |> Map.ofList
                   |> Map.toList   // This ensures the entries are sorted in ascending test variable index order.
                   |> List.map snd // This is more roundabout than using the 'Values' property, but the latter
                                   // makes no guarantee about the ordering - we want to preserve the order we
                                   // just established above.
                   |> List.toArray
                            
        member this.CreateFinalValueFrom fullTestVector =
            let rec walkTree node
                             indexForLeftmostTestVariable =
                let numberOfTestVariables =
                    (node: Node).CountTestVariables
                if indexForLeftmostTestVariable + numberOfTestVariables > uint32 (Array.length fullTestVector)
                then raise (InternalAssertionViolationException "No longer have enough entries in what's left of the vector.")
                else match node with
                        TestVariableNode levels ->
                            match fullTestVector.[int32 indexForLeftmostTestVariable] with
                                Index levelIndexFromVector ->
                                    levels.[levelIndexFromVector]
                              | SingletonPlaceholder ->
                                    raise (PreconditionViolationException "Vector is inconsistent with the tree structure - the level from the vector has the sentinel value for a singleton test case.")
                              | Exclusion ->
                                    raise (PreconditionViolationException "Vector is inconsistent with the tree structure - the level from the vector has the sentinel value for an excluded test variable.")
                      | SingletonNode testCase ->
                            match fullTestVector.[int32 indexForLeftmostTestVariable] with
                                Index _ ->
                                    raise (PreconditionViolationException "Vector is inconsistent with the tree structure - the level from the vector has a genuine test level value.")
                              | SingletonPlaceholder ->
                                    testCase
                              | Exclusion ->
                                    raise (PreconditionViolationException "Vector is inconsistent with the tree structure - the level from the vector has the sentinel value for an excluded test variable.")                                    
                      | InterleavingNode subtreeRootNodes ->
                            let firstNonExcludedTestVariableIndex =
                                let sectionOfFullTestVector =
                                    Algorithms.Range ((fullTestVector:> IList<_>),
                                                      int32 indexForLeftmostTestVariable,
                                                      int32 numberOfTestVariables)
                                match Algorithms.FindFirstIndexWhere (sectionOfFullTestVector,
                                                                      (fun testVariableLevel ->
                                                                            match testVariableLevel with
                                                                                Exclusion ->
                                                                                    false
                                                                              | _ -> true)) with
                                    -1 ->
                                        raise (PreconditionViolationException "Vector is inconsistent with the tree structure - should have found at least one non-excluded test variable level contributing to an interleave.")
                                  | index ->
                                        uint32 index + indexForLeftmostTestVariable
                            let rec discardLeftmostSubtreesInvolvingOnlyExcludedTestVariables subtreeRootNodes
                                                                                              indexForLeftmostTestVariable =
                                match subtreeRootNodes with
                                        [] ->
                                            raise (InternalAssertionViolationException "Ran out of subtree nodes but there should have definitely been enough of them.")
                                      | head :: tail ->
                                            let indexForForLeftmostTestVariableInTail =
                                                (head: Node).CountTestVariables + indexForLeftmostTestVariable
                                            if indexForForLeftmostTestVariableInTail > firstNonExcludedTestVariableIndex
                                            then subtreeRootNodes
                                                 , indexForLeftmostTestVariable
                                            else discardLeftmostSubtreesInvolvingOnlyExcludedTestVariables tail
                                                                                                           indexForForLeftmostTestVariableInTail
                            let subtreeRootNodesHeadedByNodeContainingLeftmostNonExcludedTestVariable
                                , indexForLeftmostTestVariableInNodeContainingLeftmostNonExcludedTestVariable =
                                discardLeftmostSubtreesInvolvingOnlyExcludedTestVariables subtreeRootNodes
                                                                                          indexForLeftmostTestVariable
                            walkTree (List.head subtreeRootNodesHeadedByNodeContainingLeftmostNonExcludedTestVariable)
                                     indexForLeftmostTestVariableInNodeContainingLeftmostNonExcludedTestVariable
                      | SynthesizingNode (subtreeRootNodes
                                          , synthesisDelegate) ->
                            let rec collectResultsFromSubtrees subtreeRootNodes
                                                               indexForLeftmostTestVariable =
                                match subtreeRootNodes with
                                        [] ->
                                            []
                                      | head :: tail ->
                                            let indexForForLeftmostTestVariableInTail =
                                                (head: Node).CountTestVariables + indexForLeftmostTestVariable
                                            let resultFromSubtree =
                                                walkTree head
                                                         indexForLeftmostTestVariable
                                            resultFromSubtree
                                            :: collectResultsFromSubtrees tail
                                                                          indexForForLeftmostTestVariableInTail
                            let resultsFromSubtrees =
                                collectResultsFromSubtrees subtreeRootNodes indexForLeftmostTestVariable
                            let invocationArguments =
                                resultsFromSubtrees
                                |> List.toArray
                            (synthesisDelegate.DynamicInvoke invocationArguments)
            if this.CountTestVariables > uint32 (Array.length fullTestVector)
            then raise (PreconditionViolationException "Vector is inconsistent with the tree structure - test vector has more entries than the number of test variables in the tree.")                                                             
            else walkTree this
                          0u
            
                                            
                      