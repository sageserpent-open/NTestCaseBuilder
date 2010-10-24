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
    
    type 'Result NodeVisitOperations = // A 'visitor' by any other name. :-)
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
      | InterleavingNode of seq<Node>
      | SynthesizingNode of seq<Node> * Delegate
      
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
                        if Seq.isEmpty levels
                        then None
                        else Some node
                  | SingletonNode _ as node ->
                        Some node
                  | InterleavingNode subtreeRootNodes ->
                        let prunedSubtreeRootNodes =
                            subtreeRootNodes
                            |> Seq.map walkTree
                            |> Seq.filter Option.isSome
                            |> Seq.map Option.get
                        if Seq.isEmpty prunedSubtreeRootNodes
                            then None
                            else Some (InterleavingNode prunedSubtreeRootNodes)
                  | SynthesizingNode (subtreeRootNodes
                                      , synthesisDelegate) ->
                        let prunedSubtreeRootNodes =
                            subtreeRootNodes
                            |> Seq.map walkTree
                            |> Seq.filter Option.isSome
                            |> Seq.map Option.get
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
                                
        member this.PartialTestVectorRepresentationsGroupedByStrengthUpToAndIncluding strength =
            if strength = 0u
            then raise (PreconditionViolationException "Strength must be non-zero.")
            else let rec walkTree node strength indexForLeftmostTestVariable =
                    match node with
                        TestVariableNode levels ->
                            [[[indexForLeftmostTestVariable]]]
                            , indexForLeftmostTestVariable + 1u
                            , [indexForLeftmostTestVariable
                               , Array.length levels
                                 |> uint32]
                            
                      | SingletonNode _ ->
                            []
                            , indexForLeftmostTestVariable + 1u
                            , []
                      
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
                                                                          , previousAssociationFromTestVariableIndexToNumberOfItsLevels)
                                                                         subtreeRootNode =
                                let testVariableCombinationsGroupedByStrengthFromSubtree
                                    , maximumTestVariableIndexFromSubtree
                                    , associationFromTestVariableIndexToNumberOfItsLevelsFromSubtree =
                                    walkTree subtreeRootNode strength indexForLeftmostTestVariable
                                let mergedTestVariableCombinationsGroupedByStrength =
                                    joinPairsAtEachStrength previouslyMergedTestVariableCombinations testVariableCombinationsGroupedByStrengthFromSubtree
                                let associationFromTestVariableIndexToNumberOfItsLevels =
                                    List.append associationFromTestVariableIndexToNumberOfItsLevelsFromSubtree previousAssociationFromTestVariableIndexToNumberOfItsLevels
                                mergedTestVariableCombinationsGroupedByStrength
                                , maximumTestVariableIndexFromSubtree
                                , associationFromTestVariableIndexToNumberOfItsLevels                               
                            subtreeRootNodes
                            |> Seq.fold mergeTestVariableCombinationsFromSubtree ([], indexForLeftmostTestVariable, [])
                        
                      | SynthesizingNode (subtreeRootNodes
                                          , _) ->
                            let gatherTestVariableCombinationsFromSubtree (previouslyGatheredTestVariableCombinations
                                                                           , indexForLeftmostTestVariable
                                                                           , previousAssociationFromTestVariableIndexToNumberOfItsLevels)
                                                                          subtreeRootNode =
                                let testVariableCombinationsGroupedByStrengthFromSubtree
                                    , maximumTestVariableIndexFromSubtree
                                    , associationFromTestVariableIndexToNumberOfItsLevelsFromSubtree =
                                    walkTree subtreeRootNode strength indexForLeftmostTestVariable
                                let gatheredTestVariableCombinationsGroupedBySubtreeAndThenByStrength =
                                    testVariableCombinationsGroupedByStrengthFromSubtree :: previouslyGatheredTestVariableCombinations
                                let associationFromTestVariableIndexToNumberOfItsLevels =
                                    List.append associationFromTestVariableIndexToNumberOfItsLevelsFromSubtree previousAssociationFromTestVariableIndexToNumberOfItsLevels
                                gatheredTestVariableCombinationsGroupedBySubtreeAndThenByStrength
                                , maximumTestVariableIndexFromSubtree
                                , associationFromTestVariableIndexToNumberOfItsLevels
                            // Using 'fold' causes 'resultsFromSubtrees' to be built up in reverse to the subtree sequence,
                            // and this reversal propagates consistently through the code below. The only way it could
                            // cause a problem would be due to the order of processing the subtrees, but because the combinations
                            // from sibling subtrees are simply placed in a list and because the test variable indices are already
                            // correctly calculated, it doesn't matter.
                            let testVariableCombinationsGroupedBySubtreeAndThenByStrength
                                , maximumTestVariableIndex
                                , associationFromTestVariableIndexToNumberOfItsLevels =
                                subtreeRootNodes
                                |> Seq.fold gatherTestVariableCombinationsFromSubtree ([], indexForLeftmostTestVariable, [])
                            let maximumStrengthsFromSubtrees =
                                testVariableCombinationsGroupedBySubtreeAndThenByStrength
                                |> List.map (fun testVariableCombinationsForOneSubtreeGroupedByStrength ->
                                                uint32 (List.length testVariableCombinationsForOneSubtreeGroupedByStrength))
                            let testVariableCombinationsGroupedBySubtreeAndThenByStrength =
                                testVariableCombinationsGroupedBySubtreeAndThenByStrength
                                |> List.map List.toArray
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
                                        (List.CrossProduct testVariableCombinationsBySubtree)
                                        |> List.map (List.reduce joinTestVariableCombinations)
                                    List.append testVariableCombinationsBuiltFromCrossProduct partialResult
                                (distributions |> Seq.fold addInTestVariableCombinationsForAGivenDistribution [])
                                :: partialResult
                            let testVariableCombinationsGroupedByStrength =
                                Map.foldBack addInTestVariableCombinationsForGivenStrength distributionsOfStrengthsOverSubtreesAtEachTotalStrength []
                            testVariableCombinationsGroupedByStrength
                            , maximumTestVariableIndex
                            , associationFromTestVariableIndexToNumberOfItsLevels
                 let testVariableCombinationsGroupedByStrength
                     , _
                     , associationFromTestVariableIndexToNumberOfItsLevels =
                     walkTree this strength 0u
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
                        let numberOfLevelsForTestVariable testVariableIndex
                            = associationFromTestVariableIndexToNumberOfItsLevels.[testVariableIndex]
                        testVariableCombination
                        |> List.sortWith (fun first second ->
                                            compare (numberOfLevelsForTestVariable second)
                                                    (numberOfLevelsForTestVariable first))
                    let levelEntriesForTestVariableIndicesFromList =
                        testVariableCombinationSortedByDecreasingNumberOfLevels
                        |> List.map (fun testVariableIndex ->
                                     associationFromTestVariableIndexToNumberOfItsLevels.[testVariableIndex]
                                     |> int32
                                     |> (BargainBasement.Flip List.init) (fun levelIndex -> testVariableIndex, Index levelIndex))
                    levelEntriesForTestVariableIndicesFromList
                    |> List.CrossProductWithCommonSuffix sentinelEntriesForInterleavedTestVariableIndices
                    |> List.map (fun testVectorRepresentationAsList ->
                                    Map.ofList testVectorRepresentationAsList)
                 testVariableCombinationsGroupedByStrength
                 |> List.map (fun testVariableCombinations ->
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
                            let subtreeRootNodes =
                                subtreeRootNodes
                                |> LazyList.ofSeq
                            let rec discardLeftmostSubtreesInvolvingOnlyExcludedTestVariables subtreeRootNodes
                                                                                              indexForLeftmostTestVariable =
                                match subtreeRootNodes with
                                        LazyList.Nil ->
                                            raise (InternalAssertionViolationException "Ran out of subtree nodes but there should have definitely been enough of them.")
                                      | LazyList.Cons (head
                                                       , tail) ->
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
                            walkTree (LazyList.head subtreeRootNodesHeadedByNodeContainingLeftmostNonExcludedTestVariable)
                                     indexForLeftmostTestVariableInNodeContainingLeftmostNonExcludedTestVariable
                      | SynthesizingNode (subtreeRootNodes
                                          , synthesisDelegate) ->
                            let subtreeRootNodes =
                                subtreeRootNodes
                                |> LazyList.ofSeq
                            let rec collectResultsFromSubtrees subtreeRootNodes
                                                               indexForLeftmostTestVariable =
                                match subtreeRootNodes with
                                        LazyList.Nil ->
                                            []
                                      | LazyList.Cons (head
                                                       , tail) ->
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
            
                                            
                      