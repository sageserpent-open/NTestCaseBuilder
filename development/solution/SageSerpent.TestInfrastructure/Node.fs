#nowarn "40"

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System.Collections.Generic
    open System
    open SageSerpent.Infrastructure
    open SageSerpent.Infrastructure.RandomExtensions
    open SageSerpent.Infrastructure.ListExtensions
    open SageSerpent.Infrastructure.OptionWorkflow
    open Microsoft.FSharp.Collections
    open Wintellect.PowerCollections

    type NodeVisitOperations<'Result> =
        {
            TestVariableNodeResult: array<Object> -> 'Result
            SingletonNodeResult: unit -> 'Result
            CombineResultsFromInterleavingNodeSubtrees: seq<'Result> -> 'Result
            CombineResultsFromSynthesizingNodeSubtrees: seq<'Result> -> 'Result
        }

    type TestVariable<'Data> =
        Level of 'Data
      | SingletonPlaceholder
      | Exclusion

    type FullTestVector =
        array<TestVariable<Int32>>

    type IFixedCombinationOfSubtreeNodesForSynthesis =
        abstract Prune: Option<IFixedCombinationOfSubtreeNodesForSynthesis>

        abstract Nodes: List<Node>

        abstract FinalValueCreator: Unit -> (List<FullTestVector> -> 'CallerViewOfSynthesizedTestCase)

    and Node =
        TestVariableNode of array<Object>
      | SingletonNode of Object
      | InterleavingNode of List<Node>
      | SynthesizingNode of IFixedCombinationOfSubtreeNodesForSynthesis

    module NodeDetail =
        let traverseTree nodeOperations =
            let rec memoizedCalculation =
                BargainBasement.Memoize (fun node ->
                                            match node with
                                                TestVariableNode levels ->
                                                    nodeOperations.TestVariableNodeResult levels
                                              | SingletonNode _ ->
                                                    nodeOperations.SingletonNodeResult ()
                                              | InterleavingNode subtreeRootNodes ->
                                                    subtreeRootNodes
                                                    |> Seq.map (fun subtreeHead -> memoizedCalculation subtreeHead)
                                                    |> nodeOperations.CombineResultsFromInterleavingNodeSubtrees
                                              | SynthesizingNode fixedCombinationOfSubtreeNodesForSynthesis ->
                                                    fixedCombinationOfSubtreeNodesForSynthesis.Nodes
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
                        then
                            None
                        else
                            Some node
                  | SingletonNode _ as node ->
                        Some node
                  | InterleavingNode subtreeRootNodes ->
                        let prunedSubtreeRootNodes =
                            subtreeRootNodes
                            |> List.map walkTree
                            |> List.filter Option.isSome
                            |> List.map Option.get
                        if Seq.isEmpty prunedSubtreeRootNodes
                        then
                            None
                        else
                            Some (InterleavingNode prunedSubtreeRootNodes)
                  | SynthesizingNode fixedCombinationOfSubtreeNodesForSynthesis ->
                        fixedCombinationOfSubtreeNodesForSynthesis.Prune
                        |> Option.map SynthesizingNode
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
                  | SynthesizingNode fixedCombinationOfSubtreeNodesForSynthesis ->
                        let mergeAssociationFromSubtree (indexForLeftmostTestVariable
                                                         , previouslyMergedAssociationList)
                                                        subtreeRootNode =
                            walkTree subtreeRootNode indexForLeftmostTestVariable interleavingTestVariableIndices previouslyMergedAssociationList
                        fixedCombinationOfSubtreeNodesForSynthesis.Nodes
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
                                BargainBasement.MergeAssociations List.append
                                                                  previousAssociationFromStrengthToTestVariableCombinations
                                                                  associationFromStrengthToTestVariableCombinationsFromSubtree
                            let associationFromTestVariableIndexToNumberOfItsLevels =
                                List.append associationFromTestVariableIndexToNumberOfItsLevelsFromSubtree previousAssociationFromTestVariableIndexToNumberOfItsLevels
                            mergedAssociationFromStrengthToTestVariableCombinations
                            , maximumTestVariableIndexFromSubtree
                           , associationFromTestVariableIndexToNumberOfItsLevels
                        subtreeRootNodes
                        |> Seq.fold mergeTestVariableCombinationsFromSubtree (Map.empty, indexForLeftmostTestVariable, [])

                  | SynthesizingNode fixedCombinationOfSubtreeNodesForSynthesis ->
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
                            fixedCombinationOfSubtreeNodesForSynthesis.Nodes
                            |> Seq.fold gatherTestVariableCombinationsFromSubtree ([], indexForLeftmostTestVariable, [])
                        let maximumStrengthsFromSubtrees =
                            perSubtreeAssociationsFromStrengthToTestVariableCombinations
                            |> List.map (fun associationFromStrengthToTestVariableCombinationsForOneSubtree ->
                                            if associationFromStrengthToTestVariableCombinationsForOneSubtree.IsEmpty
                                            then
                                                0u
                                            else
                                               associationFromStrengthToTestVariableCombinationsForOneSubtree
                                               |> Map.toSeq
                                               |> Seq.map fst
                                               |> Seq.max)
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
                                            |> (BargainBasement.Flip List.init) (fun levelIndex -> testVariableIndex, Level levelIndex)
                                      | None ->
                                            [(testVariableIndex, SingletonPlaceholder)])
                levelEntriesForTestVariableIndicesFromList
                |> List.CrossProductWithCommonSuffix sentinelEntriesForInterleavedTestVariableIndices
                |> List.map (fun testVectorRepresentationAsList ->
                                Map.ofList testVectorRepresentationAsList)
            associationFromStrengthToTestVariableCombinations
            |> Map.map (fun strength testVariableCombinations ->
                            let listsOfTestVectorsCorrespondingToTestVariableCombinations =
                                testVariableCombinations
                                |> List.map createTestVectorRepresentations
                            let assembleHeadTestVectorRepresentationsFromEachList listsOfTestVectorsCorrespondingToTestVariableCombinations =
                                let headsAndTailsFromListsOfTestVectorsCorrespondingToTestVariableCombinations =
                                    [ for testVectorsCorrespondingToASingleTestVariableCombination in listsOfTestVectorsCorrespondingToTestVariableCombinations do
                                        match testVectorsCorrespondingToASingleTestVariableCombination with
                                            head :: tail ->
                                                yield head
                                                      , tail
                                          | [] ->
                                                () ]
                                match headsAndTailsFromListsOfTestVectorsCorrespondingToTestVariableCombinations with
                                    [] ->
                                        None
                                  | _ ->
                                        let heads
                                            , tails =
                                            headsAndTailsFromListsOfTestVectorsCorrespondingToTestVariableCombinations
                                            |> List.unzip
                                        Some (heads
                                              , tails)
                            let groupsOfTestVectorsWhereEachGroupSpansTheTestVariableCombinations
                                = listsOfTestVectorsCorrespondingToTestVariableCombinations
                                  |> Seq.unfold assembleHeadTestVectorRepresentationsFromEachList
                            seq
                                {
                                    for group in groupsOfTestVectorsWhereEachGroupSpansTheTestVariableCombinations do
                                        yield! group
                                })
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
                                |> Level
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

        member this.FinalValueCreator () =
            let indicesInVectorForLeftmostTestVariableInEachSubtree subtreeRootNodes =
                subtreeRootNodes
                |> Array.scan (fun indexInVectorForLeftmostVariableInPreviousSubtree
                                   subtreeRootNode ->
                                indexInVectorForLeftmostVariableInPreviousSubtree
                                + int32 (subtreeRootNode: Node).CountTestVariables) 0
            match this with
                TestVariableNode levels ->
                    let levels =
                        levels
                        |> Array.map unbox  // Take an up-front, one-off performance hit so that the resulting function value doesn't have
                                            // any internal unboxing in its implementation. Converting all of the levels up-front isn't wasted
                                            // effort because we know that all the levels will be used anyway by the client code.
                    fun fullTestVector ->
                        match fullTestVector with
                            [| Level indexOfTestVariableValue |] ->
                                levels.[indexOfTestVariableValue]
                          | _ ->
                                raise (PreconditionViolationException "Vector is inconsistent with node rendering it: a test variable node expects a single test variable with a level.")
              | SingletonNode singletonTestCase ->
                    let singletonTestCase =
                        singletonTestCase
                        |> unbox
                    fun fullTestVector ->
                        match fullTestVector with
                            [| SingletonPlaceholder |] ->
                                singletonTestCase
                          | _ ->
                                raise (PreconditionViolationException "Vector is inconsistent with node rendering it: a singleton node expects a single test variable with a singleton placeholder.")
              | InterleavingNode subtreeRootNodes ->
                    let notExcluded testVariableLevel =
                        match testVariableLevel with
                            Exclusion ->
                                false
                          | _ -> true
                    let subtreeRootNodes =
                        subtreeRootNodes
                        |> Array.ofList
                    let indicesInVectorForLeftmostTestVariableInEachSubtree =
                        indicesInVectorForLeftmostTestVariableInEachSubtree subtreeRootNodes
                    fun fullTestVector ->
                        if uint32 (Array.length fullTestVector) > this.CountTestVariables
                        then
                            raise (PreconditionViolationException "Vector is inconsistent with node rendering it: it has more test variables then expected by the interleaving node.")
                        match Algorithms.FindFirstIndexWhere (fullTestVector,
                                                              Predicate notExcluded) with
                            -1 ->
                                raise (PreconditionViolationException "Vector is inconsistent with node rendering it - an interleaving node expects at least one non-excluded test variable contributing to the interleave.")
                          | indexOfLeftmostNonExcludedTestVariable ->

                                let howManyFound
                                    , leastUpperBoundOfIndex = Algorithms.BinarySearch (indicesInVectorForLeftmostTestVariableInEachSubtree,
                                                                                        indexOfLeftmostNonExcludedTestVariable)
                                let indexOfIncludedSubtree =
                                    if 0 = howManyFound
                                    then
                                        leastUpperBoundOfIndex - 1
                                    else
                                        leastUpperBoundOfIndex
                                if Array.length indicesInVectorForLeftmostTestVariableInEachSubtree = 1 + indexOfIncludedSubtree
                                then
                                    raise (LogicErrorException "Shouldn't try to index into the final value in 'indicesInVectorForLeftmostTestVariableInEachSubtree': it has no corresponding subtree.")
                                        // NOTE: said final value is however used a bit further down, but in an off-by-one context where it makes sense.
                                match Algorithms.FindLastIndexWhere (fullTestVector,
                                                                     Predicate notExcluded) with
                                    -1 ->
                                        raise (LogicErrorException "This should be already be guarded against by the check on the index of the leftmost non-excluded test variable.")
                                  | indexOfRightmostNonExcludedTestVariable ->
                                        let numberOfTestVariablesNotExcludedToTheRightByTheSubtree =
                                            indicesInVectorForLeftmostTestVariableInEachSubtree.[1 + indexOfIncludedSubtree]
                                        if indexOfRightmostNonExcludedTestVariable >= numberOfTestVariablesNotExcludedToTheRightByTheSubtree
                                        then
                                            raise (PreconditionViolationException "Vector is inconsistent with node rendering it - an interleaving node expects at least all non-excluded test variables contributing to the interleave to come from a single subtree.")
                                        let numberOfTestVariablesExcludedToTheLeftByTheSubtree =
                                            indicesInVectorForLeftmostTestVariableInEachSubtree.[indexOfIncludedSubtree]
                                        let sliceOfFullTestVectorCorrespondingToTheIncludedSubtree =
                                            fullTestVector.[numberOfTestVariablesExcludedToTheLeftByTheSubtree .. numberOfTestVariablesNotExcludedToTheRightByTheSubtree - 1]
                                        let includedSubtree =
                                            subtreeRootNodes.[indexOfIncludedSubtree]
                                        includedSubtree.FinalValueCreator () sliceOfFullTestVectorCorrespondingToTheIncludedSubtree
              | SynthesizingNode fixedCombinationOfSubtreeNodesForSynthesis ->
                    let subtreeRootNodes =
                        fixedCombinationOfSubtreeNodesForSynthesis.Nodes
                        |> Array.ofList
                    let indicesInVectorForLeftmostTestVariableInEachSubtree =
                        indicesInVectorForLeftmostTestVariableInEachSubtree subtreeRootNodes
                    let numberOfSubtrees =
                        Array.length subtreeRootNodes
                    let sliceRangesOverFullTestVector =
                        Seq.pairwise indicesInVectorForLeftmostTestVariableInEachSubtree
                        |> List.ofSeq
                    let finalValueCreator =
                        fixedCombinationOfSubtreeNodesForSynthesis.FinalValueCreator ()
                    fun fullTestVector ->
                        if uint32 (Array.length fullTestVector) > this.CountTestVariables
                        then
                            raise (PreconditionViolationException "Vector is inconsistent with node rendering it: it has more test variables then expected by the synthesizing node.")
                        let slicesOfFullTestVectorCorrespondingToSubtrees =
                            sliceRangesOverFullTestVector
                            |> List.map (fun (indexForLeftmostTestVariable
                                              , onePastIndexForRightmostTestVariable) ->
                                            fullTestVector.[indexForLeftmostTestVariable .. onePastIndexForRightmostTestVariable - 1])
                        finalValueCreator slicesOfFullTestVectorCorrespondingToSubtrees

        static member CreateSynthesizingNode subtreeRootNodes
                                             synthesisDelegate =
            let rec fixedCombinationOfSubtreeNodesForSynthesis subtreeRootNodes =
                {
                    new IFixedCombinationOfSubtreeNodesForSynthesis with
                        member this.Prune =
                            let prunedSubtreeRootNodes =
                                subtreeRootNodes
                                |> List.map (fun (node: Node) ->
                                                node.PruneTree)
                                |> List.filter Option.isSome
                                |> List.map Option.get
                            if not (Seq.isEmpty prunedSubtreeRootNodes)
                               && Seq.length prunedSubtreeRootNodes
                                  = Seq.length subtreeRootNodes
                            then
                                prunedSubtreeRootNodes
                                |> fixedCombinationOfSubtreeNodesForSynthesis
                                |> Some
                            else
                                None

                        member this.Nodes =
                            subtreeRootNodes

                        member this.FinalValueCreator () =
                            fun slicesOfFullTestVector ->
                                let resultsFromSubtrees =
                                    Seq.zip subtreeRootNodes
                                            slicesOfFullTestVector
                                    |> Seq.map (fun (subtreeRootNode
                                                     , sliceOfFullTestVectorCorrespondingToSubtree) ->
                                                    subtreeRootNode.FinalValueCreator () sliceOfFullTestVectorCorrespondingToSubtree)
                                let invocationArguments =
                                    resultsFromSubtrees
                                    |> Seq.toArray
                                (synthesisDelegate: Delegate).DynamicInvoke invocationArguments
                                |> unbox
                }
            fixedCombinationOfSubtreeNodesForSynthesis subtreeRootNodes
            |> SynthesizingNode