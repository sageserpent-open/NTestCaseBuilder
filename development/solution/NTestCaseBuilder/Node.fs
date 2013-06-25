#nowarn "40"

namespace NTestCaseBuilder

    open System.Collections
    open System.Collections.Generic
    open System
    open SageSerpent.Infrastructure
    open SageSerpent.Infrastructure.RandomExtensions
    open SageSerpent.Infrastructure.ListExtensions
    open SageSerpent.Infrastructure.OptionWorkflow
    open SageSerpent.Infrastructure.OptionExtensions
    open SageSerpent.Infrastructure.ContinuationWorkflow
    open Microsoft.FSharp.Collections

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

        abstract IsNodeZeroCost: Int32 -> Boolean

    and NodeKind =
        TestVariable of array<Object>
      | Singleton of Object
      | Interleaving of List<Node>
      | Synthesizing of IFixedCombinationOfSubtreeNodesForSynthesis

    and Node (kind: NodeKind,
              filters: List<LevelCombinationFilter>) =
        new kind =
            Node (kind,
                  List.empty)

        member this.Kind =
            kind

        member this.Filters =
            filters

        member this.WithFilter filter =
            Node (kind,
                  filter :: filters) 

    and LevelCombinationFilter =
        delegate of IDictionary<Int32, Int32 * Object> -> Boolean

    module NodeExtensions =
        let inline (|TestVariableNode|SingletonNode|InterleavingNode|SynthesizingNode|) (node: Node) =
            // NASTY HACK: I wish this was written in Scala! There, I said it.
            // Just whingeing about the inability to extend a discriminated union
            // with additional state in F#, which necessitates this hack.
            match node.Kind with
                TestVariable levels ->
                    Choice1Of4 levels
              | Singleton singletonTestCase ->
                    Choice2Of4 singletonTestCase
              | Interleaving subtreeRootNodes ->
                    Choice3Of4 subtreeRootNodes
              | Synthesizing fixedCombinationOfSubtreeNodesForSynthesis ->
                    Choice4Of4 fixedCombinationOfSubtreeNodesForSynthesis

        let inline TestVariableNode levels =
            Node (TestVariable levels)

        let inline SingletonNode singletonTestCase =
            Node (Singleton singletonTestCase)

        let inline InterleavingNode subtreeRootNodes =
            Node (Interleaving subtreeRootNodes)

        let inline SynthesizingNode fixedCombinationOfSubtreeNodesForSynthesis =
            Node (Synthesizing fixedCombinationOfSubtreeNodesForSynthesis)

    open NodeExtensions

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
                                TestVariableNodeResult = fun _ -> 1
                                SingletonNodeResult = fun () -> 1
                                CombineResultsFromInterleavingNodeSubtrees = Seq.reduce (+)
                                CombineResultsFromSynthesizingNodeSubtrees = Seq.reduce (+)
                            }

        let sumLevelCountsFromAllTestVariables =
            traverseTree    {
                                TestVariableNodeResult = fun levels -> Seq.length levels
                                SingletonNodeResult = fun () -> 0
                                CombineResultsFromInterleavingNodeSubtrees = Seq.reduce (+)
                                CombineResultsFromSynthesizingNodeSubtrees = Seq.reduce (+)
                            }

        let maximumStrengthOfTestVariableCombination =
            traverseTree    {
                                TestVariableNodeResult = fun _ -> 1
                                SingletonNodeResult = fun () -> 1
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
                            |> Option<_>.GetFromMany
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
                    indexForLeftmostTestVariable + 1
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
                                List.init (maximumTestVariableFromSubtree - indexForLeftmostTestVariable)
                                          (fun variableCount -> variableCount + indexForLeftmostTestVariable)
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
                    walkTree this 0 [] []
            HashMultiMap (result, HashIdentity.Structural)

        member this.AssociationFromStrengthToPartialTestVectorRepresentations maximumDesiredStrength =
            let randomBehaviour =
                Random 6739
            let rec walkTree node maximumDesiredStrength indexForLeftmostTestVariable =
                match node with
                    TestVariableNode levels ->
                        if 0 = maximumDesiredStrength
                        then
                            Map.empty
                        else
                            Map.ofList [1, Seq.singleton [indexForLeftmostTestVariable]]
                        , indexForLeftmostTestVariable + 1
                        , [indexForLeftmostTestVariable
                           , Array.length levels]

                  | SingletonNode _ ->
                        if 0 = maximumDesiredStrength
                        then
                            Map.empty
                        else
                            Map.ofList [1, Seq.singleton [indexForLeftmostTestVariable]]
                        , indexForLeftmostTestVariable + 1
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
                                let interleaveTestVariableCombinations firstSequence
                                                                       secondSequence =
                                    randomBehaviour.PickAlternatelyFrom [firstSequence; secondSequence]
                                BargainBasement.MergeAssociations interleaveTestVariableCombinations
                                                                  previousAssociationFromStrengthToTestVariableCombinations
                                                                  associationFromStrengthToTestVariableCombinationsFromSubtree
                            let associationFromTestVariableIndexToNumberOfItsLevels =
                                List.append associationFromTestVariableIndexToNumberOfItsLevelsFromSubtree
                                            previousAssociationFromTestVariableIndexToNumberOfItsLevels
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
                                List.append associationFromTestVariableIndexToNumberOfItsLevelsFromSubtree
                                            previousAssociationFromTestVariableIndexToNumberOfItsLevels
                            perSubtreeAssociationsFromStrengthToTestVariableCombinations
                            , maximumTestVariableIndexFromSubtree
                            , associationFromTestVariableIndexToNumberOfItsLevels
                        let subtreeNodes =
                            fixedCombinationOfSubtreeNodesForSynthesis.Nodes
                        // Using 'fold' causes 'perSubtreeAssociationsFromStrengthToTestVariableCombinations' to be built up in
                        // reverse to the subtree sequence, and this reversal propagates consistently through the code below. The
                        // only way it could cause a problem would be due to the order of processing the subtrees, but because the
                        // combinations of the same strength from sibling subtrees are simply placed in a list and because the test
                        // variable indices are already correctly calculated, it doesn't matter.
                        let perSubtreeAssociationsFromStrengthToTestVariableCombinations
                            , maximumTestVariableIndex
                            , associationFromTestVariableIndexToNumberOfItsLevels =
                            subtreeNodes
                            |> Seq.fold gatherTestVariableCombinationsFromSubtree ([], indexForLeftmostTestVariable, [])
                        let numberOfSubtrees =
                            List.length subtreeNodes
                        let maximumStrengthsFromSubtrees =
                            perSubtreeAssociationsFromStrengthToTestVariableCombinations
                            |> List.map (fun associationFromStrengthToTestVariableCombinationsForOneSubtree ->
                                            if associationFromStrengthToTestVariableCombinationsForOneSubtree.IsEmpty
                                            then
                                                0
                                            else
                                               (associationFromStrengthToTestVariableCombinationsForOneSubtree :> IDictionary<_, _>).Keys
                                               |> Seq.max)
                        // We have to cope with individual subtrees being requested to yield zero strength combinations: what we
                        // mean is that the subtree doesn't have to provide any test variables for a given distribution,
                        // in which case we yield a sentinel value of [[]] (which is an identity under the cross product).
                        // Contrast this with the case where a subtree is asked for combinations of a positive strength that it
                        // doesn't have: in this case we yield [], which is a zero under the cross product, reflecting the fact
                        // that we can't achieve the distribution in question.
                        // However, there is another subtlety: we must exclude the case where the total strength is zero: *all* test
                        // variables, ordinary *and* singleton count towards the strength, so there should not be any test variable
                        // combinations of zero strength resulting from combinations across subtrees. That way, we know that any request
                        // for zero strength combinations from a particular subtree will never result in a zero-strength combination
                        // being created for the parent synthesizing node. If we don't do this, this leads to situations where synthesizing
                        // nodes higher up the tree will build alternate combinations of the same combination with lots of trivial
                        // empty combinations of zero strength - this won't cause any logic failures due to subsequent test vector
                        // merging, but does lead to a combinatoric explosion of memory usage and time.
                        let distributionsOfStrengthsOverSubtreesAtEachTotalStrength =
                            CombinatoricUtilities.ChooseContributionsToMeetTotalsUpToLimit maximumStrengthsFromSubtrees maximumDesiredStrength
                            |> Map.remove 0
                        let addInTestVariableCombinationsForGivenTotalStrength totalStrength
                                                                               distributionsOfStrengthsOverSubtrees
                                                                               partialAssociationFromStrengthToTestVariableCombinations =
                            let addInTestVariableCombinationsForAGivenDistribution partialTestVariableCombinations distributionOfStrengthsOverSubtrees =
                                let perSubtreeTestVariableCombinations =
                                    List.zip distributionOfStrengthsOverSubtrees perSubtreeAssociationsFromStrengthToTestVariableCombinations
                                    |> List.mapi (fun indexOfSubtreeCountingFromTheRight
                                                      (strength
                                                       , associationFromStrengthToTestVariableCombinationsForOneSubtree) ->
                                                        let indexOfSubtree =
                                                            numberOfSubtrees - (1 + indexOfSubtreeCountingFromTheRight)
                                                        let strength =
                                                            if fixedCombinationOfSubtreeNodesForSynthesis.IsNodeZeroCost indexOfSubtree
                                                            then
                                                                (associationFromStrengthToTestVariableCombinationsForOneSubtree :> IDictionary<_, _>).Keys
                                                                |> Seq.max      
                                                            else
                                                                strength
                                                        match Map.tryFind strength associationFromStrengthToTestVariableCombinationsForOneSubtree with
                                                            Some testVariableCombinations when strength > 0 ->
                                                                testVariableCombinations
                                                          | Some _ ->
                                                                raise (InternalAssertionViolationException "Zero strength combinations are not permitted as results from any node.")
                                                          | None ->
                                                                if strength = 0
                                                                then
                                                                    Seq.singleton []
                                                                else
                                                                    Seq.empty)
                                let joinTestVariableCombinations =
                                   List.append
                                let testVariableCombinationsBuiltFromCrossProduct =
                                    (List.DecorrelatedCrossProduct randomBehaviour
                                                                   perSubtreeTestVariableCombinations)
                                    |> Seq.map (List.reduce joinTestVariableCombinations)
                                Seq.append testVariableCombinationsBuiltFromCrossProduct partialTestVariableCombinations
                            let testVariableCombinationsWithTotalStrength =
                                distributionsOfStrengthsOverSubtrees |> List.fold addInTestVariableCombinationsForAGivenDistribution Seq.empty
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
                walkTree this maximumDesiredStrength 0
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
                let levelEntriesForTestVariableIndicesFromList =
                    testVariableCombination
                    |> List.map (fun testVariableIndex ->
                                    match Map.tryFind testVariableIndex associationFromTestVariableIndexToNumberOfItsLevels with
                                        Some numberOfLevels ->
                                            numberOfLevels
                                            |> (BargainBasement.Flip List.init) (fun levelIndex -> testVariableIndex, Level levelIndex)
                                      | None ->
                                            [(testVariableIndex, SingletonPlaceholder)])
                levelEntriesForTestVariableIndicesFromList
                |> List.DecorrelatedCrossProductWithCommonSuffix randomBehaviour
                                                                 sentinelEntriesForInterleavedTestVariableIndices
                |> Seq.map (fun testVectorRepresentationAsList ->
                                    Map.ofList testVectorRepresentationAsList)
            associationFromStrengthToTestVariableCombinations
            |> Map.map (fun _
                            testVariableCombinations ->
                            let createTestVectorRepresentations testVariableCombinations =
                                let listsOfTestVectorsCorrespondingToTestVariableCombinations =
                                    testVariableCombinations
                                    |> List.map createTestVectorRepresentations
                                RoundRobinPickFrom listsOfTestVectorsCorrespondingToTestVariableCombinations
                            let chunkSizeThatIsSmallEnoughToAvoidMemoryPressure =
                                1000
                            seq
                                {
                                    for chunkOfTestVariableCombinations in Chunk chunkSizeThatIsSmallEnoughToAvoidMemoryPressure
                                                                                 testVariableCombinations do
                                        yield! createTestVectorRepresentations chunkOfTestVariableCombinations
                                })
            , associationFromTestVariableIndexToNumberOfItsLevels

        member this.FillOutPartialTestVectorRepresentation associationFromTestVariableIndexToNumberOfItsLevels
                                                           partialTestVectorRepresentation
                                                           (randomBehaviour: Random) =
            let associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt =
                this.AssociationFromTestVariableIndexToVariablesThatAreInterleavedWithIt
            let testVariableIndices =
                (partialTestVectorRepresentation :> IDictionary<_, _>).Keys
                |> Set.ofSeq
            let missingTestVariableIndices =
                [
                    for testVariableIndex in 0 .. this.CountTestVariables - 1 do
                        if testVariableIndices.Contains testVariableIndex
                           |> not
                        then
                            yield testVariableIndex
                ]
            let associationFromMissingTestVariableIndexToPermutationThatReshufflesItsLevelsAndLevelCount =
                missingTestVariableIndices
                |> List.map (fun missingTestVariableIndex ->
                                missingTestVariableIndex
                                , match Map.tryFind missingTestVariableIndex
                                                    associationFromTestVariableIndexToNumberOfItsLevels with
                                    Some numberOfLevels ->
                                        let shuffledLevelIndices =
                                            randomBehaviour.Shuffle (List.init numberOfLevels
                                                                               BargainBasement.Identity)
                                        ((fun unshuffledLevelIndex ->
                                            shuffledLevelIndices.[unshuffledLevelIndex])
                                         , numberOfLevels)
                                        |> Some
                                  | None ->
                                        None)
                |> Map.ofList
            let rec fillInRandomTestVariablesMarkingExcludedOnesAsWell missingTestVariableIndices
                                                                       entriesForPreviouslyExcludedTestVariableIndices =
                continuationWorkflow
                    {
                        if Set.isEmpty missingTestVariableIndices
                        then
                            return entriesForPreviouslyExcludedTestVariableIndices
                        else
                            let chosenTestVariableIndex =
                                Set.minElement missingTestVariableIndices
                            let excludedTestVariableIndices =
                                associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt.FindAll chosenTestVariableIndex
                                |> Set.ofList
                                |> Set.intersect missingTestVariableIndices
                            let entriesForExcludedTestVariableIndices =
                                excludedTestVariableIndices
                                |> Seq.map (fun excludedTestVariableIndex ->
                                                excludedTestVariableIndex
                                                , Exclusion)
                                |> List.ofSeq
                            let missingTestVariableIndicesExcludingTheChosenOneAndItsExclusions =
                                (missingTestVariableIndices
                                 |> Set.remove chosenTestVariableIndex)
                                - excludedTestVariableIndices
                            match associationFromMissingTestVariableIndexToPermutationThatReshufflesItsLevelsAndLevelCount.[chosenTestVariableIndex] with
                                Some (permutation
                                      , numberOfLevels) ->
                                    let rec tryANewLevelIndex unshuffledLevelIndex =
                                        continuationWorkflow
                                            {
                                                let levelForChosenTestVariable =
                                                    permutation unshuffledLevelIndex
                                                    |> Level
                                                return! fillInRandomTestVariablesMarkingExcludedOnesAsWell missingTestVariableIndicesExcludingTheChosenOneAndItsExclusions
                                                                                                           (List.append ((chosenTestVariableIndex
                                                                                                                          , levelForChosenTestVariable)
                                                                                                                         :: entriesForExcludedTestVariableIndices)
                                                                                                                        entriesForPreviouslyExcludedTestVariableIndices)
                                            }
                                        + continuationWorkflow
                                            {
                                                let unshuffledLevelIndex =
                                                    1 + unshuffledLevelIndex
                                                if numberOfLevels = unshuffledLevelIndex
                                                then
                                                    return! continuationWorkflow.Zero ()
                                                else
                                                    return! tryANewLevelIndex unshuffledLevelIndex
                                            }
                                    return! tryANewLevelIndex 0
                              | None ->
                                    // This case picks up a test variable index for a singleton test case:
                                    // the map is built so that it doesn't have entries for these.
                                    let levelForChosenTestVariable =
                                        SingletonPlaceholder
                                    return! fillInRandomTestVariablesMarkingExcludedOnesAsWell missingTestVariableIndicesExcludingTheChosenOneAndItsExclusions
                                                                                               (List.append ((chosenTestVariableIndex
                                                                                                              , levelForChosenTestVariable)
                                                                                                             :: entriesForExcludedTestVariableIndices)
                                                                                                            entriesForPreviouslyExcludedTestVariableIndices)
                    }
            (continuationWorkflow
                {
                    let! filledAndExcludedTestVariables =
                        fillInRandomTestVariablesMarkingExcludedOnesAsWell (missingTestVariableIndices
                                                                            |> Set.ofList)
                                                                           List.empty
                    return BargainBasement.MergeDisjointSortedAssociationLists (filledAndExcludedTestVariables
                                                                                |> List.sortBy fst)
                                                                               (partialTestVectorRepresentation
                                                                                |> Map.toList)
                           |> List.map snd
                           |> List.toArray
                }).Execute(Some,
                           (fun () ->
                                None))

        member this.FinalValueCreator () =
            let indicesInVectorForLeftmostTestVariableInEachSubtree subtreeRootNodes =
                subtreeRootNodes
                |> Array.scan (fun indexInVectorForLeftmostVariableInPreviousSubtree
                                   subtreeRootNode ->
                                indexInVectorForLeftmostVariableInPreviousSubtree
                                + (subtreeRootNode: Node).CountTestVariables) 0
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
                        let fullTestVectorLength =
                            Array.length fullTestVector
                        if fullTestVectorLength > this.CountTestVariables
                        then
                            raise (PreconditionViolationException "Vector is inconsistent with node rendering it: it has more test variables then expected by the interleaving node.")
                        match fullTestVector
                              |> Array.findIndex notExcluded with
                            -1 ->
                                raise (PreconditionViolationException "Vector is inconsistent with node rendering it - an interleaving node expects at least one non-excluded test variable contributing to the interleave.")
                          | indexOfLeftmostNonExcludedTestVariable ->

                                let leastUpperBoundOfIndex = Array.BinarySearch (indicesInVectorForLeftmostTestVariableInEachSubtree,
                                                                                 indexOfLeftmostNonExcludedTestVariable)
                                let indexOfIncludedSubtree =
                                    if 0 > leastUpperBoundOfIndex
                                    then
                                        ~~~leastUpperBoundOfIndex - 1
                                    else
                                        leastUpperBoundOfIndex
                                if Array.length indicesInVectorForLeftmostTestVariableInEachSubtree = 1 + indexOfIncludedSubtree
                                then
                                    raise (LogicErrorException "Shouldn't try to index into the final value in 'indicesInVectorForLeftmostTestVariableInEachSubtree': it has no corresponding subtree.")
                                        // NOTE: said final value is however used a bit further down, but in an off-by-one context where it makes sense.
                                let mirrorIndex index =
                                    fullTestVectorLength - (1 + index)
                                match List.init fullTestVectorLength BargainBasement.Identity
                                      |> List.find (mirrorIndex
                                                    >> (fun index ->
                                                            fullTestVector.[index])
                                                    >> notExcluded)
                                      |> mirrorIndex with
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
                        if Array.length fullTestVector > this.CountTestVariables
                        then
                            raise (PreconditionViolationException "Vector is inconsistent with node rendering it: it has more test variables then expected by the synthesizing node.")
                        let slicesOfFullTestVectorCorrespondingToSubtrees =
                            sliceRangesOverFullTestVector
                            |> List.map (fun (indexForLeftmostTestVariable
                                              , onePastIndexForRightmostTestVariable) ->
                                            fullTestVector.[indexForLeftmostTestVariable .. onePastIndexForRightmostTestVariable - 1])
                        finalValueCreator slicesOfFullTestVectorCorrespondingToSubtrees

        static member PruneAndCombine subtreeRootNodes
                                      combinePrunedSubtrees =
            let prunedSubtreeRootNodes =
                subtreeRootNodes
                |> List.map (fun (node: Node) ->
                                node.PruneTree)
                |> Option<_>.GetFromMany
            if not (Seq.isEmpty prunedSubtreeRootNodes)
                && Seq.length prunedSubtreeRootNodes
                    = Seq.length subtreeRootNodes
            then
                prunedSubtreeRootNodes
                |> combinePrunedSubtrees
                |> Some
            else
                None

        static member CreateSynthesizingNode subtreeRootNodes
                                             synthesisDelegate =
            let rec fixedCombinationOfSubtreeNodesForSynthesis subtreeRootNodes =
                {
                    new IFixedCombinationOfSubtreeNodesForSynthesis with
                        member this.Prune =
                            Node.PruneAndCombine subtreeRootNodes
                                                 fixedCombinationOfSubtreeNodesForSynthesis

                        member this.Nodes =
                            subtreeRootNodes

                        member this.FinalValueCreator () =
                            fun slicesOfFullTestVector ->
                                let resultsFromSubtrees =
                                    List.zip subtreeRootNodes
                                             slicesOfFullTestVector
                                    |> List.map (fun (subtreeRootNode
                                                      , sliceOfFullTestVectorCorrespondingToSubtree) ->
                                                    subtreeRootNode.FinalValueCreator () sliceOfFullTestVectorCorrespondingToSubtree)
                                let invocationArguments =
                                    resultsFromSubtrees
                                    |> List.toArray
                                (synthesisDelegate: Delegate).DynamicInvoke invocationArguments
                                |> unbox

                        member this.IsNodeZeroCost _ =
                            false
                }
            fixedCombinationOfSubtreeNodesForSynthesis subtreeRootNodes
            |> SynthesizingNode