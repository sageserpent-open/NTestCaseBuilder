#nowarn "40"

namespace NTestCaseBuilder

    open C5
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
    open System.Linq

    type NodeVisitOperations<'Result> =
        {
            TestVariableNodeResult: array<Object> -> 'Result
            SingletonNodeResult: unit -> 'Result
            CombineResultsFromInterleavingNodeSubtrees: List<'Result> -> 'Result
            CombineResultsFromSynthesizingNodeSubtrees: array<'Result> -> 'Result
        }

    type TestVariable<'Data> =
        Level of 'Data
      | SingletonPlaceholder
      | Exclusion

    type FullTestVector =
        array<TestVariable<Int32>>

    type IFixedCombinationOfSubtreeNodesForSynthesis =
        abstract Prune: Int32 -> Option<IFixedCombinationOfSubtreeNodesForSynthesis>

        abstract Nodes: array<Node>

        abstract FinalValueCreator: Unit -> (List<FullTestVector> -> 'CallerViewOfSynthesizedTestCase)

        abstract IsSubtreeZeroCost: Int32 -> Boolean

        abstract IsSubtreeHiddenFromFilters: Int32 -> Boolean

    and NodeKind =
        TestVariable of array<Object>
      | Singleton of Object
      | Interleaving of List<Node>
      | Synthesizing of IFixedCombinationOfSubtreeNodesForSynthesis
      | Deferral of (unit -> Node)

    and Node (kind: NodeKind,
              filters: List<LevelCombinationFilter>,
              maximumStrength: Option<Int32>,
              isZeroCost: Boolean) =
        new kind =
            Node (kind,
                  List.empty,
                  None,
                  false)

        member this.Kind =
            kind

        member this.Filters =
            filters

        member this.MaximumStrength =
            maximumStrength

        member this.IsZeroCost =
            isZeroCost

        member this.WithFilter additionalFilter =
            Node (kind,
                  additionalFilter :: filters,
                  maximumStrength,
                  isZeroCost)

        member private this.WithFilters additionalFilters =
            Node(kind,
                 List.append additionalFilters
                             filters,
                 maximumStrength,
                 isZeroCost)

        member this.WithMaximumStrength maximumStrength =
            Node (kind,
                  filters,
                  maximumStrength,
                  isZeroCost)

        member this.WithZeroCost () =
            Node (kind,
                  filters,
                  maximumStrength,
                  true)

    and LevelCombinationFilter =
        delegate of IDictionary<Int32, Int32 * Object> -> Boolean   // NOTE: the test variable index keys map to pairs of the
                                                                    // test variable level index and the corresponding
                                                                    // test variable level for the key's test variable.
    module NodeExtensions =
        let inline (|TestVariableNode|SingletonNode|InterleavingNode|SynthesizingNode|DeferralNode|) (node: Node) =
            // NASTY HACK: I wish this was written in Scala! There, I said it.
            // Just whingeing about the inability to extend a discriminated union
            // with additional state in F#, which necessitates this hack.
            match node.Kind with
                TestVariable levels ->
                    Choice1Of5 levels
              | Singleton singletonTestCase ->
                    Choice2Of5 singletonTestCase
              | Interleaving subtreeRootNodes ->
                    Choice3Of5 subtreeRootNodes
              | Synthesizing fixedCombinationOfSubtreeNodesForSynthesis ->
                    Choice4Of5 fixedCombinationOfSubtreeNodesForSynthesis
              | Deferral deferredNode ->
                    Choice5Of5 deferredNode

        let inline TestVariableNode levels =
            Node (TestVariable levels)

        let inline SingletonNode singletonTestCase =
            Node (Singleton singletonTestCase)

        let inline InterleavingNode subtreeRootNodes =
            Node (Interleaving subtreeRootNodes)

        let inline SynthesizingNode fixedCombinationOfSubtreeNodesForSynthesis =
            Node (Synthesizing fixedCombinationOfSubtreeNodesForSynthesis)

        let inline DeferralNode deferredNode =
            Node (Deferral deferredNode)

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
                                                    |> List.map (fun subtreeHead -> memoizedCalculation subtreeHead)
                                                    |> nodeOperations.CombineResultsFromInterleavingNodeSubtrees
                                              | SynthesizingNode fixedCombinationOfSubtreeNodesForSynthesis ->
                                                    fixedCombinationOfSubtreeNodesForSynthesis.Nodes
                                                    |> Array.map (fun subtreeHead -> memoizedCalculation subtreeHead)
                                                    |> nodeOperations.CombineResultsFromSynthesizingNodeSubtrees)
            memoizedCalculation

        let countTestVariables =
            traverseTree    {
                                TestVariableNodeResult = fun _ -> 1
                                SingletonNodeResult = fun () -> 1
                                CombineResultsFromInterleavingNodeSubtrees = List.reduce (+)
                                CombineResultsFromSynthesizingNodeSubtrees = Array.reduce (+)
                            }

        let sumLevelCountsFromAllTestVariables =
            traverseTree    {
                                TestVariableNodeResult = fun levels -> Seq.length levels
                                SingletonNodeResult = fun () -> 0
                                CombineResultsFromInterleavingNodeSubtrees = List.reduce (+)
                                CombineResultsFromSynthesizingNodeSubtrees = Array.reduce (+)
                            }

    open NodeDetail

    type Node with
        member this.CountTestVariables =
            countTestVariables this

        member this.SumLevelCountsFromAllTestVariables =
            sumLevelCountsFromAllTestVariables this

        member this.MaximumStrengthOfTestVariableCombination =
            let rec walkTree node =
                let maximumPossibleStrength =
                    match node with
                        TestVariableNode _ ->
                            1
                      | SingletonNode _ ->
                            1
                      | InterleavingNode subtreeRootNodes ->
                            subtreeRootNodes
                            |> List.map walkTree
                            |> List.max
                      | SynthesizingNode fixedCombinationOfSubtreeNodesForSynthesis ->
                            fixedCombinationOfSubtreeNodesForSynthesis.Nodes
                            |> Array.map walkTree
                            |> Array.reduce (+)
                match node.MaximumStrength with
                    Some maximumStrength ->
                        min maximumPossibleStrength
                            maximumStrength
                    | _ ->
                        maximumPossibleStrength
            walkTree this

        member this.PruneTree deferralBudget =
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
                            Some (((InterleavingNode prunedSubtreeRootNodes).WithFilters node.Filters).WithMaximumStrength node.MaximumStrength)
                  | SynthesizingNode fixedCombinationOfSubtreeNodesForSynthesis ->
                        fixedCombinationOfSubtreeNodesForSynthesis.Prune deferralBudget
                        |> Option.map (fun fixedCombinationOfSubtreeNodesForSynthesis ->
                                        ((SynthesizingNode fixedCombinationOfSubtreeNodesForSynthesis).WithFilters node.Filters).WithMaximumStrength node.MaximumStrength)
            walkTree this

        member this.CombinedFilter =
            let comparisonForSlicing =
                {
                    new IComparer<Int32 * _> with
                        member this.Compare((lhsIndex, _), (rhsIndex, _)) =
                            compare lhsIndex rhsIndex
                }

            let equalityForSlicing =
                {
                    new IEqualityComparer<Int32 * _> with
                        member this.Equals((lhsIndex, _), (rhsIndex, _)) =
                            lhsIndex = rhsIndex
                        member this.GetHashCode((index, _)) =
                            hash index
                }

            let rec walkTree everythingInTreeIsHiddenFromFilters
                             trampData
                             node =
                optionWorkflow
                    {
                        let! indexForLeftmostTestVariable
                             , adjustedIndexForLeftmostTestVariable
                             , testVariableIndexToLevelsAndAdjustedIndexMap
                             , filtersGroupedByNodeAndTheirBracketingIndices =
                            trampData
                        if everythingInTreeIsHiddenFromFilters
                        then
                            return (node: Node).CountTestVariables + indexForLeftmostTestVariable
                                   , adjustedIndexForLeftmostTestVariable
                                   , testVariableIndexToLevelsAndAdjustedIndexMap
                                   , filtersGroupedByNodeAndTheirBracketingIndices
                        else
                            let! onePastIndexForRightmostTestVariable
                                , onePastAdjustedIndexForRightmostTestVariable
                                , testVariableIndexToLevelsAndAdjustedIndexMap
                                , filtersGroupedByNodeAndTheirBracketingIndices =
                                match node with
                                    TestVariableNode levels ->
                                        (1 + indexForLeftmostTestVariable
                                         , 1 + adjustedIndexForLeftmostTestVariable
                                         , Map.add indexForLeftmostTestVariable
                                                   (levels
                                                    , adjustedIndexForLeftmostTestVariable)
                                                   testVariableIndexToLevelsAndAdjustedIndexMap
                                         , filtersGroupedByNodeAndTheirBracketingIndices)
                                        |> Some
                                  | SingletonNode thing ->
                                        (1 + indexForLeftmostTestVariable
                                         , 1 + adjustedIndexForLeftmostTestVariable
                                         , testVariableIndexToLevelsAndAdjustedIndexMap
                                         , filtersGroupedByNodeAndTheirBracketingIndices)
                                        |> Some
                                  | InterleavingNode subtreeRootNodes ->
                                        List.fold (walkTree false)
                                                  trampData
                                                  subtreeRootNodes
                                  | SynthesizingNode fixedCombinationOfSubtreeNodesForSynthesis ->
                                        let walkTree trampData
                                                     (everythingInTreeIsHiddenFromFilters
                                                      , node) =
                                            walkTree everythingInTreeIsHiddenFromFilters
                                                     trampData
                                                     node
                                        let subtreeRootNodes =
                                            fixedCombinationOfSubtreeNodesForSynthesis.Nodes
                                        Array.fold walkTree
                                                   trampData
                                                   (subtreeRootNodes
                                                    |> Array.mapi (fun subtreeIndex
                                                                    subtreeRootNode ->
                                                                        fixedCombinationOfSubtreeNodesForSynthesis.IsSubtreeHiddenFromFilters subtreeIndex
                                                                        , subtreeRootNode))
                            let filters =
                                node.Filters
                            let isInsane (filter: LevelCombinationFilter) =
                                filter.Invoke Map.empty
                                |> not
                            if filters
                               |> List.exists isInsane
                            then
                                return! optionWorkflow.Zero()
                            else
                                return onePastIndexForRightmostTestVariable
                                       , onePastAdjustedIndexForRightmostTestVariable
                                       , testVariableIndexToLevelsAndAdjustedIndexMap
                                       , if filters.IsEmpty
                                         then
                                            filtersGroupedByNodeAndTheirBracketingIndices
                                         else
                                            (filters
                                             , adjustedIndexForLeftmostTestVariable
                                             , onePastAdjustedIndexForRightmostTestVariable) :: filtersGroupedByNodeAndTheirBracketingIndices
                   }
            optionWorkflow
                {
                    let! _
                         , _
                         , testVariableIndexToLevelsAndAdjustedIndexMap
                         , filtersGroupedByNodeAndTheirBracketingIndices =
                        walkTree false
                                 ((0
                                   , 0
                                   , Map.empty
                                   , List.Empty)
                                  |> Some)
                                 this

                    return if filtersGroupedByNodeAndTheirBracketingIndices.IsEmpty
                            then
                                fun _ ->
                                    true    // If there are no filters in the entire subtree headed by 'this',
                                            // then the resulting trivial case is to pass all possible inputs.
                            else
                                fun (testVariableIndexAndValuePairs: seq<Int32 * TestVariable<Int32>>) ->
                                    let nonSingletonAdjustedTestVariableIndexAndLevelPairs =
                                        seq
                                            {
                                                for testVariableIndexAndValue in testVariableIndexAndValuePairs do
                                                    match testVariableIndexAndValue with
                                                        testVariableIndex
                                                        , Level levelIndex ->
                                                            match Map.tryFind testVariableIndex
                                                                              testVariableIndexToLevelsAndAdjustedIndexMap with
                                                                Some (levels
                                                                      , adjustedTestVariableIndex) ->
                                                                    yield adjustedTestVariableIndex
                                                                          , (levelIndex
                                                                             , levels.[levelIndex])
                                                              | None ->
                                                                    ()
                                                      | _ ->
                                                            ()
                                            }
                                    let vectorOfAdjustedNonSingletonTestVariableIndexAndLevelPairs =
                                        C5.SortedArray<_>(Seq.length nonSingletonAdjustedTestVariableIndexAndLevelPairs,
                                                          comparisonForSlicing,
                                                          equalityForSlicing)
                                        :> IIndexedSorted<_>
                                    vectorOfAdjustedNonSingletonTestVariableIndexAndLevelPairs.AddSorted(nonSingletonAdjustedTestVariableIndexAndLevelPairs)
                                    let vectorIsAcceptedBy (filters
                                                            , adjustedIndexForLeftmostTestVariable
                                                            , onePastAdjustedIndexForRightmostTestVariable) =
                                        let sliceOfVector =
                                            vectorOfAdjustedNonSingletonTestVariableIndexAndLevelPairs.RangeFromTo((adjustedIndexForLeftmostTestVariable
                                                                                                                    , Unchecked.defaultof<_>),
                                                                                                                   (onePastAdjustedIndexForRightmostTestVariable
                                                                                                                    , Unchecked.defaultof<_>))
                                        if sliceOfVector.IsEmpty
                                        then
                                            true
                                        else
                                            let filterInput =
                                                sliceOfVector
                                                |> Seq.map (function adjustedTestVariableIndex
                                                                     , level ->
                                                                        adjustedTestVariableIndex - adjustedIndexForLeftmostTestVariable
                                                                        , level)
                                                |> Map.ofSeq
                                                :> IDictionary<_, _>
                                            filters
                                            |> List.forall (fun (filter: LevelCombinationFilter) ->
                                                                filter.Invoke filterInput)
                                    filtersGroupedByNodeAndTheirBracketingIndices
                                    |> List.forall vectorIsAcceptedBy
                }
            |> BargainBasement.Flip defaultArg
                                    (fun _ -> false)    // This picks up the case where there is at least one mad filter
                                                        // somewhere in the subtree headed by 'this'. No input could pass
                                                        // in this case.

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
                            let onePastIndexForRightmostTestVariable
                                , associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt =
                                walkTree subtreeRootNode
                                         indexForLeftmostTestVariable
                                         interleavingTestVariableIndicesFromTheLeftSiblings
                                         previousAssociationFromTestVariableIndexToVariablesThatAreInterleavedWithIt
                            let testVariableIndicesFromNode =
                                List.init (onePastIndexForRightmostTestVariable - indexForLeftmostTestVariable)
                                          (fun variableCount -> variableCount + indexForLeftmostTestVariable)
                            onePastIndexForRightmostTestVariable
                            , List.append testVariableIndicesFromNode interleavingTestVariableIndicesFromTheLeftSiblings
                            , associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt
                        let onePastIndexForRightmostTestVariable
                            , _
                            , associationFromTestVariableIndexToVariablesThatAreInterleavedWithIt =
                            subtreeRootNodes
                            |> Seq.fold mergeAssociationFromSubtree (indexForLeftmostTestVariable
                                                                     , interleavingTestVariableIndices
                                                                     , previousAssociationFromTestVariableIndexToVariablesThatAreInterleavedWithIt)
                        onePastIndexForRightmostTestVariable
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
                let maximumDesiredStrength =
                    match (node: Node).MaximumStrength with
                        Some maximumStrength ->
                            min maximumDesiredStrength
                                maximumStrength
                      | _ ->
                            maximumDesiredStrength
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
                                , onePastIndexForRightmostTestVariable
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
                            , onePastIndexForRightmostTestVariable
                           , associationFromTestVariableIndexToNumberOfItsLevels
                        subtreeRootNodes
                        |> List.fold mergeTestVariableCombinationsFromSubtree (Map.empty, indexForLeftmostTestVariable, [])

                  | SynthesizingNode fixedCombinationOfSubtreeNodesForSynthesis ->
                        let gatherTestVariableCombinationsFromSubtree (previousPerSubtreeAssociationsFromStrengthToTestVariableCombinations
                                                                       , indexForLeftmostTestVariable
                                                                       , previousAssociationFromTestVariableIndexToNumberOfItsLevels)
                                                                      subtreeRootNode =
                            let associationFromStrengthToTestVariableCombinationsFromSubtree
                                , onePastIndexForRightmostTestVariable
                                , associationFromTestVariableIndexToNumberOfItsLevelsFromSubtree =
                                walkTree subtreeRootNode maximumDesiredStrength indexForLeftmostTestVariable
                            let perSubtreeAssociationsFromStrengthToTestVariableCombinations =
                                associationFromStrengthToTestVariableCombinationsFromSubtree :: previousPerSubtreeAssociationsFromStrengthToTestVariableCombinations
                            let associationFromTestVariableIndexToNumberOfItsLevels =
                                List.append associationFromTestVariableIndexToNumberOfItsLevelsFromSubtree
                                            previousAssociationFromTestVariableIndexToNumberOfItsLevels
                            perSubtreeAssociationsFromStrengthToTestVariableCombinations
                            , onePastIndexForRightmostTestVariable
                            , associationFromTestVariableIndexToNumberOfItsLevels
                        let subtreeRootNodes =
                            fixedCombinationOfSubtreeNodesForSynthesis.Nodes
                        // Using 'fold' causes 'perSubtreeAssociationsFromStrengthToTestVariableCombinations' to be built up in
                        // reverse to the subtree sequence, and this reversal propagates consistently through the code below. The
                        // only way it could cause a problem would be due to the order of processing the subtrees, but because the
                        // combinations of the same strength from sibling subtrees are simply placed in a list and because the test
                        // variable indices are already correctly calculated, it doesn't matter.
                        let perSubtreeAssociationsFromStrengthToTestVariableCombinations
                            , maximumTestVariableIndex
                            , associationFromTestVariableIndexToNumberOfItsLevels =
                            subtreeRootNodes
                            |> Array.fold gatherTestVariableCombinationsFromSubtree ([], indexForLeftmostTestVariable, [])
                        let numberOfSubtrees =
                            Array.length subtreeRootNodes
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
                                                        let subtreeIsZeroCost =
                                                            fixedCombinationOfSubtreeNodesForSynthesis.IsSubtreeZeroCost indexOfSubtree
                                                            || subtreeRootNodes.[indexOfSubtree].IsZeroCost
                                                        let strength =
                                                            if subtreeIsZeroCost
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

        member this.FillOutPartialTestVectorRepresentation partialTestVectorRepresentation
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
            let combinationsOfTestVariablesAssociatedWithTheirLevels node
                                                                     testVariableIndices =
                let rec combinationsOfTestVariablesAssociatedWithTheirLevels node
                                                                             testVariableIndices
                                                                             indexForLeftmostTestVariable =
                    let chunksForRelevantNodes subtreeNodes =
                        let rec chunksForRelevantNodes subtreeNodes
                                                       testVariableIndices
                                                       indexForLeftmostTestVariable
                                                       chunks =
                            match subtreeNodes with
                                [] ->
                                    match testVariableIndices with
                                        [] ->
                                            chunks
                                      | _ ->
                                            raise (PreconditionViolationException "Encountered additional test variable indices to the right of the subtree nodes.")                                           
                              | headSubtreeNode :: tailSubtreeNodes ->
                                    let indexForLeftmostTestVariableInFollowingSubtreeToTheRight =
                                        (headSubtreeNode: Node).CountTestVariables
                                        + indexForLeftmostTestVariable
                                    let rec chunkForHeadSubtreeNodePlusThoseFromTail testVariableIndices
                                                                                     relevantTestVariableIndices =
                                        let addInChunk () =
                                            match relevantTestVariableIndices with
                                                [] ->
                                                    chunks
                                              | _ ->
                                                    (indexForLeftmostTestVariable
                                                     , headSubtreeNode
                                                     , (relevantTestVariableIndices
                                                        |> List.rev)) :: chunks
                                        match testVariableIndices with
                                            [] ->
                                                addInChunk ()
                                          | headTestVariableIndex :: tailTestVariableIndices ->
                                                if indexForLeftmostTestVariable > headTestVariableIndex
                                                then
                                                    raise (PreconditionViolationException "Encountered test variable index to the left of the subtree nodes, or one that was out of sorted order.")
                                                if indexForLeftmostTestVariableInFollowingSubtreeToTheRight > headTestVariableIndex
                                                then
                                                    chunkForHeadSubtreeNodePlusThoseFromTail tailTestVariableIndices
                                                                                             (headTestVariableIndex :: relevantTestVariableIndices)
                                                else
                                                    chunksForRelevantNodes tailSubtreeNodes
                                                                           testVariableIndices
                                                                           indexForLeftmostTestVariableInFollowingSubtreeToTheRight
                                                                           (addInChunk ())
                                    chunkForHeadSubtreeNodePlusThoseFromTail testVariableIndices
                                                                             List.empty
                        chunksForRelevantNodes subtreeNodes
                                               testVariableIndices
                                               indexForLeftmostTestVariable
                                               List.empty
                    match node with
                        TestVariableNode levels ->
                            match testVariableIndices with
                                [testVariableIndex] when indexForLeftmostTestVariable = testVariableIndex ->
                                    [[testVariableIndex
                                      , Seq.init levels.Length
                                                 Level]]
                              | _ ->
                                    raise (PreconditionViolationException "Test variable indices are inconsistent with the node: a test variable node expects a single test variable index taking the leftmost value for that subtree.")
                      | SingletonNode _ ->
                            match testVariableIndices with
                                [testVariableIndex] when indexForLeftmostTestVariable = testVariableIndex ->
                                    [[testVariableIndex
                                      , Seq.singleton SingletonPlaceholder]]
                              | _ ->
                                    raise (PreconditionViolationException "Test variable indices are inconsistent with the node: a singleton node expects a single test variable index taking the leftmost value for that subtree.")
                      | InterleavingNode subtreeRootNodes ->
                            let chunks =
                                chunksForRelevantNodes subtreeRootNodes
                            let chunkAndExcludedTestVariablesPairs =
                                chunks
                                |> List.map (fun chunk ->
                                                chunk
                                                , chunks
                                                  |> List.filter ((<>) chunk)
                                                  |> List.collect (fun (_
                                                                        , _
                                                                        , excludedTestVariables) ->
                                                                            excludedTestVariables))
                            chunkAndExcludedTestVariablesPairs
                            |> List.collect (function (indexForLeftmostTestVariableInSubtree
                                                       , subtreeRootNode
                                                       , relevantTestVariableIndices)
                                                      , excludedTestVariables ->
                                                        let exclusions =
                                                            excludedTestVariables
                                                            |> List.map (fun excludedTestVariable ->
                                                                            excludedTestVariable
                                                                            , Seq.singleton Exclusion)
                                                        let combinationsWithoutTheirExclusions =
                                                            combinationsOfTestVariablesAssociatedWithTheirLevels subtreeRootNode
                                                                                                                 relevantTestVariableIndices
                                                                                                                 indexForLeftmostTestVariableInSubtree
                                                        combinationsWithoutTheirExclusions
                                                        |> List.map (List.append exclusions))
                      | SynthesizingNode fixedCombinationOfSubtreeNodesForSynthesis ->
                            let subtreeRootNodes =
                                fixedCombinationOfSubtreeNodesForSynthesis.Nodes
                            let chunks =
                                chunksForRelevantNodes (subtreeRootNodes |> List.ofArray)
                            let perChunkCombinationsForEachChunk =
                                chunks
                                |> List.map (function indexForLeftmostTestVariableInSubtree
                                                      , subtreeRootNode
                                                      , relevantTestVariableIndices ->
                                                        combinationsOfTestVariablesAssociatedWithTheirLevels subtreeRootNode
                                                                                                             relevantTestVariableIndices
                                                                                                             indexForLeftmostTestVariableInSubtree)
                            let combinationsOfCombinationsTakenAcrossChunks =
                                perChunkCombinationsForEachChunk
                                |> List.DecorrelatedCrossProduct randomBehaviour
                            combinationsOfCombinationsTakenAcrossChunks
                            |> Seq.map List.concat
                            |> List.ofSeq
                if List.isEmpty testVariableIndices
                then
                    raise (PreconditionViolationException "Must have at least one test variable index.")
                if BargainBasement.IsSorted testVariableIndices
                   |> not
                then
                    raise (PreconditionViolationException "Test variable indices must be sorted in ascending order.")
                combinationsOfTestVariablesAssociatedWithTheirLevels node
                                                                     testVariableIndices
                                                                     0
            let fillOutFrom combinationOfTestVariablesAssociatedWithTheirLevels =
                let testVariableIndices =
                    combinationOfTestVariablesAssociatedWithTheirLevels
                    |> List.map fst
                let perTestVariableLevelsForEachTestVariable =
                    combinationOfTestVariablesAssociatedWithTheirLevels
                    |> List.map snd
                let levelCombinationsForTestVariableCombination =
                    perTestVariableLevelsForEachTestVariable
                    |> List.DecorrelatedCrossProduct randomBehaviour
                levelCombinationsForTestVariableCombination
                |> Seq.map (fun levelCombination ->
                                List.zip testVariableIndices
                                         levelCombination)
            if missingTestVariableIndices
               |> List.isEmpty
            then
                partialTestVectorRepresentation.Values
                |> Array.ofSeq
                |> Some
            else
                let fillerSections =
                    combinationsOfTestVariablesAssociatedWithTheirLevels this
                                                                         missingTestVariableIndices
                    |> List.map fillOutFrom
                    |> RoundRobinPickFrom
                let fullTestVectorRepresentations =
                    fillerSections
                    |> Seq.map (fun fillerSection ->
                                    BargainBasement.MergeDisjointSortedAssociationLists (fillerSection
                                                                                         |> List.sortBy fst)
                                                                                        (partialTestVectorRepresentation
                                                                                         |> Map.toList))
                optionWorkflow
                    {
                        let! chosenFullTestVectorRepresentation =
                            fullTestVectorRepresentations
                            |> Seq.tryFind (fun fullTestVectorRepresentation ->
                                                this.CombinedFilter (fullTestVectorRepresentation :> seq<_>))
                        return chosenFullTestVectorRepresentation
                               |> List.map snd
                               |> List.toArray
                    }

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
                                      combinePrunedSubtrees
                                      deferralBudget =
            let prunedSubtreeRootNodes =
                subtreeRootNodes
                |> List.map (fun (node: Node) ->
                                node.PruneTree deferralBudget)
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
                        member this.Prune deferralBudget =
                            Node.PruneAndCombine subtreeRootNodes
                                                 fixedCombinationOfSubtreeNodesForSynthesis
                                                 deferralBudget

                        member this.Nodes =
                            subtreeRootNodes
                            |> Array.ofList

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

                        member this.IsSubtreeZeroCost _ =
                            false

                        member this.IsSubtreeHiddenFromFilters _ =
                            false
                }
            fixedCombinationOfSubtreeNodesForSynthesis subtreeRootNodes
            |> SynthesizingNode