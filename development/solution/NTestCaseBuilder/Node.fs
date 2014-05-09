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

    type IFixedCombinationOfSubtreeRootNodesForSynthesis =
        abstract Prune: Int32 * Int32 -> List<Int32 * List<IFixedCombinationOfSubtreeRootNodesForSynthesis>>

        abstract SubtreeRootNodes: array<Node>

        abstract BuildSimilarFrom: List<Node> -> IFixedCombinationOfSubtreeRootNodesForSynthesis

        abstract FinalValueCreator: Unit -> (List<FullTestVector> -> 'CallerViewOfSynthesizedTestCase)

        abstract IsSubtreeZeroCost: Int32 -> Boolean

        abstract IsSubtreeHiddenFromFilters: Int32 -> Boolean

    and [<CustomEquality; NoComparison>]
        NodeKind =
        TestVariable of array<Object>
      | Singleton of Object
      | Interleaving of List<Node>
      | Synthesizing of IFixedCombinationOfSubtreeRootNodesForSynthesis
      | Deferral of (unit -> Node)

        override this.Equals (another: Object) =
            System.Runtime.CompilerServices.RuntimeHelpers.Equals(this, another)

        override this.GetHashCode () =
            System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(this)

    and Node =
        {
            Kind: NodeKind
            Filters: List<Filter>
            FiltersForTaggedInputs: List<FilterUsingTaggedInputs>
            MaximumStrength: Option<Int32>
            DeferralBudget: Option<Int32>
            IsZeroCost: Boolean
            Tag: Option<Object>
        }
        
        static member For kind =
            {
                Kind = kind
                Filters = List.empty
                FiltersForTaggedInputs = List.Empty
                MaximumStrength = None
                DeferralBudget = None
                IsZeroCost = false
                Tag = None
            }

        member this.WithMaximumStrength maximumStrength =
            {
                this with MaximumStrength = maximumStrength
            }

        member this.WithDeferralBudget deferralBudget =
            {
                this with DeferralBudget = deferralBudget
            }

        member this.WithZeroCost () =
            {
                this with IsZeroCost = true
            }

        member this.WithTag tag =
            {
                this with Tag = tag
            }

        member this.TakePropertiesFrom another =
            {
                this with
                    Filters = another.Filters
                    FiltersForTaggedInputs = another.FiltersForTaggedInputs
                    MaximumStrength = another.MaximumStrength
                    DeferralBudget = another.DeferralBudget
                    IsZeroCost = another.IsZeroCost
                    Tag = another.Tag                    
            }

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
              | Synthesizing fixedCombinationOfSubtreeRootNodesForSynthesis ->
                    Choice4Of5 fixedCombinationOfSubtreeRootNodesForSynthesis
              | Deferral deferredNode ->
                    Choice5Of5 deferredNode

        let inline TestVariableNode levels =
            Node.For (TestVariable levels)

        let inline SingletonNode singletonTestCase =
            Node.For (Singleton singletonTestCase)

        let inline InterleavingNode subtreeRootNodes =
            Node.For (Interleaving subtreeRootNodes)

        let inline SynthesizingNode fixedCombinationOfSubtreeRootNodesForSynthesis =
            Node.For (Synthesizing fixedCombinationOfSubtreeRootNodesForSynthesis)

        let inline DeferralNode deferredNode =
            Node.For (Deferral deferredNode)

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
                                              | SynthesizingNode fixedCombinationOfSubtreeRootNodesForSynthesis ->
                                                    fixedCombinationOfSubtreeRootNodesForSynthesis.SubtreeRootNodes
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

        let foldLeftPostOrder node
                              folding
                              accumulated =
            let rec walkTree accumulated
                             node =
                match node with
                    InterleavingNode subtreeRootNodes ->
                        let accumulatedFromSubtrees =
                            List.fold walkTree
                                      accumulated
                                      subtreeRootNodes
                        folding accumulatedFromSubtrees
                                node
                  | SynthesizingNode fixedCombinationOfSubtreeRootNodesForSynthesis ->
                        let accumulatedFromSubtrees =
                            Array.fold walkTree
                                       accumulated
                                       fixedCombinationOfSubtreeRootNodesForSynthesis.SubtreeRootNodes
                        folding accumulatedFromSubtrees
                                node
                                
                  | _ ->
                        folding accumulated
                                node
            walkTree accumulated
                     node

        let filterIsInsane (filter: Filter) =
            filter.Invoke Map.empty
            |> not

        let filterUsingTaggedInputsIsInsane node
                                            (filter: FilterUsingTaggedInputs) =
            filter.Invoke {
                            new ITaggedFilterInputs with
                                member this.FilterInputsForMatchingTags tagMatchPredicate =
                                    let taggedFilterInputsInReverseOrder =
                                        foldLeftPostOrder node
                                                          (fun taggedFilterInputsInReverseOrder
                                                               node ->
                                                            match node.Tag with
                                                                Some tag when tagMatchPredicate tag ->
                                                                    (tag
                                                                     , Map.empty :> IFilterInput)
                                                                    :: taggedFilterInputsInReverseOrder
                                                              | _ ->
                                                                    taggedFilterInputsInReverseOrder)
                                                          List.empty
                                    taggedFilterInputsInReverseOrder
                                    |> List.rev
                                    |> Array.ofList
                          }
            |> not

    open NodeDetail

    type Node with
        member this.WithFilter additionalFilter =
            {
                this with Filters = additionalFilter :: this.Filters
            }

        member this.WithFilter additionalFilter =
            {
                this with FiltersForTaggedInputs = additionalFilter :: this.FiltersForTaggedInputs
            }

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
                      | SynthesizingNode fixedCombinationOfSubtreeRootNodesForSynthesis ->
                            fixedCombinationOfSubtreeRootNodesForSynthesis.SubtreeRootNodes
                            |> Array.map walkTree
                            |> Array.reduce (+)
                match node.MaximumStrength with
                    Some maximumStrength ->
                        min maximumPossibleStrength
                            maximumStrength
                  | _ ->
                        maximumPossibleStrength
            walkTree this

        member this.DeferralBudgetsOverSubtree () =
            foldLeftPostOrder this
                              (fun (nodeIndex
                                    , nodeIndexToDeferralBudgetMap)
                                   node ->
                                1 + nodeIndex
                                , match node.DeferralBudget with
                                    Some deferralBudget ->
                                        Map.add nodeIndex
                                                deferralBudget
                                                nodeIndexToDeferralBudgetMap
                                  | None ->
                                        nodeIndexToDeferralBudgetMap)
                              (0
                               , Map.empty)
            |> snd

        member this.ApplyDeferralBudgetsOverSubtree nodeIndexToDeferralBudgetMap =
            foldLeftPostOrder this
                              (fun (nodeIndex
                                    , stackOfNodesWithBudgetAppliedIfRequired: List<_>)
                                   node ->
                                let applyDeferralBudget (node: Node) =
                                    match Map.tryFind nodeIndex
                                                      nodeIndexToDeferralBudgetMap with
                                        Some deferralBudget ->
                                            node.WithDeferralBudget (Some deferralBudget)
                                      | None ->
                                            node.WithDeferralBudget None
                                1 + nodeIndex
                                , match node with
                                    InterleavingNode subtreeRootNodes ->
                                        let numberOfSubtreeRootNodes =
                                            subtreeRootNodes.Length
                                        let subtreeRootNodesWithBudgetAppliedIfRequiredInReverseOrder
                                            , remainingNodesWithBudgetAppliedIfRequired =
                                            stackOfNodesWithBudgetAppliedIfRequired.BreakOff numberOfSubtreeRootNodes
                                        ((InterleavingNode (subtreeRootNodesWithBudgetAppliedIfRequiredInReverseOrder
                                                            |> List.rev)).TakePropertiesFrom node
                                         |> applyDeferralBudget)
                                        :: remainingNodesWithBudgetAppliedIfRequired
                                  | SynthesizingNode fixedCombinationOfSubtreeRootNodesForSynthesis ->
                                        let numberOfSubtreeRootNodes =
                                            fixedCombinationOfSubtreeRootNodesForSynthesis.SubtreeRootNodes.Length
                                        let subtreeRootNodesWithBudgetAppliedIfRequiredInReverseOrder
                                            , remainingNodesWithBudgetAppliedIfRequired =
                                            stackOfNodesWithBudgetAppliedIfRequired.BreakOff numberOfSubtreeRootNodes
                                        ((SynthesizingNode (fixedCombinationOfSubtreeRootNodesForSynthesis.BuildSimilarFrom (subtreeRootNodesWithBudgetAppliedIfRequiredInReverseOrder
                                                                                                                             |> List.rev))).TakePropertiesFrom node
                                         |> applyDeferralBudget)
                                        :: remainingNodesWithBudgetAppliedIfRequired
                                  | _ ->
                                    (node
                                     |> applyDeferralBudget)
                                    :: stackOfNodesWithBudgetAppliedIfRequired)
                              (0
                               , List.Empty)
            |> snd
            |> List.head

        member this.PruneTree () =
            this.PruneTree (defaultArg this.DeferralBudget
                                       0
                            , 0)

        member internal this.PruneTree (desiredDeferralBudget,
                                        numberOfDeferralsSpent) =
            let rec walkTree node =
                let desiredDeferralBudget =
                    match (node: Node).DeferralBudget with
                        Some deferralBudget ->
                            min desiredDeferralBudget
                                deferralBudget
                      | _ ->
                            desiredDeferralBudget
                match node with
                    TestVariableNode levels ->
                        if Array.isEmpty levels
                        then
                            List.empty
                        else
                            [(numberOfDeferralsSpent
                             , node)]
                  | SingletonNode _ as node ->
                        [(numberOfDeferralsSpent
                          , node)]
                  | InterleavingNode subtreeRootNodes ->
                        let associationListFromDeferralBudgetToPrunedSubtreeRootNodes =
                            subtreeRootNodes
                            |> List.map walkTree
                            |> BargainBasement.CollectAcrossSortedAssociationLists
                        associationListFromDeferralBudgetToPrunedSubtreeRootNodes
                        |> List.map (fun (deferralBudget
                                          , prunedSubtreeRootNodes) ->
                                        deferralBudget
                                        , (InterleavingNode prunedSubtreeRootNodes).TakePropertiesFrom node)
                  | SynthesizingNode fixedCombinationOfSubtreeRootNodesForSynthesis ->
                        fixedCombinationOfSubtreeRootNodesForSynthesis.Prune (desiredDeferralBudget,
                                                                              numberOfDeferralsSpent)
                        |> List.map (fun (deferralBudget
                                          , fixedCombinationsOfSubtreeNodesForSynthesisConformingToThatBudget) ->
                                        let alternateSynthesesConformingToThatBudget =
                                            fixedCombinationsOfSubtreeNodesForSynthesisConformingToThatBudget
                                            |> List.map (fun fixedCombinationOfSubtreeRootNodesForSynthesis ->
                                                            (SynthesizingNode fixedCombinationOfSubtreeRootNodesForSynthesis).TakePropertiesFrom node)
                                        deferralBudget
                                        , match alternateSynthesesConformingToThatBudget with
                                            [uniqueSynthesis] ->
                                                uniqueSynthesis
                                          | _ ->
                                            InterleavingNode alternateSynthesesConformingToThatBudget)
                  | DeferralNode deferredNode ->
                        if numberOfDeferralsSpent = desiredDeferralBudget
                        then
                            List.empty
                        else
                            (deferredNode ()).PruneTree (desiredDeferralBudget,
                                                         (1 + numberOfDeferralsSpent))
                            |> List.map (fun (deferralBudget
                                              , deferredSubtree) ->
                                            let standInForDeferralNode =
                                                InterleavingNode [deferredSubtree]
                                            deferralBudget
                                            , standInForDeferralNode.TakePropertiesFrom node)
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

            let testVariableIndexToLevelsAndAdjustedIndexMapWithNodeContributions trampData
                                                                                  addContributionFrom
                                                                                  node =
                let rec walkTree everythingInTreeIsHiddenFromFilters
                                 trampData
                                 node =
                    let indexForLeftmostTestVariable
                        , adjustedIndexForLeftmostTestVariable
                        , testVariableIndexToLevelsAndAdjustedIndexMap
                        , contributionsFromNodes =
                        trampData
                    if everythingInTreeIsHiddenFromFilters
                    then
                        (node: Node).CountTestVariables + indexForLeftmostTestVariable
                        , adjustedIndexForLeftmostTestVariable
                        , testVariableIndexToLevelsAndAdjustedIndexMap
                        , contributionsFromNodes
                    else
                        let onePastIndexForRightmostTestVariable
                            , onePastAdjustedIndexForRightmostTestVariable
                            , testVariableIndexToLevelsAndAdjustedIndexMap
                            , contributionsFromNodes =
                            match node with
                                TestVariableNode levels ->
                                    (1 + indexForLeftmostTestVariable
                                     , 1 + adjustedIndexForLeftmostTestVariable
                                     , Map.add indexForLeftmostTestVariable
                                               (levels
                                                , adjustedIndexForLeftmostTestVariable)
                                                testVariableIndexToLevelsAndAdjustedIndexMap
                                     , contributionsFromNodes)
                              | SingletonNode thing ->
                                    (1 + indexForLeftmostTestVariable
                                     , 1 + adjustedIndexForLeftmostTestVariable
                                     , testVariableIndexToLevelsAndAdjustedIndexMap
                                     , contributionsFromNodes)
                              | InterleavingNode subtreeRootNodes ->
                                    List.fold (walkTree false)
                                               trampData
                                               subtreeRootNodes
                              | SynthesizingNode fixedCombinationOfSubtreeRootNodesForSynthesis ->
                                    let walkTree trampData
                                                 (everythingInTreeIsHiddenFromFilters
                                                  , node) =
                                        walkTree everythingInTreeIsHiddenFromFilters
                                                 trampData
                                                 node
                                    let subtreeRootNodes =
                                        fixedCombinationOfSubtreeRootNodesForSynthesis.SubtreeRootNodes
                                    Array.fold walkTree
                                               trampData
                                               (subtreeRootNodes
                                                |> Array.mapi (fun subtreeIndex
                                                                subtreeRootNode ->
                                                                    fixedCombinationOfSubtreeRootNodesForSynthesis.IsSubtreeHiddenFromFilters subtreeIndex
                                                                    , subtreeRootNode))
                        let contributionFromNode =
                            addContributionFrom node
                                                adjustedIndexForLeftmostTestVariable
                                                onePastAdjustedIndexForRightmostTestVariable
                                                contributionsFromNodes
                        onePastIndexForRightmostTestVariable
                        , onePastAdjustedIndexForRightmostTestVariable
                        , testVariableIndexToLevelsAndAdjustedIndexMap
                        , contributionFromNode
                walkTree false
                         trampData
                         node
            let addNodesWithFiltersFrom node
                                        adjustedIndexForLeftmostTestVariable
                                        onePastAdjustedIndexForRightmostTestVariable
                                        (nodesWithFiltersAndTheirBracketingIndices
                                         , nodesWithFiltersForTaggedInputsAndTheirBracketingIndices) =
                let nodesWithFiltersAndTheirBracketingIndices =
                    if node.Filters.IsEmpty
                    then
                        nodesWithFiltersAndTheirBracketingIndices
                    else
                        (node
                         , adjustedIndexForLeftmostTestVariable
                         , onePastAdjustedIndexForRightmostTestVariable) :: nodesWithFiltersAndTheirBracketingIndices
                let nodesWithFiltersForTaggedInputsAndTheirBracketingIndices =
                    if node.FiltersForTaggedInputs.IsEmpty
                    then
                        nodesWithFiltersForTaggedInputsAndTheirBracketingIndices
                    else
                        (node
                         , adjustedIndexForLeftmostTestVariable
                         , onePastAdjustedIndexForRightmostTestVariable) :: nodesWithFiltersForTaggedInputsAndTheirBracketingIndices
                nodesWithFiltersAndTheirBracketingIndices
                , nodesWithFiltersForTaggedInputsAndTheirBracketingIndices
            let _
                , _
                , testVariableIndexToLevelsAndAdjustedIndexMap
                , (nodesWithFiltersAndTheirBracketingIndices
                   , nodesWithFiltersForTaggedInputsAndTheirBracketingIndices) =
                testVariableIndexToLevelsAndAdjustedIndexMapWithNodeContributions (0
                                                                                   , 0
                                                                                   , Map.empty
                                                                                   , (List.Empty
                                                                                      , List.Empty))
                                                                                  addNodesWithFiltersFrom
                                                                                  this
            if nodesWithFiltersAndTheirBracketingIndices.IsEmpty
               && nodesWithFiltersForTaggedInputsAndTheirBracketingIndices.IsEmpty
            then
                fun _ ->
                    true    // If there are no filters in the entire subtree headed by 'this',
                            // then the resulting trivial case is to pass all possible inputs.
            else
                fun (testVariableIndexAndValuePairs: Map<Int32, TestVariable<Int32>>) ->
                    let nonSingletonAdjustedTestVariableIndexAndLevelPairs =
                        seq
                            {
                                for testVariableIndexAndValue in testVariableIndexAndValuePairs do
                                    match testVariableIndexAndValue with
                                        KeyValue(testVariableIndex
                                                 , Level levelIndex) ->
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
                    let buildFilterInput adjustedIndexForLeftmostTestVariable
                                         onePastAdjustedIndexForRightmostTestVariable =
                        let sliceOfVector =
                            vectorOfAdjustedNonSingletonTestVariableIndexAndLevelPairs.RangeFromTo((adjustedIndexForLeftmostTestVariable
                                                                                                    , Unchecked.defaultof<_>),
                                                                                                   (onePastAdjustedIndexForRightmostTestVariable
                                                                                                    , Unchecked.defaultof<_>))
                        sliceOfVector
                        |> Seq.map (function adjustedTestVariableIndex
                                             , level ->
                                                adjustedTestVariableIndex - adjustedIndexForLeftmostTestVariable
                                                , level)
                        |> Map.ofSeq
                        :> IFilterInput
                    let vectorIsAcceptedByFilters (nodeWithFilters
                                                   , adjustedIndexForLeftmostTestVariable
                                                   , onePastAdjustedIndexForRightmostTestVariable) =
                        let filters =
                            nodeWithFilters.Filters
                        if filters
                           |> List.exists filterIsInsane
                        then
                            raise (PreconditionViolationException "Insane filter detected.")
                        let filterInput =
                            buildFilterInput adjustedIndexForLeftmostTestVariable
                                             onePastAdjustedIndexForRightmostTestVariable
                        filters
                        |> List.forall (fun filter ->
                                            filter.Invoke filterInput)
                    let vectorIsAcceptedByFiltersForTaggedInputs (nodeWithFiltersForTaggedInputs
                                                                  , adjustedIndexForLeftmostTestVariable
                                                                  , onePastAdjustedIndexForRightmostTestVariable) =
                        let filtersForTaggedInputs =
                            nodeWithFiltersForTaggedInputs.FiltersForTaggedInputs
                        if filtersForTaggedInputs
                           |> List.exists (filterUsingTaggedInputsIsInsane nodeWithFiltersForTaggedInputs)
                        then
                            raise (PreconditionViolationException "Insane filter using tagged inputs detected.")
                        let taggedFilterInputs =
                            {
                                new ITaggedFilterInputs with
                                    member this.FilterInputsForMatchingTags tagMatchPredicate =
                                        let addTaggedFilterInput node
                                                                 adjustedIndexForLeftmostTestVariable
                                                                 onePastAdjustedIndexForRightmostTestVariable
                                                                 taggedFilterInputsInReverseOrder =
                                            match node.Tag with
                                                Some tag when tagMatchPredicate tag ->
                                                    let filterInputForTag =
                                                        buildFilterInput adjustedIndexForLeftmostTestVariable
                                                                         onePastAdjustedIndexForRightmostTestVariable
                                                    (tag
                                                     , filterInputForTag)
                                                    :: taggedFilterInputsInReverseOrder
                                              | _ ->
                                                    taggedFilterInputsInReverseOrder
                                        let _
                                            , shouldBeOnePastAdjustedIndexForRightmostTestVariable
                                            , _
                                            , taggedFilterInputsInReverseOrder =
                                            testVariableIndexToLevelsAndAdjustedIndexMapWithNodeContributions (0
                                                                                                               , adjustedIndexForLeftmostTestVariable   // NOTE: have to bias this as we are starting
                                                                                                                                                        // from a node within a subtree of 'this'.
                                                                                                               , Map.empty
                                                                                                               , List.empty)
                                                                                                              addTaggedFilterInput
                                                                                                              nodeWithFiltersForTaggedInputs
                                        if shouldBeOnePastAdjustedIndexForRightmostTestVariable <> onePastAdjustedIndexForRightmostTestVariable
                                        then
                                            raise (InternalAssertionViolationException "Walking the subtree of the filters' node has yielded adjusted indices that are not consistent with the tree walk performed on 'this'.")
                                        taggedFilterInputsInReverseOrder
                                        |> List.rev
                                        |> Array.ofList
                            }
                        filtersForTaggedInputs
                        |> List.forall (fun filterForTaggedInputs ->
                                            filterForTaggedInputs.Invoke taggedFilterInputs)
                    (nodesWithFiltersAndTheirBracketingIndices
                     |> List.forall vectorIsAcceptedByFilters)
                    && (nodesWithFiltersForTaggedInputsAndTheirBracketingIndices
                        |> List.forall vectorIsAcceptedByFiltersForTaggedInputs)

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
                  | SynthesizingNode fixedCombinationOfSubtreeRootNodesForSynthesis ->
                        let mergeAssociationFromSubtree (indexForLeftmostTestVariable
                                                         , previouslyMergedAssociationList)
                                                        subtreeRootNode =
                            walkTree subtreeRootNode indexForLeftmostTestVariable interleavingTestVariableIndices previouslyMergedAssociationList
                        fixedCombinationOfSubtreeRootNodesForSynthesis.SubtreeRootNodes
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
                                BargainBasement.MergeMaps interleaveTestVariableCombinations
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

                  | SynthesizingNode fixedCombinationOfSubtreeRootNodesForSynthesis ->
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
                            fixedCombinationOfSubtreeRootNodesForSynthesis.SubtreeRootNodes
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
                                                            fixedCombinationOfSubtreeRootNodesForSynthesis.IsSubtreeZeroCost indexOfSubtree
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
                      | SynthesizingNode fixedCombinationOfSubtreeRootNodesForSynthesis ->
                            let subtreeRootNodes =
                                fixedCombinationOfSubtreeRootNodesForSynthesis.SubtreeRootNodes
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
            let combinedFilter =
                Map.ofList >> this.CombinedFilter
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
                                         levelCombination
                                |> List.sortBy fst)
                |> Seq.filter (fun fillerSection ->
                                combinedFilter fillerSection)
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
                                    BargainBasement.MergeDisjointSortedAssociationLists fillerSection
                                                                                        (partialTestVectorRepresentation
                                                                                         |> Map.toList))
                optionWorkflow
                    {
                        let! chosenFullTestVectorRepresentation =
                            fullTestVectorRepresentations
                            |> Seq.tryFind (fun fullTestVectorRepresentation ->
                                                combinedFilter fullTestVectorRepresentation)
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
              | SynthesizingNode fixedCombinationOfSubtreeRootNodesForSynthesis ->
                    let subtreeRootNodes =
                        fixedCombinationOfSubtreeRootNodesForSynthesis.SubtreeRootNodes
                    let indicesInVectorForLeftmostTestVariableInEachSubtree =
                        indicesInVectorForLeftmostTestVariableInEachSubtree subtreeRootNodes
                    let numberOfSubtrees =
                        Array.length subtreeRootNodes
                    let sliceRangesOverFullTestVector =
                        Seq.pairwise indicesInVectorForLeftmostTestVariableInEachSubtree
                        |> List.ofSeq
                    let finalValueCreator =
                        fixedCombinationOfSubtreeRootNodesForSynthesis.FinalValueCreator ()
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
                                      deferralBudget
                                      numberOfDeferralsSpent =
            if subtreeRootNodes
               |> List.isEmpty
            then
                List.empty
            else
                let associationListFromDeferralBudgetToGroupOfAlternateListsOfSubtreesWhoseSynthesisConformsToTheBudget =
                    // Think that just about says it all. Might need a bit more descriptive name.
                    let maximumNumberOfDeferralsSpent =
                        List.maxBy fst
                        >> fst
                    subtreeRootNodes
                    |> List.map (fun (node: Node) ->
                                    node.PruneTree (deferralBudget,
                                                    numberOfDeferralsSpent))
                    |> List.CrossProduct
                    |> Seq.groupBy maximumNumberOfDeferralsSpent
                    |> Seq.map (fun (maximumNumberOfDeferralsSpent
                                     , crossProductTerms) ->
                                    maximumNumberOfDeferralsSpent
                                    , crossProductTerms
                                      |> Seq.map (List.map snd))
                [
                    for maximumNumberOfDeferralsSpent
                        , groupOfAlternateListsOfPrunedSubtreeRootNodes in associationListFromDeferralBudgetToGroupOfAlternateListsOfSubtreesWhoseSynthesisConformsToTheBudget do
                        let alternateSynthesesConformingToBudget =
                            [
                                for prunedSubtreeRootNodes in groupOfAlternateListsOfPrunedSubtreeRootNodes do
                                    if Seq.length prunedSubtreeRootNodes
                                       = Seq.length subtreeRootNodes
                                    then
                                        yield prunedSubtreeRootNodes
                                              |> combinePrunedSubtrees
                            ]
                        yield maximumNumberOfDeferralsSpent
                              , alternateSynthesesConformingToBudget
                ]
        static member CreateSynthesizingNode subtreeRootNodes
                                             synthesisDelegate =
            let rec fixedCombinationOfSubtreeRootNodesForSynthesis subtreeRootNodes =
                {
                    new IFixedCombinationOfSubtreeRootNodesForSynthesis with
                        member this.Prune (deferralBudget,
                                           numberOfDeferralsSpent) =
                            Node.PruneAndCombine subtreeRootNodes
                                                 fixedCombinationOfSubtreeRootNodesForSynthesis
                                                 deferralBudget
                                                 numberOfDeferralsSpent

                        member this.SubtreeRootNodes =
                            subtreeRootNodes
                            |> Array.ofList

                        member this.BuildSimilarFrom subtreeRootNodes =
                            fixedCombinationOfSubtreeRootNodesForSynthesis subtreeRootNodes

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
            fixedCombinationOfSubtreeRootNodesForSynthesis subtreeRootNodes
            |> SynthesizingNode