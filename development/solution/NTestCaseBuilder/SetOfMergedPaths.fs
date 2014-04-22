#nowarn "40"

namespace NTestCaseBuilder

    open System.Collections
    open System.Collections.Generic
    open System
    open SageSerpent.Infrastructure
    open SageSerpent.Infrastructure.OptionWorkflow
    open SageSerpent.Infrastructure.OptionExtensions
    open SageSerpent.Infrastructure.RandomExtensions
    open SageSerpent.Infrastructure.ListExtensions
    open SageSerpent.Infrastructure.ContinuationWorkflow
    open SageSerpent.Infrastructure.ChunkedListExtensions
    open Microsoft.FSharp.Collections

    module SetOfMergedPathsDetail =
        type InternalNode<'Step when 'Step: comparison> =
            {
                LevelForTestVariableIndex: 'Step
                SubtreeWithLesserLevelsForSameTestVariableIndex: BinaryTreeOfLevelsForTestVariable<'Step>
                SubtreeWithGreaterLevelsForSameTestVariableIndex: BinaryTreeOfLevelsForTestVariable<'Step>
                PathsForFollowingIndices: Paths<'Step>
            }
        and BinaryTreeOfLevelsForTestVariable<'Step when 'Step: comparison> =
            UnsuccessfulSearchTerminationNode
          | InternalNode of InternalNode<'Step>
        and WildcardNode<'Step when 'Step: comparison> =
            {
                SubtreeWithAllLevelsForSameTestVariableIndex: BinaryTreeOfLevelsForTestVariable<'Step>
                PathsForFollowingIndices: Paths<'Step>
            }
        and Paths<'Step when 'Step: comparison> =
            {
                SharedPathPrefix: ChunkedList<Option<'Step>>
                BranchingRoot: TernarySearchTree<'Step>
            }
        and TernarySearchTree<'Step when 'Step: comparison> =
            SuccessfulSearchTerminationNode
          | WildcardNode of WildcardNode<'Step>
          | BinaryTreeOfLevelsForTestVariable of BinaryTreeOfLevelsForTestVariable<'Step>

        let inline (|EmptyTernarySearchTree|_|) ternarySearchTree =
            match ternarySearchTree with
                BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode ->
                    Some ()
              | _ ->
                    None

        let EmptyTernarySearchTree =
            BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode

        let inline (|NoPaths|_|) paths =
            match paths with
                {
                    SharedPathPrefix = Nil
                    BranchingRoot = EmptyTernarySearchTree
                } ->
                    Some ()
              | _ ->
                    None

        let NoPaths =
            {
                SharedPathPrefix = Nil
                BranchingRoot = EmptyTernarySearchTree
            }

        let inline (|SingleTrivialPath|_|) paths =
            match paths with
                {
                    SharedPathPrefix = Nil
                    BranchingRoot = SuccessfulSearchTerminationNode
                } ->
                    Some ()
              | _ ->
                    None

        let SingleTrivialPath =
            {
                SharedPathPrefix = Nil
                BranchingRoot = SuccessfulSearchTerminationNode
            }

        let inline (|BranchingWithSingleLevelForLeadingTestVariable|_|) ternarySearchTree =
            match ternarySearchTree with
                BinaryTreeOfLevelsForTestVariable
                (InternalNode
                {
                    LevelForTestVariableIndex = stepForTestVariableIndex
                    SubtreeWithLesserLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                    SubtreeWithGreaterLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                    PathsForFollowingIndices = pathsForFollowingIndices
                }) ->
                    {
                        pathsForFollowingIndices with
                            SharedPathPrefix =
                                Cons (Some stepForTestVariableIndex
                                      , pathsForFollowingIndices.SharedPathPrefix)
                    }
                    |> Some
              | _ ->
                    None

        let inline (|BranchingWithJustWildcardForLeadingTestVariable|_|) ternarySearchTree =
            match ternarySearchTree with
                WildcardNode
                {
                    SubtreeWithAllLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                    PathsForFollowingIndices = pathsForFollowingIndices
                } ->
                    {
                        pathsForFollowingIndices with
                            SharedPathPrefix =
                                Cons (None
                                      , pathsForFollowingIndices.SharedPathPrefix)
                    }
                    |> Some
              | _ ->
                    None

        let inline mirrorInternalNode mirroring
                                      internalNodeRepresentation =
            if mirroring
            then
                match internalNodeRepresentation with
                    {
                        SubtreeWithLesserLevelsForSameTestVariableIndex = rootSubtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = rootSubtreeWithGreaterLevelsForSameTestVariableIndex
                    } ->
                        {
                            internalNodeRepresentation with
                                SubtreeWithLesserLevelsForSameTestVariableIndex = rootSubtreeWithGreaterLevelsForSameTestVariableIndex
                                SubtreeWithGreaterLevelsForSameTestVariableIndex = rootSubtreeWithLesserLevelsForSameTestVariableIndex
                        }
            else
                internalNodeRepresentation

        let inline (|MirroredInternalNode|_|) mirroring
                                              internalNode =
            match internalNode with
                InternalNode internalNodeRepresentation ->
                    internalNodeRepresentation
                    |> mirrorInternalNode mirroring
                    |> Some
              | _ ->
                    None

        let inline MirroredInternalNode mirroring
                                        internalNodeRepresentation =
            mirrorInternalNode mirroring
                               internalNodeRepresentation
            |> InternalNode

        type InternalNode<'Step when 'Step: comparison> with
            member this.NumberOfLevelsForLeadingTestVariable =
                match !this.CacheOfNumberOfLevelsForLeadingTestVariable with
                    None ->
                        let numberOfLevelsForLeadingTestVariable =
                            match this with
                                {
                                    SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                                    SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                                    PathsForFollowingIndices
                                        = NoPaths
                                } ->
                                    subtreeWithLesserLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                                    + subtreeWithGreaterLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                              | {
                                    SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                                    SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                                } ->
                                    1
                                    + subtreeWithLesserLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                                    + subtreeWithGreaterLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                        this.CacheOfNumberOfLevelsForLeadingTestVariable :=
                            Some numberOfLevelsForLeadingTestVariable
                        numberOfLevelsForLeadingTestVariable
                  | Some numberOfLevelsForLeadingTestVariable ->
                        numberOfLevelsForLeadingTestVariable

            // TODO: this is horrible - is this really giving any appreciable performance benefit?
            member private this.CacheOfNumberOfLevelsForLeadingTestVariable =
                ref None

        and BinaryTreeOfLevelsForTestVariable<'Step when 'Step: comparison> with
            member this.NumberOfLevelsForLeadingTestVariable =
                match this with
                    InternalNode internalNode ->
                        internalNode.NumberOfLevelsForLeadingTestVariable
                  | UnsuccessfulSearchTerminationNode ->
                        0

        type TreeSearchContextParameters =
            {
                TestVariableIndex: Int32
                HasSuffixContextOfPossibleCompletePath: Boolean
            }

            static member StartOfSearch =
                {
                    TestVariableIndex = 0
                    HasSuffixContextOfPossibleCompletePath = true
                }

            member this.IsSpecialCaseDenotingInitialState =   // The tests define this special state as not possessing any paths, not even the trivial empty one.
                0 = this.TestVariableIndex

            member this.IsCompletePath maximumNumberOfTestVariables =
                this.HasSuffixContextOfPossibleCompletePath
                && maximumNumberOfTestVariables = this.TestVariableIndex

            member this.PropagateFromDefinedLevelToNextTestVariable =
                {
                    this with
                        TestVariableIndex = this.TestVariableIndex + 1
                }

            member this.PropagateFromWildcardLevelToNextTestVariable =
                {
                    this with
                        TestVariableIndex = this.TestVariableIndex + 1
                        HasSuffixContextOfPossibleCompletePath = false
                }

        type SharedPathPrefixMatchResult<'Step> =
            AgreesWithPrefixOfIncompletePath of List<Option<'Step>> * List<Option<'Step>>   // The prefix of an incomplete path that agrees
                                                                                            // and the remainder of the incomplete path.
          | ShortenedPrefixAgreesWithEntireIncompletePath of Int32 * List<Option<'Step>>    // Length of an incomplete path and
                                                                                            // the incomplete path itself.
          | CompleteMismatch of Int32 * List<Option<'Step>> * List<Option<'Step>>   // Index of mismatch in the shared path prefix,
                                                                                    // the prefix of an incomplete path that agrees
                                                                                    // and the remainder of the incomplete path
                                                                                    // including the mismatching step at the head.

        let splayInternalNodeWithMatchingOrNeighbouringLevel internalNodeRepresentation
                                                             comparisonWrtImplicitLevel =
            let mirroredComparisonWrtImplicitLevel =
                comparisonWrtImplicitLevel
                >> (~-)
            let rec accumulateFlankingSubtrees ({
                                                    LevelForTestVariableIndex = rootLevelForTestVariableIndex
                                                    SubtreeWithLesserLevelsForSameTestVariableIndex = rootSubtreeWithLesserLevelsForSameTestVariableIndex
                                                    SubtreeWithGreaterLevelsForSameTestVariableIndex = rootSubtreeWithGreaterLevelsForSameTestVariableIndex
                                                } as internalNodeRepresentationForRoot)
                                               addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                                               addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels
                                               interchangeRolesOfFlankingSubtreesOnBehalfOfCaller =
                let addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                    , addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels =
                    if interchangeRolesOfFlankingSubtreesOnBehalfOfCaller
                    then
                        addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels
                        , addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                    else
                        addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                        , addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels
                let inline splayAtRootNode () =
                    {
                        internalNodeRepresentationForRoot with
                            SubtreeWithLesserLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                            SubtreeWithGreaterLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                    }
                    , addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels rootSubtreeWithLesserLevelsForSameTestVariableIndex
                    , addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels rootSubtreeWithGreaterLevelsForSameTestVariableIndex
                match comparisonWrtImplicitLevel rootLevelForTestVariableIndex with
                    0 ->
                        // Degenerate root node only case (step has been found)...
                        splayAtRootNode ()
                  | rootResult ->
                        // NOTE: comments for this section refer to the 'unmirrored' case. So in the mirrored case,
                        // 'zig-zag' becomes 'zag-zig', 'least upper bound' becomes 'greatest lower bound', etc.
                        let localMirroring
                            , comparisonWrtImplicitLevel
                            , internalNodeRepresentationForRoot
                            , rootSubtreeWithLesserLevelsForSameTestVariableIndex
                            , rootSubtreeWithGreaterLevelsForSameTestVariableIndex
                            , addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                            , addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels =
                            if rootResult > 0
                            then
                                true
                                , mirroredComparisonWrtImplicitLevel
                                , mirrorInternalNode true
                                                     internalNodeRepresentationForRoot
                                , rootSubtreeWithGreaterLevelsForSameTestVariableIndex
                                , rootSubtreeWithLesserLevelsForSameTestVariableIndex
                                , addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels
                                , addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                            else
                                false
                                , comparisonWrtImplicitLevel
                                , internalNodeRepresentationForRoot
                                , rootSubtreeWithLesserLevelsForSameTestVariableIndex
                                , rootSubtreeWithGreaterLevelsForSameTestVariableIndex
                                , addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                                , addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels
                        match rootSubtreeWithLesserLevelsForSameTestVariableIndex with
                            MirroredInternalNode localMirroring
                            ({
                                LevelForTestVariableIndex = zigLevelForTestVariableIndex
                                SubtreeWithLesserLevelsForSameTestVariableIndex = zigSubtreeWithLesserLevelsForSameTestVariableIndex
                                SubtreeWithGreaterLevelsForSameTestVariableIndex = zigSubtreeWithGreaterLevelsForSameTestVariableIndex
                            } as internalNodeRepresentationForZig) ->
                                match comparisonWrtImplicitLevel zigLevelForTestVariableIndex
                                      , zigSubtreeWithLesserLevelsForSameTestVariableIndex
                                      , zigSubtreeWithGreaterLevelsForSameTestVariableIndex with
                                    zigResult
                                    , InternalNode internalNodeRepresentationforZigZig
                                    , _ when zigResult < 0 ->
                                        // Zig-zig case...
                                        let addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels =
                                            (fun nodeWithLeastLevelToBeAddedToFlankingSubtreeWithGreaterLevels ->
                                                addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels
                                                    ({
                                                        internalNodeRepresentationForZig with
                                                            SubtreeWithLesserLevelsForSameTestVariableIndex = nodeWithLeastLevelToBeAddedToFlankingSubtreeWithGreaterLevels
                                                            SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                                                {
                                                                    internalNodeRepresentationForRoot with
                                                                        SubtreeWithLesserLevelsForSameTestVariableIndex = zigSubtreeWithGreaterLevelsForSameTestVariableIndex
                                                                }
                                                                |> MirroredInternalNode localMirroring

                                                    }
                                                    |> MirroredInternalNode localMirroring))
                                        accumulateFlankingSubtrees
                                            internalNodeRepresentationforZigZig
                                            addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                                            addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels
                                            localMirroring
                                  | zigResult
                                    , _
                                    , InternalNode internalNodeRepresentationforZigZag when zigResult > 0 ->
                                        // Zig-zag case...
                                        let addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels =
                                            (fun nodeWithLeastLevelToBeAddedToFlankingSubtreeWithGreaterLevels ->
                                                addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels
                                                    ({
                                                        internalNodeRepresentationForRoot with
                                                            SubtreeWithLesserLevelsForSameTestVariableIndex = nodeWithLeastLevelToBeAddedToFlankingSubtreeWithGreaterLevels
                                                    }
                                                    |> MirroredInternalNode localMirroring))
                                        let addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels =
                                            (fun nodeWithGreatestLevelToBeAddedToFlankingSubtreeWithLesserLevels ->
                                                addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                                                    ({
                                                        internalNodeRepresentationForZig with
                                                            SubtreeWithGreaterLevelsForSameTestVariableIndex = nodeWithGreatestLevelToBeAddedToFlankingSubtreeWithLesserLevels
                                                    }
                                                    |> MirroredInternalNode localMirroring))
                                        accumulateFlankingSubtrees
                                            internalNodeRepresentationforZigZag
                                            addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                                            addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels
                                            localMirroring
                                  | _ ->
                                        // Zig-only case (either the step has been found, or found least upper bound instead)...
                                        let internalNodeRepresentationForSplayedZig =
                                            {
                                                internalNodeRepresentationForZig with
                                                    SubtreeWithLesserLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                                    SubtreeWithGreaterLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                            }
                                        let flankingSubtreeWithLesserLevels
                                            = addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                                                zigSubtreeWithLesserLevelsForSameTestVariableIndex
                                        let flankingSubtreeWithGreaterLevels
                                            = addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels
                                                ({
                                                    internalNodeRepresentationForRoot with
                                                        SubtreeWithLesserLevelsForSameTestVariableIndex = zigSubtreeWithGreaterLevelsForSameTestVariableIndex
                                                 }
                                                |> MirroredInternalNode localMirroring)
                                        if localMirroring
                                        then
                                            internalNodeRepresentationForSplayedZig
                                            , flankingSubtreeWithGreaterLevels
                                            , flankingSubtreeWithLesserLevels
                                        else
                                            internalNodeRepresentationForSplayedZig
                                            , flankingSubtreeWithLesserLevels
                                            , flankingSubtreeWithGreaterLevels
                          | _ ->
                                // Degenerate root node only case (step has not been found, found least upper bound instead)...
                                splayAtRootNode ()
            accumulateFlankingSubtrees internalNodeRepresentation
                                       BargainBasement.Identity
                                       BargainBasement.Identity
                                       false
        let mismatch lhs
                     rhs
                     equals =
            let lhsLength =
                ChunkedList.length lhs
            let rec mismatch lhsIndex
                             rhs
                             rhsReversedPrefix =
                if lhsLength = lhsIndex
                then
                    AgreesWithPrefixOfIncompletePath (rhsReversedPrefix
                                                         |> List.rev
                                                         , rhs)
                else
                    match rhs with
                        rhsHead :: rhsTail ->
                            if equals lhs.[lhsIndex]
                                      rhsHead
                            then
                                mismatch (lhsIndex + 1)
                                         rhsTail
                                         (rhsHead :: rhsReversedPrefix)
                            else
                                CompleteMismatch (lhsIndex
                                                  , rhsReversedPrefix
                                                    |> List.rev
                                                  , rhs)
                      | [] ->
                            ShortenedPrefixAgreesWithEntireIncompletePath (lhsIndex
                                                                              , rhsReversedPrefix
                                                                                |> List.rev)
            mismatch 0
                     rhs
                     []

        let merge agreeingPrefixOfIncompletePathRepresentation
                  sharedPathPrefix =
            ChunkedList.zip (agreeingPrefixOfIncompletePathRepresentation
                             |> ChunkedList.ofList)
                       sharedPathPrefix
            |> ChunkedList.map (fun (fromIncompletePathRepresentation: Option<_>
                                     , fromSharedPathPrefix) ->
                               fromIncompletePathRepresentation.OrElse fromSharedPathPrefix)

    open SetOfMergedPathsDetail

    type SetOfMergedPaths<'Step when 'Step: comparison>(paths: Paths<'Step>,
                                                                                maximumNumberOfTestVariables: Int32,
                                                                                testVectorIsAcceptable: seq<Int32 * 'Step> -> Boolean) =
        let createIncompletePathSequence revealCompletePathsAgain =
            let rec traversePaths {
                                                SharedPathPrefix = sharedPathPrefix
                                                BranchingRoot = branchingRoot
                                            }
                                            (treeSearchContextParameters: TreeSearchContextParameters)
                                            incompletePathBeingBuilt =
                let treeSearchContextParameters
                    , incompletePathBeingBuilt =
                    sharedPathPrefix
                    |> ChunkedList.fold (fun ((treeSearchContextParameters: TreeSearchContextParameters)
                                              , incompletePathBeingBuilt)
                                            sharedPathPrefixStep ->
                                                match sharedPathPrefixStep with
                                                    Some stepForTestVariableIndex ->
                                                        treeSearchContextParameters.PropagateFromDefinedLevelToNextTestVariable
                                                        , ((treeSearchContextParameters.TestVariableIndex, stepForTestVariableIndex) :: incompletePathBeingBuilt)
                                                  | None ->
                                                        treeSearchContextParameters.PropagateFromWildcardLevelToNextTestVariable
                                                        , incompletePathBeingBuilt)
                                        (treeSearchContextParameters
                                         , incompletePathBeingBuilt)
                traverseTernarySearchTree branchingRoot
                                          treeSearchContextParameters
                                          incompletePathBeingBuilt
            and traverseTernarySearchTree ternarySearchTree
                                          (treeSearchContextParameters: TreeSearchContextParameters)
                                          incompletePathBeingBuilt =
                let rec traverseBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable =
                    match binaryTreeOfLevelsForTestVariable with
                        UnsuccessfulSearchTerminationNode ->
                            if maximumNumberOfTestVariables <= treeSearchContextParameters.TestVariableIndex
                            then
                                raise (InternalAssertionViolationException "The path refers to test variable indices that are greater than the permitted maximum.")

                            Seq.empty
                      | (InternalNode
                        {
                            LevelForTestVariableIndex = stepForTestVariableIndex
                            SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                            SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                            PathsForFollowingIndices = pathsForFollowingIndices
                        }) ->
                            Seq.delay (fun() ->
                                        seq
                                            {
                                                yield! traverseBinaryTreeOfLevelsForTestVariable subtreeWithLesserLevelsForSameTestVariableIndex
                                                yield! traversePaths pathsForFollowingIndices
                                                                               treeSearchContextParameters.PropagateFromDefinedLevelToNextTestVariable
                                                                               ((treeSearchContextParameters.TestVariableIndex, stepForTestVariableIndex) :: incompletePathBeingBuilt)
                                                yield! traverseBinaryTreeOfLevelsForTestVariable subtreeWithGreaterLevelsForSameTestVariableIndex
                                            })
                match ternarySearchTree with
                    SuccessfulSearchTerminationNode ->
                        if maximumNumberOfTestVariables < treeSearchContextParameters.TestVariableIndex
                            // NOTE: a subtlety - remember that 'testVariableIndex' can reach 'maximumNumberOfTestVariablesOverall'
                            // for a full (and possibly removed) path, because successful searches go through at least one
                            // node corresponding to each test variable index, *then* land on a node indicating whether the search
                            // was successful or not: so the zero-relative index gets incremented one more time.
                        then
                            raise (InternalAssertionViolationException "The path refers to test variable indices that are greater than the permitted maximum.")

                        if not revealCompletePathsAgain
                           && treeSearchContextParameters.IsCompletePath maximumNumberOfTestVariables
                           || treeSearchContextParameters.IsSpecialCaseDenotingInitialState
                        then
                            Seq.empty
                        else
                            if (List.isEmpty incompletePathBeingBuilt)
                            then
                                raise (InternalAssertionViolationException "Should not contain an empty partial vector: attempts to merge in empty partial vectors should have resulted in the original collection.")

                            if incompletePathBeingBuilt.Length > maximumNumberOfTestVariables
                            then
                                raise (InternalAssertionViolationException "The path has more entries than the permitted maximum number of test variables.")

                            let incompletePath =
                                incompletePathBeingBuilt
                                |> Map.ofList

                            if testVectorIsAcceptable (Map.toSeq incompletePath)
                               |> not
                            then
                                raise (InternalAssertionViolationException "The path is not acceptable to the filter - it should neither have been added nor formed by merging.")

                            // NOTE: as we are converting to a map, we can be cavalier about the
                            // order in which associative pairs are added to the incomplete path.
                            Seq.singleton incompletePath
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        PathsForFollowingIndices = pathsForFollowingIndices
                    } ->
                        Seq.delay (fun () ->
                                    seq
                                        {
                                            yield! traverseBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                            yield! traversePaths pathsForFollowingIndices
                                                                           treeSearchContextParameters.PropagateFromWildcardLevelToNextTestVariable
                                                                           incompletePathBeingBuilt
                                        })
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable ->
                        traverseBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
            traversePaths paths
                                    TreeSearchContextParameters.StartOfSearch
                                    []

        let fillOutIncompletePathWithIndeterminates incompletePathRepresentation =
            if (incompletePathRepresentation: Map<_, _>).Count > maximumNumberOfTestVariables
            then
                raise (InternalAssertionViolationException "The incomplete path being either merged or added has more entries than the permitted maximum number of test variables.")

            let testVariableIndicesHavingLevels =
                incompletePathRepresentation
                |> Seq.map (fun keyValuePair -> keyValuePair.Key)
                |> Set.ofSeq

            let maximumTestVariableIndexHavingLevel =
                Seq.max testVariableIndicesHavingLevels

            if maximumTestVariableIndexHavingLevel >= maximumNumberOfTestVariables
            then
                raise (PreconditionViolationException "The incomplete path being either merged or added has a test variable index that is greater than the permitted maximum.")

            let testVariableIndicesForFilledOutTestVector = // NOTE: only up to 'maximumTestVariableIndexHavingLevel' exclusive
                                                            // - this is to avoid having a tail of indeterminate entries.
                List.init maximumTestVariableIndexHavingLevel BargainBasement.Identity
                |> Set.ofList

            let testVariableIndicesForIndeterminates =
                Set.difference testVariableIndicesForFilledOutTestVector testVariableIndicesHavingLevels

            let isPrefixOfCompletePath =
                Set.isEmpty testVariableIndicesForIndeterminates

            if isPrefixOfCompletePath
            then
                let isCompletePath =
                    maximumNumberOfTestVariables = 1 + maximumTestVariableIndexHavingLevel
                incompletePathRepresentation
                |> Map.toList
                |> List.map (snd >> Some)
                , isCompletePath
            else
                let sortedAssociationListFromTestVariableIndicesToIndeterminateMarkers =
                    testVariableIndicesForIndeterminates
                    |> Set.toList
                    |> List.map (fun testVariableIndex ->
                                     testVariableIndex
                                     , None)

                let sortedAssociationListFromTestVariableIndicesToLevels =
                    incompletePathRepresentation
                    |> Map.map (fun _ step ->
                                    Some step)
                    |> Map.toList

                let mergedAssociationList =
                    BargainBasement.MergeDisjointSortedAssociationLists sortedAssociationListFromTestVariableIndicesToLevels
                                                                        sortedAssociationListFromTestVariableIndicesToIndeterminateMarkers

                mergedAssociationList
                |> List.map snd
                , false


        let add paths
                newIncompletePathRepresentation =
            let justOnePathFrom incompletePathRepresentation =
                {
                    SharedPathPrefix =
                        incompletePathRepresentation
                        |> ChunkedList.ofList
                    BranchingRoot =
                        SuccessfulSearchTerminationNode
                }
            let rec addToPaths ({
                                            SharedPathPrefix = sharedPathPrefix
                                            BranchingRoot = branchingRoot
                                         } as paths)
                                         newIncompletePathRepresentation =
                    match mismatch sharedPathPrefix
                                   newIncompletePathRepresentation
                                   (=) with
                        AgreesWithPrefixOfIncompletePath (agreeingPrefixOfNewIncompletePathRepresentation
                                                             , remainderOfNewIncompletePathRepresentation) ->
                            match branchingRoot with
                                EmptyTernarySearchTree ->
                                    justOnePathFrom newIncompletePathRepresentation
                              | _ ->
                                    {
                                        SharedPathPrefix =
                                            sharedPathPrefix
                                        BranchingRoot =
                                            addToTernarySearchTree branchingRoot
                                                                   remainderOfNewIncompletePathRepresentation
                                    }
                      | CompleteMismatch (indexOfMismatchOnSharedPathStep
                                          , agreeingPrefixOfNewIncompletePathRepresentation
                                          , mismatchingSuffixOfNewIncompletePathRepresentation) ->
                            let pathsAfterMismatch =
                                {
                                    paths with
                                        SharedPathPrefix = sharedPathPrefix.[1 + indexOfMismatchOnSharedPathStep ..]
                                }
                            let sharedPathPrefixSplit =
                                match sharedPathPrefix.[indexOfMismatchOnSharedPathStep] with
                                    Some stepFromMismatchingSharedPathStep ->
                                        {
                                            LevelForTestVariableIndex = stepFromMismatchingSharedPathStep
                                            SubtreeWithLesserLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                            SubtreeWithGreaterLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                            PathsForFollowingIndices = pathsAfterMismatch
                                        }
                                        |> InternalNode
                                        |> BinaryTreeOfLevelsForTestVariable
                                  | None ->
                                        {
                                            SubtreeWithAllLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                            PathsForFollowingIndices = pathsAfterMismatch
                                        }
                                        |> WildcardNode
                            {
                                SharedPathPrefix =
                                    sharedPathPrefix.[.. indexOfMismatchOnSharedPathStep - 1]
                                BranchingRoot =
                                    addToTernarySearchTree sharedPathPrefixSplit
                                                           mismatchingSuffixOfNewIncompletePathRepresentation
                            }
                      | ShortenedPrefixAgreesWithEntireIncompletePath _ ->
                            raise (InternalAssertionViolationException "Attempt to add a new incomplete path that is a prefix of a previous one.")
            and addToTernarySearchTree ternarySearchTree
                                       newIncompletePathRepresentation =
                let rec addLevelToBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                    stepFromNewIncompletePathRepresentation
                                                                    tailFromNewIncompletePathRepresentation =
                    match binaryTreeOfLevelsForTestVariable with
                        UnsuccessfulSearchTerminationNode ->
                            {
                                LevelForTestVariableIndex = stepFromNewIncompletePathRepresentation
                                SubtreeWithLesserLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                SubtreeWithGreaterLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                PathsForFollowingIndices =
                                    justOnePathFrom tailFromNewIncompletePathRepresentation
                            }
                            |> InternalNode
                      | (InternalNode internalNodeRepresentation) ->
                            let comparisonWrtImplicitLevel =
                                compare stepFromNewIncompletePathRepresentation
                            let ({
                                    LevelForTestVariableIndex = splayedLevelForTestVariableIndex
                                    PathsForFollowingIndices = splayedPathsForFollowingIndices
                                 } as splayedInternalNodeRepresentation)
                                , flankingSubtreeWithLesserLevels
                                , flankingSubtreeWithGreaterLevels =
                                splayInternalNodeWithMatchingOrNeighbouringLevel internalNodeRepresentation
                                                                                 comparisonWrtImplicitLevel
                            match comparisonWrtImplicitLevel splayedLevelForTestVariableIndex with
                                result when result < 0 ->
                                    let flankingSubtreeWithGreaterLevels =
                                        {
                                            splayedInternalNodeRepresentation with
                                                SubtreeWithGreaterLevelsForSameTestVariableIndex = flankingSubtreeWithGreaterLevels
                                        }
                                        |> InternalNode
                                    {
                                        LevelForTestVariableIndex = stepFromNewIncompletePathRepresentation
                                        SubtreeWithLesserLevelsForSameTestVariableIndex = flankingSubtreeWithLesserLevels
                                        SubtreeWithGreaterLevelsForSameTestVariableIndex = flankingSubtreeWithGreaterLevels
                                        PathsForFollowingIndices =
                                            justOnePathFrom tailFromNewIncompletePathRepresentation
                                    }
                                    |> InternalNode
                              | result when result > 0 ->
                                    let flankingSubtreeWithLesserLevels =
                                        {
                                            splayedInternalNodeRepresentation with
                                                SubtreeWithLesserLevelsForSameTestVariableIndex = flankingSubtreeWithLesserLevels
                                        }
                                        |> InternalNode
                                    {
                                        LevelForTestVariableIndex = stepFromNewIncompletePathRepresentation
                                        SubtreeWithLesserLevelsForSameTestVariableIndex = flankingSubtreeWithLesserLevels
                                        SubtreeWithGreaterLevelsForSameTestVariableIndex = flankingSubtreeWithGreaterLevels
                                        PathsForFollowingIndices =
                                            justOnePathFrom tailFromNewIncompletePathRepresentation
                                    }
                                    |> InternalNode
                              | _ ->
                                    let modifiedPathsForFollowingIndices =
                                        addToPaths splayedPathsForFollowingIndices
                                                             tailFromNewIncompletePathRepresentation
                                    {
                                        splayedInternalNodeRepresentation with
                                            SubtreeWithLesserLevelsForSameTestVariableIndex = flankingSubtreeWithLesserLevels
                                            SubtreeWithGreaterLevelsForSameTestVariableIndex = flankingSubtreeWithGreaterLevels
                                            PathsForFollowingIndices = modifiedPathsForFollowingIndices
                                    }
                                    |> InternalNode
                match ternarySearchTree
                      , newIncompletePathRepresentation with
                  | SuccessfulSearchTerminationNode
                    , _ ->
                        raise (InternalAssertionViolationException "Attempt to add a new incomplete path that has a previous one as a prefix or is the same as it.")
                        // The above is really a precondition violation, but the precondition should have been enforced at a higher step within the implementation and not by the client.
                  | WildcardNode
                    ({
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                     } as wildcardNodeRepresentation)
                    , Some stepFromNewIncompletePathRepresentation :: tailFromNewIncompletePathRepresentation ->
                        let modifiedSubtreeWithAllLevelsForSameTestVariableIndex =
                            addLevelToBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                                                        stepFromNewIncompletePathRepresentation
                                                                        tailFromNewIncompletePathRepresentation
                        {
                            wildcardNodeRepresentation with
                                SubtreeWithAllLevelsForSameTestVariableIndex = modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                        }
                        |> WildcardNode
                  | WildcardNode
                    ({
                        PathsForFollowingIndices = pathsForFollowingIndices
                     } as wildcardNodeRepresentation)
                    , None :: tailFromNewIncompletePathRepresentation ->
                        let modifiedPathsForFollowingIndices =
                            addToPaths pathsForFollowingIndices
                                                 tailFromNewIncompletePathRepresentation
                        {
                            wildcardNodeRepresentation with
                                PathsForFollowingIndices = modifiedPathsForFollowingIndices
                        }
                        |> WildcardNode
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                    , Some stepFromNewIncompletePathRepresentation :: tailFromNewIncompletePathRepresentation ->
                        addLevelToBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                    stepFromNewIncompletePathRepresentation
                                                                    tailFromNewIncompletePathRepresentation
                        |> BinaryTreeOfLevelsForTestVariable
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                    , None :: tailFromNewIncompletePathRepresentation ->
                        {
                            SubtreeWithAllLevelsForSameTestVariableIndex = binaryTreeOfLevelsForTestVariable
                            PathsForFollowingIndices =
                                justOnePathFrom tailFromNewIncompletePathRepresentation
                        }
                        |> WildcardNode
                  | _
                    , [] ->
                        raise (InternalAssertionViolationException "Attempt to add a new incomplete path that is already mergeable with a previous one.")
                        // The above is really a precondition violation, but the precondition should have been enforced at a higher step within the implementation and not by the client.
            addToPaths paths
                                 newIncompletePathRepresentation

        let remove paths
                   queryIncompletePathRepresentation
                   existingCompletePathBlockedRemovalContinuation =
            let removeInternalNodeWithGreatestLevelInSubtree subtreeInternalNodeRepresentation =
                let comparisonWrtPositiveInfinity _ =
                    1
                let {
                        LevelForTestVariableIndex = splayedLevelForTestVariableIndex
                        PathsForFollowingIndices = splayedPathsForFollowingIndices
                    }
                    , flankingSubtreeWithLesserLevels
                    , _ =
                    splayInternalNodeWithMatchingOrNeighbouringLevel subtreeInternalNodeRepresentation
                                                                     comparisonWrtPositiveInfinity
                splayedLevelForTestVariableIndex
                , splayedPathsForFollowingIndices
                , flankingSubtreeWithLesserLevels
            let removeInternalNodeWithLeastLevelInSubtree subtreeInternalNodeRepresentation =
                let comparisonWrtNegativeInfinity _ =
                    -1
                let {
                        LevelForTestVariableIndex = splayedLevelForTestVariableIndex
                        PathsForFollowingIndices = splayedPathsForFollowingIndices
                    }
                    , _
                    , flankingSubtreeWithGreaterLevels =
                    splayInternalNodeWithMatchingOrNeighbouringLevel subtreeInternalNodeRepresentation
                                                                     comparisonWrtNegativeInfinity
                splayedLevelForTestVariableIndex
                , splayedPathsForFollowingIndices
                , flankingSubtreeWithGreaterLevels
            let buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees stepForTestVariableIndex
                                                                                        subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                        subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                        pathsForFollowingIndices =
                match subtreeWithLesserLevelsForSameTestVariableIndex
                      , subtreeWithGreaterLevelsForSameTestVariableIndex
                      , pathsForFollowingIndices with
                  | UnsuccessfulSearchTerminationNode
                    , UnsuccessfulSearchTerminationNode
                    , NoPaths ->
                        UnsuccessfulSearchTerminationNode
                  | UnsuccessfulSearchTerminationNode
                    , _
                    , NoPaths ->
                        subtreeWithGreaterLevelsForSameTestVariableIndex
                  | _
                    , UnsuccessfulSearchTerminationNode
                    , NoPaths ->
                        subtreeWithLesserLevelsForSameTestVariableIndex
                  | InternalNode subtreeWithLesserLevelsForSameTestVariableIndexInternalNodeRepresentation
                    , InternalNode subtreeWithGreaterLevelsForSameTestVariableIndexInternalNodeRepresentation
                    , NoPaths ->
                        if subtreeWithLesserLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                           > subtreeWithGreaterLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                        then
                            let stepForTestVariableIndexFromRemovedNode
                                , pathsForFollowingIndicesFromRemovedNode
                                , subtreeWithLesserLevelsForSameTestVariableIndexWithoutThatNode =
                                    removeInternalNodeWithGreatestLevelInSubtree subtreeWithLesserLevelsForSameTestVariableIndexInternalNodeRepresentation
                            ({
                                LevelForTestVariableIndex =
                                    stepForTestVariableIndexFromRemovedNode
                                SubtreeWithLesserLevelsForSameTestVariableIndex =
                                    subtreeWithLesserLevelsForSameTestVariableIndexWithoutThatNode
                                SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                    subtreeWithGreaterLevelsForSameTestVariableIndex
                                PathsForFollowingIndices =
                                    pathsForFollowingIndicesFromRemovedNode
                            }
                            |> InternalNode)
                        else
                            let stepForTestVariableIndexFromRemovedNode
                                , pathsForFollowingIndicesFromRemovedNode
                                , subtreeWithGreaterLevelsForSameTestVariableIndexWithoutThatNode =
                                    removeInternalNodeWithLeastLevelInSubtree subtreeWithGreaterLevelsForSameTestVariableIndexInternalNodeRepresentation
                            ({
                                LevelForTestVariableIndex =
                                    stepForTestVariableIndexFromRemovedNode
                                SubtreeWithLesserLevelsForSameTestVariableIndex =
                                    subtreeWithLesserLevelsForSameTestVariableIndex
                                SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                    subtreeWithGreaterLevelsForSameTestVariableIndexWithoutThatNode
                                PathsForFollowingIndices =
                                    pathsForFollowingIndicesFromRemovedNode
                            }
                            |> InternalNode)
                  | _ ->
                        ({
                            LevelForTestVariableIndex =
                                stepForTestVariableIndex
                            SubtreeWithLesserLevelsForSameTestVariableIndex =
                                subtreeWithLesserLevelsForSameTestVariableIndex
                            SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                subtreeWithGreaterLevelsForSameTestVariableIndex
                            PathsForFollowingIndices =
                                pathsForFollowingIndices
                        }
                        |> InternalNode)
            let buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees subtreeWithAllLevelsForSameTestVariableIndex
                                                                                        pathsForFollowingIndices =
                match subtreeWithAllLevelsForSameTestVariableIndex
                      , pathsForFollowingIndices with
                    UnsuccessfulSearchTerminationNode
                    , NoPaths ->
                        EmptyTernarySearchTree
                  | _
                    , NoPaths ->
                        BinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                  | _ ->
                        {
                            SubtreeWithAllLevelsForSameTestVariableIndex =
                                subtreeWithAllLevelsForSameTestVariableIndex
                            PathsForFollowingIndices =
                                pathsForFollowingIndices
                        }
                        |> WildcardNode
            let rec removeFromPaths {
                                                SharedPathPrefix = sharedPathPrefix
                                                BranchingRoot = branchingRoot
                                              }
                                              queryIncompletePathRepresentation
                                              treeSearchContextParameters
                                              removedIncompletePathInReverse =
                let treeSearchContextParametersAfter sharedPathPrefix =
                    sharedPathPrefix
                    |> ChunkedList.fold (fun (treeSearchContextParameters: TreeSearchContextParameters)
                                             sharedPathPrefixStep ->
                                            match sharedPathPrefixStep with
                                                Some stepForTestVariableIndex ->
                                                    treeSearchContextParameters.PropagateFromDefinedLevelToNextTestVariable
                                              | None ->
                                                    treeSearchContextParameters.PropagateFromWildcardLevelToNextTestVariable)
                                        treeSearchContextParameters
                let removeWhenSharedPathPrefixAgreesWithQuery agreeingPrefixOfQueryIncompletePathRepresentation
                                                              remainderOfQueryIncompletePathRepresentation =
                    let removedIncompletePathSharedPathPrefixInReverse =
                        fun () ->
                            let mergedSharedPathPrefix =
                                merge agreeingPrefixOfQueryIncompletePathRepresentation
                                      sharedPathPrefix
                                |> ChunkedList.toList
                            LazyList.append (mergedSharedPathPrefix
                                             |> List.rev
                                             |> LazyList.ofList)
                                            removedIncompletePathInReverse
                        |> LazyList.delayed
                    continuationWorkflow
                        {
                            let! modifiedBranchingRoot
                                 , removedIncompletePath =
                                removeFromTernarySearchTree branchingRoot
                                                            remainderOfQueryIncompletePathRepresentation
                                                            (treeSearchContextParametersAfter sharedPathPrefix)
                                                            removedIncompletePathSharedPathPrefixInReverse
                            match modifiedBranchingRoot with
                                EmptyTernarySearchTree ->
                                    return NoPaths
                                           , removedIncompletePath
                              | BranchingWithSingleLevelForLeadingTestVariable modifiedPathsEquivalentToBranchingRoot ->
                                    return {
                                                modifiedPathsEquivalentToBranchingRoot with
                                                    SharedPathPrefix =
                                                        ChunkedList.append sharedPathPrefix
                                                                           modifiedPathsEquivalentToBranchingRoot.SharedPathPrefix
                                           }
                                           , removedIncompletePath
                              | BranchingWithJustWildcardForLeadingTestVariable modifiedPathsEquivalentToBranchingRoot ->
                                    return {
                                                modifiedPathsEquivalentToBranchingRoot with
                                                    SharedPathPrefix =
                                                        ChunkedList.append sharedPathPrefix
                                                                           modifiedPathsEquivalentToBranchingRoot.SharedPathPrefix
                                           }
                                           , removedIncompletePath
                              | _ ->
                                    return {
                                                SharedPathPrefix = sharedPathPrefix
                                                BranchingRoot = modifiedBranchingRoot
                                           }
                                           , removedIncompletePath
                        }
                continuationWorkflow
                    {
                        match mismatch sharedPathPrefix
                                       queryIncompletePathRepresentation
                                       (fun lhs
                                            rhs ->
                                                match lhs
                                                      , rhs with
                                                    Some lhsLevel
                                                    , Some rhsLevel ->
                                                        lhsLevel = rhsLevel
                                                  | _ ->
                                                        true) with
                            AgreesWithPrefixOfIncompletePath (agreeingPrefixOfQueryIncompletePathRepresentation
                                                                 , remainderOfQueryIncompletePathRepresentation) ->
                                return! removeWhenSharedPathPrefixAgreesWithQuery agreeingPrefixOfQueryIncompletePathRepresentation
                                                                                  remainderOfQueryIncompletePathRepresentation
                          | ShortenedPrefixAgreesWithEntireIncompletePath (lengthOfQueryIncompletePathRepresentation
                                                                              , agreeingPrefixOfQueryIncompletePathRepresentation) ->
                                let paddingForRemainderOfQueryIncompletePathRepresentation =
                                    List.replicate (sharedPathPrefix.Length - lengthOfQueryIncompletePathRepresentation)
                                                   None
                                return! removeWhenSharedPathPrefixAgreesWithQuery (List.append agreeingPrefixOfQueryIncompletePathRepresentation
                                                                                               paddingForRemainderOfQueryIncompletePathRepresentation)
                                                                                  []
                          | CompleteMismatch _ ->
                                return! continuationWorkflow.Zero ()
                    }
            and removeFromTernarySearchTree ternarySearchTree
                                            queryIncompletePathRepresentation
                                            (treeSearchContextParameters: TreeSearchContextParameters)
                                            removedIncompletePathInReverse =
                let buildResultFromInternalNodeModifyingSubtreeForFollowingTestVariableIndices tailFromQueryIncompletePathRepresentation
                                                                                               stepForTestVariableIndex
                                                                                               subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                               subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                               pathsForFollowingIndices =
                    let removedIncompletePathWithNewLevelInReverse =
                        LazyList.cons (Some stepForTestVariableIndex)
                                      removedIncompletePathInReverse
                    continuationWorkflow
                        {
                            let! modifiedSubtreeForFollowingTestVariableIndices
                                 , removedIncompletePath =
                                removeFromPaths pathsForFollowingIndices
                                                          tailFromQueryIncompletePathRepresentation
                                                          treeSearchContextParameters.PropagateFromDefinedLevelToNextTestVariable
                                                          removedIncompletePathWithNewLevelInReverse
                            let modifiedBinarySearchTree =
                                buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees stepForTestVariableIndex
                                                                                                        subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                        subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                        modifiedSubtreeForFollowingTestVariableIndices
                            return modifiedBinarySearchTree
                                   , removedIncompletePath

                        }
                let buildResultFromWildcardNodeModifyingSubtreeForFollowingTestVariableIndices headFromQueryIncompletePathRepresentation
                                                                                               tailFromQueryIncompletePathRepresentation
                                                                                               subtreeWithAllLevelsForSameTestVariableIndex
                                                                                               pathsForFollowingIndices =
                    let removedIncompletePathWithNewLevelInReverse =
                        LazyList.cons headFromQueryIncompletePathRepresentation
                                      removedIncompletePathInReverse
                    continuationWorkflow
                        {
                            let! modifiedSubtreeForFollowingTestVariableIndices
                                 , removedIncompletePath =
                                removeFromPaths pathsForFollowingIndices
                                                          tailFromQueryIncompletePathRepresentation
                                                          treeSearchContextParameters.PropagateFromWildcardLevelToNextTestVariable
                                                          removedIncompletePathWithNewLevelInReverse
                            let modifiedTernarySearchTree =
                                buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees subtreeWithAllLevelsForSameTestVariableIndex
                                                                                                        modifiedSubtreeForFollowingTestVariableIndices
                            return modifiedTernarySearchTree
                                   , removedIncompletePath
                        }

                let removeLevelFromBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                     stepFromQueryIncompletePathRepresentation
                                                                     tailFromQueryIncompletePathRepresentation =
                    match binaryTreeOfLevelsForTestVariable with
                        UnsuccessfulSearchTerminationNode ->
                            if maximumNumberOfTestVariables <= treeSearchContextParameters.TestVariableIndex
                            then
                                raise (InternalAssertionViolationException "The path refers to test variable indices that are greater than the permitted maximum.")

                            continuationWorkflow.Zero ()
                      | InternalNode internalNodeRepresentation ->
                            let comparisonWrtImplicitLevel =
                                compare stepFromQueryIncompletePathRepresentation
                            let {
                                    LevelForTestVariableIndex = splayedLevelForTestVariableIndex
                                    PathsForFollowingIndices = splayedPathsForFollowingIndices
                                }
                                , flankingSubtreeWithLesserLevels
                                , flankingSubtreeWithGreaterLevels =
                                splayInternalNodeWithMatchingOrNeighbouringLevel internalNodeRepresentation
                                                                                 comparisonWrtImplicitLevel
                            match comparisonWrtImplicitLevel splayedLevelForTestVariableIndex with
                                0 ->
                                    buildResultFromInternalNodeModifyingSubtreeForFollowingTestVariableIndices tailFromQueryIncompletePathRepresentation
                                                                                                               splayedLevelForTestVariableIndex
                                                                                                               flankingSubtreeWithLesserLevels
                                                                                                               flankingSubtreeWithGreaterLevels
                                                                                                               splayedPathsForFollowingIndices
                              | _ ->
                                    continuationWorkflow.Zero ()
                let rec removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                                 tailFromQueryIncompletePathRepresentation =
                    match binaryTreeOfLevelsForTestVariable with
                        UnsuccessfulSearchTerminationNode ->
                            if maximumNumberOfTestVariables <= treeSearchContextParameters.TestVariableIndex
                            then
                                raise (InternalAssertionViolationException "The path refers to test variable indices that are greater than the permitted maximum.")

                            continuationWorkflow.Zero ()

                      | InternalNode
                        {
                            LevelForTestVariableIndex = stepForTestVariableIndex
                            SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                            SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                            PathsForFollowingIndices = pathsForFollowingIndices
                        } ->
                            buildResultFromInternalNodeModifyingSubtreeForFollowingTestVariableIndices tailFromQueryIncompletePathRepresentation
                                                                                                       stepForTestVariableIndex
                                                                                                       subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                       subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                       pathsForFollowingIndices
                            + continuationWorkflow
                                {
                                    let! modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                         , removedIncompletePath =
                                        removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                 tailFromQueryIncompletePathRepresentation
                                    let modifiedBinaryTree =
                                        buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees stepForTestVariableIndex
                                                                                                                modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                pathsForFollowingIndices
                                    return modifiedBinaryTree
                                           , removedIncompletePath
                                }
                            + continuationWorkflow
                                {
                                    let! modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex
                                         , removedIncompletePath =
                                        removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                 tailFromQueryIncompletePathRepresentation
                                    let modifiedBinaryTree =
                                        buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees stepForTestVariableIndex
                                                                                                                subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                pathsForFollowingIndices
                                    return modifiedBinaryTree
                                           , removedIncompletePath
                                }

                match ternarySearchTree
                      , queryIncompletePathRepresentation with
                    SuccessfulSearchTerminationNode
                    , _ ->
                        if maximumNumberOfTestVariables < treeSearchContextParameters.TestVariableIndex
                            // NOTE: a subtlety - remember that 'testVariableIndex' can reach 'maximumNumberOfTestVariablesOverall'
                            // for a full (and possibly removed) path, because successful searches go through at least one
                            // node corresponding to each test variable index, *then* land on a node indicating whether the search
                            // was successful or not: so the zero-relative index gets incremented one more time.
                        then
                            raise (InternalAssertionViolationException "The path refers to test variable indices that are greater than the permitted maximum.")

                        if treeSearchContextParameters.IsCompletePath maximumNumberOfTestVariables
                        then
                            existingCompletePathBlockedRemovalContinuation ()
                        else
                            let mergedIncompletePathRepresentation =
                                List.append (removedIncompletePathInReverse
                                             |> LazyList.toList
                                             |> List.rev)
                                            queryIncompletePathRepresentation
                            let mergedIncompletePath =
                                [
                                    for testVariableIndex
                                        , testVariableLevel in mergedIncompletePathRepresentation
                                                               |> List.mapi (fun testVariableIndex
                                                                                 testVariableLevel ->
                                                                                    testVariableIndex
                                                                                    , testVariableLevel) do
                                        match testVariableLevel with
                                            Some testVariableLevel ->
                                                yield testVariableIndex
                                                      , testVariableLevel
                                          | None ->
                                                ()
                                ]
                            continuationWorkflow
                                {
                                    if testVectorIsAcceptable mergedIncompletePath
                                    then
                                        return EmptyTernarySearchTree
                                               , mergedIncompletePathRepresentation
                                    else
                                        return! continuationWorkflow.Zero ()
                                }
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        PathsForFollowingIndices = pathsForFollowingIndices
                    }
                    , ((Some stepFromQueryIncompletePathRepresentation) as headFromQueryIncompletePathRepresentation) :: tailFromQueryIncompletePathRepresentation ->
                        continuationWorkflow
                            {
                                let! modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                     , removedIncompletePath =
                                    removeLevelFromBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                                                                     stepFromQueryIncompletePathRepresentation
                                                                                     tailFromQueryIncompletePathRepresentation
                                let modifiedTernarySearchTree =
                                    buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                                                                                            pathsForFollowingIndices
                                return modifiedTernarySearchTree
                                       , removedIncompletePath
                            }
                        + buildResultFromWildcardNodeModifyingSubtreeForFollowingTestVariableIndices headFromQueryIncompletePathRepresentation
                                                                                                     tailFromQueryIncompletePathRepresentation
                                                                                                     subtreeWithAllLevelsForSameTestVariableIndex
                                                                                                     pathsForFollowingIndices
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        PathsForFollowingIndices = pathsForFollowingIndices
                    }
                    , None :: tailFromQueryIncompletePathRepresentation ->
                        continuationWorkflow
                            {
                                let! modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                     , removedIncompletePath =
                                    removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                                                                             tailFromQueryIncompletePathRepresentation
                                let modifiedTernarySearchTree =
                                    buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                                                                                            pathsForFollowingIndices
                                return modifiedTernarySearchTree
                                       , removedIncompletePath
                            }
                        + buildResultFromWildcardNodeModifyingSubtreeForFollowingTestVariableIndices None
                                                                                                     tailFromQueryIncompletePathRepresentation
                                                                                                     subtreeWithAllLevelsForSameTestVariableIndex
                                                                                                     pathsForFollowingIndices
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                    , Some stepFromQueryIncompletePathRepresentation :: tailFromQueryIncompletePathRepresentation ->
                        continuationWorkflow
                            {
                                let! modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                     , removedIncompletePath =
                                     removeLevelFromBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                                      stepFromQueryIncompletePathRepresentation
                                                                                      tailFromQueryIncompletePathRepresentation
                                return BinaryTreeOfLevelsForTestVariable modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                       , removedIncompletePath
                            }
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                    , None :: tailFromQueryIncompletePathRepresentation ->
                        continuationWorkflow
                            {
                                let! modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                     , removedIncompletePath =
                                    removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                                             tailFromQueryIncompletePathRepresentation
                                return BinaryTreeOfLevelsForTestVariable modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                       , removedIncompletePath
                            }
                  | _
                    , [] ->
                        // This has the effect of padding out a query incomplete path on the fly, thereby
                        // allowing a match as a prefix of some suitable stored vector, should one already be present.
                        removeFromTernarySearchTree ternarySearchTree
                                                    [None]
                                                    treeSearchContextParameters
                                                    removedIncompletePathInReverse
            removeFromPaths paths
                                      queryIncompletePathRepresentation
                                      TreeSearchContextParameters.StartOfSearch
                                      LazyList.empty

        let checkInvariant paths =
            let rec checkInvariantOfPaths {
                                                        SharedPathPrefix = sharedPathPrefix
                                                        BranchingRoot = branchingRoot
                                                    }
                                                    testVariableIndex =
                let numberOfSuccessfulPathsFromSubtreeForFollowingIndices =
                    checkInvariantOfTernarySearchTree branchingRoot
                                                      (testVariableIndex + sharedPathPrefix.Length)
                match numberOfSuccessfulPathsFromSubtreeForFollowingIndices with
                    0 when 0 < ChunkedList.length sharedPathPrefix ->
                        raise (InvariantViolationException "Redundant non-empty shared path prefix with no successful search paths leading through it.")
                  | _ ->
                        numberOfSuccessfulPathsFromSubtreeForFollowingIndices
            and checkInvariantOfTernarySearchTree ternarySearchTree
                                                  testVariableIndex =
                let rec checkInvariantOfBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                          lowerBound
                                                                          upperBound =
                    match binaryTreeOfLevelsForTestVariable with
                        UnsuccessfulSearchTerminationNode ->
                            if maximumNumberOfTestVariables <= testVariableIndex
                            then
                                raise (InternalAssertionViolationException "The path refers to test variable indices that are greater than the permitted maximum.")

                            0
                      | InternalNode
                        {
                            LevelForTestVariableIndex = stepForTestVariableIndex
                            SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                            SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                            PathsForFollowingIndices = pathsForFollowingIndices
                        } ->
                            let liftedLevel =
                                Finite stepForTestVariableIndex
                            if liftedLevel >= upperBound
                            then
                                raise (InvariantViolationException "Level is greater than or equal to exclusive upper bound.")
                            if liftedLevel <= lowerBound
                            then
                                raise (InvariantViolationException "Level is less than or equal to exclusive lower bound.")
                            let numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex =
                                checkInvariantOfBinaryTreeOfLevelsForTestVariable subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                  lowerBound
                                                                                  liftedLevel
                            let numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex =
                                checkInvariantOfBinaryTreeOfLevelsForTestVariable subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                  liftedLevel
                                                                                  upperBound
                            let numberOfSuccessfulPathsFromPathsForFollowingIndices =
                                checkInvariantOfPaths pathsForFollowingIndices
                                                                (testVariableIndex + 1)
                            match numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex
                                  , numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex
                                  , numberOfSuccessfulPathsFromPathsForFollowingIndices with
                                0
                                , 0
                                , 0 ->
                                    raise (InvariantViolationException "Redundant internal node with no successful search paths leading through it.")
                              | _
                                , 0
                                , 0 ->
                                    raise (InvariantViolationException "Redundant internal node with all successful search paths leading via subtree for lesser steps.")
                              | 0
                                , _
                                , 0 ->
                                    raise (InvariantViolationException "Redundant internal node with all successful search paths leading via subtree for greater steps.")
                              | _
                                , _
                                , 0 ->
                                    raise (InvariantViolationException "Redundant internal node with its own 'ghost' step that participates in no successful search paths.")
//                              | 0
//                                , _
//                                , _ ->
//                                    if numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex > 1
//                                    then
//                                        Diagnostics.Debug.Print ("Lone greater subtree with: {0} successful paths through it.", numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex)
//                                    numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex
//                                    + numberOfSuccessfulPathsFromSubtreeForFollowingIndices
//                              | _
//                                , 0
//                                , _ ->
//                                    if numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex > 1
//                                    then
//                                        Diagnostics.Debug.Print ("Lone lesser subtree with: {0} successful paths through it.", numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex)
//                                    numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex
//                                    + numberOfSuccessfulPathsFromSubtreeForFollowingIndices
                              | _ ->
                                    numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex
                                    + numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex
                                    + numberOfSuccessfulPathsFromPathsForFollowingIndices
                match ternarySearchTree with
                    SuccessfulSearchTerminationNode ->
                        if maximumNumberOfTestVariables < testVariableIndex  // NOTE: a subtlety - remember that 'testVariableIndex' can reach 'maximumNumberOfTestVariablesOverall'
                                                                             // for a full (and possibly removed) path, because successful searches go through at least one
                                                                             // node corresponding to each test variable index, *then* land on a node indicating whether the search
                                                                             // was successful or not: so the zero-relative index gets incremented one more time.
                        then
                            raise (InternalAssertionViolationException "The path refers to test variable indices that are greater than the permitted maximum.")

                        1
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable ->
                        checkInvariantOfBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                          NegativeInfinity
                                                                          PositiveInfinity
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        PathsForFollowingIndices = pathsForFollowingIndices
                    } ->
                        let numberOfSuccessfulPathsFromSubtreeWithAllLevelsForSameTestVariableIndex =
                            checkInvariantOfBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                                                              NegativeInfinity
                                                                              PositiveInfinity
                        match pathsForFollowingIndices with
                            SingleTrivialPath when numberOfSuccessfulPathsFromSubtreeWithAllLevelsForSameTestVariableIndex > 0 ->
                                raise (LogicErrorException "Found wildcard path that always merges with at least one path using the non-wildcard match - should have been merged.")
                          | _ ->
                            ()
                        let numberOfSuccessfulPathsFromPathsForFollowingIndices =
                            checkInvariantOfPaths pathsForFollowingIndices
                                                            (testVariableIndex + 1)
                        match numberOfSuccessfulPathsFromSubtreeWithAllLevelsForSameTestVariableIndex
                              , numberOfSuccessfulPathsFromPathsForFollowingIndices with
                            0
                            , 0 ->
                                raise (LogicErrorException "Redundant wildcard node with no successful search paths leading through it.")
                          | _
                            , 0 ->
                                raise (LogicErrorException "Redundant wildcard node that has no successful paths using its wildcard match leading through it.")
                          | _ ->
                                numberOfSuccessfulPathsFromSubtreeWithAllLevelsForSameTestVariableIndex
                                + numberOfSuccessfulPathsFromPathsForFollowingIndices

            if 0 = checkInvariantOfPaths paths
                                                   0
            then
                raise (LogicErrorException "No successful search paths but tree should be non-empty.")

        member this.MaximumNumberOfTestVariables =
            maximumNumberOfTestVariables

        member this.EnumerationOfMergedTestVectors revealCompletePathsAgain =
            createIncompletePathSequence revealCompletePathsAgain

        static member Initial maximumNumberOfTestVariablesOverall
                              testVectorIsAcceptable =
            SetOfMergedPaths<'Step> (SingleTrivialPath,
                                                            maximumNumberOfTestVariablesOverall,
                                                            testVectorIsAcceptable)

        member this.MergeOrAdd incompletePathRepresentationInExternalForm =
            if Map.isEmpty incompletePathRepresentationInExternalForm
               || (testVectorIsAcceptable (incompletePathRepresentationInExternalForm
                                           |> Map.toSeq)
                   |> not)
            then
                this
                , None
            else
                let incompletePathRepresentation
                    , incompletePathRepresentationIsActuallyAlreadyFull =
                    fillOutIncompletePathWithIndeterminates incompletePathRepresentationInExternalForm
                let detectAndConvertCompletePath incompletePathRepresentation =
                    let lengthOfIncompletePathRepresentation =
                        List.length incompletePathRepresentation
                    if lengthOfIncompletePathRepresentation > maximumNumberOfTestVariables
                    then
                        raise (InternalAssertionViolationException "The merged incomplete path has more entries than the permitted maximum number of test variables.")

                    if lengthOfIncompletePathRepresentation < maximumNumberOfTestVariables
                       || incompletePathRepresentation
                          |> List.exists Option.isNone
                    then
                        None
                    else
                        let testVariableLevelsForCompletePath =
                            incompletePathRepresentation
                            |> List.map Option.get
                        let completePath =
                            testVariableLevelsForCompletePath
                            |> List.mapi (fun testVariableIndex
                                              testVariableLevel ->
                                              testVariableIndex
                                              , testVariableLevel)
                            |> Map.ofList
                        if testVectorIsAcceptable (Map.toSeq completePath)
                           |> not
                        then
                            raise (InternalAssertionViolationException "The merged full path is not acceptable to the filter.")
                        Some completePath
                let modifiedPaths
                    , completePathBeingOfferedNowForEarlyAccess =
                    ContinuationMonad<_, _>.CallCC ((fun () ->
                                                        continuationWorkflow
                                                            {
                                                                return paths
                                                                       , None
                                                            }),
                                                    (fun existingCompletePathBlockedRemovalContinuation ->
                                                        continuationWorkflow
                                                            {
                                                                let! pathsWithoutMergeCandidate
                                                                     , mergedIncompletePathRepresentation =
                                                                    remove paths
                                                                           incompletePathRepresentation
                                                                           existingCompletePathBlockedRemovalContinuation

                                                                do! continuationWorkflow
                                                                        {
                                                                            let lengthOfIncompletePathRepresentation =
                                                                                incompletePathRepresentation
                                                                                |> List.length
                                                                            let lengthOfMergedIncompletePathRepresentation =
                                                                                mergedIncompletePathRepresentation
                                                                                |> List.length
                                                                            if lengthOfMergedIncompletePathRepresentation < lengthOfIncompletePathRepresentation
                                                                            then
                                                                                raise (InternalAssertionViolationException "The merged removed incomplete path should be as least as long as the original.")
                                                                            let truncatedMergedIncompletePathRepresentation
                                                                                , _ =
                                                                                mergedIncompletePathRepresentation.BreakOff lengthOfIncompletePathRepresentation
                                                                            List.zip incompletePathRepresentation
                                                                                     truncatedMergedIncompletePathRepresentation
                                                                            |> List.iter (fun (original
                                                                                               , merged) ->
                                                                                            let consistent =
                                                                                                match original
                                                                                                      , merged with
                                                                                                    Some fromOriginal
                                                                                                    , Some fromMerged ->
                                                                                                        fromOriginal = fromMerged
                                                                                                  | Some _
                                                                                                    , None ->
                                                                                                        false
                                                                                                  | _ ->
                                                                                                        true
                                                                                            if not consistent
                                                                                            then
                                                                                                raise (InternalAssertionViolationException "The merged removed incomplete path is not acceptable to the filter."))
                                                                            let! _
                                                                                , remergedIncompletePathRepresentation =
                                                                                remove pathsWithoutMergeCandidate
                                                                                       mergedIncompletePathRepresentation
                                                                                       (fun _ -> raise (InternalAssertionViolationException "This should not be called."))
                                                                            do
                                                                                let externalFormFor incompletePathRepresentation =
                                                                                    seq
                                                                                        {
                                                                                            for testVariableIndex
                                                                                                , testVariableLevel in incompletePathRepresentation
                                                                                                                       |> List.mapi (fun testVariableIndex
                                                                                                                                         testVariableLevel ->
                                                                                                                                            testVariableIndex
                                                                                                                                            , testVariableLevel) do
                                                                                                match testVariableLevel with
                                                                                                    Some testVariableLevel ->
                                                                                                        yield testVariableIndex
                                                                                                              , testVariableLevel
                                                                                                  | None ->
                                                                                                        ()
                                                                                        }
                                                                                    |> Map.ofSeq
                                                                                let mergedIncompletePathRepresentationInExternalForm =
                                                                                    externalFormFor mergedIncompletePathRepresentation
                                                                                if mergedIncompletePathRepresentationInExternalForm
                                                                                   |> Map.toSeq
                                                                                   |> testVectorIsAcceptable
                                                                                   |> not
                                                                                then
                                                                                    raise (InternalAssertionViolationException "The merged removed incomplete path should be passed by the filter.")
                                                                                let checkWhatHasBeenMergedInToMake mergedIncompletePathRepresentationInExternalForm =
                                                                                    let whatHasBeenMergedIn =
                                                                                        mergedIncompletePathRepresentationInExternalForm
                                                                                        |> Map.filter (fun testVariableIndex
                                                                                                           _ ->
                                                                                                           Map.containsKey testVariableIndex
                                                                                                                           incompletePathRepresentationInExternalForm
                                                                                                           |> not)
                                                                                        |> Map.toList
                                                                                    for sampleSize in 1 .. List.length whatHasBeenMergedIn - 1 do
                                                                                        for sample in CombinatoricUtilities.GenerateCombinationsOfGivenSizePreservingOrder sampleSize
                                                                                                                                                                           whatHasBeenMergedIn do
                                                                                            let incompletePathBetweenTheQueryAndTheMergeResult =
                                                                                                seq
                                                                                                    {
                                                                                                        yield! incompletePathRepresentationInExternalForm
                                                                                                               |> Map.toSeq
                                                                                                        yield! sample
                                                                                                    }
                                                                                            if testVectorIsAcceptable (incompletePathBetweenTheQueryAndTheMergeResult
                                                                                                                       |> Seq.sortBy fst)
                                                                                               |> not
                                                                                            then
                                                                                                testVectorIsAcceptable (mergedIncompletePathRepresentationInExternalForm
                                                                                                                        |> Map.toSeq
                                                                                                                        |> Seq.sortBy fst)
                                                                                                |> ignore
                                                                                                testVectorIsAcceptable (incompletePathBetweenTheQueryAndTheMergeResult
                                                                                                                        |> Seq.sortBy fst)
                                                                                                |> ignore
                                                                                                printf "incompletePathBetweenTheQueryAndTheMergeResult: %A\n" (List.ofSeq (incompletePathBetweenTheQueryAndTheMergeResult |> Seq.sortBy fst))
                                                                                                raise (PreconditionViolationException "The filter is inconsistent - it forbids a vector that is a subset of a larger vector that is passed.")

                                                                                let remergedIncompletePathRepresentationInExternalForm =
                                                                                    externalFormFor remergedIncompletePathRepresentation
                                                                                
                                                                                printf "incompletePathRepresentationInExternalForm: %A\n" (Map.toList incompletePathRepresentationInExternalForm)
                                                                                printf "mergedIncompletePathRepresentationInExternalForm: %A\n" (Map.toList mergedIncompletePathRepresentationInExternalForm)
                                                                                printf "remergedIncompletePathRepresentationInExternalForm: %A\n" (Map.toList remergedIncompletePathRepresentationInExternalForm)

                                                                                checkWhatHasBeenMergedInToMake mergedIncompletePathRepresentationInExternalForm
                                                                                checkWhatHasBeenMergedInToMake remergedIncompletePathRepresentationInExternalForm

                                                                                raise (InternalAssertionViolationException "The merged removed incomplete path still matches with something left behind!")
                                                                        }
                                                                    + continuationWorkflow
                                                                        {
                                                                            return ()
                                                                        }

                                                                return add pathsWithoutMergeCandidate
                                                                           mergedIncompletePathRepresentation
                                                                       , detectAndConvertCompletePath mergedIncompletePathRepresentation
                                                            }))
                    + continuationWorkflow
                        {
                            let modifiedPaths =
                                add paths
                                    incompletePathRepresentation

                            if not incompletePathRepresentationIsActuallyAlreadyFull
                            then
                                let! _
                                     , shouldBeIdenticalToWhatWasAdded =
                                        remove modifiedPaths
                                               incompletePathRepresentation
                                               (fun _ -> raise (InternalAssertionViolationException "This should not be called."))

                                if shouldBeIdenticalToWhatWasAdded <> incompletePathRepresentation
                                then
                                    raise (InternalAssertionViolationException "Adding an unmergeable incomplete path has caused it to change state from the original.")

                            return modifiedPaths
                                   , if incompletePathRepresentationIsActuallyAlreadyFull
                                     then
                                        Some incompletePathRepresentationInExternalForm
                                     else
                                        None
                        }
                    |> ContinuationMonad<_, _>.Execute
                if 7 = (hash this) % 100
                then
                    // Invariant check...
                    checkInvariant modifiedPaths
                    // ... end of invariant check.

                SetOfMergedPaths (modifiedPaths,
                                                        maximumNumberOfTestVariables,
                                                        testVectorIsAcceptable)
                , completePathBeingOfferedNowForEarlyAccess