#nowarn "40"

namespace SageSerpent.Infrastructure

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
                StepForPathStepIndex: 'Step
                SubtreeWithLesserStepsForSamePathStepIndex: BinaryTreeOfAlternateStepsForPathIndex<'Step>
                SubtreeWithGreaterStepsForSamePathStepIndex: BinaryTreeOfAlternateStepsForPathIndex<'Step>
                PathsForFollowingIndices: Paths<'Step>
            }
        and BinaryTreeOfAlternateStepsForPathIndex<'Step when 'Step: comparison> =
            UnsuccessfulSearchTerminationNode
          | InternalNode of InternalNode<'Step>
        and WildcardNode<'Step when 'Step: comparison> =
            {
                SubtreeWithAllStepsForSamePathStepIndex: BinaryTreeOfAlternateStepsForPathIndex<'Step>
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
          | BinaryTreeOfAlternateStepsForPathIndex of BinaryTreeOfAlternateStepsForPathIndex<'Step>

        let inline (|EmptyTernarySearchTree|_|) ternarySearchTree =
            match ternarySearchTree with
                BinaryTreeOfAlternateStepsForPathIndex UnsuccessfulSearchTerminationNode ->
                    Some ()
              | _ ->
                    None

        let EmptyTernarySearchTree =
            BinaryTreeOfAlternateStepsForPathIndex UnsuccessfulSearchTerminationNode

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

        let inline (|BranchingWithSingleStepForLeadingPathIndex|_|) ternarySearchTree =
            match ternarySearchTree with
                BinaryTreeOfAlternateStepsForPathIndex
                (InternalNode
                {
                    StepForPathStepIndex = stepForPathStepIndex
                    SubtreeWithLesserStepsForSamePathStepIndex = UnsuccessfulSearchTerminationNode
                    SubtreeWithGreaterStepsForSamePathStepIndex = UnsuccessfulSearchTerminationNode
                    PathsForFollowingIndices = pathsForFollowingIndices
                }) ->
                    {
                        pathsForFollowingIndices with
                            SharedPathPrefix =
                                Cons (Some stepForPathStepIndex
                                      , pathsForFollowingIndices.SharedPathPrefix)
                    }
                    |> Some
              | _ ->
                    None

        let inline (|BranchingWithJustWildcardForLeadingPathIndex|_|) ternarySearchTree =
            match ternarySearchTree with
                WildcardNode
                {
                    SubtreeWithAllStepsForSamePathStepIndex = UnsuccessfulSearchTerminationNode
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
                        SubtreeWithLesserStepsForSamePathStepIndex = rootSubtreeWithLesserStepsForSamePathStepIndex
                        SubtreeWithGreaterStepsForSamePathStepIndex = rootSubtreeWithGreaterStepsForSamePathStepIndex
                    } ->
                        {
                            internalNodeRepresentation with
                                SubtreeWithLesserStepsForSamePathStepIndex = rootSubtreeWithGreaterStepsForSamePathStepIndex
                                SubtreeWithGreaterStepsForSamePathStepIndex = rootSubtreeWithLesserStepsForSamePathStepIndex
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
            member this.NumberOfStepsForLeadingPathIndex =
                match !this.CacheOfNumberOfStepsForLeadingPathIndex with
                    None ->
                        let numberOfStepsForLeadingPathIndex =
                            match this with
                                {
                                    SubtreeWithLesserStepsForSamePathStepIndex = subtreeWithLesserStepsForSamePathStepIndex
                                    SubtreeWithGreaterStepsForSamePathStepIndex = subtreeWithGreaterStepsForSamePathStepIndex
                                    PathsForFollowingIndices
                                        = NoPaths
                                } ->
                                    subtreeWithLesserStepsForSamePathStepIndex.NumberOfStepsForLeadingPathIndex
                                    + subtreeWithGreaterStepsForSamePathStepIndex.NumberOfStepsForLeadingPathIndex
                              | {
                                    SubtreeWithLesserStepsForSamePathStepIndex = subtreeWithLesserStepsForSamePathStepIndex
                                    SubtreeWithGreaterStepsForSamePathStepIndex = subtreeWithGreaterStepsForSamePathStepIndex
                                } ->
                                    1
                                    + subtreeWithLesserStepsForSamePathStepIndex.NumberOfStepsForLeadingPathIndex
                                    + subtreeWithGreaterStepsForSamePathStepIndex.NumberOfStepsForLeadingPathIndex
                        this.CacheOfNumberOfStepsForLeadingPathIndex :=
                            Some numberOfStepsForLeadingPathIndex
                        numberOfStepsForLeadingPathIndex
                  | Some numberOfStepsForLeadingPathIndex ->
                        numberOfStepsForLeadingPathIndex

            // TODO: this is horrible - is this really giving any appreciable performance benefit?
            member private this.CacheOfNumberOfStepsForLeadingPathIndex =
                ref None

        and BinaryTreeOfAlternateStepsForPathIndex<'Step when 'Step: comparison> with
            member this.NumberOfStepsForLeadingPathIndex =
                match this with
                    InternalNode internalNode ->
                        internalNode.NumberOfStepsForLeadingPathIndex
                  | UnsuccessfulSearchTerminationNode ->
                        0

        type TreeSearchContextParameters =
            {
                PathStepIndex: Int32
                HasSuffixContextOfPossibleCompletePath: Boolean
            }

            static member StartOfSearch =
                {
                    PathStepIndex = 0
                    HasSuffixContextOfPossibleCompletePath = true
                }

            member this.IsSpecialCaseDenotingInitialState =   // The tests define this special state as not possessing any paths, not even the trivial empty one.
                0 = this.PathStepIndex

            member this.IsCompletePath maximumNumberOfPathIndices =
                this.HasSuffixContextOfPossibleCompletePath
                && maximumNumberOfPathIndices = this.PathStepIndex

            member this.PropagateFromDefinedStepToNextPathIndex =
                {
                    this with
                        PathStepIndex = this.PathStepIndex + 1
                }

            member this.PropagateFromWildcardStepToNextPathIndex =
                {
                    this with
                        PathStepIndex = this.PathStepIndex + 1
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

        let splayInternalNodeWithMatchingOrNeighbouringStep internalNodeRepresentation
                                                             comparisonWrtImplicitStep =
            let mirroredComparisonWrtImplicitStep =
                comparisonWrtImplicitStep
                >> (~-)
            let rec accumulateFlankingSubtrees ({
                                                    StepForPathStepIndex = rootStepForPathStepIndex
                                                    SubtreeWithLesserStepsForSamePathStepIndex = rootSubtreeWithLesserStepsForSamePathStepIndex
                                                    SubtreeWithGreaterStepsForSamePathStepIndex = rootSubtreeWithGreaterStepsForSamePathStepIndex
                                                } as internalNodeRepresentationForRoot)
                                               addNodeWithGreatestStepToFlankingSubtreeWithLesserSteps
                                               addNodeWithLeastStepToFlankingSubtreeWithGreaterSteps
                                               interchangeRolesOfFlankingSubtreesOnBehalfOfCaller =
                let addNodeWithGreatestStepToFlankingSubtreeWithLesserSteps
                    , addNodeWithLeastStepToFlankingSubtreeWithGreaterSteps =
                    if interchangeRolesOfFlankingSubtreesOnBehalfOfCaller
                    then
                        addNodeWithLeastStepToFlankingSubtreeWithGreaterSteps
                        , addNodeWithGreatestStepToFlankingSubtreeWithLesserSteps
                    else
                        addNodeWithGreatestStepToFlankingSubtreeWithLesserSteps
                        , addNodeWithLeastStepToFlankingSubtreeWithGreaterSteps
                let inline splayAtRootNode () =
                    {
                        internalNodeRepresentationForRoot with
                            SubtreeWithLesserStepsForSamePathStepIndex = UnsuccessfulSearchTerminationNode
                            SubtreeWithGreaterStepsForSamePathStepIndex = UnsuccessfulSearchTerminationNode
                    }
                    , addNodeWithGreatestStepToFlankingSubtreeWithLesserSteps rootSubtreeWithLesserStepsForSamePathStepIndex
                    , addNodeWithLeastStepToFlankingSubtreeWithGreaterSteps rootSubtreeWithGreaterStepsForSamePathStepIndex
                match comparisonWrtImplicitStep rootStepForPathStepIndex with
                    0 ->
                        // Degenerate root node only case (step has been found)...
                        splayAtRootNode ()
                  | rootResult ->
                        // NOTE: comments for this section refer to the 'unmirrored' case. So in the mirrored case,
                        // 'zig-zag' becomes 'zag-zig', 'least upper bound' becomes 'greatest lower bound', etc.
                        let localMirroring
                            , comparisonWrtImplicitStep
                            , internalNodeRepresentationForRoot
                            , rootSubtreeWithLesserStepsForSamePathStepIndex
                            , rootSubtreeWithGreaterStepsForSamePathStepIndex
                            , addNodeWithGreatestStepToFlankingSubtreeWithLesserSteps
                            , addNodeWithLeastStepToFlankingSubtreeWithGreaterSteps =
                            if rootResult > 0
                            then
                                true
                                , mirroredComparisonWrtImplicitStep
                                , mirrorInternalNode true
                                                     internalNodeRepresentationForRoot
                                , rootSubtreeWithGreaterStepsForSamePathStepIndex
                                , rootSubtreeWithLesserStepsForSamePathStepIndex
                                , addNodeWithLeastStepToFlankingSubtreeWithGreaterSteps
                                , addNodeWithGreatestStepToFlankingSubtreeWithLesserSteps
                            else
                                false
                                , comparisonWrtImplicitStep
                                , internalNodeRepresentationForRoot
                                , rootSubtreeWithLesserStepsForSamePathStepIndex
                                , rootSubtreeWithGreaterStepsForSamePathStepIndex
                                , addNodeWithGreatestStepToFlankingSubtreeWithLesserSteps
                                , addNodeWithLeastStepToFlankingSubtreeWithGreaterSteps
                        match rootSubtreeWithLesserStepsForSamePathStepIndex with
                            MirroredInternalNode localMirroring
                            ({
                                StepForPathStepIndex = zigStepForPathStepIndex
                                SubtreeWithLesserStepsForSamePathStepIndex = zigSubtreeWithLesserStepsForSamePathStepIndex
                                SubtreeWithGreaterStepsForSamePathStepIndex = zigSubtreeWithGreaterStepsForSamePathStepIndex
                            } as internalNodeRepresentationForZig) ->
                                match comparisonWrtImplicitStep zigStepForPathStepIndex
                                      , zigSubtreeWithLesserStepsForSamePathStepIndex
                                      , zigSubtreeWithGreaterStepsForSamePathStepIndex with
                                    zigResult
                                    , InternalNode internalNodeRepresentationforZigZig
                                    , _ when zigResult < 0 ->
                                        // Zig-zig case...
                                        let addNodeWithLeastStepToFlankingSubtreeWithGreaterSteps =
                                            (fun nodeWithLeastStepToBeAddedToFlankingSubtreeWithGreaterSteps ->
                                                addNodeWithLeastStepToFlankingSubtreeWithGreaterSteps
                                                    ({
                                                        internalNodeRepresentationForZig with
                                                            SubtreeWithLesserStepsForSamePathStepIndex = nodeWithLeastStepToBeAddedToFlankingSubtreeWithGreaterSteps
                                                            SubtreeWithGreaterStepsForSamePathStepIndex =
                                                                {
                                                                    internalNodeRepresentationForRoot with
                                                                        SubtreeWithLesserStepsForSamePathStepIndex = zigSubtreeWithGreaterStepsForSamePathStepIndex
                                                                }
                                                                |> MirroredInternalNode localMirroring

                                                    }
                                                    |> MirroredInternalNode localMirroring))
                                        accumulateFlankingSubtrees
                                            internalNodeRepresentationforZigZig
                                            addNodeWithGreatestStepToFlankingSubtreeWithLesserSteps
                                            addNodeWithLeastStepToFlankingSubtreeWithGreaterSteps
                                            localMirroring
                                  | zigResult
                                    , _
                                    , InternalNode internalNodeRepresentationforZigZag when zigResult > 0 ->
                                        // Zig-zag case...
                                        let addNodeWithLeastStepToFlankingSubtreeWithGreaterSteps =
                                            (fun nodeWithLeastStepToBeAddedToFlankingSubtreeWithGreaterSteps ->
                                                addNodeWithLeastStepToFlankingSubtreeWithGreaterSteps
                                                    ({
                                                        internalNodeRepresentationForRoot with
                                                            SubtreeWithLesserStepsForSamePathStepIndex = nodeWithLeastStepToBeAddedToFlankingSubtreeWithGreaterSteps
                                                    }
                                                    |> MirroredInternalNode localMirroring))
                                        let addNodeWithGreatestStepToFlankingSubtreeWithLesserSteps =
                                            (fun nodeWithGreatestStepToBeAddedToFlankingSubtreeWithLesserSteps ->
                                                addNodeWithGreatestStepToFlankingSubtreeWithLesserSteps
                                                    ({
                                                        internalNodeRepresentationForZig with
                                                            SubtreeWithGreaterStepsForSamePathStepIndex = nodeWithGreatestStepToBeAddedToFlankingSubtreeWithLesserSteps
                                                    }
                                                    |> MirroredInternalNode localMirroring))
                                        accumulateFlankingSubtrees
                                            internalNodeRepresentationforZigZag
                                            addNodeWithGreatestStepToFlankingSubtreeWithLesserSteps
                                            addNodeWithLeastStepToFlankingSubtreeWithGreaterSteps
                                            localMirroring
                                  | _ ->
                                        // Zig-only case (either the step has been found, or found least upper bound instead)...
                                        let internalNodeRepresentationForSplayedZig =
                                            {
                                                internalNodeRepresentationForZig with
                                                    SubtreeWithLesserStepsForSamePathStepIndex = UnsuccessfulSearchTerminationNode
                                                    SubtreeWithGreaterStepsForSamePathStepIndex = UnsuccessfulSearchTerminationNode
                                            }
                                        let flankingSubtreeWithLesserSteps
                                            = addNodeWithGreatestStepToFlankingSubtreeWithLesserSteps
                                                zigSubtreeWithLesserStepsForSamePathStepIndex
                                        let flankingSubtreeWithGreaterSteps
                                            = addNodeWithLeastStepToFlankingSubtreeWithGreaterSteps
                                                ({
                                                    internalNodeRepresentationForRoot with
                                                        SubtreeWithLesserStepsForSamePathStepIndex = zigSubtreeWithGreaterStepsForSamePathStepIndex
                                                 }
                                                |> MirroredInternalNode localMirroring)
                                        if localMirroring
                                        then
                                            internalNodeRepresentationForSplayedZig
                                            , flankingSubtreeWithGreaterSteps
                                            , flankingSubtreeWithLesserSteps
                                        else
                                            internalNodeRepresentationForSplayedZig
                                            , flankingSubtreeWithLesserSteps
                                            , flankingSubtreeWithGreaterSteps
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
                                                        maximumNumberOfPathIndices: Int32,
                                                        pathIsAcceptable: Map<Int32, 'Step> -> Boolean) =
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
                                                    Some stepForPathStepIndex ->
                                                        treeSearchContextParameters.PropagateFromDefinedStepToNextPathIndex
                                                        , ((treeSearchContextParameters.PathStepIndex, stepForPathStepIndex) :: incompletePathBeingBuilt)
                                                  | None ->
                                                        treeSearchContextParameters.PropagateFromWildcardStepToNextPathIndex
                                                        , incompletePathBeingBuilt)
                                        (treeSearchContextParameters
                                         , incompletePathBeingBuilt)
                traverseTernarySearchTree branchingRoot
                                          treeSearchContextParameters
                                          incompletePathBeingBuilt
            and traverseTernarySearchTree ternarySearchTree
                                          (treeSearchContextParameters: TreeSearchContextParameters)
                                          incompletePathBeingBuilt =
                let rec traverseBinaryTreeOfAlternateStepsForPathIndex binaryTreeOfStepsForPathIndex =
                    match binaryTreeOfStepsForPathIndex with
                        UnsuccessfulSearchTerminationNode ->
                            if maximumNumberOfPathIndices <= treeSearchContextParameters.PathStepIndex
                            then
                                raise (InternalAssertionViolationException "The path refers to path step indices that are greater than the permitted maximum.")

                            Seq.empty
                      | (InternalNode
                        {
                            StepForPathStepIndex = stepForPathStepIndex
                            SubtreeWithLesserStepsForSamePathStepIndex = subtreeWithLesserStepsForSamePathStepIndex
                            SubtreeWithGreaterStepsForSamePathStepIndex = subtreeWithGreaterStepsForSamePathStepIndex
                            PathsForFollowingIndices = pathsForFollowingIndices
                        }) ->
                            Seq.delay (fun() ->
                                        seq
                                            {
                                                yield! traverseBinaryTreeOfAlternateStepsForPathIndex subtreeWithLesserStepsForSamePathStepIndex
                                                yield! traversePaths pathsForFollowingIndices
                                                                     treeSearchContextParameters.PropagateFromDefinedStepToNextPathIndex
                                                                     ((treeSearchContextParameters.PathStepIndex, stepForPathStepIndex) :: incompletePathBeingBuilt)
                                                yield! traverseBinaryTreeOfAlternateStepsForPathIndex subtreeWithGreaterStepsForSamePathStepIndex
                                            })
                match ternarySearchTree with
                    SuccessfulSearchTerminationNode ->
                        if maximumNumberOfPathIndices < treeSearchContextParameters.PathStepIndex
                            // NOTE: a subtlety - remember that 'pathStepIndex' can reach 'maximumNumberOfPathIndicesOverall'
                            // for a full (and possibly removed) path, because successful searches go through at least one
                            // node corresponding to each path step index, *then* land on a node indicating whether the search
                            // was successful or not: so the zero-relative index gets incremented one more time.
                        then
                            raise (InternalAssertionViolationException "The path refers to path step indices that are greater than the permitted maximum.")

                        if not revealCompletePathsAgain
                           && treeSearchContextParameters.IsCompletePath maximumNumberOfPathIndices
                           || treeSearchContextParameters.IsSpecialCaseDenotingInitialState
                        then
                            Seq.empty
                        else
                            if (List.isEmpty incompletePathBeingBuilt)
                            then
                                raise (InternalAssertionViolationException "Should not contain an empty partial vector: attempts to merge in empty partial vectors should have resulted in the original collection.")

                            if incompletePathBeingBuilt.Length > maximumNumberOfPathIndices
                            then
                                raise (InternalAssertionViolationException "The path has more steps than the permitted maximum number of steps.")

                            let incompletePath =
                                incompletePathBeingBuilt
                                |> Map.ofList

                            if pathIsAcceptable incompletePath
                               |> not
                            then
                                raise (InternalAssertionViolationException "The path is not acceptable to the filter - it should neither have been added nor formed by merging.")

                            // NOTE: as we are converting to a map, we can be cavalier about the
                            // order in which associative pairs are added to the incomplete path.
                            Seq.singleton incompletePath
                  | WildcardNode
                    {
                        SubtreeWithAllStepsForSamePathStepIndex = subtreeWithAllStepsForSamePathStepIndex
                        PathsForFollowingIndices = pathsForFollowingIndices
                    } ->
                        Seq.delay (fun () ->
                                    seq
                                        {
                                            yield! traverseBinaryTreeOfAlternateStepsForPathIndex subtreeWithAllStepsForSamePathStepIndex
                                            yield! traversePaths pathsForFollowingIndices
                                                                 treeSearchContextParameters.PropagateFromWildcardStepToNextPathIndex
                                                                 incompletePathBeingBuilt
                                        })
                  | BinaryTreeOfAlternateStepsForPathIndex binaryTreeOfStepsForPathIndex ->
                        traverseBinaryTreeOfAlternateStepsForPathIndex binaryTreeOfStepsForPathIndex
            traversePaths paths
                          TreeSearchContextParameters.StartOfSearch
                          []

        let fillOutIncompletePathWithIndeterminates incompletePathRepresentation =
            if (incompletePathRepresentation: Map<_, _>).Count > maximumNumberOfPathIndices
            then
                raise (InternalAssertionViolationException "The incomplete path being either merged or added has more steps than the permitted maximum number of steps.")

            let pathStepIndicesHavingSteps =
                incompletePathRepresentation
                |> Seq.map (fun keyValuePair -> keyValuePair.Key)
                |> Set.ofSeq

            let maximumPathStepIndexHavingStep =
                Seq.max pathStepIndicesHavingSteps

            if maximumPathStepIndexHavingStep >= maximumNumberOfPathIndices
            then
                raise (PreconditionViolationException "The incomplete path being either merged or added has a path step index that is greater than the permitted maximum.")

            let pathStepIndicesForFilledOutPath = // NOTE: only up to 'maximumPathStepIndexHavingStep' exclusive
                                                            // - this is to avoid having a tail of indeterminate steps.
                List.init maximumPathStepIndexHavingStep BargainBasement.Identity
                |> Set.ofList

            let pathStepIndicesForIndeterminates =
                Set.difference pathStepIndicesForFilledOutPath pathStepIndicesHavingSteps

            let isPrefixOfCompletePath =
                Set.isEmpty pathStepIndicesForIndeterminates

            if isPrefixOfCompletePath
            then
                let isCompletePath =
                    maximumNumberOfPathIndices = 1 + maximumPathStepIndexHavingStep
                incompletePathRepresentation
                |> Map.toList
                |> List.map (snd >> Some)
                , isCompletePath
            else
                let sortedAssociationListFromPathStepIndicesToIndeterminateMarkers =
                    pathStepIndicesForIndeterminates
                    |> Set.toList
                    |> List.map (fun pathStepIndex ->
                                     pathStepIndex
                                     , None)

                let sortedAssociationListFromPathStepIndicesToSteps =
                    incompletePathRepresentation
                    |> Map.map (fun _ step ->
                                    Some step)
                    |> Map.toList

                let mergedAssociationList =
                    BargainBasement.MergeDisjointSortedAssociationLists sortedAssociationListFromPathStepIndicesToSteps
                                                                        sortedAssociationListFromPathStepIndicesToIndeterminateMarkers

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
                                            StepForPathStepIndex = stepFromMismatchingSharedPathStep
                                            SubtreeWithLesserStepsForSamePathStepIndex = UnsuccessfulSearchTerminationNode
                                            SubtreeWithGreaterStepsForSamePathStepIndex = UnsuccessfulSearchTerminationNode
                                            PathsForFollowingIndices = pathsAfterMismatch
                                        }
                                        |> InternalNode
                                        |> BinaryTreeOfAlternateStepsForPathIndex
                                  | None ->
                                        {
                                            SubtreeWithAllStepsForSamePathStepIndex = UnsuccessfulSearchTerminationNode
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
                let rec addStepToBinaryTreeOfAlternateStepsForPathIndex binaryTreeOfStepsForPathIndex
                                                                        stepFromNewIncompletePathRepresentation
                                                                        tailFromNewIncompletePathRepresentation =
                    match binaryTreeOfStepsForPathIndex with
                        UnsuccessfulSearchTerminationNode ->
                            {
                                StepForPathStepIndex = stepFromNewIncompletePathRepresentation
                                SubtreeWithLesserStepsForSamePathStepIndex = UnsuccessfulSearchTerminationNode
                                SubtreeWithGreaterStepsForSamePathStepIndex = UnsuccessfulSearchTerminationNode
                                PathsForFollowingIndices =
                                    justOnePathFrom tailFromNewIncompletePathRepresentation
                            }
                            |> InternalNode
                      | (InternalNode internalNodeRepresentation) ->
                            let comparisonWrtImplicitStep =
                                compare stepFromNewIncompletePathRepresentation
                            let ({
                                    StepForPathStepIndex = splayedStepForPathStepIndex
                                    PathsForFollowingIndices = splayedPathsForFollowingIndices
                                 } as splayedInternalNodeRepresentation)
                                , flankingSubtreeWithLesserSteps
                                , flankingSubtreeWithGreaterSteps =
                                splayInternalNodeWithMatchingOrNeighbouringStep internalNodeRepresentation
                                                                                comparisonWrtImplicitStep
                            match comparisonWrtImplicitStep splayedStepForPathStepIndex with
                                result when result < 0 ->
                                    let flankingSubtreeWithGreaterSteps =
                                        {
                                            splayedInternalNodeRepresentation with
                                                SubtreeWithGreaterStepsForSamePathStepIndex = flankingSubtreeWithGreaterSteps
                                        }
                                        |> InternalNode
                                    {
                                        StepForPathStepIndex = stepFromNewIncompletePathRepresentation
                                        SubtreeWithLesserStepsForSamePathStepIndex = flankingSubtreeWithLesserSteps
                                        SubtreeWithGreaterStepsForSamePathStepIndex = flankingSubtreeWithGreaterSteps
                                        PathsForFollowingIndices =
                                            justOnePathFrom tailFromNewIncompletePathRepresentation
                                    }
                                    |> InternalNode
                              | result when result > 0 ->
                                    let flankingSubtreeWithLesserSteps =
                                        {
                                            splayedInternalNodeRepresentation with
                                                SubtreeWithLesserStepsForSamePathStepIndex = flankingSubtreeWithLesserSteps
                                        }
                                        |> InternalNode
                                    {
                                        StepForPathStepIndex = stepFromNewIncompletePathRepresentation
                                        SubtreeWithLesserStepsForSamePathStepIndex = flankingSubtreeWithLesserSteps
                                        SubtreeWithGreaterStepsForSamePathStepIndex = flankingSubtreeWithGreaterSteps
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
                                            SubtreeWithLesserStepsForSamePathStepIndex = flankingSubtreeWithLesserSteps
                                            SubtreeWithGreaterStepsForSamePathStepIndex = flankingSubtreeWithGreaterSteps
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
                        SubtreeWithAllStepsForSamePathStepIndex = subtreeWithAllStepsForSamePathStepIndex
                     } as wildcardNodeRepresentation)
                    , Some stepFromNewIncompletePathRepresentation :: tailFromNewIncompletePathRepresentation ->
                        let modifiedSubtreeWithAllStepsForSamePathStepIndex =
                            addStepToBinaryTreeOfAlternateStepsForPathIndex subtreeWithAllStepsForSamePathStepIndex
                                                                            stepFromNewIncompletePathRepresentation
                                                                            tailFromNewIncompletePathRepresentation
                        {
                            wildcardNodeRepresentation with
                                SubtreeWithAllStepsForSamePathStepIndex = modifiedSubtreeWithAllStepsForSamePathStepIndex
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
                  | BinaryTreeOfAlternateStepsForPathIndex binaryTreeOfStepsForPathIndex
                    , Some stepFromNewIncompletePathRepresentation :: tailFromNewIncompletePathRepresentation ->
                        addStepToBinaryTreeOfAlternateStepsForPathIndex binaryTreeOfStepsForPathIndex
                                                                        stepFromNewIncompletePathRepresentation
                                                                        tailFromNewIncompletePathRepresentation
                        |> BinaryTreeOfAlternateStepsForPathIndex
                  | BinaryTreeOfAlternateStepsForPathIndex binaryTreeOfStepsForPathIndex
                    , None :: tailFromNewIncompletePathRepresentation ->
                        {
                            SubtreeWithAllStepsForSamePathStepIndex = binaryTreeOfStepsForPathIndex
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
            let removeInternalNodeWithGreatestStepInSubtree subtreeInternalNodeRepresentation =
                let comparisonWrtPositiveInfinity _ =
                    1
                let {
                        StepForPathStepIndex = splayedStepForPathStepIndex
                        PathsForFollowingIndices = splayedPathsForFollowingIndices
                    }
                    , flankingSubtreeWithLesserSteps
                    , _ =
                    splayInternalNodeWithMatchingOrNeighbouringStep subtreeInternalNodeRepresentation
                                                                    comparisonWrtPositiveInfinity
                splayedStepForPathStepIndex
                , splayedPathsForFollowingIndices
                , flankingSubtreeWithLesserSteps
            let removeInternalNodeWithLeastStepInSubtree subtreeInternalNodeRepresentation =
                let comparisonWrtNegativeInfinity _ =
                    -1
                let {
                        StepForPathStepIndex = splayedStepForPathStepIndex
                        PathsForFollowingIndices = splayedPathsForFollowingIndices
                    }
                    , _
                    , flankingSubtreeWithGreaterSteps =
                    splayInternalNodeWithMatchingOrNeighbouringStep subtreeInternalNodeRepresentation
                                                                    comparisonWrtNegativeInfinity
                splayedStepForPathStepIndex
                , splayedPathsForFollowingIndices
                , flankingSubtreeWithGreaterSteps
            let buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees stepForPathStepIndex
                                                                                        subtreeWithLesserStepsForSamePathStepIndex
                                                                                        subtreeWithGreaterStepsForSamePathStepIndex
                                                                                        pathsForFollowingIndices =
                match subtreeWithLesserStepsForSamePathStepIndex
                      , subtreeWithGreaterStepsForSamePathStepIndex
                      , pathsForFollowingIndices with
                  | UnsuccessfulSearchTerminationNode
                    , UnsuccessfulSearchTerminationNode
                    , NoPaths ->
                        UnsuccessfulSearchTerminationNode
                  | UnsuccessfulSearchTerminationNode
                    , _
                    , NoPaths ->
                        subtreeWithGreaterStepsForSamePathStepIndex
                  | _
                    , UnsuccessfulSearchTerminationNode
                    , NoPaths ->
                        subtreeWithLesserStepsForSamePathStepIndex
                  | InternalNode subtreeWithLesserStepsForSamePathStepIndexInternalNodeRepresentation
                    , InternalNode subtreeWithGreaterStepsForSamePathStepIndexInternalNodeRepresentation
                    , NoPaths ->
                        if subtreeWithLesserStepsForSamePathStepIndex.NumberOfStepsForLeadingPathIndex
                           > subtreeWithGreaterStepsForSamePathStepIndex.NumberOfStepsForLeadingPathIndex
                        then
                            let stepForPathStepIndexFromRemovedNode
                                , pathsForFollowingIndicesFromRemovedNode
                                , subtreeWithLesserStepsForSamePathStepIndexWithoutThatNode =
                                    removeInternalNodeWithGreatestStepInSubtree subtreeWithLesserStepsForSamePathStepIndexInternalNodeRepresentation
                            ({
                                StepForPathStepIndex =
                                    stepForPathStepIndexFromRemovedNode
                                SubtreeWithLesserStepsForSamePathStepIndex =
                                    subtreeWithLesserStepsForSamePathStepIndexWithoutThatNode
                                SubtreeWithGreaterStepsForSamePathStepIndex =
                                    subtreeWithGreaterStepsForSamePathStepIndex
                                PathsForFollowingIndices =
                                    pathsForFollowingIndicesFromRemovedNode
                            }
                            |> InternalNode)
                        else
                            let stepForPathStepIndexFromRemovedNode
                                , pathsForFollowingIndicesFromRemovedNode
                                , subtreeWithGreaterStepsForSamePathStepIndexWithoutThatNode =
                                    removeInternalNodeWithLeastStepInSubtree subtreeWithGreaterStepsForSamePathStepIndexInternalNodeRepresentation
                            ({
                                StepForPathStepIndex =
                                    stepForPathStepIndexFromRemovedNode
                                SubtreeWithLesserStepsForSamePathStepIndex =
                                    subtreeWithLesserStepsForSamePathStepIndex
                                SubtreeWithGreaterStepsForSamePathStepIndex =
                                    subtreeWithGreaterStepsForSamePathStepIndexWithoutThatNode
                                PathsForFollowingIndices =
                                    pathsForFollowingIndicesFromRemovedNode
                            }
                            |> InternalNode)
                  | _ ->
                        ({
                            StepForPathStepIndex =
                                stepForPathStepIndex
                            SubtreeWithLesserStepsForSamePathStepIndex =
                                subtreeWithLesserStepsForSamePathStepIndex
                            SubtreeWithGreaterStepsForSamePathStepIndex =
                                subtreeWithGreaterStepsForSamePathStepIndex
                            PathsForFollowingIndices =
                                pathsForFollowingIndices
                        }
                        |> InternalNode)
            let buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees subtreeWithAllStepsForSamePathStepIndex
                                                                                        pathsForFollowingIndices =
                match subtreeWithAllStepsForSamePathStepIndex
                      , pathsForFollowingIndices with
                    UnsuccessfulSearchTerminationNode
                    , NoPaths ->
                        EmptyTernarySearchTree
                  | _
                    , NoPaths ->
                        BinaryTreeOfAlternateStepsForPathIndex subtreeWithAllStepsForSamePathStepIndex
                  | _ ->
                        {
                            SubtreeWithAllStepsForSamePathStepIndex =
                                subtreeWithAllStepsForSamePathStepIndex
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
                                                Some stepForPathStepIndex ->
                                                    treeSearchContextParameters.PropagateFromDefinedStepToNextPathIndex
                                              | None ->
                                                    treeSearchContextParameters.PropagateFromWildcardStepToNextPathIndex)
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
                              | BranchingWithSingleStepForLeadingPathIndex modifiedPathsEquivalentToBranchingRoot ->
                                    return {
                                                modifiedPathsEquivalentToBranchingRoot with
                                                    SharedPathPrefix =
                                                        ChunkedList.append sharedPathPrefix
                                                                           modifiedPathsEquivalentToBranchingRoot.SharedPathPrefix
                                           }
                                           , removedIncompletePath
                              | BranchingWithJustWildcardForLeadingPathIndex modifiedPathsEquivalentToBranchingRoot ->
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
                                                    Some lhsStep
                                                    , Some rhsStep ->
                                                        lhsStep = rhsStep
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
                let buildResultFromInternalNodeModifyingSubtreeForFollowingPathStepIndices tailFromQueryIncompletePathRepresentation
                                                                                           stepForPathStepIndex
                                                                                           subtreeWithLesserStepsForSamePathStepIndex
                                                                                           subtreeWithGreaterStepsForSamePathStepIndex
                                                                                           pathsForFollowingIndices =
                    let removedIncompletePathWithNewStepInReverse =
                        LazyList.cons (Some stepForPathStepIndex)
                                      removedIncompletePathInReverse
                    continuationWorkflow
                        {
                            let! modifiedSubtreeForFollowingPathStepIndices
                                 , removedIncompletePath =
                                removeFromPaths pathsForFollowingIndices
                                                          tailFromQueryIncompletePathRepresentation
                                                          treeSearchContextParameters.PropagateFromDefinedStepToNextPathIndex
                                                          removedIncompletePathWithNewStepInReverse
                            let modifiedBinarySearchTree =
                                buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees stepForPathStepIndex
                                                                                                        subtreeWithLesserStepsForSamePathStepIndex
                                                                                                        subtreeWithGreaterStepsForSamePathStepIndex
                                                                                                        modifiedSubtreeForFollowingPathStepIndices
                            return modifiedBinarySearchTree
                                   , removedIncompletePath

                        }
                let buildResultFromWildcardNodeModifyingSubtreeForFollowingPathStepIndices headFromQueryIncompletePathRepresentation
                                                                                           tailFromQueryIncompletePathRepresentation
                                                                                           subtreeWithAllStepsForSamePathStepIndex
                                                                                           pathsForFollowingIndices =
                    let removedIncompletePathWithNewStepInReverse =
                        LazyList.cons headFromQueryIncompletePathRepresentation
                                      removedIncompletePathInReverse
                    continuationWorkflow
                        {
                            let! modifiedSubtreeForFollowingPathStepIndices
                                 , removedIncompletePath =
                                removeFromPaths pathsForFollowingIndices
                                                          tailFromQueryIncompletePathRepresentation
                                                          treeSearchContextParameters.PropagateFromWildcardStepToNextPathIndex
                                                          removedIncompletePathWithNewStepInReverse
                            let modifiedTernarySearchTree =
                                buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees subtreeWithAllStepsForSamePathStepIndex
                                                                                                        modifiedSubtreeForFollowingPathStepIndices
                            return modifiedTernarySearchTree
                                   , removedIncompletePath
                        }

                let removeStepFromBinaryTreeOfAlternateStepsForPathIndex binaryTreeOfStepsForPathIndex
                                                                         stepFromQueryIncompletePathRepresentation
                                                                         tailFromQueryIncompletePathRepresentation =
                    match binaryTreeOfStepsForPathIndex with
                        UnsuccessfulSearchTerminationNode ->
                            if maximumNumberOfPathIndices <= treeSearchContextParameters.PathStepIndex
                            then
                                raise (InternalAssertionViolationException "The path refers to path step indices that are greater than the permitted maximum.")

                            continuationWorkflow.Zero ()
                      | InternalNode internalNodeRepresentation ->
                            let comparisonWrtImplicitStep =
                                compare stepFromQueryIncompletePathRepresentation
                            let {
                                    StepForPathStepIndex = splayedStepForPathStepIndex
                                    PathsForFollowingIndices = splayedPathsForFollowingIndices
                                }
                                , flankingSubtreeWithLesserSteps
                                , flankingSubtreeWithGreaterSteps =
                                splayInternalNodeWithMatchingOrNeighbouringStep internalNodeRepresentation
                                                                                 comparisonWrtImplicitStep
                            match comparisonWrtImplicitStep splayedStepForPathStepIndex with
                                0 ->
                                    buildResultFromInternalNodeModifyingSubtreeForFollowingPathStepIndices tailFromQueryIncompletePathRepresentation
                                                                                                           splayedStepForPathStepIndex
                                                                                                           flankingSubtreeWithLesserSteps
                                                                                                           flankingSubtreeWithGreaterSteps
                                                                                                           splayedPathsForFollowingIndices
                              | _ ->
                                    continuationWorkflow.Zero ()
                let rec removeWildcardStepFromBinaryTreeOfAlternateStepsForPathIndex binaryTreeOfStepsForPathIndex
                                                                                     tailFromQueryIncompletePathRepresentation =
                    match binaryTreeOfStepsForPathIndex with
                        UnsuccessfulSearchTerminationNode ->
                            if maximumNumberOfPathIndices <= treeSearchContextParameters.PathStepIndex
                            then
                                raise (InternalAssertionViolationException "The path refers to path step indices that are greater than the permitted maximum.")

                            continuationWorkflow.Zero ()

                      | InternalNode
                        {
                            StepForPathStepIndex = stepForPathStepIndex
                            SubtreeWithLesserStepsForSamePathStepIndex = subtreeWithLesserStepsForSamePathStepIndex
                            SubtreeWithGreaterStepsForSamePathStepIndex = subtreeWithGreaterStepsForSamePathStepIndex
                            PathsForFollowingIndices = pathsForFollowingIndices
                        } ->
                            buildResultFromInternalNodeModifyingSubtreeForFollowingPathStepIndices tailFromQueryIncompletePathRepresentation
                                                                                                   stepForPathStepIndex
                                                                                                   subtreeWithLesserStepsForSamePathStepIndex
                                                                                                   subtreeWithGreaterStepsForSamePathStepIndex
                                                                                                   pathsForFollowingIndices
                            + continuationWorkflow
                                {
                                    let! modifiedSubtreeWithLesserStepsForSamePathStepIndex
                                         , removedIncompletePath =
                                        removeWildcardStepFromBinaryTreeOfAlternateStepsForPathIndex subtreeWithLesserStepsForSamePathStepIndex
                                                                                                     tailFromQueryIncompletePathRepresentation
                                    let modifiedBinaryTree =
                                        buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees stepForPathStepIndex
                                                                                                                modifiedSubtreeWithLesserStepsForSamePathStepIndex
                                                                                                                subtreeWithGreaterStepsForSamePathStepIndex
                                                                                                                pathsForFollowingIndices
                                    return modifiedBinaryTree
                                           , removedIncompletePath
                                }
                            + continuationWorkflow
                                {
                                    let! modifiedSubtreeWithGreaterStepsForSamePathStepIndex
                                         , removedIncompletePath =
                                        removeWildcardStepFromBinaryTreeOfAlternateStepsForPathIndex subtreeWithGreaterStepsForSamePathStepIndex
                                                                                                     tailFromQueryIncompletePathRepresentation
                                    let modifiedBinaryTree =
                                        buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees stepForPathStepIndex
                                                                                                                subtreeWithLesserStepsForSamePathStepIndex
                                                                                                                modifiedSubtreeWithGreaterStepsForSamePathStepIndex
                                                                                                                pathsForFollowingIndices
                                    return modifiedBinaryTree
                                           , removedIncompletePath
                                }

                match ternarySearchTree
                      , queryIncompletePathRepresentation with
                    SuccessfulSearchTerminationNode
                    , _ ->
                        if maximumNumberOfPathIndices < treeSearchContextParameters.PathStepIndex
                            // NOTE: a subtlety - remember that 'pathStepIndex' can reach 'maximumNumberOfPathIndicesOverall'
                            // for a full (and possibly removed) path, because successful searches go through at least one
                            // node corresponding to each path step index, *then* land on a node indicating whether the search
                            // was successful or not: so the zero-relative index gets incremented one more time.
                        then
                            raise (InternalAssertionViolationException "The path refers to path step indices that are greater than the permitted maximum.")

                        if treeSearchContextParameters.IsCompletePath maximumNumberOfPathIndices
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
                                    for pathStepIndex
                                        , step in mergedIncompletePathRepresentation
                                                               |> List.mapi (fun pathStepIndex
                                                                                 step ->
                                                                                    pathStepIndex
                                                                                    , step) do
                                        match step with
                                            Some step ->
                                                yield pathStepIndex
                                                      , step
                                          | None ->
                                                ()
                                ]
                                |> Map.ofList
                            continuationWorkflow
                                {
                                    if pathIsAcceptable mergedIncompletePath
                                    then
                                        return EmptyTernarySearchTree
                                               , mergedIncompletePathRepresentation
                                    else
                                        return! continuationWorkflow.Zero ()
                                }
                  | WildcardNode
                    {
                        SubtreeWithAllStepsForSamePathStepIndex = subtreeWithAllStepsForSamePathStepIndex
                        PathsForFollowingIndices = pathsForFollowingIndices
                    }
                    , ((Some stepFromQueryIncompletePathRepresentation) as headFromQueryIncompletePathRepresentation) :: tailFromQueryIncompletePathRepresentation ->
                        continuationWorkflow
                            {
                                let! modifiedSubtreeWithAllStepsForSamePathStepIndex
                                     , removedIncompletePath =
                                    removeStepFromBinaryTreeOfAlternateStepsForPathIndex subtreeWithAllStepsForSamePathStepIndex
                                                                                         stepFromQueryIncompletePathRepresentation
                                                                                         tailFromQueryIncompletePathRepresentation
                                let modifiedTernarySearchTree =
                                    buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees modifiedSubtreeWithAllStepsForSamePathStepIndex
                                                                                                            pathsForFollowingIndices
                                return modifiedTernarySearchTree
                                       , removedIncompletePath
                            }
                        + buildResultFromWildcardNodeModifyingSubtreeForFollowingPathStepIndices headFromQueryIncompletePathRepresentation
                                                                                                 tailFromQueryIncompletePathRepresentation
                                                                                                 subtreeWithAllStepsForSamePathStepIndex
                                                                                                 pathsForFollowingIndices
                  | WildcardNode
                    {
                        SubtreeWithAllStepsForSamePathStepIndex = subtreeWithAllStepsForSamePathStepIndex
                        PathsForFollowingIndices = pathsForFollowingIndices
                    }
                    , None :: tailFromQueryIncompletePathRepresentation ->
                        continuationWorkflow
                            {
                                let! modifiedSubtreeWithAllStepsForSamePathStepIndex
                                     , removedIncompletePath =
                                    removeWildcardStepFromBinaryTreeOfAlternateStepsForPathIndex subtreeWithAllStepsForSamePathStepIndex
                                                                                                 tailFromQueryIncompletePathRepresentation
                                let modifiedTernarySearchTree =
                                    buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees modifiedSubtreeWithAllStepsForSamePathStepIndex
                                                                                                            pathsForFollowingIndices
                                return modifiedTernarySearchTree
                                       , removedIncompletePath
                            }
                        + buildResultFromWildcardNodeModifyingSubtreeForFollowingPathStepIndices None
                                                                                                 tailFromQueryIncompletePathRepresentation
                                                                                                 subtreeWithAllStepsForSamePathStepIndex
                                                                                                 pathsForFollowingIndices
                  | BinaryTreeOfAlternateStepsForPathIndex binaryTreeOfStepsForPathIndex
                    , Some stepFromQueryIncompletePathRepresentation :: tailFromQueryIncompletePathRepresentation ->
                        continuationWorkflow
                            {
                                let! modifiedSubtreeWithAllStepsForSamePathStepIndex
                                     , removedIncompletePath =
                                     removeStepFromBinaryTreeOfAlternateStepsForPathIndex binaryTreeOfStepsForPathIndex
                                                                                          stepFromQueryIncompletePathRepresentation
                                                                                          tailFromQueryIncompletePathRepresentation
                                return BinaryTreeOfAlternateStepsForPathIndex modifiedSubtreeWithAllStepsForSamePathStepIndex
                                       , removedIncompletePath
                            }
                  | BinaryTreeOfAlternateStepsForPathIndex binaryTreeOfStepsForPathIndex
                    , None :: tailFromQueryIncompletePathRepresentation ->
                        continuationWorkflow
                            {
                                let! modifiedSubtreeWithAllStepsForSamePathStepIndex
                                     , removedIncompletePath =
                                    removeWildcardStepFromBinaryTreeOfAlternateStepsForPathIndex binaryTreeOfStepsForPathIndex
                                                                                                 tailFromQueryIncompletePathRepresentation
                                return BinaryTreeOfAlternateStepsForPathIndex modifiedSubtreeWithAllStepsForSamePathStepIndex
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
                                          pathStepIndex =
                let numberOfSuccessfulPathsFromSubtreeForFollowingIndices =
                    checkInvariantOfTernarySearchTree branchingRoot
                                                      (pathStepIndex + sharedPathPrefix.Length)
                match numberOfSuccessfulPathsFromSubtreeForFollowingIndices with
                    0 when 0 < ChunkedList.length sharedPathPrefix ->
                        raise (InvariantViolationException "Redundant non-empty shared path prefix with no successful search paths leading through it.")
                  | _ ->
                        numberOfSuccessfulPathsFromSubtreeForFollowingIndices
            and checkInvariantOfTernarySearchTree ternarySearchTree
                                                  pathStepIndex =
                let rec checkInvariantOfBinaryTreeOfAlternateStepsForPathIndex binaryTreeOfStepsForPathIndex
                                                                               lowerBound
                                                                               upperBound =
                    match binaryTreeOfStepsForPathIndex with
                        UnsuccessfulSearchTerminationNode ->
                            if maximumNumberOfPathIndices <= pathStepIndex
                            then
                                raise (InternalAssertionViolationException "The path refers to path step indices that are greater than the permitted maximum.")

                            0
                      | InternalNode
                        {
                            StepForPathStepIndex = stepForPathStepIndex
                            SubtreeWithLesserStepsForSamePathStepIndex = subtreeWithLesserStepsForSamePathStepIndex
                            SubtreeWithGreaterStepsForSamePathStepIndex = subtreeWithGreaterStepsForSamePathStepIndex
                            PathsForFollowingIndices = pathsForFollowingIndices
                        } ->
                            let liftedStep =
                                Finite stepForPathStepIndex
                            if liftedStep >= upperBound
                            then
                                raise (InvariantViolationException "Step is greater than or equal to exclusive upper bound.")
                            if liftedStep <= lowerBound
                            then
                                raise (InvariantViolationException "Step is less than or equal to exclusive lower bound.")
                            let numberOfSuccessfulPathsFromSubtreeWithLesserStepsForSamePathStepIndex =
                                checkInvariantOfBinaryTreeOfAlternateStepsForPathIndex subtreeWithLesserStepsForSamePathStepIndex
                                                                                       lowerBound
                                                                                       liftedStep
                            let numberOfSuccessfulPathsFromSubtreeWithGreaterStepsForSamePathStepIndex =
                                checkInvariantOfBinaryTreeOfAlternateStepsForPathIndex subtreeWithGreaterStepsForSamePathStepIndex
                                                                                       liftedStep
                                                                                       upperBound
                            let numberOfSuccessfulPathsFromPathsForFollowingIndices =
                                checkInvariantOfPaths pathsForFollowingIndices
                                                      (pathStepIndex + 1)
                            match numberOfSuccessfulPathsFromSubtreeWithLesserStepsForSamePathStepIndex
                                  , numberOfSuccessfulPathsFromSubtreeWithGreaterStepsForSamePathStepIndex
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
//                                    if numberOfSuccessfulPathsFromSubtreeWithGreaterStepsForSamePathStepIndex > 1
//                                    then
//                                        Diagnostics.Debug.Print ("Lone greater subtree with: {0} successful paths through it.", numberOfSuccessfulPathsFromSubtreeWithGreaterStepsForSamePathStepIndex)
//                                    numberOfSuccessfulPathsFromSubtreeWithGreaterStepsForSamePathStepIndex
//                                    + numberOfSuccessfulPathsFromSubtreeForFollowingIndices
//                              | _
//                                , 0
//                                , _ ->
//                                    if numberOfSuccessfulPathsFromSubtreeWithLesserStepsForSamePathStepIndex > 1
//                                    then
//                                        Diagnostics.Debug.Print ("Lone lesser subtree with: {0} successful paths through it.", numberOfSuccessfulPathsFromSubtreeWithLesserStepsForSamePathStepIndex)
//                                    numberOfSuccessfulPathsFromSubtreeWithLesserStepsForSamePathStepIndex
//                                    + numberOfSuccessfulPathsFromSubtreeForFollowingIndices
                              | _ ->
                                    numberOfSuccessfulPathsFromSubtreeWithLesserStepsForSamePathStepIndex
                                    + numberOfSuccessfulPathsFromSubtreeWithGreaterStepsForSamePathStepIndex
                                    + numberOfSuccessfulPathsFromPathsForFollowingIndices
                match ternarySearchTree with
                    SuccessfulSearchTerminationNode ->
                        if maximumNumberOfPathIndices < pathStepIndex   // NOTE: a subtlety - remember that 'pathStepIndex' can reach 'maximumNumberOfPathIndicesOverall'
                                                                        // for a full (and possibly removed) path, because successful searches go through at least one
                                                                        // node corresponding to each path step index, *then* land on a node indicating whether the search
                                                                        // was successful or not: so the zero-relative index gets incremented one more time.
                        then
                            raise (InternalAssertionViolationException "The path refers to path step indices that are greater than the permitted maximum.")

                        1
                  | BinaryTreeOfAlternateStepsForPathIndex binaryTreeOfStepsForPathIndex ->
                        checkInvariantOfBinaryTreeOfAlternateStepsForPathIndex binaryTreeOfStepsForPathIndex
                                                                               NegativeInfinity
                                                                               PositiveInfinity
                  | WildcardNode
                    {
                        SubtreeWithAllStepsForSamePathStepIndex = subtreeWithAllStepsForSamePathStepIndex
                        PathsForFollowingIndices = pathsForFollowingIndices
                    } ->
                        let numberOfSuccessfulPathsFromSubtreeWithAllStepsForSamePathStepIndex =
                            checkInvariantOfBinaryTreeOfAlternateStepsForPathIndex subtreeWithAllStepsForSamePathStepIndex
                                                                                   NegativeInfinity
                                                                                   PositiveInfinity
                        match pathsForFollowingIndices with
                            SingleTrivialPath when numberOfSuccessfulPathsFromSubtreeWithAllStepsForSamePathStepIndex > 0 ->
                                raise (LogicErrorException "Found wildcard path that always merges with at least one path using the non-wildcard match - should have been merged.")
                          | _ ->
                            ()
                        let numberOfSuccessfulPathsFromPathsForFollowingIndices =
                            checkInvariantOfPaths pathsForFollowingIndices
                                                  (pathStepIndex + 1)
                        match numberOfSuccessfulPathsFromSubtreeWithAllStepsForSamePathStepIndex
                              , numberOfSuccessfulPathsFromPathsForFollowingIndices with
                            0
                            , 0 ->
                                raise (LogicErrorException "Redundant wildcard node with no successful search paths leading through it.")
                          | _
                            , 0 ->
                                raise (LogicErrorException "Redundant wildcard node that has no successful paths using its wildcard match leading through it.")
                          | _ ->
                                numberOfSuccessfulPathsFromSubtreeWithAllStepsForSamePathStepIndex
                                + numberOfSuccessfulPathsFromPathsForFollowingIndices

            if 0 = checkInvariantOfPaths paths
                                                   0
            then
                raise (LogicErrorException "No successful search paths but tree should be non-empty.")

        member this.NumberOfStepsInACompletePath =
            maximumNumberOfPathIndices

        member this.EnumerationOfMergedPaths revealCompletePathsAgain =
            createIncompletePathSequence revealCompletePathsAgain

        static member Initial (maximumNumberOfPathIndicesOverall,
                               pathIsAcceptable) =
            SetOfMergedPaths<'Step> (SingleTrivialPath,
                                     maximumNumberOfPathIndicesOverall,
                                     pathIsAcceptable)

        static member Initial maximumNumberOfPathIndicesOverall =
            SetOfMergedPaths<'Step> (SingleTrivialPath,
                                     maximumNumberOfPathIndicesOverall,
                                     (fun _ ->
                                        true))

        member this.MergeOrAdd incompletePathRepresentationInExternalForm =
            if Map.isEmpty incompletePathRepresentationInExternalForm
               || (pathIsAcceptable incompletePathRepresentationInExternalForm
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
                    if lengthOfIncompletePathRepresentation > maximumNumberOfPathIndices
                    then
                        raise (InternalAssertionViolationException "The merged incomplete path has more steps than the permitted maximum number of steps.")

                    if lengthOfIncompletePathRepresentation < maximumNumberOfPathIndices
                       || incompletePathRepresentation
                          |> List.exists Option.isNone
                    then
                        None
                    else
                        let stepsForCompletePath =
                            incompletePathRepresentation
                            |> List.map Option.get
                        let completePath =
                            stepsForCompletePath
                            |> List.mapi (fun pathStepIndex
                                              step ->
                                              pathStepIndex
                                              , step)
                            |> Map.ofList
                        if pathIsAcceptable completePath
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
                                                                                            for pathStepIndex
                                                                                                , step in incompletePathRepresentation
                                                                                                                       |> List.mapi (fun pathStepIndex
                                                                                                                                         step ->
                                                                                                                                            pathStepIndex
                                                                                                                                            , step) do
                                                                                                match step with
                                                                                                    Some step ->
                                                                                                        yield pathStepIndex
                                                                                                              , step
                                                                                                  | None ->
                                                                                                        ()
                                                                                        }
                                                                                    |> Map.ofSeq
                                                                                let mergedIncompletePathRepresentationInExternalForm =
                                                                                    externalFormFor mergedIncompletePathRepresentation
                                                                                if mergedIncompletePathRepresentationInExternalForm
                                                                                   |> pathIsAcceptable
                                                                                   |> not
                                                                                then
                                                                                    raise (InternalAssertionViolationException "The merged removed incomplete path should be passed by the filter.")
                                                                                let checkWhatHasBeenMergedInToMake mergedIncompletePathRepresentationInExternalForm =
                                                                                    let whatHasBeenMergedIn =
                                                                                        mergedIncompletePathRepresentationInExternalForm
                                                                                        |> Map.filter (fun pathStepIndex
                                                                                                           _ ->
                                                                                                           Map.containsKey pathStepIndex
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
                                                                                            if pathIsAcceptable (incompletePathBetweenTheQueryAndTheMergeResult
                                                                                                                 |>Map.ofSeq)
                                                                                               |> not
                                                                                            then
                                                                                                pathIsAcceptable mergedIncompletePathRepresentationInExternalForm
                                                                                                |> ignore
                                                                                                pathIsAcceptable (incompletePathBetweenTheQueryAndTheMergeResult
                                                                                                                  |> Map.ofSeq)
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
                                                        maximumNumberOfPathIndices,
                                                        pathIsAcceptable)
                , completePathBeingOfferedNowForEarlyAccess