﻿#nowarn "40"

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
        type InternalNode<'Level when 'Level: comparison> =
            {
                LevelForTestVariableIndex: 'Level
                SubtreeWithLesserLevelsForSameTestVariableIndex: BinaryTreeOfLevelsForTestVariable<'Level>
                SubtreeWithGreaterLevelsForSameTestVariableIndex: BinaryTreeOfLevelsForTestVariable<'Level>
                TestVectorPathsForFollowingIndices: TestVectorPaths<'Level>
            }
        and BinaryTreeOfLevelsForTestVariable<'Level when 'Level: comparison> =
            UnsuccessfulSearchTerminationNode
          | InternalNode of InternalNode<'Level>
        and WildcardNode<'Level when 'Level: comparison> =
            {
                SubtreeWithAllLevelsForSameTestVariableIndex: BinaryTreeOfLevelsForTestVariable<'Level>
                TestVectorPathsForFollowingIndices: TestVectorPaths<'Level>
            }
        and TestVectorPaths<'Level when 'Level: comparison> =
            {
                SharedPathPrefix: ChunkedList<Option<'Level>>
                BranchingRoot: TernarySearchTree<'Level>
            }
        and TernarySearchTree<'Level when 'Level: comparison> =
            SuccessfulSearchTerminationNode
          | WildcardNode of WildcardNode<'Level>
          | BinaryTreeOfLevelsForTestVariable of BinaryTreeOfLevelsForTestVariable<'Level>

        let inline (|EmptyTernarySearchTree|_|) ternarySearchTree =
            match ternarySearchTree with
                BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode ->
                    Some ()
              | _ ->
                    None

        let EmptyTernarySearchTree =
            BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode

        let inline (|NoTestVectorPaths|_|) testVectorPaths =
            match testVectorPaths with
                {
                    SharedPathPrefix = Nil
                    BranchingRoot = EmptyTernarySearchTree
                } ->
                    Some ()
              | _ ->
                    None

        let NoTestVectorPaths =
            {
                SharedPathPrefix = Nil
                BranchingRoot = EmptyTernarySearchTree
            }

        let inline (|SingleTrivialPath|_|) testVectorPaths =
            match testVectorPaths with
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
                    LevelForTestVariableIndex = levelForTestVariableIndex
                    SubtreeWithLesserLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                    SubtreeWithGreaterLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                    TestVectorPathsForFollowingIndices = testVectorPathsForFollowingIndices
                }) ->
                    {
                        testVectorPathsForFollowingIndices with
                            SharedPathPrefix =
                                Cons (Some levelForTestVariableIndex
                                      , testVectorPathsForFollowingIndices.SharedPathPrefix)
                    }
                    |> Some
              | _ ->
                    None

        let inline (|BranchingWithJustWildcardForLeadingTestVariable|_|) ternarySearchTree =
            match ternarySearchTree with
                WildcardNode
                {
                    SubtreeWithAllLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                    TestVectorPathsForFollowingIndices = testVectorPathsForFollowingIndices
                } ->
                    {
                        testVectorPathsForFollowingIndices with
                            SharedPathPrefix =
                                Cons (None
                                      , testVectorPathsForFollowingIndices.SharedPathPrefix)
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

        type InternalNode<'Level when 'Level: comparison> with
            member this.NumberOfLevelsForLeadingTestVariable =
                match !this.CacheOfNumberOfLevelsForLeadingTestVariable with
                    None ->
                        let numberOfLevelsForLeadingTestVariable =
                            match this with
                                {
                                    SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                                    SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                                    TestVectorPathsForFollowingIndices
                                        = NoTestVectorPaths
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

        and BinaryTreeOfLevelsForTestVariable<'Level when 'Level: comparison> with
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

            member this.IsSpecialCaseDenotingInitialState =   // The tests define this special state as not possessing any test vectors, not even the trivial empty one.
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

        type SharedPathPrefixMatchResult<'Level> =
            AgreesWithPrefixOfIncompletePath of List<Option<'Level>> * List<Option<'Level>>  // The prefix of partial test vector representation that agrees
                                                                                                // and the remainder of the partial test vector representation.
          | ShortenedPrefixAgreesWithEntireIncompletePath of Int32 * List<Option<'Level>>    // Length of partial test vector representation and
                                                                                                // the partial test vector representation itself.
          | CompleteMismatch of Int32 * List<Option<'Level>> * List<Option<'Level>> // Index of mismatch in the shared path prefix,
                                                                                    // the prefix of partial test vector representation that agrees
                                                                                    // and the remainder of the partial test vector representation
                                                                                    // including the mismatching level at the head.

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
                        // Degenerate root node only case (level has been found)...
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
                                        // Zig-only case (either the level has been found, or found least upper bound instead)...
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
                                // Degenerate root node only case (level has not been found, found least upper bound instead)...
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

    type SetOfMergedPaths<'Level when 'Level: comparison>(testVectorPaths: TestVectorPaths<'Level>,
                                                                                maximumNumberOfTestVariables: Int32,
                                                                                testVectorIsAcceptable: seq<Int32 * 'Level> -> Boolean) =
        let createIncompletePathSequence revealCompletePathsAgain =
            let rec traverseTestVectorPaths {
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
                                                    Some levelForTestVariableIndex ->
                                                        treeSearchContextParameters.PropagateFromDefinedLevelToNextTestVariable
                                                        , ((treeSearchContextParameters.TestVariableIndex, levelForTestVariableIndex) :: incompletePathBeingBuilt)
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
                                raise (InternalAssertionViolationException "The test vector refers to test variable indices that are greater than the permitted maximum.")

                            Seq.empty
                      | (InternalNode
                        {
                            LevelForTestVariableIndex = levelForTestVariableIndex
                            SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                            SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                            TestVectorPathsForFollowingIndices = testVectorPathsForFollowingIndices
                        }) ->
                            Seq.delay (fun() ->
                                        seq
                                            {
                                                yield! traverseBinaryTreeOfLevelsForTestVariable subtreeWithLesserLevelsForSameTestVariableIndex
                                                yield! traverseTestVectorPaths testVectorPathsForFollowingIndices
                                                                               treeSearchContextParameters.PropagateFromDefinedLevelToNextTestVariable
                                                                               ((treeSearchContextParameters.TestVariableIndex, levelForTestVariableIndex) :: incompletePathBeingBuilt)
                                                yield! traverseBinaryTreeOfLevelsForTestVariable subtreeWithGreaterLevelsForSameTestVariableIndex
                                            })
                match ternarySearchTree with
                    SuccessfulSearchTerminationNode ->
                        if maximumNumberOfTestVariables < treeSearchContextParameters.TestVariableIndex
                            // NOTE: a subtlety - remember that 'testVariableIndex' can reach 'maximumNumberOfTestVariablesOverall'
                            // for a full (and possibly removed) test vector, because successful searches go through at least one
                            // node corresponding to each test variable index, *then* land on a node indicating whether the search
                            // was successful or not: so the zero-relative index gets incremented one more time.
                        then
                            raise (InternalAssertionViolationException "The test vector refers to test variable indices that are greater than the permitted maximum.")

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
                                raise (InternalAssertionViolationException "The test vector has more entries than the permitted maximum number of test variables.")

                            let incompletePath =
                                incompletePathBeingBuilt
                                |> Map.ofList

                            if testVectorIsAcceptable (Map.toSeq incompletePath)
                               |> not
                            then
                                raise (InternalAssertionViolationException "The test vector is not acceptable to the filter - it should neither have been added nor formed by merging.")

                            // NOTE: as we are converting to a map, we can be cavalier about the
                            // order in which associative pairs are added to the partial test vector.
                            Seq.singleton incompletePath
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        TestVectorPathsForFollowingIndices = testVectorPathsForFollowingIndices
                    } ->
                        Seq.delay (fun () ->
                                    seq
                                        {
                                            yield! traverseBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                            yield! traverseTestVectorPaths testVectorPathsForFollowingIndices
                                                                           treeSearchContextParameters.PropagateFromWildcardLevelToNextTestVariable
                                                                           incompletePathBeingBuilt
                                        })
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable ->
                        traverseBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
            traverseTestVectorPaths testVectorPaths
                                    TreeSearchContextParameters.StartOfSearch
                                    []

        let fillOutIncompletePathWithIndeterminates incompletePathRepresentation =
            if (incompletePathRepresentation: Map<_, _>).Count > maximumNumberOfTestVariables
            then
                raise (InternalAssertionViolationException "The partial test vector being either merged or added has more entries than the permitted maximum number of test variables.")

            let testVariableIndicesHavingLevels =
                incompletePathRepresentation
                |> Seq.map (fun keyValuePair -> keyValuePair.Key)
                |> Set.ofSeq

            let maximumTestVariableIndexHavingLevel =
                Seq.max testVariableIndicesHavingLevels

            if maximumTestVariableIndexHavingLevel >= maximumNumberOfTestVariables
            then
                raise (PreconditionViolationException "The partial test vector being either merged or added has a test variable index that is greater than the permitted maximum.")

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
                    |> Map.map (fun _ level ->
                                    Some level)
                    |> Map.toList

                let mergedAssociationList =
                    BargainBasement.MergeDisjointSortedAssociationLists sortedAssociationListFromTestVariableIndicesToLevels
                                                                        sortedAssociationListFromTestVariableIndicesToIndeterminateMarkers

                mergedAssociationList
                |> List.map snd
                , false


        let add testVectorPaths
                newIncompletePathRepresentation =
            let justOnePathFrom incompletePathRepresentation =
                {
                    SharedPathPrefix =
                        incompletePathRepresentation
                        |> ChunkedList.ofList
                    BranchingRoot =
                        SuccessfulSearchTerminationNode
                }
            let rec addToTestVectorPaths ({
                                            SharedPathPrefix = sharedPathPrefix
                                            BranchingRoot = branchingRoot
                                         } as testVectorPaths)
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
                            let testVectorPathsAfterMismatch =
                                {
                                    testVectorPaths with
                                        SharedPathPrefix = sharedPathPrefix.[1 + indexOfMismatchOnSharedPathStep ..]
                                }
                            let sharedPathPrefixSplit =
                                match sharedPathPrefix.[indexOfMismatchOnSharedPathStep] with
                                    Some levelFromMismatchingSharedPathStep ->
                                        {
                                            LevelForTestVariableIndex = levelFromMismatchingSharedPathStep
                                            SubtreeWithLesserLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                            SubtreeWithGreaterLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                            TestVectorPathsForFollowingIndices = testVectorPathsAfterMismatch
                                        }
                                        |> InternalNode
                                        |> BinaryTreeOfLevelsForTestVariable
                                  | None ->
                                        {
                                            SubtreeWithAllLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                            TestVectorPathsForFollowingIndices = testVectorPathsAfterMismatch
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
                            raise (InternalAssertionViolationException "Attempt to add a new partial test vector representation that is a prefix of a previous one.")
            and addToTernarySearchTree ternarySearchTree
                                       newIncompletePathRepresentation =
                let rec addLevelToBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                    levelFromNewIncompletePathRepresentation
                                                                    tailFromNewIncompletePathRepresentation =
                    match binaryTreeOfLevelsForTestVariable with
                        UnsuccessfulSearchTerminationNode ->
                            {
                                LevelForTestVariableIndex = levelFromNewIncompletePathRepresentation
                                SubtreeWithLesserLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                SubtreeWithGreaterLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                TestVectorPathsForFollowingIndices =
                                    justOnePathFrom tailFromNewIncompletePathRepresentation
                            }
                            |> InternalNode
                      | (InternalNode internalNodeRepresentation) ->
                            let comparisonWrtImplicitLevel =
                                compare levelFromNewIncompletePathRepresentation
                            let ({
                                    LevelForTestVariableIndex = splayedLevelForTestVariableIndex
                                    TestVectorPathsForFollowingIndices = splayedTestVectorPathsForFollowingIndices
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
                                        LevelForTestVariableIndex = levelFromNewIncompletePathRepresentation
                                        SubtreeWithLesserLevelsForSameTestVariableIndex = flankingSubtreeWithLesserLevels
                                        SubtreeWithGreaterLevelsForSameTestVariableIndex = flankingSubtreeWithGreaterLevels
                                        TestVectorPathsForFollowingIndices =
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
                                        LevelForTestVariableIndex = levelFromNewIncompletePathRepresentation
                                        SubtreeWithLesserLevelsForSameTestVariableIndex = flankingSubtreeWithLesserLevels
                                        SubtreeWithGreaterLevelsForSameTestVariableIndex = flankingSubtreeWithGreaterLevels
                                        TestVectorPathsForFollowingIndices =
                                            justOnePathFrom tailFromNewIncompletePathRepresentation
                                    }
                                    |> InternalNode
                              | _ ->
                                    let modifiedTestVectorPathsForFollowingIndices =
                                        addToTestVectorPaths splayedTestVectorPathsForFollowingIndices
                                                             tailFromNewIncompletePathRepresentation
                                    {
                                        splayedInternalNodeRepresentation with
                                            SubtreeWithLesserLevelsForSameTestVariableIndex = flankingSubtreeWithLesserLevels
                                            SubtreeWithGreaterLevelsForSameTestVariableIndex = flankingSubtreeWithGreaterLevels
                                            TestVectorPathsForFollowingIndices = modifiedTestVectorPathsForFollowingIndices
                                    }
                                    |> InternalNode
                match ternarySearchTree
                      , newIncompletePathRepresentation with
                  | SuccessfulSearchTerminationNode
                    , _ ->
                        raise (InternalAssertionViolationException "Attempt to add a new partial test vector representation that has a previous one as a prefix or is the same as it.")
                        // The above is really a precondition violation, but the precondition should have been enforced at a higher level within the implementation and not by the client.
                  | WildcardNode
                    ({
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                     } as wildcardNodeRepresentation)
                    , Some levelFromNewIncompletePathRepresentation :: tailFromNewIncompletePathRepresentation ->
                        let modifiedSubtreeWithAllLevelsForSameTestVariableIndex =
                            addLevelToBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                                                        levelFromNewIncompletePathRepresentation
                                                                        tailFromNewIncompletePathRepresentation
                        {
                            wildcardNodeRepresentation with
                                SubtreeWithAllLevelsForSameTestVariableIndex = modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                        }
                        |> WildcardNode
                  | WildcardNode
                    ({
                        TestVectorPathsForFollowingIndices = testVectorPathsForFollowingIndices
                     } as wildcardNodeRepresentation)
                    , None :: tailFromNewIncompletePathRepresentation ->
                        let modifiedTestVectorPathsForFollowingIndices =
                            addToTestVectorPaths testVectorPathsForFollowingIndices
                                                 tailFromNewIncompletePathRepresentation
                        {
                            wildcardNodeRepresentation with
                                TestVectorPathsForFollowingIndices = modifiedTestVectorPathsForFollowingIndices
                        }
                        |> WildcardNode
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                    , Some levelFromNewIncompletePathRepresentation :: tailFromNewIncompletePathRepresentation ->
                        addLevelToBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                    levelFromNewIncompletePathRepresentation
                                                                    tailFromNewIncompletePathRepresentation
                        |> BinaryTreeOfLevelsForTestVariable
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                    , None :: tailFromNewIncompletePathRepresentation ->
                        {
                            SubtreeWithAllLevelsForSameTestVariableIndex = binaryTreeOfLevelsForTestVariable
                            TestVectorPathsForFollowingIndices =
                                justOnePathFrom tailFromNewIncompletePathRepresentation
                        }
                        |> WildcardNode
                  | _
                    , [] ->
                        raise (InternalAssertionViolationException "Attempt to add a new partial test vector representation that is already mergeable with a previous one.")
                        // The above is really a precondition violation, but the precondition should have been enforced at a higher level within the implementation and not by the client.
            addToTestVectorPaths testVectorPaths
                                 newIncompletePathRepresentation

        let remove testVectorPaths
                   queryIncompletePathRepresentation
                   existingCompletePathBlockedRemovalContinuation =
            let removeInternalNodeWithGreatestLevelInSubtree subtreeInternalNodeRepresentation =
                let comparisonWrtPositiveInfinity _ =
                    1
                let {
                        LevelForTestVariableIndex = splayedLevelForTestVariableIndex
                        TestVectorPathsForFollowingIndices = splayedTestVectorPathsForFollowingIndices
                    }
                    , flankingSubtreeWithLesserLevels
                    , _ =
                    splayInternalNodeWithMatchingOrNeighbouringLevel subtreeInternalNodeRepresentation
                                                                     comparisonWrtPositiveInfinity
                splayedLevelForTestVariableIndex
                , splayedTestVectorPathsForFollowingIndices
                , flankingSubtreeWithLesserLevels
            let removeInternalNodeWithLeastLevelInSubtree subtreeInternalNodeRepresentation =
                let comparisonWrtNegativeInfinity _ =
                    -1
                let {
                        LevelForTestVariableIndex = splayedLevelForTestVariableIndex
                        TestVectorPathsForFollowingIndices = splayedTestVectorPathsForFollowingIndices
                    }
                    , _
                    , flankingSubtreeWithGreaterLevels =
                    splayInternalNodeWithMatchingOrNeighbouringLevel subtreeInternalNodeRepresentation
                                                                     comparisonWrtNegativeInfinity
                splayedLevelForTestVariableIndex
                , splayedTestVectorPathsForFollowingIndices
                , flankingSubtreeWithGreaterLevels
            let buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                        subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                        subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                        testVectorPathsForFollowingIndices =
                match subtreeWithLesserLevelsForSameTestVariableIndex
                      , subtreeWithGreaterLevelsForSameTestVariableIndex
                      , testVectorPathsForFollowingIndices with
                  | UnsuccessfulSearchTerminationNode
                    , UnsuccessfulSearchTerminationNode
                    , NoTestVectorPaths ->
                        UnsuccessfulSearchTerminationNode
                  | UnsuccessfulSearchTerminationNode
                    , _
                    , NoTestVectorPaths ->
                        subtreeWithGreaterLevelsForSameTestVariableIndex
                  | _
                    , UnsuccessfulSearchTerminationNode
                    , NoTestVectorPaths ->
                        subtreeWithLesserLevelsForSameTestVariableIndex
                  | InternalNode subtreeWithLesserLevelsForSameTestVariableIndexInternalNodeRepresentation
                    , InternalNode subtreeWithGreaterLevelsForSameTestVariableIndexInternalNodeRepresentation
                    , NoTestVectorPaths ->
                        if subtreeWithLesserLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                           > subtreeWithGreaterLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                        then
                            let levelForTestVariableIndexFromRemovedNode
                                , testVectorPathsForFollowingIndicesFromRemovedNode
                                , subtreeWithLesserLevelsForSameTestVariableIndexWithoutThatNode =
                                    removeInternalNodeWithGreatestLevelInSubtree subtreeWithLesserLevelsForSameTestVariableIndexInternalNodeRepresentation
                            ({
                                LevelForTestVariableIndex =
                                    levelForTestVariableIndexFromRemovedNode
                                SubtreeWithLesserLevelsForSameTestVariableIndex =
                                    subtreeWithLesserLevelsForSameTestVariableIndexWithoutThatNode
                                SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                    subtreeWithGreaterLevelsForSameTestVariableIndex
                                TestVectorPathsForFollowingIndices =
                                    testVectorPathsForFollowingIndicesFromRemovedNode
                            }
                            |> InternalNode)
                        else
                            let levelForTestVariableIndexFromRemovedNode
                                , testVectorPathsForFollowingIndicesFromRemovedNode
                                , subtreeWithGreaterLevelsForSameTestVariableIndexWithoutThatNode =
                                    removeInternalNodeWithLeastLevelInSubtree subtreeWithGreaterLevelsForSameTestVariableIndexInternalNodeRepresentation
                            ({
                                LevelForTestVariableIndex =
                                    levelForTestVariableIndexFromRemovedNode
                                SubtreeWithLesserLevelsForSameTestVariableIndex =
                                    subtreeWithLesserLevelsForSameTestVariableIndex
                                SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                    subtreeWithGreaterLevelsForSameTestVariableIndexWithoutThatNode
                                TestVectorPathsForFollowingIndices =
                                    testVectorPathsForFollowingIndicesFromRemovedNode
                            }
                            |> InternalNode)
                  | _ ->
                        ({
                            LevelForTestVariableIndex =
                                levelForTestVariableIndex
                            SubtreeWithLesserLevelsForSameTestVariableIndex =
                                subtreeWithLesserLevelsForSameTestVariableIndex
                            SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                subtreeWithGreaterLevelsForSameTestVariableIndex
                            TestVectorPathsForFollowingIndices =
                                testVectorPathsForFollowingIndices
                        }
                        |> InternalNode)
            let buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees subtreeWithAllLevelsForSameTestVariableIndex
                                                                                        testVectorPathsForFollowingIndices =
                match subtreeWithAllLevelsForSameTestVariableIndex
                      , testVectorPathsForFollowingIndices with
                    UnsuccessfulSearchTerminationNode
                    , NoTestVectorPaths ->
                        EmptyTernarySearchTree
                  | _
                    , NoTestVectorPaths ->
                        BinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                  | _ ->
                        {
                            SubtreeWithAllLevelsForSameTestVariableIndex =
                                subtreeWithAllLevelsForSameTestVariableIndex
                            TestVectorPathsForFollowingIndices =
                                testVectorPathsForFollowingIndices
                        }
                        |> WildcardNode
            let rec removeFromTestVectorPaths {
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
                                                Some levelForTestVariableIndex ->
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
                                    return NoTestVectorPaths
                                           , removedIncompletePath
                              | BranchingWithSingleLevelForLeadingTestVariable modifiedTestVectorPathsEquivalentToBranchingRoot ->
                                    return {
                                                modifiedTestVectorPathsEquivalentToBranchingRoot with
                                                    SharedPathPrefix =
                                                        ChunkedList.append sharedPathPrefix
                                                                           modifiedTestVectorPathsEquivalentToBranchingRoot.SharedPathPrefix
                                           }
                                           , removedIncompletePath
                              | BranchingWithJustWildcardForLeadingTestVariable modifiedTestVectorPathsEquivalentToBranchingRoot ->
                                    return {
                                                modifiedTestVectorPathsEquivalentToBranchingRoot with
                                                    SharedPathPrefix =
                                                        ChunkedList.append sharedPathPrefix
                                                                           modifiedTestVectorPathsEquivalentToBranchingRoot.SharedPathPrefix
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
                                                                                               levelForTestVariableIndex
                                                                                               subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                               subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                               testVectorPathsForFollowingIndices =
                    let removedIncompletePathWithNewLevelInReverse =
                        LazyList.cons (Some levelForTestVariableIndex)
                                      removedIncompletePathInReverse
                    continuationWorkflow
                        {
                            let! modifiedSubtreeForFollowingTestVariableIndices
                                 , removedIncompletePath =
                                removeFromTestVectorPaths testVectorPathsForFollowingIndices
                                                          tailFromQueryIncompletePathRepresentation
                                                          treeSearchContextParameters.PropagateFromDefinedLevelToNextTestVariable
                                                          removedIncompletePathWithNewLevelInReverse
                            let modifiedBinarySearchTree =
                                buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                        subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                        subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                        modifiedSubtreeForFollowingTestVariableIndices
                            return modifiedBinarySearchTree
                                   , removedIncompletePath

                        }
                let buildResultFromWildcardNodeModifyingSubtreeForFollowingTestVariableIndices headFromQueryIncompletePathRepresentation
                                                                                               tailFromQueryIncompletePathRepresentation
                                                                                               subtreeWithAllLevelsForSameTestVariableIndex
                                                                                               testVectorPathsForFollowingIndices =
                    let removedIncompletePathWithNewLevelInReverse =
                        LazyList.cons headFromQueryIncompletePathRepresentation
                                      removedIncompletePathInReverse
                    continuationWorkflow
                        {
                            let! modifiedSubtreeForFollowingTestVariableIndices
                                 , removedIncompletePath =
                                removeFromTestVectorPaths testVectorPathsForFollowingIndices
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
                                                                     levelFromQueryIncompletePathRepresentation
                                                                     tailFromQueryIncompletePathRepresentation =
                    match binaryTreeOfLevelsForTestVariable with
                        UnsuccessfulSearchTerminationNode ->
                            if maximumNumberOfTestVariables <= treeSearchContextParameters.TestVariableIndex
                            then
                                raise (InternalAssertionViolationException "The test vector refers to test variable indices that are greater than the permitted maximum.")

                            continuationWorkflow.Zero ()
                      | InternalNode internalNodeRepresentation ->
                            let comparisonWrtImplicitLevel =
                                compare levelFromQueryIncompletePathRepresentation
                            let {
                                    LevelForTestVariableIndex = splayedLevelForTestVariableIndex
                                    TestVectorPathsForFollowingIndices = splayedTestVectorPathsForFollowingIndices
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
                                                                                                               splayedTestVectorPathsForFollowingIndices
                              | _ ->
                                    continuationWorkflow.Zero ()
                let rec removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                                 tailFromQueryIncompletePathRepresentation =
                    match binaryTreeOfLevelsForTestVariable with
                        UnsuccessfulSearchTerminationNode ->
                            if maximumNumberOfTestVariables <= treeSearchContextParameters.TestVariableIndex
                            then
                                raise (InternalAssertionViolationException "The test vector refers to test variable indices that are greater than the permitted maximum.")

                            continuationWorkflow.Zero ()

                      | InternalNode
                        {
                            LevelForTestVariableIndex = levelForTestVariableIndex
                            SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                            SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                            TestVectorPathsForFollowingIndices = testVectorPathsForFollowingIndices
                        } ->
                            buildResultFromInternalNodeModifyingSubtreeForFollowingTestVariableIndices tailFromQueryIncompletePathRepresentation
                                                                                                       levelForTestVariableIndex
                                                                                                       subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                       subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                       testVectorPathsForFollowingIndices
                            + continuationWorkflow
                                {
                                    let! modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                         , removedIncompletePath =
                                        removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                 tailFromQueryIncompletePathRepresentation
                                    let modifiedBinaryTree =
                                        buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                                modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                testVectorPathsForFollowingIndices
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
                                        buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                                subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                testVectorPathsForFollowingIndices
                                    return modifiedBinaryTree
                                           , removedIncompletePath
                                }

                match ternarySearchTree
                      , queryIncompletePathRepresentation with
                    SuccessfulSearchTerminationNode
                    , _ ->
                        if maximumNumberOfTestVariables < treeSearchContextParameters.TestVariableIndex
                            // NOTE: a subtlety - remember that 'testVariableIndex' can reach 'maximumNumberOfTestVariablesOverall'
                            // for a full (and possibly removed) test vector, because successful searches go through at least one
                            // node corresponding to each test variable index, *then* land on a node indicating whether the search
                            // was successful or not: so the zero-relative index gets incremented one more time.
                        then
                            raise (InternalAssertionViolationException "The test vector refers to test variable indices that are greater than the permitted maximum.")

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
                        TestVectorPathsForFollowingIndices = testVectorPathsForFollowingIndices
                    }
                    , ((Some levelFromQueryIncompletePathRepresentation) as headFromQueryIncompletePathRepresentation) :: tailFromQueryIncompletePathRepresentation ->
                        continuationWorkflow
                            {
                                let! modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                     , removedIncompletePath =
                                    removeLevelFromBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                                                                     levelFromQueryIncompletePathRepresentation
                                                                                     tailFromQueryIncompletePathRepresentation
                                let modifiedTernarySearchTree =
                                    buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                                                                                            testVectorPathsForFollowingIndices
                                return modifiedTernarySearchTree
                                       , removedIncompletePath
                            }
                        + buildResultFromWildcardNodeModifyingSubtreeForFollowingTestVariableIndices headFromQueryIncompletePathRepresentation
                                                                                                     tailFromQueryIncompletePathRepresentation
                                                                                                     subtreeWithAllLevelsForSameTestVariableIndex
                                                                                                     testVectorPathsForFollowingIndices
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        TestVectorPathsForFollowingIndices = testVectorPathsForFollowingIndices
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
                                                                                                            testVectorPathsForFollowingIndices
                                return modifiedTernarySearchTree
                                       , removedIncompletePath
                            }
                        + buildResultFromWildcardNodeModifyingSubtreeForFollowingTestVariableIndices None
                                                                                                     tailFromQueryIncompletePathRepresentation
                                                                                                     subtreeWithAllLevelsForSameTestVariableIndex
                                                                                                     testVectorPathsForFollowingIndices
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                    , Some levelFromQueryIncompletePathRepresentation :: tailFromQueryIncompletePathRepresentation ->
                        continuationWorkflow
                            {
                                let! modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                     , removedIncompletePath =
                                     removeLevelFromBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                                      levelFromQueryIncompletePathRepresentation
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
                        // This has the effect of padding out a query partial test vector on the fly, thereby
                        // allowing a match as a prefix of some suitable stored vector, should one already be present.
                        removeFromTernarySearchTree ternarySearchTree
                                                    [None]
                                                    treeSearchContextParameters
                                                    removedIncompletePathInReverse
            removeFromTestVectorPaths testVectorPaths
                                      queryIncompletePathRepresentation
                                      TreeSearchContextParameters.StartOfSearch
                                      LazyList.empty

        let checkInvariant testVectorPaths =
            let rec checkInvariantOfTestVectorPaths {
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
                                raise (InternalAssertionViolationException "The test vector refers to test variable indices that are greater than the permitted maximum.")

                            0
                      | InternalNode
                        {
                            LevelForTestVariableIndex = levelForTestVariableIndex
                            SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                            SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                            TestVectorPathsForFollowingIndices = testVectorPathsForFollowingIndices
                        } ->
                            let liftedLevel =
                                Finite levelForTestVariableIndex
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
                            let numberOfSuccessfulPathsFromTestVectorPathsForFollowingIndices =
                                checkInvariantOfTestVectorPaths testVectorPathsForFollowingIndices
                                                                (testVariableIndex + 1)
                            match numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex
                                  , numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex
                                  , numberOfSuccessfulPathsFromTestVectorPathsForFollowingIndices with
                                0
                                , 0
                                , 0 ->
                                    raise (InvariantViolationException "Redundant internal node with no successful search paths leading through it.")
                              | _
                                , 0
                                , 0 ->
                                    raise (InvariantViolationException "Redundant internal node with all successful search paths leading via subtree for lesser levels.")
                              | 0
                                , _
                                , 0 ->
                                    raise (InvariantViolationException "Redundant internal node with all successful search paths leading via subtree for greater levels.")
                              | _
                                , _
                                , 0 ->
                                    raise (InvariantViolationException "Redundant internal node with its own 'ghost' level that participates in no successful search paths.")
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
                                    + numberOfSuccessfulPathsFromTestVectorPathsForFollowingIndices
                match ternarySearchTree with
                    SuccessfulSearchTerminationNode ->
                        if maximumNumberOfTestVariables < testVariableIndex  // NOTE: a subtlety - remember that 'testVariableIndex' can reach 'maximumNumberOfTestVariablesOverall'
                                                                             // for a full (and possibly removed) test vector, because successful searches go through at least one
                                                                             // node corresponding to each test variable index, *then* land on a node indicating whether the search
                                                                             // was successful or not: so the zero-relative index gets incremented one more time.
                        then
                            raise (InternalAssertionViolationException "The test vector refers to test variable indices that are greater than the permitted maximum.")

                        1
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable ->
                        checkInvariantOfBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                          NegativeInfinity
                                                                          PositiveInfinity
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        TestVectorPathsForFollowingIndices = testVectorPathsForFollowingIndices
                    } ->
                        let numberOfSuccessfulPathsFromSubtreeWithAllLevelsForSameTestVariableIndex =
                            checkInvariantOfBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                                                              NegativeInfinity
                                                                              PositiveInfinity
                        match testVectorPathsForFollowingIndices with
                            SingleTrivialPath when numberOfSuccessfulPathsFromSubtreeWithAllLevelsForSameTestVariableIndex > 0 ->
                                raise (LogicErrorException "Found wildcard path that always merges with at least one path using the non-wildcard match - should have been merged.")
                          | _ ->
                            ()
                        let numberOfSuccessfulPathsFromTestVectorPathsForFollowingIndices =
                            checkInvariantOfTestVectorPaths testVectorPathsForFollowingIndices
                                                            (testVariableIndex + 1)
                        match numberOfSuccessfulPathsFromSubtreeWithAllLevelsForSameTestVariableIndex
                              , numberOfSuccessfulPathsFromTestVectorPathsForFollowingIndices with
                            0
                            , 0 ->
                                raise (LogicErrorException "Redundant wildcard node with no successful search paths leading through it.")
                          | _
                            , 0 ->
                                raise (LogicErrorException "Redundant wildcard node that has no successful paths using its wildcard match leading through it.")
                          | _ ->
                                numberOfSuccessfulPathsFromSubtreeWithAllLevelsForSameTestVariableIndex
                                + numberOfSuccessfulPathsFromTestVectorPathsForFollowingIndices

            if 0 = checkInvariantOfTestVectorPaths testVectorPaths
                                                   0
            then
                raise (LogicErrorException "No successful search paths but tree should be non-empty.")

        member this.MaximumNumberOfTestVariables =
            maximumNumberOfTestVariables

        member this.EnumerationOfMergedTestVectors revealCompletePathsAgain =
            createIncompletePathSequence revealCompletePathsAgain

        static member Initial maximumNumberOfTestVariablesOverall
                              testVectorIsAcceptable =
            SetOfMergedPaths<'Level> (SingleTrivialPath,
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
                        raise (InternalAssertionViolationException "The merged partial test vector has more entries than the permitted maximum number of test variables.")

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
                            raise (InternalAssertionViolationException "The merged full test vector is not acceptable to the filter.")
                        Some completePath
                let modifiedTestVectorPaths
                    , completePathBeingOfferedNowForEarlyAccess =
                    ContinuationMonad<_, _>.CallCC ((fun () ->
                                                        continuationWorkflow
                                                            {
                                                                return testVectorPaths
                                                                       , None
                                                            }),
                                                    (fun existingCompletePathBlockedRemovalContinuation ->
                                                        continuationWorkflow
                                                            {
                                                                let! testVectorPathsWithoutMergeCandidate
                                                                     , mergedIncompletePathRepresentation =
                                                                    remove testVectorPaths
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
                                                                                raise (InternalAssertionViolationException "The merged removed partial test vector should be as least as long as the original.")
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
                                                                                                raise (InternalAssertionViolationException "The merged removed partial test vector is not acceptable to the filter."))
                                                                            let! _
                                                                                , remergedIncompletePathRepresentation =
                                                                                remove testVectorPathsWithoutMergeCandidate
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
                                                                                    raise (InternalAssertionViolationException "The merged removed partial test vector should be passed by the filter.")
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

                                                                                raise (InternalAssertionViolationException "The merged removed partial test vector still matches with something left behind!")
                                                                        }
                                                                    + continuationWorkflow
                                                                        {
                                                                            return ()
                                                                        }

                                                                return add testVectorPathsWithoutMergeCandidate
                                                                           mergedIncompletePathRepresentation
                                                                       , detectAndConvertCompletePath mergedIncompletePathRepresentation
                                                            }))
                    + continuationWorkflow
                        {
                            let modifiedTestVectorPaths =
                                add testVectorPaths
                                    incompletePathRepresentation

                            if not incompletePathRepresentationIsActuallyAlreadyFull
                            then
                                let! _
                                     , shouldBeIdenticalToWhatWasAdded =
                                        remove modifiedTestVectorPaths
                                               incompletePathRepresentation
                                               (fun _ -> raise (InternalAssertionViolationException "This should not be called."))

                                if shouldBeIdenticalToWhatWasAdded <> incompletePathRepresentation
                                then
                                    raise (InternalAssertionViolationException "Adding an unmergeable partial test vector has caused it to change state from the original.")

                            return modifiedTestVectorPaths
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
                    checkInvariant modifiedTestVectorPaths
                    // ... end of invariant check.

                SetOfMergedPaths (modifiedTestVectorPaths,
                                                        maximumNumberOfTestVariables,
                                                        testVectorIsAcceptable)
                , completePathBeingOfferedNowForEarlyAccess