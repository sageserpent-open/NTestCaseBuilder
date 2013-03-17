#nowarn "40"

namespace NTestCaseBuilder

    open System.Collections
    open System.Collections.Generic
    open System
    open SageSerpent.Infrastructure
    open SageSerpent.Infrastructure.OptionWorkflow
    open SageSerpent.Infrastructure.OptionExtensions
    open SageSerpent.Infrastructure.RandomExtensions
    open SageSerpent.Infrastructure.ContinuationWorkflow
    open Microsoft.FSharp.Collections

    module MergedPartialTestVectorRepresentationsDetail =
        type AugmentedInternalNode<'Level when 'Level: comparison> (internalNode: InternalNode<'Level>) =
            let numberOfLevelsForLeadingTestVariable =
                match internalNode with
                    {
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        UniquePathForFollowingIndices
                            =   {
                                    Steps = _   // This is slightly defensive - the invariant is stronger in practice; it states
                                                // that any subtree that is just an unsuccessful unique path should be pruned back
                                                // so that its unique prefix is empty.
                                    SubtreeForFollowingIndices = BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode
                                }
                    } ->
                        subtreeWithLesserLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                        + subtreeWithGreaterLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                  | {
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                    } ->
                        1u
                        + subtreeWithLesserLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                        + subtreeWithGreaterLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable

            member this.InternalNode =
                internalNode

            member this.NumberOfLevelsForLeadingTestVariable =
                numberOfLevelsForLeadingTestVariable

        and InternalNode<'Level when 'Level: comparison> =
            {
                LevelForTestVariableIndex: 'Level
                SubtreeWithLesserLevelsForSameTestVariableIndex: BinaryTreeOfLevelsForTestVariable<'Level>
                SubtreeWithGreaterLevelsForSameTestVariableIndex: BinaryTreeOfLevelsForTestVariable<'Level>
                UniquePathForFollowingIndices: UniquePath<'Level>
            }
        and BinaryTreeOfLevelsForTestVariable<'Level when 'Level: comparison> =
            UnsuccessfulSearchTerminationNode
          | AugmentedInternalNode of AugmentedInternalNode<'Level>

            member this.NumberOfLevelsForLeadingTestVariable =
                match this with
                    AugmentedInternalNode augmentedInternalNode ->
                        augmentedInternalNode.NumberOfLevelsForLeadingTestVariable
                  | UnsuccessfulSearchTerminationNode ->
                        0u
        and WildcardNode<'Level when 'Level: comparison> =
            {
                SubtreeWithAllLevelsForSameTestVariableIndex: BinaryTreeOfLevelsForTestVariable<'Level>
                UniquePathForFollowingIndices: UniquePath<'Level>
            }
        and UniquePath<'Level when 'Level: comparison> =
            {
                Steps: List<Option<'Level>>
                SubtreeForFollowingIndices: TernarySearchTree<'Level>
            }
        and TernarySearchTree<'Level when 'Level: comparison> =
            SuccessfulSearchTerminationNode
          | WildcardNode of WildcardNode<'Level>
          | BinaryTreeOfLevelsForTestVariable of BinaryTreeOfLevelsForTestVariable<'Level>

        type TreeSearchContextParameters =
            {
                TestVariableIndex: UInt32
                HasSuffixContextOfPossibleFullTestVector: Boolean
            }

            static member StartOfSearch =
                {
                    TestVariableIndex = 0u
                    HasSuffixContextOfPossibleFullTestVector = true
                }

            member this.IsSpecialCaseDenotingInitialState =   // The tests define this special state as not possessing any test vectors, not even the trivial empty one.
                0u = this.TestVariableIndex

            member this.IsFullTestVector maximumNumberOfTestVariables =
                this.HasSuffixContextOfPossibleFullTestVector
                && maximumNumberOfTestVariables = this.TestVariableIndex

            member this.PropagateFromDefinedLevelToNextTestVariable =
                {
                    this with
                        TestVariableIndex = this.TestVariableIndex + 1u
                }

            member this.PropagateFromWildcardLevelToNextTestVariable =
                {
                    this with
                        TestVariableIndex = this.TestVariableIndex + 1u
                        HasSuffixContextOfPossibleFullTestVector = false
                }

        let inline (|InternalNode|) (augmentedInternalNode: AugmentedInternalNode<'Level>) =
            augmentedInternalNode.InternalNode

        let inline InternalNode (internalNode: InternalNode<'Level>) =
            new AugmentedInternalNode<'Level> (internalNode)

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

        let inline (|MirroredInternalNode|) mirroring =
            (|InternalNode|)
            >> mirrorInternalNode mirroring

        let inline MirroredInternalNode mirroring =
            mirrorInternalNode mirroring
            >> InternalNode

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
                            AugmentedInternalNode
                            (MirroredInternalNode localMirroring
                            ({
                                LevelForTestVariableIndex = zigLevelForTestVariableIndex
                                SubtreeWithLesserLevelsForSameTestVariableIndex = zigSubtreeWithLesserLevelsForSameTestVariableIndex
                                SubtreeWithGreaterLevelsForSameTestVariableIndex = zigSubtreeWithGreaterLevelsForSameTestVariableIndex
                            } as internalNodeRepresentationForZig)) ->
                                match comparisonWrtImplicitLevel zigLevelForTestVariableIndex
                                      , zigSubtreeWithLesserLevelsForSameTestVariableIndex
                                      , zigSubtreeWithGreaterLevelsForSameTestVariableIndex with
                                    zigResult
                                    , AugmentedInternalNode (InternalNode internalNodeRepresentationforZigZig)
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
                                                                |> AugmentedInternalNode
                                                    }
                                                    |> MirroredInternalNode localMirroring
                                                    |> AugmentedInternalNode))
                                        accumulateFlankingSubtrees
                                            internalNodeRepresentationforZigZig
                                            addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                                            addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels
                                            localMirroring
                                  | zigResult
                                    , _
                                    , AugmentedInternalNode (InternalNode internalNodeRepresentationforZigZag) when zigResult > 0 ->
                                        // Zig-zag case...
                                        let addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels =
                                            (fun nodeWithLeastLevelToBeAddedToFlankingSubtreeWithGreaterLevels ->
                                                addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels
                                                    ({
                                                        internalNodeRepresentationForRoot with
                                                            SubtreeWithLesserLevelsForSameTestVariableIndex = nodeWithLeastLevelToBeAddedToFlankingSubtreeWithGreaterLevels
                                                    }
                                                    |> MirroredInternalNode localMirroring
                                                    |> AugmentedInternalNode))
                                        let addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels =
                                            (fun nodeWithGreatestLevelToBeAddedToFlankingSubtreeWithLesserLevels ->
                                                addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                                                    ({
                                                        internalNodeRepresentationForZig with
                                                            SubtreeWithGreaterLevelsForSameTestVariableIndex = nodeWithGreatestLevelToBeAddedToFlankingSubtreeWithLesserLevels
                                                    }
                                                    |> MirroredInternalNode localMirroring
                                                    |> AugmentedInternalNode))
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
                                                |> MirroredInternalNode localMirroring
                                                |> AugmentedInternalNode)
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

    open MergedPartialTestVectorRepresentationsDetail

    type MergedPartialTestVectorRepresentations<'Level when 'Level: comparison>(uniquePath: UniquePath<'Level>,
                                                                                maximumNumberOfTestVariables: UInt32) =
        let createPartialTestVectorSequence revealFullTestVectorsAgain =
            let rec traverseUniquePath {
                                            Steps = _
                                            SubtreeForFollowingIndices = subtreeForFollowingIndices
                                       }
                                       (traversalContextParameters: TreeSearchContextParameters)
                                       partialTestVectorBeingBuilt =
                traverseTernarySearchTree subtreeForFollowingIndices
                                          traversalContextParameters
                                          partialTestVectorBeingBuilt
            and traverseTernarySearchTree ternarySearchTree
                                          (traversalContextParameters: TreeSearchContextParameters)
                                          partialTestVectorBeingBuilt =
                let rec traverseBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable =
                    match binaryTreeOfLevelsForTestVariable with
                        UnsuccessfulSearchTerminationNode ->
                            if maximumNumberOfTestVariables <= traversalContextParameters.TestVariableIndex
                            then
                                raise (InternalAssertionViolationException "The test vector refers to test variable indices that are greater than the permitted maximum.")

                            Seq.empty
                      | AugmentedInternalNode
                        (InternalNode
                        {
                            LevelForTestVariableIndex = levelForTestVariableIndex
                            SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                            SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                            UniquePathForFollowingIndices = uniquePathForFollowingIndices
                        }) ->
                            Seq.delay (fun() ->
                                        seq
                                            {
                                                yield! traverseBinaryTreeOfLevelsForTestVariable subtreeWithLesserLevelsForSameTestVariableIndex
                                                yield! traverseUniquePath uniquePathForFollowingIndices
                                                                          traversalContextParameters.PropagateFromDefinedLevelToNextTestVariable
                                                                          ((traversalContextParameters.TestVariableIndex, levelForTestVariableIndex) :: partialTestVectorBeingBuilt)
                                                yield! traverseBinaryTreeOfLevelsForTestVariable subtreeWithGreaterLevelsForSameTestVariableIndex
                                            })
                match ternarySearchTree with
                    SuccessfulSearchTerminationNode ->
                        if maximumNumberOfTestVariables < traversalContextParameters.TestVariableIndex
                            // NOTE: a subtlety - remember that 'testVariableIndex' can reach 'maximumNumberOfTestVariablesOverall'
                            // for a full (and possibly removed) test vector, because successful searches go through at least one
                            // node corresponding to each test variable index, *then* land on a node indicating whether the search
                            // was successful or not: so the zero-relative index gets incremented one more time.
                        then
                            raise (InternalAssertionViolationException "The test vector refers to test variable indices that are greater than the permitted maximum.")

                        if not revealFullTestVectorsAgain
                           && traversalContextParameters.IsFullTestVector maximumNumberOfTestVariables
                           || traversalContextParameters.IsSpecialCaseDenotingInitialState
                        then
                            Seq.empty
                        else
                            if (List.isEmpty partialTestVectorBeingBuilt)
                            then
                                raise (InternalAssertionViolationException "Should not contain an empty partial vector: attempts to merge in empty partial vectors should have resulted in the original collection.")

                            if uint32 partialTestVectorBeingBuilt.Length > maximumNumberOfTestVariables
                            then
                                raise (InternalAssertionViolationException "The test vector has more entries than the permitted maximum number of test variables.")

                            // NOTE: as we are converting to a map, we can be cavalier about the
                            // order in which associative pairs are added to the partial test vector.
                            Seq.singleton (partialTestVectorBeingBuilt
                                           |> Map.ofList)
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        UniquePathForFollowingIndices = uniquePathForFollowingIndices
                    } ->
                        Seq.delay (fun () ->
                                    seq
                                        {
                                            yield! traverseBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                            yield! traverseUniquePath uniquePathForFollowingIndices
                                                                      traversalContextParameters.PropagateFromWildcardLevelToNextTestVariable
                                                                      partialTestVectorBeingBuilt
                                        })
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable ->
                        traverseBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
            traverseUniquePath uniquePath
                               TreeSearchContextParameters.StartOfSearch
                               []

        let fillOutPartialTestVectorWithIndeterminates partialTestVectorRepresentation =
            if uint32 (partialTestVectorRepresentation: Map<_, _>).Count > maximumNumberOfTestVariables
            then
                raise (InternalAssertionViolationException "The partial test vector being either merged or added has more entries than the permitted maximum number of test variables.")

            let testVariableIndicesHavingLevels =
                partialTestVectorRepresentation
                |> Seq.map (fun keyValuePair -> keyValuePair.Key)
                |> Set.ofSeq

            let maximumTestVariableIndexHavingLevel =
                Seq.max testVariableIndicesHavingLevels

            if maximumTestVariableIndexHavingLevel >= maximumNumberOfTestVariables
            then
                raise (PreconditionViolationException "The partial test vector being either merged or added has a test variable index that is greater than the permitted maximum.")

            let testVariableIndicesForFilledOutTestVector = // NOTE: only up to 'maximumTestVariableIndexHavingLevel' exclusive
                                                            // - this is to avoid having a tail of indeterminate entries.
                Seq.init (int32 maximumTestVariableIndexHavingLevel) uint32
                |> Set.ofSeq

            let testVariableIndicesForIndeterminates =
                Set.difference testVariableIndicesForFilledOutTestVector testVariableIndicesHavingLevels

            let isPrefixOfFullTestVector =
                Set.isEmpty testVariableIndicesForIndeterminates

            if isPrefixOfFullTestVector
            then
                let isFullTestVector =
                    maximumNumberOfTestVariables = 1u + maximumTestVariableIndexHavingLevel
                partialTestVectorRepresentation
                |> Map.toList
                |> List.map (snd >> Some)
                , isFullTestVector
            else
                let sortedAssociationListFromTestVariableIndicesToIndeterminateMarkers =
                    testVariableIndicesForIndeterminates
                    |> Set.toList
                    |> List.map (fun testVariableIndex ->
                                     testVariableIndex
                                     , None)

                let sortedAssociationListFromTestVariableIndicesToLevels =
                    partialTestVectorRepresentation
                    |> Map.map (fun _ level ->
                                    Some level)
                    |> Map.toList

                let mergedAssociationList =
                    BargainBasement.MergeDisjointSortedAssociationLists sortedAssociationListFromTestVariableIndicesToLevels
                                                                        sortedAssociationListFromTestVariableIndicesToIndeterminateMarkers

                mergedAssociationList
                |> List.map snd
                , false


        let add uniquePath
                newPartialTestVectorRepresentation =
            let rec addToUniquePath {
                                        Steps = _
                                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                                    }
                                    newPartialTestVectorRepresentation =
                {
                    Steps = []
                    SubtreeForFollowingIndices =
                        addToTernarySearchTree subtreeForFollowingIndices
                                               newPartialTestVectorRepresentation
                }
            and addToTernarySearchTree ternarySearchTree
                                       newPartialTestVectorRepresentation =
                let buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation newPartialTestVectorRepresentation =
                    // TODO: revisit this - capitalise on the unique path steps.
                        List.foldBack (fun optionalLevel
                                           degenerateLinearSubtree ->
                                                match optionalLevel with
                                                    Some level ->
                                                        {
                                                            LevelForTestVariableIndex = level
                                                            SubtreeWithLesserLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                                            SubtreeWithGreaterLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                                            UniquePathForFollowingIndices =
                                                                {
                                                                    Steps = []
                                                                    SubtreeForFollowingIndices = degenerateLinearSubtree
                                                                }
                                                        }
                                                        |> InternalNode
                                                        |> AugmentedInternalNode
                                                        |> BinaryTreeOfLevelsForTestVariable
                                                    | None ->
                                                        {
                                                            SubtreeWithAllLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                                            UniquePathForFollowingIndices =
                                                                {
                                                                    Steps = []
                                                                    SubtreeForFollowingIndices = degenerateLinearSubtree
                                                                }
                                                        }
                                                        |> WildcardNode)
                                      newPartialTestVectorRepresentation SuccessfulSearchTerminationNode
                let rec addLevelToBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                    levelFromNewPartialTestVectorRepresentation
                                                                    tailFromNewPartialTestVectorRepresentation =
                    match binaryTreeOfLevelsForTestVariable with
                        UnsuccessfulSearchTerminationNode ->
                            {
                                LevelForTestVariableIndex = levelFromNewPartialTestVectorRepresentation
                                SubtreeWithLesserLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                SubtreeWithGreaterLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                UniquePathForFollowingIndices =
                                    {
                                        Steps = []
                                        SubtreeForFollowingIndices =
                                            buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation tailFromNewPartialTestVectorRepresentation
                                    }
                            }
                            |> InternalNode
                            |> AugmentedInternalNode
                      | AugmentedInternalNode
                        (InternalNode internalNodeRepresentation) ->
                            let comparisonWrtImplicitLevel =
                                compare levelFromNewPartialTestVectorRepresentation
                            let ({
                                    LevelForTestVariableIndex = splayedLevelForTestVariableIndex
                                    UniquePathForFollowingIndices = splayedUniquePathForFollowingIndices
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
                                        |> AugmentedInternalNode
                                    {
                                        LevelForTestVariableIndex = levelFromNewPartialTestVectorRepresentation
                                        SubtreeWithLesserLevelsForSameTestVariableIndex = flankingSubtreeWithLesserLevels
                                        SubtreeWithGreaterLevelsForSameTestVariableIndex = flankingSubtreeWithGreaterLevels
                                        UniquePathForFollowingIndices =
                                            {
                                                Steps = []
                                                SubtreeForFollowingIndices =
                                                    buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation tailFromNewPartialTestVectorRepresentation
                                            }
                                    }
                                    |> InternalNode
                                    |> AugmentedInternalNode
                              | result when result > 0 ->
                                    let flankingSubtreeWithLesserLevels =
                                        {
                                            splayedInternalNodeRepresentation with
                                                SubtreeWithLesserLevelsForSameTestVariableIndex = flankingSubtreeWithLesserLevels
                                        }
                                        |> InternalNode
                                        |> AugmentedInternalNode
                                    {
                                        LevelForTestVariableIndex = levelFromNewPartialTestVectorRepresentation
                                        SubtreeWithLesserLevelsForSameTestVariableIndex = flankingSubtreeWithLesserLevels
                                        SubtreeWithGreaterLevelsForSameTestVariableIndex = flankingSubtreeWithGreaterLevels
                                        UniquePathForFollowingIndices =
                                            {
                                                Steps = []
                                                SubtreeForFollowingIndices =
                                                    buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation tailFromNewPartialTestVectorRepresentation
                                            }
                                    }
                                    |> InternalNode
                                    |> AugmentedInternalNode
                              | _ ->
                                    let modifiedUniquePathForFollowingIndices =
                                        addToUniquePath splayedUniquePathForFollowingIndices
                                                        tailFromNewPartialTestVectorRepresentation
                                    {
                                        splayedInternalNodeRepresentation with
                                            SubtreeWithLesserLevelsForSameTestVariableIndex = flankingSubtreeWithLesserLevels
                                            SubtreeWithGreaterLevelsForSameTestVariableIndex = flankingSubtreeWithGreaterLevels
                                            UniquePathForFollowingIndices = modifiedUniquePathForFollowingIndices
                                    }
                                    |> InternalNode
                                    |> AugmentedInternalNode
                match ternarySearchTree
                      , newPartialTestVectorRepresentation with
                  | SuccessfulSearchTerminationNode
                    , _ :: _ ->
                        buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation newPartialTestVectorRepresentation
                  | WildcardNode
                    ({
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                     } as wildcardNodeRepresentation)
                    , Some levelFromNewPartialTestVectorRepresentation :: tailFromNewPartialTestVectorRepresentation ->
                        let modifiedSubtreeWithAllLevelsForSameTestVariableIndex =
                            addLevelToBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                                                        levelFromNewPartialTestVectorRepresentation
                                                                        tailFromNewPartialTestVectorRepresentation
                        {
                            wildcardNodeRepresentation with
                                SubtreeWithAllLevelsForSameTestVariableIndex = modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                        } |> WildcardNode
                  | WildcardNode
                    ({
                        UniquePathForFollowingIndices = uniquePathForFollowingIndices
                     } as wildcardNodeRepresentation)
                    , None :: tailFromNewPartialTestVectorRepresentation ->
                        let modifiedUniquePathForFollowingIndices =
                            addToUniquePath uniquePathForFollowingIndices
                                            tailFromNewPartialTestVectorRepresentation
                        {
                            wildcardNodeRepresentation with
                                UniquePathForFollowingIndices = modifiedUniquePathForFollowingIndices
                        }
                        |> WildcardNode
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                    , Some levelFromNewPartialTestVectorRepresentation :: tailFromNewPartialTestVectorRepresentation ->
                        addLevelToBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                    levelFromNewPartialTestVectorRepresentation
                                                                    tailFromNewPartialTestVectorRepresentation
                        |> BinaryTreeOfLevelsForTestVariable
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                    , None :: tailFromNewPartialTestVectorRepresentation ->
                        {
                            SubtreeWithAllLevelsForSameTestVariableIndex = binaryTreeOfLevelsForTestVariable
                            UniquePathForFollowingIndices =
                                {
                                    Steps = []
                                    SubtreeForFollowingIndices =
                                        buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation tailFromNewPartialTestVectorRepresentation
                                }
                        }
                        |> WildcardNode
                  | _
                    , [] ->
                        raise (InternalAssertionViolationException "Attempt to add a new partial test vector representation that is already mergeable with or equivalent to a previous one.")
                        // The above is really a precondition violation, but the precondition should have been enforced at a higher level within the implementation and not by the client.
            addToUniquePath uniquePath
                            newPartialTestVectorRepresentation

        let remove uniquePath
                   queryPartialTestVectorRepresentation
                   existingFullTestVectorBlockedRemovalContinuation =
            let removeInternalNodeWithGreatestLevelInSubtree subtreeInternalNodeRepresentation =
                let comparisonWrtPositiveInfinity _ =
                    1
                let {
                        LevelForTestVariableIndex = splayedLevelForTestVariableIndex
                        UniquePathForFollowingIndices = splayedUniquePathForFollowingIndices
                    }
                    , flankingSubtreeWithLesserLevels
                    , _ =
                    splayInternalNodeWithMatchingOrNeighbouringLevel subtreeInternalNodeRepresentation
                                                                     comparisonWrtPositiveInfinity
                splayedLevelForTestVariableIndex
                , splayedUniquePathForFollowingIndices
                , flankingSubtreeWithLesserLevels
            let removeInternalNodeWithLeastLevelInSubtree subtreeInternalNodeRepresentation =
                let comparisonWrtNegativeInfinity _ =
                    -1
                let {
                        LevelForTestVariableIndex = splayedLevelForTestVariableIndex
                        UniquePathForFollowingIndices = splayedUniquePathForFollowingIndices
                    }
                    , _
                    , flankingSubtreeWithGreaterLevels =
                    splayInternalNodeWithMatchingOrNeighbouringLevel subtreeInternalNodeRepresentation
                                                                     comparisonWrtNegativeInfinity
                splayedLevelForTestVariableIndex
                , splayedUniquePathForFollowingIndices
                , flankingSubtreeWithGreaterLevels
            let buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                        subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                        subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                        ({
                                                                                            Steps = _
                                                                                            SubtreeForFollowingIndices = subtreeForFollowingIndices
                                                                                        } as uniquePathForFollowingIndices) =
                // THIS IS WRONG - THE STEPS ARE BEING THROWN AWAY IN *NEARLY* ALL THE CASES - BUT EVEN WORSE, IN ONE CASE THEY MAKE IT THROUGH!
                match subtreeWithLesserLevelsForSameTestVariableIndex
                      , subtreeWithGreaterLevelsForSameTestVariableIndex
                      , subtreeForFollowingIndices with
                  | UnsuccessfulSearchTerminationNode
                    , UnsuccessfulSearchTerminationNode
                    , BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode ->
                        UnsuccessfulSearchTerminationNode
                  | UnsuccessfulSearchTerminationNode
                    , _
                    , BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode ->
                        subtreeWithGreaterLevelsForSameTestVariableIndex
                  | _
                    , UnsuccessfulSearchTerminationNode
                    , BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode ->
                        subtreeWithLesserLevelsForSameTestVariableIndex
                  | AugmentedInternalNode (InternalNode subtreeWithLesserLevelsForSameTestVariableIndexInternalNodeRepresentation)
                    , AugmentedInternalNode (InternalNode subtreeWithGreaterLevelsForSameTestVariableIndexInternalNodeRepresentation)
                    , BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode ->
                        if subtreeWithLesserLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                           > subtreeWithGreaterLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                        then
                            let levelForTestVariableIndexFromRemovedNode
                                , uniquePathForFollowingIndicesFromRemovedNode
                                , subtreeWithLesserLevelsForSameTestVariableIndexWithoutThatNode =
                                    removeInternalNodeWithGreatestLevelInSubtree subtreeWithLesserLevelsForSameTestVariableIndexInternalNodeRepresentation
                            ({
                                LevelForTestVariableIndex =
                                    levelForTestVariableIndexFromRemovedNode
                                SubtreeWithLesserLevelsForSameTestVariableIndex =
                                    subtreeWithLesserLevelsForSameTestVariableIndexWithoutThatNode
                                SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                    subtreeWithGreaterLevelsForSameTestVariableIndex
                                UniquePathForFollowingIndices =
                                    uniquePathForFollowingIndicesFromRemovedNode
                            }
                            |> InternalNode
                            |> AugmentedInternalNode)
                        else
                            let levelForTestVariableIndexFromRemovedNode
                                , uniquePathForFollowingIndicesFromRemovedNode
                                , subtreeWithGreaterLevelsForSameTestVariableIndexWithoutThatNode =
                                    removeInternalNodeWithLeastLevelInSubtree subtreeWithGreaterLevelsForSameTestVariableIndexInternalNodeRepresentation
                            ({
                                LevelForTestVariableIndex =
                                    levelForTestVariableIndexFromRemovedNode
                                SubtreeWithLesserLevelsForSameTestVariableIndex =
                                    subtreeWithLesserLevelsForSameTestVariableIndex
                                SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                    subtreeWithGreaterLevelsForSameTestVariableIndexWithoutThatNode
                                UniquePathForFollowingIndices =
                                    uniquePathForFollowingIndicesFromRemovedNode
                            }
                            |> InternalNode
                            |> AugmentedInternalNode)
                  | _ ->
                        ({
                            LevelForTestVariableIndex =
                                levelForTestVariableIndex
                            SubtreeWithLesserLevelsForSameTestVariableIndex =
                                subtreeWithLesserLevelsForSameTestVariableIndex
                            SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                subtreeWithGreaterLevelsForSameTestVariableIndex
                            UniquePathForFollowingIndices =
                                uniquePathForFollowingIndices
                        }
                        |> InternalNode
                        |> AugmentedInternalNode)
            let buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees subtreeWithAllLevelsForSameTestVariableIndex
                                                                                        ({
                                                                                            Steps = _
                                                                                            SubtreeForFollowingIndices = subtreeForFollowingIndices
                                                                                        } as uniquePathForFollowingIndices) =
                match subtreeWithAllLevelsForSameTestVariableIndex
                      , subtreeForFollowingIndices with
                    UnsuccessfulSearchTerminationNode
                    , BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode ->
                        BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode
                  | _
                    , BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode ->
                        BinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                  | _ ->
                        {
                            SubtreeWithAllLevelsForSameTestVariableIndex =
                                subtreeWithAllLevelsForSameTestVariableIndex
                            UniquePathForFollowingIndices =
                                uniquePathForFollowingIndices
                        }
                        |> WildcardNode
            let rec removeFromUniquePath {
                                            Steps = _
                                            SubtreeForFollowingIndices = subtreeForFollowingIndices
                                         }
                                         queryPartialTestVectorRepresentation
                                         treeSearchContextParameters =
                continuationWorkflow
                    {
                        let! modifiedSubtreeForFollowingIndices
                             , removedPartialTestVector =
                            removeFromTernarySearchTree subtreeForFollowingIndices
                                                        queryPartialTestVectorRepresentation
                                                        treeSearchContextParameters
                        return {
                                    Steps = []
                                    SubtreeForFollowingIndices = modifiedSubtreeForFollowingIndices
                               }
                               , removedPartialTestVector
                    }
            and removeFromTernarySearchTree ternarySearchTree
                                            queryPartialTestVectorRepresentation
                                            (treeSearchContextParameters: TreeSearchContextParameters) =
                let buildResultFromInternalNodeModifyingSubtreeForFollowingTestVariableIndices tailFromQueryPartialTestVectorRepresentation
                                                                                               levelForTestVariableIndex
                                                                                               subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                               subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                               uniquePathForFollowingIndices =
                    continuationWorkflow
                        {
                            let! modifiedSubtreeForFollowingTestVariableIndices
                                 , removedPartialTestVector =
                                removeFromUniquePath uniquePathForFollowingIndices
                                                     tailFromQueryPartialTestVectorRepresentation
                                                     treeSearchContextParameters.PropagateFromDefinedLevelToNextTestVariable
                            let modifiedBinarySearchTree =
                                buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                        subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                        subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                        modifiedSubtreeForFollowingTestVariableIndices
                            return modifiedBinarySearchTree
                                   , (Some levelForTestVariableIndex :: removedPartialTestVector)

                        }
                let buildResultFromWildcardNodeModifyingSubtreeForFollowingTestVariableIndices headFromQueryPartialTestVectorRepresentation
                                                                                               tailFromQueryPartialTestVectorRepresentation
                                                                                               subtreeWithAllLevelsForSameTestVariableIndex
                                                                                               uniquePathForFollowingIndices =
                    continuationWorkflow
                        {
                            let! modifiedSubtreeForFollowingTestVariableIndices
                                 , removedPartialTestVector =
                                removeFromUniquePath uniquePathForFollowingIndices
                                                     tailFromQueryPartialTestVectorRepresentation
                                                     treeSearchContextParameters.PropagateFromWildcardLevelToNextTestVariable
                            let modifiedTernarySearchTree =
                                buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees subtreeWithAllLevelsForSameTestVariableIndex
                                                                                                        modifiedSubtreeForFollowingTestVariableIndices
                            return modifiedTernarySearchTree
                                   , headFromQueryPartialTestVectorRepresentation :: removedPartialTestVector
                        }

                let rec removeLevelFromBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                         levelFromQueryPartialTestVectorRepresentation
                                                                         tailFromQueryPartialTestVectorRepresentation =
                    match binaryTreeOfLevelsForTestVariable with
                        UnsuccessfulSearchTerminationNode ->
                            if maximumNumberOfTestVariables <= treeSearchContextParameters.TestVariableIndex
                            then
                                raise (InternalAssertionViolationException "The test vector refers to test variable indices that are greater than the permitted maximum.")

                            continuationWorkflow.Zero ()
                      | AugmentedInternalNode
                        (InternalNode internalNodeRepresentation) ->
                            let comparisonWrtImplicitLevel =
                                compare levelFromQueryPartialTestVectorRepresentation
                            let {
                                    LevelForTestVariableIndex = splayedLevelForTestVariableIndex
                                    UniquePathForFollowingIndices = splayedUniquePathForFollowingIndices
                                }
                                , flankingSubtreeWithLesserLevels
                                , flankingSubtreeWithGreaterLevels =
                                splayInternalNodeWithMatchingOrNeighbouringLevel internalNodeRepresentation
                                                                                 comparisonWrtImplicitLevel
                            match comparisonWrtImplicitLevel splayedLevelForTestVariableIndex with
                                0 ->
                                    buildResultFromInternalNodeModifyingSubtreeForFollowingTestVariableIndices tailFromQueryPartialTestVectorRepresentation
                                                                                                               splayedLevelForTestVariableIndex
                                                                                                               flankingSubtreeWithLesserLevels
                                                                                                               flankingSubtreeWithGreaterLevels
                                                                                                               splayedUniquePathForFollowingIndices
                              | _ ->
                                    continuationWorkflow.Zero ()
                and removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                             tailFromQueryPartialTestVectorRepresentation =
                    match binaryTreeOfLevelsForTestVariable with
                        UnsuccessfulSearchTerminationNode ->
                            if maximumNumberOfTestVariables <= treeSearchContextParameters.TestVariableIndex
                            then
                                raise (InternalAssertionViolationException "The test vector refers to test variable indices that are greater than the permitted maximum.")

                            continuationWorkflow.Zero ()

                      | AugmentedInternalNode
                        (InternalNode
                        {
                            LevelForTestVariableIndex = levelForTestVariableIndex
                            SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                            SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                            UniquePathForFollowingIndices = uniquePathForFollowingIndices
                        }) ->
                            buildResultFromInternalNodeModifyingSubtreeForFollowingTestVariableIndices tailFromQueryPartialTestVectorRepresentation
                                                                                                       levelForTestVariableIndex
                                                                                                       subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                       subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                       uniquePathForFollowingIndices
                            + continuationWorkflow
                                {
                                    let! modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                         , removedPartialTestVector =
                                        removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                 tailFromQueryPartialTestVectorRepresentation
                                    let modifiedBinaryTree =
                                        buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                                modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                uniquePathForFollowingIndices
                                    return modifiedBinaryTree
                                           , removedPartialTestVector
                                }
                            + continuationWorkflow
                                {
                                    let! modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex
                                         , removedPartialTestVector =
                                        removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                 tailFromQueryPartialTestVectorRepresentation
                                    let modifiedBinaryTree =
                                        buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                                subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                uniquePathForFollowingIndices
                                    return modifiedBinaryTree
                                           , removedPartialTestVector
                                }

                match ternarySearchTree
                      , queryPartialTestVectorRepresentation with
                    SuccessfulSearchTerminationNode
                    , _ ->
                        if maximumNumberOfTestVariables < treeSearchContextParameters.TestVariableIndex
                            // NOTE: a subtlety - remember that 'testVariableIndex' can reach 'maximumNumberOfTestVariablesOverall'
                            // for a full (and possibly removed) test vector, because successful searches go through at least one
                            // node corresponding to each test variable index, *then* land on a node indicating whether the search
                            // was successful or not: so the zero-relative index gets incremented one more time.
                        then
                            raise (InternalAssertionViolationException "The test vector refers to test variable indices that are greater than the permitted maximum.")

                        if treeSearchContextParameters.IsFullTestVector maximumNumberOfTestVariables
                        then
                            existingFullTestVectorBlockedRemovalContinuation ()
                        else
                            continuationWorkflow
                                {
                                        return BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode
                                               , queryPartialTestVectorRepresentation
                                }
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        UniquePathForFollowingIndices = uniquePathForFollowingIndices
                    }
                    , ((Some levelFromQueryPartialTestVectorRepresentation) as headFromQueryPartialTestVectorRepresentation) :: tailFromQueryPartialTestVectorRepresentation ->
                        continuationWorkflow
                            {
                                let! modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                     , removedPartialTestVector =
                                    removeLevelFromBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                                                                     levelFromQueryPartialTestVectorRepresentation
                                                                                     tailFromQueryPartialTestVectorRepresentation
                                let modifiedTernarySearchTree =
                                    buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                                                                                            uniquePathForFollowingIndices
                                return modifiedTernarySearchTree
                                       , removedPartialTestVector
                            }
                        + buildResultFromWildcardNodeModifyingSubtreeForFollowingTestVariableIndices headFromQueryPartialTestVectorRepresentation
                                                                                                     tailFromQueryPartialTestVectorRepresentation
                                                                                                     subtreeWithAllLevelsForSameTestVariableIndex
                                                                                                     uniquePathForFollowingIndices
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        UniquePathForFollowingIndices = uniquePathForFollowingIndices
                    }
                    , None :: tailFromQueryPartialTestVectorRepresentation ->
                        continuationWorkflow
                            {
                                let! modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                     , removedPartialTestVector =
                                    removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                                                                             tailFromQueryPartialTestVectorRepresentation
                                let modifiedTernarySearchTree =
                                    buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                                                                                            uniquePathForFollowingIndices
                                return modifiedTernarySearchTree
                                       , removedPartialTestVector
                            }
                        + buildResultFromWildcardNodeModifyingSubtreeForFollowingTestVariableIndices None
                                                                                                     tailFromQueryPartialTestVectorRepresentation
                                                                                                     subtreeWithAllLevelsForSameTestVariableIndex
                                                                                                     uniquePathForFollowingIndices
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                    , Some levelFromQueryPartialTestVectorRepresentation :: tailFromQueryPartialTestVectorRepresentation ->
                        continuationWorkflow
                            {
                                let! modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                     , removedPartialTestVector =
                                     removeLevelFromBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                                      levelFromQueryPartialTestVectorRepresentation
                                                                                      tailFromQueryPartialTestVectorRepresentation
                                return BinaryTreeOfLevelsForTestVariable modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                       , removedPartialTestVector
                            }
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                    , None :: tailFromQueryPartialTestVectorRepresentation ->
                        continuationWorkflow
                            {
                                let! modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                     , removedPartialTestVector =
                                    removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                                             tailFromQueryPartialTestVectorRepresentation
                                return BinaryTreeOfLevelsForTestVariable modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                       , removedPartialTestVector
                            }
                  | _
                    , [] ->
                        // This has the effect of padding out a query partial test vector on the fly, thereby
                        // allowing a match as a prefix of some suitable stored vector, should one already be present.
                        removeFromTernarySearchTree ternarySearchTree
                                                    [None]
                                                    treeSearchContextParameters
            removeFromUniquePath uniquePath
                                 queryPartialTestVectorRepresentation
                                 TreeSearchContextParameters.StartOfSearch

        let checkInvariant uniquePath =
            let rec checkInvariantOfUniquePath {
                                                    Steps = steps
                                                    SubtreeForFollowingIndices = subtreeForFollowingIndices
                                               }
                                               lowerBound
                                               upperBound
                                               testVariableIndex =
                let numberOfSuccessfulPathsFromSubtreeForFollowingIndices =
                    checkInvariantOfTernarySearchTree subtreeForFollowingIndices
                                                      lowerBound
                                                      upperBound
                                                      testVariableIndex
                match numberOfSuccessfulPathsFromSubtreeForFollowingIndices
                      , steps with
                    0u
                    , _ :: _ ->
                        raise (InvariantViolationException "Redundant non-empty unique prefix with no successful search paths leading through it.")
                  | _ ->
                    numberOfSuccessfulPathsFromSubtreeForFollowingIndices
            and checkInvariantOfTernarySearchTree ternarySearchTree
                                                  lowerBound
                                                  upperBound
                                                  testVariableIndex =
                let rec checkInvariantOfBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                          lowerBound
                                                                          upperBound =
                    match binaryTreeOfLevelsForTestVariable with
                        UnsuccessfulSearchTerminationNode ->
                            if maximumNumberOfTestVariables <= testVariableIndex
                            then
                                raise (InternalAssertionViolationException "The test vector refers to test variable indices that are greater than the permitted maximum.")

                            0u
                      | AugmentedInternalNode
                        (InternalNode
                        {
                            LevelForTestVariableIndex = levelForTestVariableIndex
                            SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                            SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                            UniquePathForFollowingIndices = uniquePathForFollowingIndices
                        }) ->
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
                            let numberOfSuccessfulPathsFromUniquePathForFollowingIndices =
                                checkInvariantOfUniquePath uniquePathForFollowingIndices
                                                           NegativeInfinity
                                                           PositiveInfinity
                                                           (testVariableIndex + 1u)
                            match numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex
                                  , numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex
                                  , numberOfSuccessfulPathsFromUniquePathForFollowingIndices with
                                0u
                                , 0u
                                , 0u ->
                                    raise (InvariantViolationException "Redundant internal node with no successful search paths leading through it.")
                              | _
                                , 0u
                                , 0u ->
                                    raise (InvariantViolationException "Redundant internal node with all successful search paths leading via subtree for lesser levels.")
                              | 0u
                                , _
                                , 0u ->
                                    raise (InvariantViolationException "Redundant internal node with all successful search paths leading via subtree for greater levels.")
                              | _
                                , _
                                , 0u ->
                                    raise (InvariantViolationException "Redundant internal node with its own 'ghost' level that participates in no successful search paths.")
//                              | 0u
//                                , _
//                                , _ ->
//                                    if numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex > 1u
//                                    then
//                                        Diagnostics.Debug.Print ("Lone greater subtree with: {0} successful paths through it.", numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex)
//                                    numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex
//                                    + numberOfSuccessfulPathsFromSubtreeForFollowingIndices
//                              | _
//                                , 0u
//                                , _ ->
//                                    if numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex > 1u
//                                    then
//                                        Diagnostics.Debug.Print ("Lone lesser subtree with: {0} successful paths through it.", numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex)
//                                    numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex
//                                    + numberOfSuccessfulPathsFromSubtreeForFollowingIndices
                              | _ ->
                                    numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex
                                    + numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex
                                    + numberOfSuccessfulPathsFromUniquePathForFollowingIndices
                match ternarySearchTree with
                    SuccessfulSearchTerminationNode ->
                        if maximumNumberOfTestVariables < testVariableIndex  // NOTE: a subtlety - remember that 'testVariableIndex' can reach 'maximumNumberOfTestVariablesOverall'
                                                                             // for a full (and possibly removed) test vector, because successful searches go through at least one
                                                                             // node corresponding to each test variable index, *then* land on a node indicating whether the search
                                                                             // was successful or not: so the zero-relative index gets incremented one more time.
                        then
                            raise (InternalAssertionViolationException "The test vector refers to test variable indices that are greater than the permitted maximum.")

                        1u
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable ->
                        checkInvariantOfBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                          lowerBound
                                                                          upperBound
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        UniquePathForFollowingIndices = uniquePathForFollowingIndices
                    } ->
                        let numberOfSuccessfulPathsFromSubtreeWithAllLevelsForSameTestVariableIndex =
                            checkInvariantOfBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                                                              lowerBound
                                                                              upperBound
                        let numberOfSuccessfulPathsFromUniquePathForFollowingIndices =
                            checkInvariantOfUniquePath uniquePathForFollowingIndices
                                                       NegativeInfinity
                                                       PositiveInfinity
                                                       (testVariableIndex + 1u)
                        match numberOfSuccessfulPathsFromSubtreeWithAllLevelsForSameTestVariableIndex
                              , numberOfSuccessfulPathsFromUniquePathForFollowingIndices with
                            0u
                            , 0u ->
                                raise (LogicErrorException "Redundant wildcard node with no successful search paths leading through it.")
                          | _
                            , 0u ->
                                raise (LogicErrorException "Redundant wildcard node that has no successful paths using its wildcard match leading through it.")
                          | _
                            , _ ->
                                numberOfSuccessfulPathsFromSubtreeWithAllLevelsForSameTestVariableIndex
                                + numberOfSuccessfulPathsFromUniquePathForFollowingIndices

            if 0u = checkInvariantOfUniquePath uniquePath
                                               NegativeInfinity
                                               PositiveInfinity
                                               0u
            then
                raise (LogicErrorException "No successful search paths but tree should be non-empty.")

        member this.MaximumNumberOfTestVariables =
            maximumNumberOfTestVariables

        member this.EnumerationOfMergedTestVectors revealFullTestVectorsAgain =
            createPartialTestVectorSequence revealFullTestVectorsAgain

        static member Initial maximumNumberOfTestVariablesOverall =
            MergedPartialTestVectorRepresentations<'Level> ({
                                                                Steps = []
                                                                SubtreeForFollowingIndices = SuccessfulSearchTerminationNode
                                                            },
                                                            maximumNumberOfTestVariablesOverall)

        member this.MergeOrAdd partialTestVectorRepresentationInExternalForm =
            if Map.isEmpty partialTestVectorRepresentationInExternalForm
            then
                this
                , None
            else
                let partialTestVectorRepresentation
                    , partialTestVectorRepresentationIsActuallyAlreadyFull =
                    fillOutPartialTestVectorWithIndeterminates partialTestVectorRepresentationInExternalForm
                let detectAndConvertFullTestVector partialTestVectorRepresentation =
                    let lengthOfPartialTestVectorRepresentation =
                        List.length partialTestVectorRepresentation
                        |> uint32

                    if lengthOfPartialTestVectorRepresentation > maximumNumberOfTestVariables
                    then
                        raise (InternalAssertionViolationException "The merged removed partial test vector has more entries than the permitted maximum number of test variables.")

                    if lengthOfPartialTestVectorRepresentation
                        |> uint32 < maximumNumberOfTestVariables
                        || partialTestVectorRepresentation
                            |> List.exists Option.isNone
                    then
                        None
                    else
                        let testVariableIndicesForFullTestVector =
                            List.init (int32 maximumNumberOfTestVariables)
                                      uint32

                        let testVariableLevelsForFullTestVector =
                            partialTestVectorRepresentation
                            |> List.map Option.get

                        List.zip testVariableIndicesForFullTestVector
                                 testVariableLevelsForFullTestVector
                        |> Map.ofList
                        |> Some
                let modifiedUniquePath
                    , fullTestVectorBeingOfferedNowForEarlyAccess =
                    ContinuationMonad<_, _>.CallCC ((fun () ->
                                                        continuationWorkflow
                                                            {
                                                                return uniquePath
                                                                       , None
                                                            }),
                                                    (fun existingFullTestVectorBlockedRemovalContinuation ->
                                                        continuationWorkflow
                                                            {
                                                                let! uniquePathWithoutMergeCandidate
                                                                     , mergedPartialTestVectorRepresentation =
                                                                    remove uniquePath
                                                                           partialTestVectorRepresentation
                                                                           existingFullTestVectorBlockedRemovalContinuation

//                                                                do! continuationWorkflow
//                                                                        {
//                                                                            let! _ =
//                                                                                remove uniquePathWithoutMergeCandidate
//                                                                                       mergedPartialTestVectorRepresentation
//                                                                                       (fun _ -> raise (InternalAssertionViolationException "This should not be called."))
//                                                                            do raise (InternalAssertionViolationException "The merged removed partial test vector still matches with something left behind!")
//                                                                        }
//                                                                    + continuationWorkflow
//                                                                        {
//                                                                            return ()
//                                                                        }

                                                                return add uniquePathWithoutMergeCandidate
                                                                           mergedPartialTestVectorRepresentation
                                                                       , detectAndConvertFullTestVector mergedPartialTestVectorRepresentation
                                                            }))
                    + continuationWorkflow
                        {
                            return add uniquePath
                                       partialTestVectorRepresentation
                                   , if partialTestVectorRepresentationIsActuallyAlreadyFull
                                        then
                                            Some partialTestVectorRepresentationInExternalForm
                                        else
                                            None
                        }
                    |> ContinuationMonad<_, _>.Execute
//                if 7 = (hash this) % 100
//                then
//                    // Invariant check...
//                    checkInvariant modifiedUniquePath
//                    // ... end of invariant check.

                MergedPartialTestVectorRepresentations (modifiedUniquePath,
                                                        maximumNumberOfTestVariables)
                , fullTestVectorBeingOfferedNowForEarlyAccess
