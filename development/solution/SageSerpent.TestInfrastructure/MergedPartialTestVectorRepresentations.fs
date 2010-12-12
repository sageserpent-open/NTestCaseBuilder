#nowarn "40"

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System.Collections.Generic
    open System
    open SageSerpent.Infrastructure
    open SageSerpent.Infrastructure.OptionWorkflow
    open SageSerpent.Infrastructure.OptionExtensions
    open SageSerpent.Infrastructure.RandomExtensions
    open Microsoft.FSharp.Collections
    
    module MergedPartialTestVectorRepresentationsDetail =
        type AugmentedInternalNode<'Level when 'Level: comparison>(internalNode: InternalNode<'Level>) =
            let numberOfLevelsForLeadingTestVariable =
                match internalNode with
                    {
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode
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
                SubtreeForFollowingIndices: TernarySearchTree<'Level>
            }
        and BinaryTreeOfLevelsForTestVariable<'Level when 'Level: comparison> =
            UnsuccessfulSearchTerminationNode
          | AugmentedInternalNode of AugmentedInternalNode<'Level>
          
            member this.NumberOfLevelsForLeadingTestVariable =
                match this with
                    AugmentedInternalNode augmentedInternalNode ->
                        augmentedInternalNode.NumberOfLevelsForLeadingTestVariable
                  | _ ->
                        0u
                        
        and WildcardNode<'Level when 'Level: comparison> =
            {
                SubtreeWithAllLevelsForSameTestVariableIndex: BinaryTreeOfLevelsForTestVariable<'Level>
                SubtreeForFollowingIndices: TernarySearchTree<'Level>
            }
        and TernarySearchTree<'Level when 'Level: comparison> =
            SuccessfulSearchTerminationNode
          | WildcardNode of WildcardNode<'Level>
          | BinaryTreeOfLevelsForTestVariable of BinaryTreeOfLevelsForTestVariable<'Level>
          
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
                                                    SubtreeForFollowingIndices = rootSubtreeForFollowingIndices
                                                } as internalNodeRepresentationForRoot)
                                               addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                                               addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels =
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
                                        if localMirroring
                                        then
                                            accumulateFlankingSubtrees
                                                internalNodeRepresentationforZigZig
                                                addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels
                                                addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                                        else
                                            accumulateFlankingSubtrees
                                                internalNodeRepresentationforZigZig
                                                addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                                                addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels
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
                                        if localMirroring
                                        then                                                                                                                                         
                                            accumulateFlankingSubtrees
                                                internalNodeRepresentationforZigZag
                                                addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels
                                                addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                                        else
                                            accumulateFlankingSubtrees
                                                internalNodeRepresentationforZigZag
                                                addNodeWithGreatestLevelToFlankingSubtreeWithLesserLevels
                                                addNodeWithLeastLevelToFlankingSubtreeWithGreaterLevels
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
                             
    open MergedPartialTestVectorRepresentationsDetail
        
    type MergedPartialTestVectorRepresentations<'Level when 'Level: comparison>(ternarySearchTree: TernarySearchTree<'Level>) =
        let createPartialTestVectorSequence () =
            let rec traverseTernarySearchTree ternarySearchTree
                                              testVariableIndex
                                              partialTestVectorBeingBuilt =
                let rec traverseBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable =
                    match binaryTreeOfLevelsForTestVariable with
                        UnsuccessfulSearchTerminationNode ->
                            Seq.empty
                      | AugmentedInternalNode
                        (InternalNode
                        {
                            LevelForTestVariableIndex = levelForTestVariableIndex
                            SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                            SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                            SubtreeForFollowingIndices = subtreeForFollowingIndices
                        }) ->
                            Seq.delay (fun() ->
                                        seq
                                            {
                                                yield! traverseBinaryTreeOfLevelsForTestVariable subtreeWithLesserLevelsForSameTestVariableIndex
                                                yield! traverseTernarySearchTree subtreeForFollowingIndices
                                                                                 (testVariableIndex + 1u)
                                                                                 ((testVariableIndex, levelForTestVariableIndex) :: partialTestVectorBeingBuilt)
                                                yield! traverseBinaryTreeOfLevelsForTestVariable subtreeWithGreaterLevelsForSameTestVariableIndex                                                                
                                            })               
                match ternarySearchTree with
                    SuccessfulSearchTerminationNode ->
                        // NOTE: as we are converting to a map, we can be cavalier about the
                        // order in which associative pairs are added to the partial test vector.
                        seq
                            {
                                if not (List.isEmpty partialTestVectorBeingBuilt)
                                then
                                    yield partialTestVectorBeingBuilt
                                          |> Map.ofList
                                else
                                    raise (InternalAssertionViolationException "Should not contain an empty partial vector: attempts to merge in empty partial vectors result in the original collection.")
                            }
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    } ->
                        Seq.delay (fun () ->
                                    seq
                                        {
                                            yield! traverseBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                            yield! traverseTernarySearchTree subtreeForFollowingIndices
                                                                             (testVariableIndex + 1u)
                                                                             partialTestVectorBeingBuilt
                                        })
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable ->
                        traverseBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
            traverseTernarySearchTree ternarySearchTree 0u []

        let fillOutPartialTestVectorWithIndeterminates partialTestVectorRepresentation =
            let fillIfNecessary expectedPreviousTestVariableIndex
                                previousTestVariableIndex
                                partialResult =
                match previousTestVariableIndex with
                    Some previousTestVariableIndex when previousTestVariableIndex > expectedPreviousTestVariableIndex ->
                        let filledOutSection =
                            List.init (int32 (previousTestVariableIndex - expectedPreviousTestVariableIndex))
                                      (fun _ -> None)
                        List.append filledOutSection partialResult
                  | _ ->
                        partialResult
            let rec fillInNonConsecutiveIndicesWithIndeterminateEntries partialTestVectorRepresentation =
                match partialTestVectorRepresentation with
                    [] ->
                        []
                        , None
                  | (testVariableIndex, level) :: tail ->
                        let partialResult
                            , previousTestVariableIndex =
                            fillInNonConsecutiveIndicesWithIndeterminateEntries tail
                        Some (level: 'Level) :: fillIfNecessary (testVariableIndex + 1u) previousTestVariableIndex partialResult
                        , Some testVariableIndex
            let partialTestVectorPossiblyWithLeadingEntriesMissing
                , lowestTestVariableIndex =
                partialTestVectorRepresentation
                |> Map.toList
                |> fillInNonConsecutiveIndicesWithIndeterminateEntries  
            fillIfNecessary 0u lowestTestVariableIndex partialTestVectorPossiblyWithLeadingEntriesMissing

        let add ternarySearchTree
                newPartialTestVectorRepresentation
                randomBehaviour =
            let rec addToTernarySearchTree ternarySearchTree
                                           newPartialTestVectorRepresentation =
                let buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation newPartialTestVectorRepresentation =
                        List.foldBack (fun optionalLevel
                                           degenerateLinearSubtree ->
                                            match optionalLevel with
                                                Some level ->
                                                    {
                                                        LevelForTestVariableIndex = level
                                                        SubtreeWithLesserLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                                        SubtreeWithGreaterLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                                        SubtreeForFollowingIndices = degenerateLinearSubtree
                                                    }
                                                    |> InternalNode
                                                    |> AugmentedInternalNode
                                                    |> BinaryTreeOfLevelsForTestVariable
                                              | None ->
                                                    {
                                                        SubtreeWithAllLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                                        SubtreeForFollowingIndices = degenerateLinearSubtree
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
                                SubtreeForFollowingIndices =
                                    buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation tailFromNewPartialTestVectorRepresentation
                            }
                            |> InternalNode
                            |> AugmentedInternalNode
                      | AugmentedInternalNode
                        (InternalNode internalNodeRepresentation) ->
                            let comparisonWrtImplicitLevel =
                                compare levelFromNewPartialTestVectorRepresentation
                            let ({
                                    LevelForTestVariableIndex = splayedLevelForTestVariableIndex
                                    SubtreeForFollowingIndices = splayedSubtreeForFollowingIndices
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
                                        SubtreeForFollowingIndices =
                                            buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation tailFromNewPartialTestVectorRepresentation
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
                                        SubtreeForFollowingIndices =
                                            buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation tailFromNewPartialTestVectorRepresentation
                                    }
                                    |> InternalNode
                                    |> AugmentedInternalNode                                    
                              | _ ->
                                    let modifiedSubtreeForFollowingIndices =
                                        addToTernarySearchTree splayedSubtreeForFollowingIndices
                                                               tailFromNewPartialTestVectorRepresentation
                                    {
                                        splayedInternalNodeRepresentation with
                                            SubtreeWithLesserLevelsForSameTestVariableIndex = flankingSubtreeWithLesserLevels
                                            SubtreeWithGreaterLevelsForSameTestVariableIndex = flankingSubtreeWithGreaterLevels                                    
                                            SubtreeForFollowingIndices = modifiedSubtreeForFollowingIndices
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
                        WildcardNode
                            {
                                wildcardNodeRepresentation with
                                    SubtreeWithAllLevelsForSameTestVariableIndex = modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                            }
                  | WildcardNode
                    ({
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                     } as wildcardNodeRepresentation)
                    , None :: tailFromNewPartialTestVectorRepresentation ->
                        let modifiedSubtreeForFollowingIndices =
                            addToTernarySearchTree subtreeForFollowingIndices
                                                   tailFromNewPartialTestVectorRepresentation
                        {
                            wildcardNodeRepresentation with
                                SubtreeForFollowingIndices = modifiedSubtreeForFollowingIndices
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
                            SubtreeForFollowingIndices = buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation tailFromNewPartialTestVectorRepresentation
                        }
                        |> WildcardNode
                  | _
                    , [] ->
                        raise (InternalAssertionViolationException "Attempt to add a new partial test vector representation that is already mergeable with or equivalent to a previous one.")
                        // The above is really a precondition violation, but the precondition should have been enforced at a higher level within the implementation and not by the client.
            addToTernarySearchTree ternarySearchTree newPartialTestVectorRepresentation

        let remove ternarySearchTree
                   queryPartialTestVectorRepresentation =
            let removeInternalNodeWithGreatestLevelInSubtree subtreeInternalNodeRepresentation =
                let comparisonWrtPositiveInfinity _ =
                    1
                let {
                        LevelForTestVariableIndex = splayedLevelForTestVariableIndex
                        SubtreeForFollowingIndices = splayedSubtreeForFollowingIndices
                    }
                    , flankingSubtreeWithLesserLevels
                    , _ =
                    splayInternalNodeWithMatchingOrNeighbouringLevel subtreeInternalNodeRepresentation
                                                                     comparisonWrtPositiveInfinity
                splayedLevelForTestVariableIndex
                , splayedSubtreeForFollowingIndices
                , flankingSubtreeWithLesserLevels
            let removeInternalNodeWithLeastLevelInSubtree subtreeInternalNodeRepresentation =
                let comparisonWrtNegativeInfinity _ =
                    -1
                let {
                        LevelForTestVariableIndex = splayedLevelForTestVariableIndex
                        SubtreeForFollowingIndices = splayedSubtreeForFollowingIndices
                    }
                    , _
                    , flankingSubtreeWithGreaterLevels =
                    splayInternalNodeWithMatchingOrNeighbouringLevel subtreeInternalNodeRepresentation
                                                                     comparisonWrtNegativeInfinity
                splayedLevelForTestVariableIndex
                , splayedSubtreeForFollowingIndices
                , flankingSubtreeWithGreaterLevels                   
            let buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                        subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                        subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                        subtreeForFollowingIndices =
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
                                , subtreeForFollowingIndicesFromRemovedNode
                                , subtreeWithLesserLevelsForSameTestVariableIndexWithoutThatNode =
                                    removeInternalNodeWithGreatestLevelInSubtree subtreeWithLesserLevelsForSameTestVariableIndexInternalNodeRepresentation
                            ({
                                LevelForTestVariableIndex =
                                    levelForTestVariableIndexFromRemovedNode
                                SubtreeWithLesserLevelsForSameTestVariableIndex =
                                    subtreeWithLesserLevelsForSameTestVariableIndexWithoutThatNode
                                SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                    subtreeWithGreaterLevelsForSameTestVariableIndex
                                SubtreeForFollowingIndices =
                                    subtreeForFollowingIndicesFromRemovedNode
                            }
                            |> InternalNode
                            |> AugmentedInternalNode)
                        else
                            let levelForTestVariableIndexFromRemovedNode
                                , subtreeForFollowingIndicesFromRemovedNode
                                , subtreeWithGreaterLevelsForSameTestVariableIndexWithoutThatNode =
                                    removeInternalNodeWithLeastLevelInSubtree subtreeWithGreaterLevelsForSameTestVariableIndexInternalNodeRepresentation
                            ({
                                LevelForTestVariableIndex =
                                    levelForTestVariableIndexFromRemovedNode
                                SubtreeWithLesserLevelsForSameTestVariableIndex =
                                    subtreeWithLesserLevelsForSameTestVariableIndex
                                SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                    subtreeWithGreaterLevelsForSameTestVariableIndexWithoutThatNode
                                SubtreeForFollowingIndices =
                                    subtreeForFollowingIndicesFromRemovedNode
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
                            SubtreeForFollowingIndices =
                                subtreeForFollowingIndices
                        }
                        |> InternalNode
                        |> AugmentedInternalNode)
            let buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees subtreeWithAllLevelsForSameTestVariableIndex
                                                                                        subtreeForFollowingIndices =
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
                            SubtreeForFollowingIndices =
                                subtreeForFollowingIndices
                        }
                        |> WildcardNode
            let rec removeLevelFromBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                     levelFromQueryPartialTestVectorRepresentation
                                                                     tailFromQueryPartialTestVectorRepresentation =
                match binaryTreeOfLevelsForTestVariable with
                    UnsuccessfulSearchTerminationNode ->
                        None
                  | AugmentedInternalNode
                    (InternalNode internalNodeRepresentation) ->
                        let comparisonWrtImplicitLevel =
                            compare levelFromQueryPartialTestVectorRepresentation
                        let {
                                LevelForTestVariableIndex = splayedLevelForTestVariableIndex
                                SubtreeForFollowingIndices = splayedSubtreeForFollowingIndices
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
                                                                                                           splayedSubtreeForFollowingIndices
                          | _ ->
                                None
            and removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                         tailFromQueryPartialTestVectorRepresentation =
                match binaryTreeOfLevelsForTestVariable with
                    UnsuccessfulSearchTerminationNode ->
                        None
                  | AugmentedInternalNode
                    (InternalNode
                    {
                        LevelForTestVariableIndex = levelForTestVariableIndex
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    }) ->
                        buildResultFromInternalNodeModifyingSubtreeForFollowingTestVariableIndices tailFromQueryPartialTestVectorRepresentation
                                                                                                   levelForTestVariableIndex
                                                                                                   subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                   subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                   subtreeForFollowingIndices
                        |> BargainBasement.Flip Option.LazyMPlus
                                                (lazy optionWorkflow
                                                        {
                                                            let! modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                                                 , removedPartialTestVector =
                                                                removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                         tailFromQueryPartialTestVectorRepresentation
                                                            return buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                                                           modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                                           subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                                           subtreeForFollowingIndices
                                                                   , removedPartialTestVector
                                                        })
                        |> BargainBasement.Flip Option.LazyMPlus
                                                (lazy optionWorkflow
                                                        {
                                                            let! modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex
                                                                 , removedPartialTestVector =
                                                                removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                         tailFromQueryPartialTestVectorRepresentation
                                                            return buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                                                           subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                                           modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                                           subtreeForFollowingIndices
                                                                   , removedPartialTestVector
                                                        })                                                                                                           
            and removeFromTernarySearchTree ternarySearchTree
                                            queryPartialTestVectorRepresentation =
                let inline adaptResult result =
                    optionWorkflow
                        {
                            let! binaryTreeOfLevelsForTestVariable
                                 , partialTestVector = result
                            return BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                   , partialTestVector
                        }
                match ternarySearchTree
                      , queryPartialTestVectorRepresentation with                
                    BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode
                    , _ ->
                        None
                  | SuccessfulSearchTerminationNode
                    , _ ->
                        Some (BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode
                              , queryPartialTestVectorRepresentation)
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    }
                    , ((Some levelFromQueryPartialTestVectorRepresentation) as headFromQueryPartialTestVectorRepresentation) :: tailFromQueryPartialTestVectorRepresentation ->
                        optionWorkflow
                            {
                                let! modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                     , removedPartialTestVector = 
                                    removeLevelFromBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                                                                     levelFromQueryPartialTestVectorRepresentation
                                                                                     tailFromQueryPartialTestVectorRepresentation
                                return buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                                                                                               subtreeForFollowingIndices
                                       , removedPartialTestVector
                            }
                        |> BargainBasement.Flip Option.LazyMPlus
                                                (lazy buildResultFromWildcardNodeModifyingSubtreeForFollowingTestVariableIndices headFromQueryPartialTestVectorRepresentation
                                                                                                                                 tailFromQueryPartialTestVectorRepresentation
                                                                                                                                 subtreeWithAllLevelsForSameTestVariableIndex
                                                                                                                                 subtreeForFollowingIndices)
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    }
                    , None :: tailFromQueryPartialTestVectorRepresentation ->
                        optionWorkflow
                            {
                                let! modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                     , removedPartialTestVector = 
                                    removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                                                                             tailFromQueryPartialTestVectorRepresentation
                                return buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                                                                                               subtreeForFollowingIndices
                                       , removedPartialTestVector
                            }
                        |> BargainBasement.Flip Option.LazyMPlus
                                                (lazy buildResultFromWildcardNodeModifyingSubtreeForFollowingTestVariableIndices None
                                                                                                                                 tailFromQueryPartialTestVectorRepresentation
                                                                                                                                 subtreeWithAllLevelsForSameTestVariableIndex
                                                                                                                                 subtreeForFollowingIndices)
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                    , Some levelFromQueryPartialTestVectorRepresentation :: tailFromQueryPartialTestVectorRepresentation ->
                        removeLevelFromBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                         levelFromQueryPartialTestVectorRepresentation
                                                                         tailFromQueryPartialTestVectorRepresentation
                        |> adaptResult
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                    , None :: tailFromQueryPartialTestVectorRepresentation ->
                        removeWildcardLevelFromBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                                 tailFromQueryPartialTestVectorRepresentation
                        |> adaptResult
                  | _
                    , [] ->
                        // This has the effect of padding out a query partial test vector on the fly, thereby
                        // allowing a match as a prefix of some suitable stored vector, should one already be present.
                        removeFromTernarySearchTree ternarySearchTree
                                                    [None]
            and buildResultFromInternalNodeModifyingSubtreeForFollowingTestVariableIndices tailFromQueryPartialTestVectorRepresentation
                                                                                           levelForTestVariableIndex
                                                                                           subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                           subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                           subtreeForFollowingIndices = 
                    optionWorkflow  
                        {
                            let! modifiedSubtreeForFollowingTestVariableIndices
                                 , removedPartialTestVector =
                                removeFromTernarySearchTree subtreeForFollowingIndices
                                                            tailFromQueryPartialTestVectorRepresentation
                            return buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                           subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                           subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                           modifiedSubtreeForFollowingTestVariableIndices
                                   , (Some levelForTestVariableIndex :: removedPartialTestVector)
                        }
            and buildResultFromWildcardNodeModifyingSubtreeForFollowingTestVariableIndices headFromQueryPartialTestVectorRepresentation
                                                                                           tailFromQueryPartialTestVectorRepresentation
                                                                                           subtreeWithAllLevelsForSameTestVariableIndex
                                                                                           subtreeForFollowingIndices =
                    optionWorkflow
                        {
                            let! modifiedSubtreeForFollowingTestVariableIndices
                                 , removedPartialTestVector =
                                removeFromTernarySearchTree subtreeForFollowingIndices
                                                            tailFromQueryPartialTestVectorRepresentation
                            return buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees subtreeWithAllLevelsForSameTestVariableIndex
                                                                                                           modifiedSubtreeForFollowingTestVariableIndices
                                   , (headFromQueryPartialTestVectorRepresentation :: removedPartialTestVector)
                        }
            removeFromTernarySearchTree ternarySearchTree queryPartialTestVectorRepresentation
            
        let checkInvariant ternarySearchTree =
            let rec checkInvariantOfTernarySearchTree ternarySearchTree
                                                      lowerBound
                                                      upperBound =
                let rec checkInvariantOfBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                          lowerBound
                                                                          upperBound =
                    match binaryTreeOfLevelsForTestVariable with
                        UnsuccessfulSearchTerminationNode ->
                            0u
                      | AugmentedInternalNode
                        (InternalNode
                        {
                            LevelForTestVariableIndex = levelForTestVariableIndex
                            SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                            SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                            SubtreeForFollowingIndices = subtreeForFollowingIndices
                        }) ->
                            let liftedLevel =
                                Finite levelForTestVariableIndex
                            if liftedLevel >= upperBound
                            then
                                raise (LogicErrorException "Level is greater than or equal to exclusive upper bound.")
                            if liftedLevel <= lowerBound
                            then
                                raise (LogicErrorException "Level is less than or equal to exclusive lower bound.")
                            let numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex =
                                checkInvariantOfBinaryTreeOfLevelsForTestVariable subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                  lowerBound
                                                                                  liftedLevel
                            let numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex =
                                checkInvariantOfBinaryTreeOfLevelsForTestVariable subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                  liftedLevel
                                                                                  upperBound
                            let numberOfSuccessfulPathsFromSubtreeForFollowingIndices =
                                checkInvariantOfTernarySearchTree subtreeForFollowingIndices
                                                                  NegativeInfinity
                                                                  PositiveInfinity
                            match numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex
                                  , numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex
                                  , numberOfSuccessfulPathsFromSubtreeForFollowingIndices with
                                0u
                                , 0u
                                , 0u ->
                                    raise (LogicErrorException "Redundant internal node with no successful search paths leading through it.")
                              | _
                                , 0u
                                , 0u ->
                                    raise (LogicErrorException "Redundant internal node with all successful search paths leading via subtree for lesser levels.")
                              | 0u
                                , _
                                , 0u ->
                                    raise (LogicErrorException "Redundant internal node with all successful search paths leading via subtree for greater levels.")
                              | _
                                , _
                                , 0u ->
                                    raise (LogicErrorException "Redundant internal node with its own 'ghost' level that participates in no successful search paths.")
                              | 0u
                                , _
                                , _ ->
                                    if numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex > 1u
                                    then
                                        Diagnostics.Debug.Print ("Lone greater subtree with: {0} successful paths through it.", numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex)
                                    numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex
                                    + numberOfSuccessfulPathsFromSubtreeForFollowingIndices
                              | _
                                , 0u
                                , _ ->
                                    if numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex > 1u
                                    then
                                        Diagnostics.Debug.Print ("Lone lesser subtree with: {0} successful paths through it.", numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex)
                                    numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex
                                    + numberOfSuccessfulPathsFromSubtreeForFollowingIndices
                              | _ ->
                                    numberOfSuccessfulPathsFromSubtreeWithLesserLevelsForSameTestVariableIndex
                                    + numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex
                                    + numberOfSuccessfulPathsFromSubtreeForFollowingIndices                                                                          
                match ternarySearchTree with
                    SuccessfulSearchTerminationNode ->
                        1u
                  | BinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable ->
                        checkInvariantOfBinaryTreeOfLevelsForTestVariable binaryTreeOfLevelsForTestVariable
                                                                          lowerBound
                                                                          upperBound
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    } ->
                        let numberOfSuccessfulPathsFromSubtreeWithAllLevelsForSameTestVariableIndex =
                            checkInvariantOfBinaryTreeOfLevelsForTestVariable subtreeWithAllLevelsForSameTestVariableIndex
                                                                              lowerBound
                                                                              upperBound
                        let numberOfSuccessfulPathsFromSubtreeForFollowingIndices =
                            checkInvariantOfTernarySearchTree subtreeForFollowingIndices
                                                              NegativeInfinity
                                                              PositiveInfinity
                        match numberOfSuccessfulPathsFromSubtreeWithAllLevelsForSameTestVariableIndex
                              , numberOfSuccessfulPathsFromSubtreeForFollowingIndices with
                            0u
                            , 0u ->
                                raise (LogicErrorException "Redundant wildcard node with no successful search paths leading through it.")
                          | _
                            , 0u ->
                                raise (LogicErrorException "Redundant wildcard node that has no successful paths using its wildcard match leading through it.")
                          | _
                            , _ ->
                                numberOfSuccessfulPathsFromSubtreeWithAllLevelsForSameTestVariableIndex
                                + numberOfSuccessfulPathsFromSubtreeForFollowingIndices

            if 0u = checkInvariantOfTernarySearchTree ternarySearchTree
                                                      NegativeInfinity
                                                      PositiveInfinity
            then
                raise (LogicErrorException "No successful search paths but tree is should be non-empty.")
                                                      
        interface IEnumerable<Map<UInt32, 'Level>> with
            member this.GetEnumerator () =
                createPartialTestVectorSequence().GetEnumerator ()
        interface IEnumerable with
            member this.GetEnumerator () =
                (createPartialTestVectorSequence() :> IEnumerable).GetEnumerator ()
                
        static member Initial =
            MergedPartialTestVectorRepresentations<'Level> (BinaryTreeOfLevelsForTestVariable UnsuccessfulSearchTerminationNode)

        member this.MergeOrAdd partialTestVectorRepresentation
                               randomBehaviour =
            if Map.isEmpty partialTestVectorRepresentation
            then
                this
            else
                let partialTestVectorRepresentation =
                    fillOutPartialTestVectorWithIndeterminates partialTestVectorRepresentation
                let modifiedTernarySearchTree =
                    match remove ternarySearchTree
                                 partialTestVectorRepresentation with
                        Some (ternarySearchTreeWithoutMergeCandidate
                              , mergedPartialTestVectorRepresentation) ->
//                            // Postcondition check...
//                            match remove ternarySearchTreeWithoutMergeCandidate
//                                         mergedPartialTestVectorRepresentation with
//                                Some _ ->
//                                    raise (LogicErrorException "The merged removed partial vector still matches with something left behind!")
//                              | _ ->
//                                    ()
//                            // ... end of check.  
                            add ternarySearchTreeWithoutMergeCandidate
                                mergedPartialTestVectorRepresentation
                                randomBehaviour
                      | None ->
                            add ternarySearchTree
                                partialTestVectorRepresentation
                                randomBehaviour
//                // Invariant check...
//                checkInvariant modifiedTernarySearchTree
//                // ... end of invariant check.
                MergedPartialTestVectorRepresentations modifiedTernarySearchTree
