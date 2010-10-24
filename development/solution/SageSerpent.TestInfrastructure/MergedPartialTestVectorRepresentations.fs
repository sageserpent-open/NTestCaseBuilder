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
                        LevelForTestVariableIndex = _
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    } ->
                        let levelContributedByInternalNodeItself =
                            match subtreeForFollowingIndices with
                                UnsuccessfulSearchTerminationNode ->
                                    0u
                              | _ ->
                                    1u
                        levelContributedByInternalNodeItself
                        + subtreeWithLesserLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                        + subtreeWithGreaterLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                        
            member this.InternalNode =
                internalNode
                
            member this.NumberOfLevelsForLeadingTestVariable =
                numberOfLevelsForLeadingTestVariable                
                
        and InternalNode<'Level when 'Level: comparison> =
            {
                LevelForTestVariableIndex: 'Level
                SubtreeWithLesserLevelsForSameTestVariableIndex: TernarySearchTree<'Level>
                SubtreeWithGreaterLevelsForSameTestVariableIndex: TernarySearchTree<'Level>
                SubtreeForFollowingIndices: TernarySearchTree<'Level>
            }
        and WildcardNode<'Level when 'Level: comparison> =
            {
                SubtreeWithAllLevelsForSameTestVariableIndex: TernarySearchTree<'Level>
                SubtreeForFollowingIndices: TernarySearchTree<'Level>
            }
        and TernarySearchTree<'Level when 'Level: comparison> =
            SuccessfulSearchTerminationNode
          | UnsuccessfulSearchTerminationNode
          | WildcardNode of WildcardNode<'Level>
          | AugmentedInternalNode of AugmentedInternalNode<'Level>
          
            member this.NumberOfLevelsForLeadingTestVariable =
                match this with
                    AugmentedInternalNode augmentedInternalNode ->
                        augmentedInternalNode.NumberOfLevelsForLeadingTestVariable
                  | _ ->
                        0u
                
        let inline (|InternalNode|) (augmentedInternalNode: AugmentedInternalNode<'Level>) =
            augmentedInternalNode.InternalNode
            
        let inline InternalNode (internalNode: InternalNode<'Level>) =
            new AugmentedInternalNode<'Level> (internalNode) 
            
    open MergedPartialTestVectorRepresentationsDetail
        
    type MergedPartialTestVectorRepresentations<'Level when 'Level: comparison>(ternarySearchTree: TernarySearchTree<'Level>) =
        let createPartialTestVectorSequence () =
            let rec traverseTree tree
                                 testVariableIndex
                                 partialTestVectorBeingBuilt
                                 subtreeIsForANewTestVariableIndex =
                match tree with
                    SuccessfulSearchTerminationNode ->
                        // NOTE: as we are converting to a map, we can be cavalier about the
                        // order in which associative pairs are added to the partial test vector.
                        seq
                            {
                                 if subtreeIsForANewTestVariableIndex
                                 then
                                    if not (List.isEmpty partialTestVectorBeingBuilt)
                                    then yield partialTestVectorBeingBuilt
                                             |> Map.ofList
                                    else raise (InternalAssertionViolationException "Should not contain an empty partial vector: attempts to merge in empty partial vectors result in the original collection.")
                                 else
                                    raise (InternalAssertionViolationException "A successful search cannot terminate on a left or right subtree.")
                            }
                  | UnsuccessfulSearchTerminationNode ->
                        Seq.empty
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    } ->
                        Seq.delay (fun () ->
                                    seq
                                        {
                                            yield! traverseTree subtreeWithAllLevelsForSameTestVariableIndex
                                                                testVariableIndex
                                                                partialTestVectorBeingBuilt
                                                                false
                                            yield! traverseTree subtreeForFollowingIndices
                                                                (testVariableIndex + 1u)
                                                                partialTestVectorBeingBuilt
                                                                true
                                        })
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
                                            yield! traverseTree subtreeWithLesserLevelsForSameTestVariableIndex
                                                                testVariableIndex
                                                                partialTestVectorBeingBuilt
                                                                false
                                            yield! traverseTree subtreeForFollowingIndices
                                                                (testVariableIndex + 1u)
                                                                ((testVariableIndex, levelForTestVariableIndex) :: partialTestVectorBeingBuilt)
                                                                true
                                            yield! traverseTree subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                testVariableIndex
                                                                partialTestVectorBeingBuilt
                                                                false
                                        })
            traverseTree ternarySearchTree 0u [] true

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
            let rec add tree
                        newPartialTestVectorRepresentation
                        treeIsForNextTestVariableIndex
                        bringNewInternalNodeUpToSubtreeRoot =
                let buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation newPartialTestVectorRepresentation =
                        List.foldBack (fun optionalLevel
                                         degenerateLinearSubtree ->
                                            match optionalLevel with
                                                Some level ->
                                                    AugmentedInternalNode
                                                        (InternalNode
                                                        {
                                                            LevelForTestVariableIndex = level
                                                            SubtreeWithLesserLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                                            SubtreeWithGreaterLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                                            SubtreeForFollowingIndices = degenerateLinearSubtree
                                                        })
                                              | None ->
                                                    WildcardNode
                                                        {
                                                            SubtreeWithAllLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                                            SubtreeForFollowingIndices = degenerateLinearSubtree
                                                        })
                                      newPartialTestVectorRepresentation SuccessfulSearchTerminationNode  
                match tree
                      , newPartialTestVectorRepresentation with
                    UnsuccessfulSearchTerminationNode
                    , [] when not treeIsForNextTestVariableIndex ->
                        raise (InternalAssertionViolationException "Left or right subtrees should only be traversed with a non-empty new partial test vector representation.")                                                    
                  | UnsuccessfulSearchTerminationNode
                    , _ ->
                        buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation newPartialTestVectorRepresentation
                  | SuccessfulSearchTerminationNode
                    , _ :: _ when treeIsForNextTestVariableIndex ->
                        buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation newPartialTestVectorRepresentation
                  | WildcardNode
                    ({
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = _
                     } as wildcardNodeRepresentation)
                    , Some _ :: _ when treeIsForNextTestVariableIndex ->
                        let modifiedSubtreeWithAllLevelsForSameTestVariableIndex =
                            add subtreeWithAllLevelsForSameTestVariableIndex
                                newPartialTestVectorRepresentation
                                false
                                false
                        WildcardNode
                            {
                                wildcardNodeRepresentation with
                                    SubtreeWithAllLevelsForSameTestVariableIndex = modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                            }
                  | WildcardNode
                    ({
                        SubtreeWithAllLevelsForSameTestVariableIndex = _
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                     } as wildcardNodeRepresentation)
                    , None :: tailFromNewPartialTestVectorRepresentation when treeIsForNextTestVariableIndex ->
                        let modifiedSubtreeForFollowingIndices =
                            add subtreeForFollowingIndices
                                tailFromNewPartialTestVectorRepresentation
                                true
                                false
                        WildcardNode
                            {
                                wildcardNodeRepresentation with
                                    SubtreeForFollowingIndices = modifiedSubtreeForFollowingIndices
                            }
                  | AugmentedInternalNode
                    (InternalNode
                    ({
                        LevelForTestVariableIndex = levelForTestVariableIndex
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                     } as internalNodeRepresentation))
                    , Some levelFromNewPartialTestVectorRepresentation :: tailFromNewPartialTestVectorRepresentation ->
                        let bringNewInternalNodeUpToSubtreeRoot =
                            if bringNewInternalNodeUpToSubtreeRoot
                            then
                                true
                            else
                                let chosenNumber =
                                    (randomBehaviour: Random).ChooseAnyNumberFromOneTo (1u + (tree: TernarySearchTree<'Level>).NumberOfLevelsForLeadingTestVariable)
                                1u = chosenNumber
                        match compare levelFromNewPartialTestVectorRepresentation levelForTestVariableIndex with
                            result when result < 0  ->
                                let modifiedSubtreeWithLesserLevelsForSameTestVariableIndex =
                                    add subtreeWithLesserLevelsForSameTestVariableIndex
                                        newPartialTestVectorRepresentation
                                        false
                                        bringNewInternalNodeUpToSubtreeRoot
                                match modifiedSubtreeWithLesserLevelsForSameTestVariableIndex with
                                    AugmentedInternalNode
                                    (InternalNode
                                    ({
                                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeToSwapAcross   
                                     } as modifiedSubtreeNodeRepresentation)) when bringNewInternalNodeUpToSubtreeRoot ->
                                        AugmentedInternalNode
                                            (InternalNode
                                            {
                                                modifiedSubtreeNodeRepresentation with
                                                   SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                                        AugmentedInternalNode
                                                            (InternalNode
                                                            {
                                                                internalNodeRepresentation with
                                                                   SubtreeWithLesserLevelsForSameTestVariableIndex =
                                                                        subtreeToSwapAcross
                                                            })  
                                            })
                                  | _ ->
                                        AugmentedInternalNode
                                            (InternalNode
                                            {
                                                internalNodeRepresentation with
                                                   SubtreeWithLesserLevelsForSameTestVariableIndex =
                                                        modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                            })
                          | result when result > 0 ->
                                let modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex =
                                    add subtreeWithGreaterLevelsForSameTestVariableIndex
                                        newPartialTestVectorRepresentation
                                        false
                                        bringNewInternalNodeUpToSubtreeRoot
                                match modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex with
                                    AugmentedInternalNode
                                    (InternalNode
                                    ({
                                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeToSwapAcross   
                                     } as modifiedSubtreeNodeRepresentation)) when bringNewInternalNodeUpToSubtreeRoot ->
                                        AugmentedInternalNode
                                            (InternalNode
                                            {
                                                modifiedSubtreeNodeRepresentation with
                                                   SubtreeWithLesserLevelsForSameTestVariableIndex =
                                                        AugmentedInternalNode
                                                            (InternalNode
                                                            {
                                                                internalNodeRepresentation with
                                                                   SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                                                        subtreeToSwapAcross
                                                            })  
                                            })
                                  | _ ->
                                        AugmentedInternalNode
                                            (InternalNode
                                            {
                                                internalNodeRepresentation with
                                                    SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                                        modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex
                                            })
                          | _ ->
                                let modifiedSubtreeForFollowingIndices =
                                    add subtreeForFollowingIndices
                                        tailFromNewPartialTestVectorRepresentation
                                        true
                                        false
                                AugmentedInternalNode
                                    (InternalNode
                                    {
                                        internalNodeRepresentation with
                                            SubtreeForFollowingIndices =
                                                modifiedSubtreeForFollowingIndices
                                    })
                  | AugmentedInternalNode _
                    , None :: tailFromNewPartialTestVectorRepresentation when not treeIsForNextTestVariableIndex ->
                        raise (InternalAssertionViolationException "Cannot build a wildcard node at the root of a left or right subtree.")
                  | AugmentedInternalNode _
                    , None :: tailFromNewPartialTestVectorRepresentation ->
                        WildcardNode
                            {
                                SubtreeWithAllLevelsForSameTestVariableIndex = tree
                                SubtreeForFollowingIndices = buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation tailFromNewPartialTestVectorRepresentation
                            }
                  | _
                    , _ :: _ when not treeIsForNextTestVariableIndex ->
                        raise (InternalAssertionViolationException "This kind of node should not be the root of a left or right subtree.")
                  | _
                    , _ :: _ ->
                        raise (InternalAssertionViolationException "NOT ACTUALLY POSSIBLE: COVERED BY THE PREVIOUS PATTERN, BUT THE COMPILER DOESN'T KNOW THAT.")
                  | _
                    , [] when treeIsForNextTestVariableIndex ->
                        raise (InternalAssertionViolationException "Attempt to add a new partial test vector representation that is already mergeable with or equivalent to a previous one.")
                        // The above is really a precondition violation, but the precondition should have been enforced by the implementation and not by the client.
                  | _
                    , [] ->
                        raise (InternalAssertionViolationException ("Two problems: left or right subtrees should only be traversed with a non-empty new partial test vector representation"
                                                                     + " this kind of node should not be the root of a left or right subtree."))
            add ternarySearchTree newPartialTestVectorRepresentation true false

        let remove tree
                   queryPartialTestVectorRepresentation =
            let rec buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                            subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                            subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                            subtreeForFollowingIndices =
                match subtreeWithLesserLevelsForSameTestVariableIndex
                      , subtreeWithGreaterLevelsForSameTestVariableIndex
                      , subtreeForFollowingIndices with
                  | UnsuccessfulSearchTerminationNode
                    , UnsuccessfulSearchTerminationNode
                    , UnsuccessfulSearchTerminationNode ->
                        UnsuccessfulSearchTerminationNode
                  | UnsuccessfulSearchTerminationNode
                    , _
                    , UnsuccessfulSearchTerminationNode ->
                        subtreeWithGreaterLevelsForSameTestVariableIndex
                  | _
                    , UnsuccessfulSearchTerminationNode
                    , UnsuccessfulSearchTerminationNode ->
                        subtreeWithLesserLevelsForSameTestVariableIndex
                  | _
                    , _
                    , UnsuccessfulSearchTerminationNode ->
                        if subtreeWithLesserLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                           > subtreeWithGreaterLevelsForSameTestVariableIndex.NumberOfLevelsForLeadingTestVariable
                        then
                            let levelForTestVariableIndexFromRemovedNode
                                , subtreeForFollowingIndicesFromRemovedNode
                                , subtreeWithLesserLevelsForSameTestVariableIndexWithoutThatNode = removeInternalNodeWithGreatestLevelInSubtree subtreeWithLesserLevelsForSameTestVariableIndex
                            AugmentedInternalNode
                                (InternalNode
                                {
                                    LevelForTestVariableIndex =
                                        levelForTestVariableIndexFromRemovedNode
                                    SubtreeWithLesserLevelsForSameTestVariableIndex =
                                        subtreeWithLesserLevelsForSameTestVariableIndexWithoutThatNode
                                    SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                        subtreeWithGreaterLevelsForSameTestVariableIndex
                                    SubtreeForFollowingIndices =
                                        subtreeForFollowingIndicesFromRemovedNode
                                })
                        else
                            let levelForTestVariableIndexFromRemovedNode
                                , subtreeForFollowingIndicesFromRemovedNode
                                , subtreeWithGreaterLevelsForSameTestVariableIndexWithoutThatNode = removeInternalNodeWithLeastLevelInSubtree subtreeWithGreaterLevelsForSameTestVariableIndex
                            AugmentedInternalNode
                                (InternalNode
                                {
                                    LevelForTestVariableIndex =
                                        levelForTestVariableIndexFromRemovedNode
                                    SubtreeWithLesserLevelsForSameTestVariableIndex =
                                        subtreeWithLesserLevelsForSameTestVariableIndex
                                    SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                        subtreeWithGreaterLevelsForSameTestVariableIndexWithoutThatNode
                                    SubtreeForFollowingIndices =
                                        subtreeForFollowingIndicesFromRemovedNode
                                })                        
                  | _ ->
                        AugmentedInternalNode
                            (InternalNode
                            {
                                LevelForTestVariableIndex =
                                    levelForTestVariableIndex
                                SubtreeWithLesserLevelsForSameTestVariableIndex =
                                    subtreeWithLesserLevelsForSameTestVariableIndex
                                SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                    subtreeWithGreaterLevelsForSameTestVariableIndex
                                SubtreeForFollowingIndices =
                                    subtreeForFollowingIndices
                            })
            and removeInternalNodeWithGreatestLevelInSubtree subtree =
                match subtree with
                    AugmentedInternalNode
                    (InternalNode
                    {
                        LevelForTestVariableIndex = levelForTestVariableIndex
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    }) ->
                        levelForTestVariableIndex
                        , subtreeForFollowingIndices
                        , buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                  subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                  UnsuccessfulSearchTerminationNode
                                                                                                  UnsuccessfulSearchTerminationNode                                                                                                 
                  | AugmentedInternalNode
                    (InternalNode
                    {
                        LevelForTestVariableIndex = levelForTestVariableIndex
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    }) ->
                        let levelForTestVariableIndexFromRemovedNode
                            , subtreeForFollowingIndicesFromRemovedNode
                            , remainingSubtreeWithGreaterLevelsForSameTestVariableIndex = removeInternalNodeWithGreatestLevelInSubtree subtreeWithGreaterLevelsForSameTestVariableIndex
                        levelForTestVariableIndexFromRemovedNode
                        , subtreeForFollowingIndicesFromRemovedNode
                        , buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                  subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                  remainingSubtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                  subtreeForFollowingIndices
                  | _ ->
                        raise (InternalAssertionViolationException "This kind of node should not have been encountered.")
            and removeInternalNodeWithLeastLevelInSubtree subtree =
                match subtree with
                    AugmentedInternalNode
                    (InternalNode
                    {
                        LevelForTestVariableIndex = levelForTestVariableIndex
                        SubtreeWithLesserLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    }) ->
                        levelForTestVariableIndex
                        , subtreeForFollowingIndices
                        , buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                  UnsuccessfulSearchTerminationNode
                                                                                                  subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                  UnsuccessfulSearchTerminationNode                                                                                                 
                  | AugmentedInternalNode
                    (InternalNode
                    {
                        LevelForTestVariableIndex = levelForTestVariableIndex
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    }) ->
                        let levelForTestVariableIndexFromRemovedNode
                            , subtreeForFollowingIndicesFromRemovedNode
                            , remainingSubtreeWithLesserLevelsForSameTestVariableIndex = removeInternalNodeWithLeastLevelInSubtree subtreeWithLesserLevelsForSameTestVariableIndex
                        levelForTestVariableIndexFromRemovedNode
                        , subtreeForFollowingIndicesFromRemovedNode
                        , buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                  remainingSubtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                  subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                  subtreeForFollowingIndices
                  | _ ->
                        raise (InternalAssertionViolationException "This kind of node should not have been encountered.")
            let buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees subtreeWithAllLevelsForSameTestVariableIndex
                                                                                        subtreeForFollowingIndices =
                match subtreeWithAllLevelsForSameTestVariableIndex
                      , subtreeForFollowingIndices with
                    UnsuccessfulSearchTerminationNode
                    , UnsuccessfulSearchTerminationNode ->
                        UnsuccessfulSearchTerminationNode
                  | _
                    , UnsuccessfulSearchTerminationNode ->
                        subtreeWithAllLevelsForSameTestVariableIndex
                  | _ ->
                        WildcardNode
                            {
                                SubtreeWithAllLevelsForSameTestVariableIndex =
                                    subtreeWithAllLevelsForSameTestVariableIndex
                                SubtreeForFollowingIndices =
                                    subtreeForFollowingIndices
                            }
            let rec remove tree
                           queryPartialTestVectorRepresentation
                           treeIsForNextTestVariableIndex =
                match tree
                      , queryPartialTestVectorRepresentation with                
                    UnsuccessfulSearchTerminationNode
                    , [] when not treeIsForNextTestVariableIndex ->
                        raise (InternalAssertionViolationException "Left or right subtrees should only be traversed with a non-empty new partial test vector representation.")                                                    
                  | UnsuccessfulSearchTerminationNode
                    , _ ->
                        None
                  | SuccessfulSearchTerminationNode
                    , _ when treeIsForNextTestVariableIndex ->
                        Some (UnsuccessfulSearchTerminationNode
                              , queryPartialTestVectorRepresentation)
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    }
                    , headFromQueryPartialTestVectorRepresentation :: tailFromQueryPartialTestVectorRepresentation when treeIsForNextTestVariableIndex ->
                        buildResultFromWildcardNodeModifyingSubtreeForAllLevelsForTheSameTestVariableIndex queryPartialTestVectorRepresentation
                                                                                                           subtreeWithAllLevelsForSameTestVariableIndex
                                                                                                           subtreeForFollowingIndices
                        |> BargainBasement.Flip Option.LazyMPlus
                                                (lazy buildResultFromWildcardNodeModifyingSubtreeForFollowingTestVariableIndices queryPartialTestVectorRepresentation
                                                                                                                                 subtreeWithAllLevelsForSameTestVariableIndex
                                                                                                                                 subtreeForFollowingIndices)
                  | AugmentedInternalNode
                    (InternalNode
                    {
                        LevelForTestVariableIndex = levelForTestVariableIndex
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    })
                    , Some levelFromQueryPartialTestVectorRepresentation :: tailFromQueryPartialTestVectorRepresentation ->
                        match compare levelFromQueryPartialTestVectorRepresentation levelForTestVariableIndex with
                            result when result < 0 ->
                                buildResultFromInternalNodeModifyingSubtreeForLesserLevelsForTheSameTestVariableIndex queryPartialTestVectorRepresentation
                                                                                                                      levelForTestVariableIndex
                                                                                                                      subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                      subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                      subtreeForFollowingIndices                          
                          | result when result > 0 ->
                                buildResultFromInternalNodeModifyingSubtreeForGreaterLevelsForTheSameTestVariableIndex queryPartialTestVectorRepresentation
                                                                                                                       levelForTestVariableIndex
                                                                                                                       subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                       subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                       subtreeForFollowingIndices                          
                          | _ ->
                                buildResultFromInternalNodeModifyingSubtreeForFollowingTestVariableIndices queryPartialTestVectorRepresentation
                                                                                                           levelForTestVariableIndex
                                                                                                           subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                           subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                           subtreeForFollowingIndices
                  | AugmentedInternalNode
                    (InternalNode
                    {
                        LevelForTestVariableIndex = levelForTestVariableIndex
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    })
                    , None :: tailFromQueryPartialTestVectorRepresentation ->
                        buildResultFromInternalNodeModifyingSubtreeForFollowingTestVariableIndices queryPartialTestVectorRepresentation
                                                                                                   levelForTestVariableIndex
                                                                                                   subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                   subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                   subtreeForFollowingIndices
                        |> BargainBasement.Flip Option.LazyMPlus
                                                (lazy buildResultFromInternalNodeModifyingSubtreeForLesserLevelsForTheSameTestVariableIndex queryPartialTestVectorRepresentation
                                                                                                                                            levelForTestVariableIndex
                                                                                                                                            subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                                            subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                                            subtreeForFollowingIndices)
                        |> BargainBasement.Flip Option.LazyMPlus
                                           (lazy buildResultFromInternalNodeModifyingSubtreeForGreaterLevelsForTheSameTestVariableIndex queryPartialTestVectorRepresentation
                                                                                                                                        levelForTestVariableIndex
                                                                                                                                        subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                                        subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                                        subtreeForFollowingIndices)
                  | _
                    , _ :: _ when not treeIsForNextTestVariableIndex ->
                        raise (InternalAssertionViolationException "This kind of node should not be the root of a left or right subtree.")
                  | _
                    , _ :: _ ->
                        raise (InternalAssertionViolationException "NOT ACTUALLY POSSIBLE: COVERED BY THE PREVIOUS PATTERN, BUT THE COMPILER DOESN'T KNOW THAT.")
                  | _
                    , [] when treeIsForNextTestVariableIndex ->
                        // This has the effect of padding out a query partial test vector on the fly, thereby
                        // allowing a match as a prefix of some suitable stored vector, should one already be present.
                        remove tree
                               [None]
                               treeIsForNextTestVariableIndex
                  | _
                    , [] ->
                        raise (InternalAssertionViolationException ("Left or right subtrees should only be traversed with a non-empty new partial test vector representation"))
            and buildResultFromInternalNodeModifyingSubtreeForLesserLevelsForTheSameTestVariableIndex queryPartialTestVectorRepresentation
                                                                                                      levelForTestVariableIndex
                                                                                                      subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                      subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                      subtreeForFollowingIndices =
                    optionWorkflow
                        {
                            let! modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                 , removedPartialTestVector =
                                remove subtreeWithLesserLevelsForSameTestVariableIndex
                                       queryPartialTestVectorRepresentation
                                       false
                            return buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                           modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                           subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                           subtreeForFollowingIndices
                                   , removedPartialTestVector
                        }
            and buildResultFromInternalNodeModifyingSubtreeForGreaterLevelsForTheSameTestVariableIndex queryPartialTestVectorRepresentation
                                                                                                       levelForTestVariableIndex
                                                                                                       subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                       subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                       subtreeForFollowingIndices =
                    optionWorkflow
                        {
                            let! modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex
                                 , removedPartialTestVector =
                                remove subtreeWithGreaterLevelsForSameTestVariableIndex
                                       queryPartialTestVectorRepresentation
                                       false
                            return buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                           subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                           modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                           subtreeForFollowingIndices
                                   , removedPartialTestVector
                        }
            and buildResultFromInternalNodeModifyingSubtreeForFollowingTestVariableIndices queryPartialTestVectorRepresentation
                                                                                           levelForTestVariableIndex
                                                                                           subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                           subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                           subtreeForFollowingIndices = 
                    optionWorkflow  
                        {
                            let! modifiedSubtreeForFollowingTestVariableIndices
                                 , removedPartialTestVector =
                                remove subtreeForFollowingIndices
                                       (List.tail queryPartialTestVectorRepresentation)
                                       true
                            return buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                           subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                           subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                           modifiedSubtreeForFollowingTestVariableIndices
                                   , (Some levelForTestVariableIndex :: removedPartialTestVector)
                        }
            and buildResultFromWildcardNodeModifyingSubtreeForAllLevelsForTheSameTestVariableIndex queryPartialTestVectorRepresentation
                                                                                                   subtreeWithAllLevelsForSameTestVariableIndex
                                                                                                   subtreeForFollowingIndices =
                    optionWorkflow
                        {
                            let! modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                 , removedPartialTestVector = 
                                remove subtreeWithAllLevelsForSameTestVariableIndex
                                       queryPartialTestVectorRepresentation
                                       true
                            return buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                                                                                           subtreeForFollowingIndices
                                   , removedPartialTestVector
                        }
            and buildResultFromWildcardNodeModifyingSubtreeForFollowingTestVariableIndices queryPartialTestVectorRepresentation
                                                                                           subtreeWithAllLevelsForSameTestVariableIndex
                                                                                           subtreeForFollowingIndices =
                    optionWorkflow
                        {
                            let! modifiedSubtreeForFollowingTestVariableIndices
                                 , removedPartialTestVector =
                                remove subtreeForFollowingIndices
                                       (List.tail queryPartialTestVectorRepresentation)
                                       true
                            return buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees subtreeWithAllLevelsForSameTestVariableIndex
                                                                                                           modifiedSubtreeForFollowingTestVariableIndices
                                   , (List.head queryPartialTestVectorRepresentation :: removedPartialTestVector)
                        }
            remove tree queryPartialTestVectorRepresentation true
            
        let checkInvariant tree =
            let rec checkInvariant tree
                                   treeIsForNextTestVariableIndex
                                   lowerBound
                                   upperBound =
                match tree with
                    SuccessfulSearchTerminationNode when not treeIsForNextTestVariableIndex ->
                        raise (LogicErrorException "Successful searches cannot terminate at left or right subtree heads.")
                  | SuccessfulSearchTerminationNode ->
                        1u
                  | UnsuccessfulSearchTerminationNode ->
                        0u
                  | WildcardNode _ when not treeIsForNextTestVariableIndex ->
                        raise (LogicErrorException "Wildcard matches must be located at the head of a subtree for all levels belonging to a test variable.")
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    } ->
                        let numberOfSuccessfulPathsFromSubtreeWithAllLevelsForSameTestVariableIndex =
                            checkInvariant subtreeWithAllLevelsForSameTestVariableIndex
                                           false
                                           lowerBound
                                           upperBound
                        let numberOfSuccessfulPathsFromSubtreeForFollowingIndices =
                            checkInvariant subtreeForFollowingIndices
                                           true
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
                            checkInvariant subtreeWithLesserLevelsForSameTestVariableIndex
                                           false
                                           lowerBound
                                           liftedLevel
                        let numberOfSuccessfulPathsFromSubtreeWithGreaterLevelsForSameTestVariableIndex =
                            checkInvariant subtreeWithGreaterLevelsForSameTestVariableIndex
                                           false
                                           liftedLevel
                                           upperBound
                        let numberOfSuccessfulPathsFromSubtreeForFollowingIndices =
                            checkInvariant subtreeForFollowingIndices
                                           true
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
            if 0u = checkInvariant tree
                                   true
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
            MergedPartialTestVectorRepresentations<'Level> (UnsuccessfulSearchTerminationNode)

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
//                            match remove ternarySearchTreeWithoutMergeCandidate
//                                         mergedPartialTestVectorRepresentation with
//                                Some _ ->
//                                    raise (LogicErrorException "The merged removed partial vector still matches with something left behind!")
//                              | _ ->
//                                    ()   
                            add ternarySearchTreeWithoutMergeCandidate
                                mergedPartialTestVectorRepresentation
                                randomBehaviour
                      | None ->
                            add ternarySearchTree
                                partialTestVectorRepresentation
                                randomBehaviour
//                checkInvariant modifiedTernarySearchTree
                MergedPartialTestVectorRepresentations modifiedTernarySearchTree
