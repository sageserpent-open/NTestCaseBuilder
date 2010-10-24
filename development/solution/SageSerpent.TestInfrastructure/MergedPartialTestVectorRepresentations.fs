namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System.Collections.Generic
    open System
    open SageSerpent.Infrastructure
    open Microsoft.FSharp.Collections
    
    type InternalNodeRepresentation<'Level when 'Level: comparison> =
        {
            LevelForTestVariableIndex: 'Level
            SubtreeWithLesserLevelsForSameTestVariableIndex: MergedPartialTestVectorRepresentations<'Level>
            SubtreeWithGreaterLevelsForSameTestVariableIndex: MergedPartialTestVectorRepresentations<'Level>
            SubtreeForFollowingIndices: MergedPartialTestVectorRepresentations<'Level>
        }
    and WildcardNodeRepresentation<'Level when 'Level: comparison> =
        {
            SubtreeWithAllLevelsForSameTestVariableIndex: MergedPartialTestVectorRepresentations<'Level>
            SubtreeForFollowingIndices: MergedPartialTestVectorRepresentations<'Level>
        }
    and MergedPartialTestVectorRepresentations<'Level when 'Level: comparison> =
        SuccessfulSearchTerminationNode
      | UnsuccessfulSearchTerminationNode
      | WildcardNode of WildcardNodeRepresentation<'Level>
      | InternalNode of InternalNodeRepresentation<'Level>
      
        interface IEnumerable<Map<UInt32, 'Level>> with
            member this.GetEnumerator () =
                this.CreatePartialTestVectorSequence().GetEnumerator ()
        interface IEnumerable with
            member this.GetEnumerator () =
                (this.CreatePartialTestVectorSequence() :> IEnumerable).GetEnumerator ()
                
        member private this.CreatePartialTestVectorSequence () =
            let rec traverseTree tree
                                 testVariableIndex
                                 partialTestVectorBeingBuilt
                                 subtreeIsForANewTestVariableIndex =
                match tree with
                    SuccessfulSearchTerminationNode ->
                        // NOTE: as we are converting to a map, we can be cavalier about the
                        // order in which associative pairs are added to the partial test vector.
                        seq {if subtreeIsForANewTestVariableIndex
                             then if not (List.isEmpty partialTestVectorBeingBuilt)
                                  then yield partialTestVectorBeingBuilt
                                             |> Map.ofList
                                  else raise (InternalAssertionViolationException "Should not contain an empty partial vector: attempts to merge in empty partial vectors result in the original collection.")
                             else raise (InternalAssertionViolationException "A successful search cannot terminate on a left or right subtree.")}
                  | UnsuccessfulSearchTerminationNode ->
                        Seq.empty
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    } -> Seq.concat [Seq.delay (fun () ->
                                                        traverseTree subtreeWithAllLevelsForSameTestVariableIndex
                                                                     testVariableIndex
                                                                     partialTestVectorBeingBuilt
                                                                     false);
                                     Seq.delay (fun () ->
                                                        traverseTree subtreeForFollowingIndices
                                                                     (testVariableIndex + 1u)
                                                                     partialTestVectorBeingBuilt
                                                                     true)]
                  | InternalNode
                    {
                        LevelForTestVariableIndex = levelForTestVariableIndex
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    } -> Seq.concat [Seq.delay (fun () ->
                                                        traverseTree subtreeWithLesserLevelsForSameTestVariableIndex
                                                                     testVariableIndex
                                                                     partialTestVectorBeingBuilt
                                                                     false);
                                     Seq.delay (fun () ->
                                                        traverseTree subtreeForFollowingIndices
                                                                     (testVariableIndex + 1u)
                                                                     ((testVariableIndex, levelForTestVariableIndex) :: partialTestVectorBeingBuilt)
                                                                     true);
                                     Seq.delay (fun () ->
                                                        traverseTree subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                     testVariableIndex
                                                                     partialTestVectorBeingBuilt
                                                                     false)]
            traverseTree this 0u [] true
                
        static member private FillOutPartialTestVectorWithIndeterminates partialTestVectorRepresentation =
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
                                 
        member private this.Add newPartialTestVectorRepresentation =
            let rec add tree
                        newPartialTestVectorRepresentation
                        treeIsForNextTestVariableIndex =
                let buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation newPartialTestVectorRepresentation =
                        List.foldBack (fun optionalLevel
                                         degenerateLinearSubtree ->
                                            match optionalLevel with
                                                Some level ->
                                                    InternalNode
                                                        {
                                                            LevelForTestVariableIndex = level
                                                            SubtreeWithLesserLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                                            SubtreeWithGreaterLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                                            SubtreeForFollowingIndices = degenerateLinearSubtree
                                                        }
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
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    }
                    , Some _ :: _ when treeIsForNextTestVariableIndex ->
                        WildcardNode
                            {
                                SubtreeWithAllLevelsForSameTestVariableIndex = add subtreeWithAllLevelsForSameTestVariableIndex
                                                                                   newPartialTestVectorRepresentation
                                                                                   true
                                SubtreeForFollowingIndices = subtreeForFollowingIndices
                            }      
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    }
                    , None :: tailFromNewPartialTestVectorRepresentation when treeIsForNextTestVariableIndex ->
                        WildcardNode
                            {
                                SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                                SubtreeForFollowingIndices = add subtreeForFollowingIndices
                                                               tailFromNewPartialTestVectorRepresentation
                                                               true
                            }      
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    }
                    , [] when treeIsForNextTestVariableIndex ->
                        WildcardNode
                            {
                                SubtreeWithAllLevelsForSameTestVariableIndex = add subtreeWithAllLevelsForSameTestVariableIndex
                                                                                   []
                                                                                   true
                                SubtreeForFollowingIndices = subtreeForFollowingIndices
                            }
                  | InternalNode
                    {
                        LevelForTestVariableIndex = levelForTestVariableIndex
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    }
                    , Some levelFromNewPartialTestVectorRepresentation :: tailFromNewPartialTestVectorRepresentation ->
                        match compare levelFromNewPartialTestVectorRepresentation levelForTestVariableIndex with
                            result when result < 0  ->
                            InternalNode
                                {
                                    LevelForTestVariableIndex =
                                        levelForTestVariableIndex
                                    SubtreeWithLesserLevelsForSameTestVariableIndex =
                                        add subtreeWithLesserLevelsForSameTestVariableIndex newPartialTestVectorRepresentation false
                                    SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                        subtreeWithGreaterLevelsForSameTestVariableIndex
                                    SubtreeForFollowingIndices =
                                        subtreeForFollowingIndices
                                }
                          | result when result > 0 ->
                            InternalNode
                                {
                                    LevelForTestVariableIndex =
                                        levelForTestVariableIndex
                                    SubtreeWithLesserLevelsForSameTestVariableIndex =
                                        subtreeWithLesserLevelsForSameTestVariableIndex
                                    SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                        add subtreeWithGreaterLevelsForSameTestVariableIndex newPartialTestVectorRepresentation false
                                    SubtreeForFollowingIndices =
                                        subtreeForFollowingIndices
                                }
                          | _ ->
                            InternalNode
                                {
                                    LevelForTestVariableIndex =
                                        levelForTestVariableIndex
                                    SubtreeWithLesserLevelsForSameTestVariableIndex =
                                        subtreeWithLesserLevelsForSameTestVariableIndex
                                    SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                        subtreeWithGreaterLevelsForSameTestVariableIndex
                                    SubtreeForFollowingIndices =
                                        add subtreeForFollowingIndices tailFromNewPartialTestVectorRepresentation true
                                }
                  | InternalNode
                    {
                        LevelForTestVariableIndex = _
                        SubtreeWithLesserLevelsForSameTestVariableIndex = _
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = _
                        SubtreeForFollowingIndices = _
                    }
                    , None :: tailFromNewPartialTestVectorRepresentation when not treeIsForNextTestVariableIndex ->
                        raise (InternalAssertionViolationException "Cannot build a wildcard node at the root of a left or right subtree.")
                  | InternalNode
                    {
                        LevelForTestVariableIndex = _
                        SubtreeWithLesserLevelsForSameTestVariableIndex = _
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = _
                        SubtreeForFollowingIndices = _
                    }
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
            add this newPartialTestVectorRepresentation true
                           
        member private this.Remove queryPartialTestVectorRepresentation =
            let buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                        subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                        subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                        subtreeForFollowingIndices =
                match subtreeWithLesserLevelsForSameTestVariableIndex
                      , subtreeWithGreaterLevelsForSameTestVariableIndex
                      , subtreeForFollowingIndices with
                    UnsuccessfulSearchTerminationNode
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
                  | _ ->
                        InternalNode
                            {
                                LevelForTestVariableIndex =
                                    levelForTestVariableIndex
                                SubtreeWithLesserLevelsForSameTestVariableIndex =
                                    subtreeWithLesserLevelsForSameTestVariableIndex
                                SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                    subtreeWithGreaterLevelsForSameTestVariableIndex
                                SubtreeForFollowingIndices =
                                    subtreeForFollowingIndices
                            }
            let buildResultFromInternalNodeFromPartialResultFromSubtreeForLesserLevelsForTheSameTestVariableIndex levelForTestVariableIndex
                                                                                                                  subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                  subtreeForFollowingIndices =
                Option.bind (fun (modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                  , removedPartialTestVector) ->
                                    Some (buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                                  modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                  subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                  subtreeForFollowingIndices
                                          , removedPartialTestVector))
            let buildResultFromInternalNodeFromPartialResultFromSubtreeForGreaterLevelsForTheSameTestVariableIndex levelForTestVariableIndex
                                                                                                                   subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                   subtreeForFollowingIndices =
                Option.bind (fun (modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex
                                  , removedPartialTestVector) ->
                                    Some (buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                                  subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                  modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                  subtreeForFollowingIndices
                                          , removedPartialTestVector))
            let buildResultFromInternalNodeFromPartialResultFromSubtreeForFollowingTestVariableIndices levelForTestVariableIndex
                                                                                                       subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                       subtreeWithGreaterLevelsForSameTestVariableIndex =
                Option.bind (fun (modifiedSubtreeForFollowingTestVariableIndices
                                  , removedPartialTestVector) ->
                                    Some (buildResultSubtreeFromInternalNodeWithPruningOfDegenerateLinearSubtrees levelForTestVariableIndex
                                                                                                                  subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                  subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                  modifiedSubtreeForFollowingTestVariableIndices
                                          , (Some levelForTestVariableIndex :: removedPartialTestVector)))
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
            let buildResultFromWildcardNodeFromPartialResultFromSubtreeForAllLevelsForTheSameTestVariableIndex subtreeForFollowingIndices =
                Option.bind (fun (modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                  , removedPartialTestVector) ->
                                    Some (buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees modifiedSubtreeWithAllLevelsForSameTestVariableIndex
                                                                                                                  subtreeForFollowingIndices
                                          , removedPartialTestVector))
            let buildResultFromWildcardNodeFromPartialResultFromSubtreeForFollowingTestVariableIndices optionalLevelForTestVariableIndex
                                                                                                       subtreeWithAllLevelsForSameTestVariableIndex =
                Option.bind (fun (modifiedSubtreeForFollowingTestVariableIndices
                                  , removedPartialTestVector) ->
                                    Some (buildResultSubtreeFromWildcardNodeWithPruningOfDegenerateLinearSubtrees subtreeWithAllLevelsForSameTestVariableIndex
                                                                                                                  modifiedSubtreeForFollowingTestVariableIndices
                                          , (optionalLevelForTestVariableIndex :: removedPartialTestVector)))
            let rec remove tree
                           queryPartialTestVectorRepresentation
                           treeIsForNextTestVariableIndex
                           testVariableIndex =
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
                        remove subtreeWithAllLevelsForSameTestVariableIndex
                               queryPartialTestVectorRepresentation
                               true
                               testVariableIndex
                        |> buildResultFromWildcardNodeFromPartialResultFromSubtreeForAllLevelsForTheSameTestVariableIndex subtreeForFollowingIndices                                         
                        |> BargainBasement.DeferredDefault (fun () ->
                                                                remove subtreeForFollowingIndices
                                                                       tailFromQueryPartialTestVectorRepresentation
                                                                       true
                                                                       (testVariableIndex + 1u)
                                                                |> buildResultFromWildcardNodeFromPartialResultFromSubtreeForFollowingTestVariableIndices headFromQueryPartialTestVectorRepresentation
                                                                                                                                                          subtreeWithAllLevelsForSameTestVariableIndex)
                  | WildcardNode
                    {
                        SubtreeWithAllLevelsForSameTestVariableIndex = subtreeWithAllLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    }
                    , [] when treeIsForNextTestVariableIndex ->
                        // This has the effect of padding out a query partial test vector on the fly, thereby
                        // allowing a match as a prefix of some suitable stored vector, should one already be present.      
                        remove subtreeWithAllLevelsForSameTestVariableIndex
                               [None]
                               true
                               testVariableIndex
                        |> buildResultFromWildcardNodeFromPartialResultFromSubtreeForAllLevelsForTheSameTestVariableIndex subtreeForFollowingIndices                                         
                        |> BargainBasement.DeferredDefault (fun () ->
                                                                remove subtreeForFollowingIndices
                                                                       [None]
                                                                       true
                                                                       (testVariableIndex + 1u)
                                                                |> buildResultFromWildcardNodeFromPartialResultFromSubtreeForFollowingTestVariableIndices None
                                                                                                                                                          subtreeWithAllLevelsForSameTestVariableIndex)
                  | InternalNode
                    {
                        LevelForTestVariableIndex = levelForTestVariableIndex
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    }
                    , Some levelFromQueryPartialTestVectorRepresentation :: tailFromQueryPartialTestVectorRepresentation ->
                        match compare levelFromQueryPartialTestVectorRepresentation levelForTestVariableIndex with
                            result when result < 0 ->
                                remove subtreeWithLesserLevelsForSameTestVariableIndex
                                       queryPartialTestVectorRepresentation
                                       false
                                       testVariableIndex
                                |> buildResultFromInternalNodeFromPartialResultFromSubtreeForLesserLevelsForTheSameTestVariableIndex levelForTestVariableIndex
                                                                                                                                     subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                                     subtreeForFollowingIndices
                          | result when result > 0 ->
                                remove subtreeWithGreaterLevelsForSameTestVariableIndex
                                       queryPartialTestVectorRepresentation
                                       false
                                       testVariableIndex
                                |> buildResultFromInternalNodeFromPartialResultFromSubtreeForGreaterLevelsForTheSameTestVariableIndex levelForTestVariableIndex
                                                                                                                                      subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                                      subtreeForFollowingIndices
                          | _ ->
                                remove subtreeForFollowingIndices
                                       tailFromQueryPartialTestVectorRepresentation
                                       true
                                       (testVariableIndex + 1u)
                                |> buildResultFromInternalNodeFromPartialResultFromSubtreeForFollowingTestVariableIndices levelForTestVariableIndex
                                                                                                                          subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                          subtreeWithGreaterLevelsForSameTestVariableIndex
                  | InternalNode
                    {
                        LevelForTestVariableIndex = levelForTestVariableIndex
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    }
                    , None :: tailFromQueryPartialTestVectorRepresentation ->
                        remove subtreeForFollowingIndices
                               tailFromQueryPartialTestVectorRepresentation
                               true
                               (testVariableIndex + 1u)
                        |> buildResultFromInternalNodeFromPartialResultFromSubtreeForFollowingTestVariableIndices levelForTestVariableIndex
                                                                                                                  subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                  subtreeWithGreaterLevelsForSameTestVariableIndex
                        |> BargainBasement.DeferredDefault (fun () ->
                                                                remove subtreeWithLesserLevelsForSameTestVariableIndex 
                                                                       queryPartialTestVectorRepresentation
                                                                       false
                                                                       testVariableIndex
                                                                |> buildResultFromInternalNodeFromPartialResultFromSubtreeForLesserLevelsForTheSameTestVariableIndex levelForTestVariableIndex
                                                                                                                                                                     subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                                                                     subtreeForFollowingIndices)
                        |> BargainBasement.DeferredDefault (fun () ->
                                                                remove subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                       queryPartialTestVectorRepresentation
                                                                       false
                                                                       testVariableIndex
                                                                |> buildResultFromInternalNodeFromPartialResultFromSubtreeForGreaterLevelsForTheSameTestVariableIndex levelForTestVariableIndex
                                                                                                                                                                      subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                                                                      subtreeForFollowingIndices)
                  | InternalNode
                    {
                        LevelForTestVariableIndex = levelForTestVariableIndex
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForFollowingIndices = subtreeForFollowingIndices
                    }
                    , [] ->
                        // This has the effect of padding out a query partial test vector on the fly, thereby 
                        // allowing a match as a prefix of some suitable stored vector, should one already be present.
                        remove subtreeForFollowingIndices
                               [None]
                               true
                               (testVariableIndex + 1u)
                        |> buildResultFromInternalNodeFromPartialResultFromSubtreeForFollowingTestVariableIndices levelForTestVariableIndex
                                                                                                                  subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                  subtreeWithGreaterLevelsForSameTestVariableIndex
                        |> BargainBasement.DeferredDefault (fun () ->
                                                                remove subtreeWithLesserLevelsForSameTestVariableIndex 
                                                                       [None]
                                                                       false
                                                                       testVariableIndex
                                                                |> buildResultFromInternalNodeFromPartialResultFromSubtreeForLesserLevelsForTheSameTestVariableIndex levelForTestVariableIndex
                                                                                                                                                                     subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                                                                     subtreeForFollowingIndices)
                        |> BargainBasement.DeferredDefault (fun () ->
                                                                remove subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                       [None]
                                                                       false
                                                                       testVariableIndex
                                                                |> buildResultFromInternalNodeFromPartialResultFromSubtreeForGreaterLevelsForTheSameTestVariableIndex levelForTestVariableIndex
                                                                                                                                                                      subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                                                                      subtreeForFollowingIndices)
                  | _
                    , _ :: _ when not treeIsForNextTestVariableIndex ->
                        raise (InternalAssertionViolationException "This kind of node should not be the root of a left or right subtree.")
                  | _
                    , _ :: _ ->
                        raise (InternalAssertionViolationException "NOT ACTUALLY POSSIBLE: COVERED BY THE PREVIOUS PATTERN, BUT THE COMPILER DOESN'T KNOW THAT.")
                  | _
                    , [] when treeIsForNextTestVariableIndex ->
                        raise (InternalAssertionViolationException "Attempt to remove a partial test vector representation that is a prefix of a previous one without extending it.")
                  | _
                    , [] ->
                        raise (InternalAssertionViolationException ("Two problems: left or right subtrees should only be traversed with a non-empty new partial test vector representation"
                                                                    + " this kind of node should not be the root of a left or right subtree."))                
            remove this queryPartialTestVectorRepresentation true 0u
            
        member this.MergeOrAdd partialTestVectorRepresentation =
            if Map.isEmpty partialTestVectorRepresentation
            then this
            else let partialTestVectorRepresentation =
                    MergedPartialTestVectorRepresentations.FillOutPartialTestVectorWithIndeterminates partialTestVectorRepresentation
                 match this.Remove partialTestVectorRepresentation with
                    Some (thisWithoutMergeCandidate
                          , mergedPartialTestVectorRepresentation) ->
                        thisWithoutMergeCandidate.Add mergedPartialTestVectorRepresentation
                  | None ->
                        this.Add partialTestVectorRepresentation
        
    module MergedPartialTestVectorRepresentations =
        let initial =
            UnsuccessfulSearchTerminationNode
    