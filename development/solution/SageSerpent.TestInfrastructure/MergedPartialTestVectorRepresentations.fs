namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System.Collections.Generic
    open System
    open SageSerpent.Infrastructure
    open Microsoft.FSharp.Collections
    
    type LevelRepresentation<'Level when 'Level: comparison> =
        Level of 'Level
      | Indeterminate                  
    and InternalNodeRepresentation<'Level when 'Level: comparison> =
        {
            LevelForTestVariableIndex: LevelRepresentation<'Level>
            SubtreeWithLesserLevelsForSameTestVariableIndex: MergedPartialTestVectorRepresentations<'Level>
            SubtreeWithGreaterLevelsForSameTestVariableIndex: MergedPartialTestVectorRepresentations<'Level>
            SubtreeForGreaterIndices: MergedPartialTestVectorRepresentations<'Level>
        }
    and MergedPartialTestVectorRepresentations<'Level when 'Level: comparison> =
        SuccessfulSearchTerminationNode
      | UnsuccessfulSearchTerminationNode
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
                  | InternalNode
                    {
                        LevelForTestVariableIndex = levelForTestVariableIndex
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForGreaterIndices = subtreeForGreaterIndices
                    } -> Seq.concat [Seq.delay (fun () ->
                                                        traverseTree subtreeWithLesserLevelsForSameTestVariableIndex
                                                                     testVariableIndex
                                                                     partialTestVectorBeingBuilt
                                                                     false);
                                     Seq.delay (fun () ->
                                                        traverseTree subtreeForGreaterIndices
                                                                     (testVariableIndex + 1u)
                                                                     (match levelForTestVariableIndex with
                                                                        Level level ->
                                                                            (testVariableIndex, level) :: partialTestVectorBeingBuilt
                                                                      | Indeterminate ->
                                                                            partialTestVectorBeingBuilt)
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
                                      (fun _ -> Indeterminate)
                        List.append filledOutSection partialResult
                  | _ ->
                        partialResult
            let rec fillInNonConsecutiveIndicesWithIndeterminateEntries partialTestVectorRepresentation =
                match partialTestVectorRepresentation with
                    [] ->
                        []
                        , None
                  | head :: tail ->
                        match head with
                            testVariableIndex, level ->
                                let partialResult
                                    , previousTestVariableIndex =
                                    fillInNonConsecutiveIndicesWithIndeterminateEntries tail
                                Level (level: 'Level) :: fillIfNecessary (testVariableIndex + 1u) previousTestVariableIndex partialResult
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
                let buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation () =
                        List.foldBack (fun level
                                         degenerateLinearSubtree ->
                                            InternalNode
                                                {
                                                    LevelForTestVariableIndex = level
                                                    SubtreeWithLesserLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                                    SubtreeWithGreaterLevelsForSameTestVariableIndex = UnsuccessfulSearchTerminationNode
                                                    SubtreeForGreaterIndices = degenerateLinearSubtree
                                                }) newPartialTestVectorRepresentation SuccessfulSearchTerminationNode                
                match tree with
                    SuccessfulSearchTerminationNode ->
                        if List.isEmpty newPartialTestVectorRepresentation
                        then if treeIsForNextTestVariableIndex
                             then raise (InternalAssertionViolationException "Attempt to add a new partial test vector representation that is already mergeable with or equivalent to a previous one.")
                                    // The above is really a precondition violation, but the precondition should have been enforced by the implementation and not by the client.
                             else raise (InternalAssertionViolationException ("Two problems: left or right subtrees should only be added to with a non-empty new partial test vector representation"
                                                                              + " and a successful search cannot terminate on a left or right subtree."))
                        buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation ()                             
                  | UnsuccessfulSearchTerminationNode ->
                        if List.isEmpty newPartialTestVectorRepresentation
                           && not treeIsForNextTestVariableIndex
                        then raise (InternalAssertionViolationException "Left or right subtrees should only be added to with a non-empty new partial test vector representation.")
                        buildDegenerateLinearSubtreeForDanglingSuffixOfNewPartialTestVectorRepresentation ()                             
                  | InternalNode
                    {
                        LevelForTestVariableIndex = levelForTestVariableIndex
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForGreaterIndices = subtreeForGreaterIndices
                    } ->
                        match newPartialTestVectorRepresentation with
                            [] ->
                                if treeIsForNextTestVariableIndex
                                then raise (InternalAssertionViolationException "Attempt to add a new partial test vector representation that is a prefix of a previous one.")
                                        // The above is really a precondition violation, but the precondition should have been enforced by the implementation and not by the client.
                                else raise (InternalAssertionViolationException "Left or right subtrees should only be added to with a non-empty new partial test vector representation.")
                          | headFromNewPartialTestVectorRepresentation :: tailFromNewPartialTestVectorRepresentation ->                            
                                match compare headFromNewPartialTestVectorRepresentation levelForTestVariableIndex with
                                    result when result < 0  ->
                                    InternalNode
                                        {
                                            LevelForTestVariableIndex =
                                                levelForTestVariableIndex
                                            SubtreeWithLesserLevelsForSameTestVariableIndex =
                                                add subtreeWithLesserLevelsForSameTestVariableIndex newPartialTestVectorRepresentation false
                                            SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                                subtreeWithGreaterLevelsForSameTestVariableIndex
                                            SubtreeForGreaterIndices =
                                                subtreeForGreaterIndices
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
                                            SubtreeForGreaterIndices =
                                                subtreeForGreaterIndices
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
                                            SubtreeForGreaterIndices =
                                                add subtreeForGreaterIndices tailFromNewPartialTestVectorRepresentation true
                                        }
            add this newPartialTestVectorRepresentation true
                           
        member private this.Remove queryPartialTestVectorRepresentation =
            let rec remove tree
                           queryPartialTestVectorRepresentation
                           treeIsForNextTestVariableIndex
                           testVariableIndex =
                match tree with
                    SuccessfulSearchTerminationNode ->
                        if treeIsForNextTestVariableIndex
                        then // It is tempting to try to optimise this case by checking to see whether the query partial test vector didn't need to have
                             // any of its wildcard levels filled out by merging from the matching stored vector: this would allow the detection of attempts
                             // to add a vector that has an existing stored vector as an exact prefix, or of duplicate additions of the same vector. Don't
                             // bother: the client of this class is 'TestCaseEnumerableFactoryCommonImplementation' and that will always add longer vectors
                             // before shorter ones, so the first variation of this case *never* occurs and the second variation has yet to be observed over
                             // extensive testing of 'TestCaseEnumerableFactoryCommonImplementation'.
                             Some (UnsuccessfulSearchTerminationNode
                                   , queryPartialTestVectorRepresentation)
                        else if List.isEmpty queryPartialTestVectorRepresentation
                             then raise (InternalAssertionViolationException ("Two problems: left or right subtrees should only be searched with a non-empty query partial test vector representation"
                                                                              + " and a successful search cannot terminate on a left or right subtree."))
                             else raise (InternalAssertionViolationException "A successful search cannot terminate on a left or right subtree.")
                  | UnsuccessfulSearchTerminationNode ->
                        if List.isEmpty queryPartialTestVectorRepresentation
                           && not treeIsForNextTestVariableIndex
                        then raise (InternalAssertionViolationException "Left or right subtrees should only be searched with a non-empty query partial test vector representation.")
                        None        
                  | InternalNode
                    {
                        LevelForTestVariableIndex = levelForTestVariableIndex
                        SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                        SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                        SubtreeForGreaterIndices = subtreeForGreaterIndices
                    } ->
                        let buildResultSubtreeWithPruningOfDegenerateLinearSubtrees subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                    subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                    subtreeForGreaterIndices =
                            match subtreeWithLesserLevelsForSameTestVariableIndex
                                  , subtreeWithGreaterLevelsForSameTestVariableIndex
                                  , subtreeForGreaterIndices with
                                UnsuccessfulSearchTerminationNode
                                , UnsuccessfulSearchTerminationNode
                                , UnsuccessfulSearchTerminationNode ->
                                    UnsuccessfulSearchTerminationNode
                              | _ ->
                                    InternalNode
                                        {
                                            LevelForTestVariableIndex =
                                                levelForTestVariableIndex
                                            SubtreeWithLesserLevelsForSameTestVariableIndex =
                                                subtreeWithLesserLevelsForSameTestVariableIndex
                                            SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                                subtreeWithGreaterLevelsForSameTestVariableIndex
                                            SubtreeForGreaterIndices =
                                                subtreeForGreaterIndices
                                        }
                        let removeInAppropriateSubtree headFromQueryPartialTestVectorRepresentation
                                                       tailFromQueryPartialTestVectorRepresentation
                                                       levelForTestVariableIndex =
                            let buildResultFromPartialResultFromSubtreeForLesserLevelsForTheSameTestVariableIndex =
                                Option.bind (fun (modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                                  , removedPartialTestVector) ->
                                                    Some (buildResultSubtreeWithPruningOfDegenerateLinearSubtrees modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                  subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                  subtreeForGreaterIndices
                                                          , removedPartialTestVector))
                            let buildResultFromPartialResultFromSubtreeForGreaterLevelsForTheSameTestVariableIndex =
                                Option.bind (fun (modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex
                                                  , removedPartialTestVector) ->
                                                    Some (buildResultSubtreeWithPruningOfDegenerateLinearSubtrees subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                  modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                  subtreeForGreaterIndices
                                                          , removedPartialTestVector))
                            let buildResultFromPartialResultFromSubtreeForFollowingTestVariableIndices mergedLevelForTestVariableIndex =
                                Option.bind (fun (modifiedSubtreeForGreaterIndices
                                                  , removedPartialTestVector) ->
                                                    Some (buildResultSubtreeWithPruningOfDegenerateLinearSubtrees subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                                  subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                                  modifiedSubtreeForGreaterIndices
                                                          , (mergedLevelForTestVariableIndex :: removedPartialTestVector)))
                            match headFromQueryPartialTestVectorRepresentation
                                  , levelForTestVariableIndex with
                              Level _
                              , Level _ ->
                                    match compare headFromQueryPartialTestVectorRepresentation levelForTestVariableIndex with
                                        result when result < 0 ->
                                            remove subtreeWithLesserLevelsForSameTestVariableIndex (headFromQueryPartialTestVectorRepresentation
                                                                                                     :: tailFromQueryPartialTestVectorRepresentation)
                                                                                                   false
                                                                                                   testVariableIndex
                                            |> buildResultFromPartialResultFromSubtreeForLesserLevelsForTheSameTestVariableIndex
                                      | result when result > 0 ->
                                            remove subtreeWithGreaterLevelsForSameTestVariableIndex (headFromQueryPartialTestVectorRepresentation
                                                                                                      :: tailFromQueryPartialTestVectorRepresentation)
                                                                                                    false
                                                                                                    testVariableIndex
                                            |> buildResultFromPartialResultFromSubtreeForGreaterLevelsForTheSameTestVariableIndex
                                      | _ ->
                                            remove subtreeForGreaterIndices
                                                   tailFromQueryPartialTestVectorRepresentation
                                                   true
                                                   (testVariableIndex + 1u)
                                            |> buildResultFromPartialResultFromSubtreeForFollowingTestVariableIndices levelForTestVariableIndex
                            | Level _
                              , Indeterminate ->
                                    remove subtreeForGreaterIndices
                                           tailFromQueryPartialTestVectorRepresentation
                                           true
                                           (testVariableIndex + 1u)
                                    |> buildResultFromPartialResultFromSubtreeForFollowingTestVariableIndices headFromQueryPartialTestVectorRepresentation
                                    |> BargainBasement.DeferredDefault (fun () ->
                                                                            match compare headFromQueryPartialTestVectorRepresentation levelForTestVariableIndex with
                                                                                result when result < 0 ->
                                                                                    remove subtreeWithLesserLevelsForSameTestVariableIndex (headFromQueryPartialTestVectorRepresentation
                                                                                                                                             :: tailFromQueryPartialTestVectorRepresentation)
                                                                                                                                           false
                                                                                                                                           testVariableIndex
                                                                                    |> buildResultFromPartialResultFromSubtreeForLesserLevelsForTheSameTestVariableIndex
                                                                              | result when result > 0 -> 
                                                                                    remove subtreeWithGreaterLevelsForSameTestVariableIndex (headFromQueryPartialTestVectorRepresentation
                                                                                                                                              :: tailFromQueryPartialTestVectorRepresentation)
                                                                                                                                            false
                                                                                                                                            testVariableIndex
                                                                                    |> buildResultFromPartialResultFromSubtreeForGreaterLevelsForTheSameTestVariableIndex
                                                                              | _ ->
                                                                                    raise (InternalAssertionViolationException "Should not get an exact match as the query level is definite but the stored one is indeterminate."))
                            | _ ->
                                    remove subtreeForGreaterIndices
                                           tailFromQueryPartialTestVectorRepresentation
                                           true
                                           (testVariableIndex + 1u)
                                    |> buildResultFromPartialResultFromSubtreeForFollowingTestVariableIndices levelForTestVariableIndex
                                    |> BargainBasement.DeferredDefault (fun () ->
                                                                            remove subtreeWithLesserLevelsForSameTestVariableIndex (headFromQueryPartialTestVectorRepresentation
                                                                                                                                     :: tailFromQueryPartialTestVectorRepresentation)
                                                                                                                                   false
                                                                                                                                   testVariableIndex
                                                                            |> buildResultFromPartialResultFromSubtreeForLesserLevelsForTheSameTestVariableIndex)
                                    |> BargainBasement.DeferredDefault (fun () ->
                                                                            remove subtreeWithGreaterLevelsForSameTestVariableIndex (headFromQueryPartialTestVectorRepresentation
                                                                                                                                      :: tailFromQueryPartialTestVectorRepresentation)
                                                                                                                                    false
                                                                                                                                    testVariableIndex
                                                                            |> buildResultFromPartialResultFromSubtreeForGreaterLevelsForTheSameTestVariableIndex)
                        match queryPartialTestVectorRepresentation with
                            [] ->
                                if treeIsForNextTestVariableIndex
                                then    // Keep going by padding 'queryPartialTestVectorRepresentation' with wildcards until we come to
                                        // the end of the matching stored partial test vector. This allows us to match query vectors that
                                        // are prefixes of stored ones.
                                        // As above, it is tempting to apply an optimisation here to detect the case where the query partial test vector
                                        // is a suffix of a stored vector with an exact match without requiring any of its wildcard levels to be filled
                                        // out by merging. Unlike the case above, this has been observed through testing, but only occurs about 6% of the
                                        // time during a test run, so again the optimisation has been omitted.
                                     removeInAppropriateSubtree Indeterminate
                                                                []
                                                                levelForTestVariableIndex                  
                                else raise (InternalAssertionViolationException "Left or right subtrees should only be searched with a non-empty query partial test vector representation.") 
                          | headFromQueryPartialTestVectorRepresentation :: tailFromQueryPartialTestVectorRepresentation ->
                                removeInAppropriateSubtree headFromQueryPartialTestVectorRepresentation
                                                           tailFromQueryPartialTestVectorRepresentation
                                                           levelForTestVariableIndex
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
    