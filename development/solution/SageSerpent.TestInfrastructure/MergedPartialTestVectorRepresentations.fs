#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System.Collections.Generic
    open System
    open SageSerpent.Infrastructure
    open Microsoft.FSharp.Collections
    
    type LevelRepresentation =
        Level of Object
      | Indeterminate                     
    and InternalNodeRepresentation =
        {
            LevelForTestVariableIndex: LevelRepresentation
            SubtreeWithLesserLevelsForSameTestVariableIndex: MergedPartialTestVectorRepresentations
            SubtreeWithGreaterLevelsForSameTestVariableIndex: MergedPartialTestVectorRepresentations
            SubtreeForGreaterIndices: MergedPartialTestVectorRepresentations
        }
    and MergedPartialTestVectorRepresentations =
        LeafNode
      | InternalNode of InternalNodeRepresentation
      
        interface IEnumerable<Map<UInt32, Object>> with
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
                    LeafNode ->
                        seq { if subtreeIsForANewTestVariableIndex
                              then yield partialTestVectorBeingBuilt
                                            |> Map.of_list }    // NOTE: as we are converting to a map, we can be cavalier about the
                                                                // order in which associative pairs are added to the partial test vector.
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
                                Level level :: fillIfNecessary (testVariableIndex + 1u) previousTestVariableIndex partialResult
                                , Some testVariableIndex
            let partialTestVectorPossiblyWithLeadingEntriesMissing
                , lowestTestVariableIndex =
                partialTestVectorRepresentation
                |> Map.to_list
                |> fillInNonConsecutiveIndicesWithIndeterminateEntries  
            fillIfNecessary 0u lowestTestVariableIndex partialTestVectorPossiblyWithLeadingEntriesMissing
                                 
        static member private Merge first second =
            match first
                  , second with
                (headFromFirst :: tailFromFirst)
                , (headFromSecond :: tailFromSecond) ->
                    (match headFromFirst
                           , headFromSecond with
                        Level _
                        , _ ->
                            headFromFirst
                      | _ ->
                        headFromSecond) :: MergedPartialTestVectorRepresentations.Merge tailFromFirst tailFromSecond
              | (_ :: _)
                , [] ->
                    first
              | []
                , (_ :: _) ->
                    second
              | []
                , [] ->
                    []   
                                
        member private this.Add newPartialTestVectorRepresentation =
            let rec add tree newPartialTestVectorRepresentation treeIsForNextTestVariableIndex =
                match tree with
                    LeafNode ->
                        if List.is_empty newPartialTestVectorRepresentation
                        then if treeIsForNextTestVariableIndex
                             then raise (InternalAssertionViolationException "Attempt to add a new partial test vector representation that is already mergeable with or equivalent to a previous one.")
                                    // The above is really a precondition violation, but the precondition should have been enforced by the implementation and not by the client.
                             else raise (InternalAssertionViolationException "Left or right subtrees should only be added to with a non-empty new partial test vector representation.")
                        List.fold_right (fun level
                                             degenerateLinearSubtree ->
                                                InternalNode
                                                    {
                                                        LevelForTestVariableIndex = level
                                                        SubtreeWithLesserLevelsForSameTestVariableIndex = LeafNode
                                                        SubtreeWithGreaterLevelsForSameTestVariableIndex = LeafNode
                                                        SubtreeForGreaterIndices = degenerateLinearSubtree
                                                    }) newPartialTestVectorRepresentation LeafNode
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
                                    -1 ->
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
                                  | 0 ->
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
                                  | 1 ->
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
                                  | _ -> raise (InternalAssertionViolationException "Comparison violated postcondition.") 
            add this newPartialTestVectorRepresentation true
                           
        member private this.Remove queryPartialTestVectorRepresentation =
            let rec remove tree queryPartialTestVectorRepresentation treeIsForNextTestVariableIndex testVariableIndex =
                match tree with
                    LeafNode ->
                        if treeIsForNextTestVariableIndex
                        then Some (LeafNode
                                   , [])
                        else if List.is_empty queryPartialTestVectorRepresentation
                             then raise (InternalAssertionViolationException "Left or right subtrees should only be searched with a non-empty query partial test vector representation.")
                             else None                        
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
                                LeafNode
                                , LeafNode
                                , LeafNode ->
                                    LeafNode
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
                        let buildResultFromPartialResultFromSubtreeForFollowingTestVariableIndices =
                            Option.bind (fun (modifiedSubtreeForGreaterIndices
                                              , removedPartialTestVector) ->
                                                Some (buildResultSubtreeWithPruningOfDegenerateLinearSubtrees subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                              subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                              modifiedSubtreeForGreaterIndices
                                                      , (levelForTestVariableIndex :: removedPartialTestVector)))
                        match queryPartialTestVectorRepresentation with
                            [] ->
                                if treeIsForNextTestVariableIndex
                                then    // Keep going by recursing on the subtree for the next test variable index: this is effectively
                                        // the same as padding 'queryPartialTestVectorRepresentation' with wildcards until we come to
                                        // the end of the matching stored partial test vector. This allows us to match query vectors that
                                        // are prefixes of stored ones.
                                     remove subtreeForGreaterIndices [] true (testVariableIndex + 1u)
                                     |> buildResultFromPartialResultFromSubtreeForFollowingTestVariableIndices                  
                                else raise (InternalAssertionViolationException "Left or right subtrees should only be searched with a non-empty query partial test vector representation.") 
                          | headFromQueryPartialTestVectorRepresentation :: tailFromQueryPartialTestVectorRepresentation ->
                                match headFromQueryPartialTestVectorRepresentation
                                      , levelForTestVariableIndex with
                                  Level _
                                  , Level _ ->
                                        match compare headFromQueryPartialTestVectorRepresentation levelForTestVariableIndex with
                                            -1 ->
                                                remove subtreeWithLesserLevelsForSameTestVariableIndex queryPartialTestVectorRepresentation false testVariableIndex
                                                |> buildResultFromPartialResultFromSubtreeForLesserLevelsForTheSameTestVariableIndex
                                          | 0 ->
                                                remove subtreeForGreaterIndices tailFromQueryPartialTestVectorRepresentation true (testVariableIndex + 1u)
                                                |> buildResultFromPartialResultFromSubtreeForFollowingTestVariableIndices
                                          | 1 -> 
                                                remove subtreeWithGreaterLevelsForSameTestVariableIndex queryPartialTestVectorRepresentation false testVariableIndex
                                                |> buildResultFromPartialResultFromSubtreeForGreaterLevelsForTheSameTestVariableIndex
                                          | _ -> raise (InternalAssertionViolationException "Comparison violated postcondition.")
                                | Level _
                                  , Indeterminate ->
                                        remove subtreeForGreaterIndices tailFromQueryPartialTestVectorRepresentation true (testVariableIndex + 1u)
                                        |> buildResultFromPartialResultFromSubtreeForFollowingTestVariableIndices
                                        |> BargainBasement.DeferredDefault (fun () ->
                                                                                match compare headFromQueryPartialTestVectorRepresentation levelForTestVariableIndex with
                                                                                    -1 ->
                                                                                        remove subtreeWithLesserLevelsForSameTestVariableIndex queryPartialTestVectorRepresentation false testVariableIndex
                                                                                        |> buildResultFromPartialResultFromSubtreeForLesserLevelsForTheSameTestVariableIndex
                                                                                  | 0 ->
                                                                                        raise (InternalAssertionViolationException "Should not get an exact match as the query level is definite but the stored one is indeterminate.")
                                                                                  | 1 -> 
                                                                                        remove subtreeWithGreaterLevelsForSameTestVariableIndex queryPartialTestVectorRepresentation false testVariableIndex
                                                                                        |> buildResultFromPartialResultFromSubtreeForGreaterLevelsForTheSameTestVariableIndex
                                                                                  | _ -> raise (InternalAssertionViolationException "Comparison violated postcondition."))
                                | _ ->
                                        remove subtreeForGreaterIndices tailFromQueryPartialTestVectorRepresentation true (testVariableIndex + 1u)
                                        |> buildResultFromPartialResultFromSubtreeForFollowingTestVariableIndices
                                        |> BargainBasement.DeferredDefault (fun () ->
                                                                                remove subtreeWithLesserLevelsForSameTestVariableIndex queryPartialTestVectorRepresentation false testVariableIndex
                                                                                |> buildResultFromPartialResultFromSubtreeForLesserLevelsForTheSameTestVariableIndex)
                                        |> BargainBasement.DeferredDefault (fun () ->
                                                                                remove subtreeWithGreaterLevelsForSameTestVariableIndex queryPartialTestVectorRepresentation false testVariableIndex
                                                                                |> buildResultFromPartialResultFromSubtreeForGreaterLevelsForTheSameTestVariableIndex)
                                                
            remove this queryPartialTestVectorRepresentation true 0u                  
                                  
        static member Initial =
            LeafNode
            
        member this.MergeOrAdd partialTestVectorRepresentation =
            if Map.is_empty partialTestVectorRepresentation
            then this
            else let partialTestVectorRepresentation =
                    MergedPartialTestVectorRepresentations.FillOutPartialTestVectorWithIndeterminates partialTestVectorRepresentation
                 match this.Remove partialTestVectorRepresentation with
                    Some (thisWithoutMergeCandidate
                          , mergeCandidate) ->
                        let mergedPartialTestVectorRepresentation =
                            MergedPartialTestVectorRepresentations.Merge mergeCandidate partialTestVectorRepresentation
                        thisWithoutMergeCandidate.Add mergedPartialTestVectorRepresentation
                  | None ->
                        this.Add partialTestVectorRepresentation

            

        
