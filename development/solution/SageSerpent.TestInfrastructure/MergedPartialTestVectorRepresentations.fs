#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System.Collections.Generic
    open System
    open SageSerpent.Infrastructure
    open Microsoft.FSharp.Collections
    
    type LevelRepresentation =
        Level of Object
      | Wildcard
        interface IComparable with
            override this.CompareTo that =
                match this, unbox that with
                    Level thisLevel, Level thatLevel ->
                        compare thisLevel thatLevel
                  | _ ->
                        0
                        
        override this.Equals that =
              match this, unbox that with
                Level thisLevel, Level thatLevel ->
                    thisLevel.Equals thatLevel
              | _ ->
                    true
                     
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
            let rec traverseTree tree testVariableIndex partialTestVectorBeingBuilt =
                match tree with
                    LeafNode ->
                        seq { yield partialTestVectorBeingBuilt
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
                                                                     partialTestVectorBeingBuilt);
                                     Seq.delay (fun () ->
                                                        traverseTree subtreeForGreaterIndices
                                                                     (testVariableIndex + 1u)
                                                                     (match levelForTestVariableIndex with
                                                                        Level level ->
                                                                            (testVariableIndex, level) :: partialTestVectorBeingBuilt
                                                                      | Wildcard ->
                                                                            partialTestVectorBeingBuilt));
                                     Seq.delay (fun () ->
                                                        traverseTree subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                     testVariableIndex
                                                                     partialTestVectorBeingBuilt)]
            traverseTree this 0u []
                
        static member private FillOutPartialTestVectorWithWildcards partialTestVectorRepresentation =
            let rec fillInNonConsecutiveIndicesWithWildcardEntries partialTestVectorRepresentation =
                match partialTestVectorRepresentation with
                    [] ->
                        []
                  | head :: tail ->
                        match head with
                            testVariableIndex, level ->
                                let partialResult = fillInNonConsecutiveIndicesWithWildcardEntries tail
                                let expectedPreviousTestVariableIndex = testVariableIndex + 1u
                                match tail with
                                    (previousTestVariableIndex, _) :: _ when previousTestVariableIndex > expectedPreviousTestVariableIndex ->
                                        let filledOutSection =
                                            List.init (int32 (previousTestVariableIndex - expectedPreviousTestVariableIndex))
                                                      (fun _ -> Wildcard)
                                        Level level :: List.append filledOutSection partialResult
                                  | _ ->
                                        Level level :: partialResult  
            partialTestVectorRepresentation
            |> Map.to_list
            |> fillInNonConsecutiveIndicesWithWildcardEntries                           

        static member private Merge first second =
            List.zip first second
            |> List.map (fun (first, second) ->
                            match first, second with
                                Level _, _ ->
                                    first
                              | _ ->
                                second)
                                
        member private this.Add partialTestVectorRepresentation =
            match this with
                LeafNode ->
                    if List.is_empty partialTestVectorRepresentation
                    then raise (InternalAssertionViolationException "Attempt to add a partial test vector representation that is already mergeable with or equivalent to a previous one.")
                    List.fold_right (fun level
                                         degenerateLinearSubtree ->
                                            InternalNode
                                                {
                                                    LevelForTestVariableIndex = level
                                                    SubtreeWithLesserLevelsForSameTestVariableIndex = LeafNode
                                                    SubtreeWithGreaterLevelsForSameTestVariableIndex = LeafNode
                                                    SubtreeForGreaterIndices = degenerateLinearSubtree
                                                }) partialTestVectorRepresentation LeafNode
              | InternalNode
                {
                    LevelForTestVariableIndex = levelForTestVariableIndex
                    SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                    SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                    SubtreeForGreaterIndices = subtreeForGreaterIndices
                } ->
                    match partialTestVectorRepresentation with
                        [] ->
                            raise (PreconditionViolationException "Attempt to add a partial test vector representation that is a prefix of a previous one.")
                      | head :: tail ->                            
                            match compare head levelForTestVariableIndex with
                                -1 ->
                                InternalNode
                                    {
                                        LevelForTestVariableIndex =
                                            levelForTestVariableIndex
                                        SubtreeWithLesserLevelsForSameTestVariableIndex =
                                            subtreeWithLesserLevelsForSameTestVariableIndex.Add partialTestVectorRepresentation
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
                                            subtreeForGreaterIndices.Add tail
                                    }
                              | 1 ->
                                InternalNode
                                    {
                                        LevelForTestVariableIndex =
                                            levelForTestVariableIndex
                                        SubtreeWithLesserLevelsForSameTestVariableIndex =
                                            subtreeWithLesserLevelsForSameTestVariableIndex
                                        SubtreeWithGreaterLevelsForSameTestVariableIndex =
                                            subtreeWithGreaterLevelsForSameTestVariableIndex.Add partialTestVectorRepresentation
                                        SubtreeForGreaterIndices =
                                            subtreeForGreaterIndices
                                    }
                              | _ -> raise (InternalAssertionViolationException "'compare' function violated postcondition.") 
                            
        member private this.Remove partialTestVectorRepresentation =
            match this with
                LeafNode ->
                    if List.is_empty partialTestVectorRepresentation
                    then Some (LeafNode
                               , [])
                    else None                         
              | InternalNode
                {
                    LevelForTestVariableIndex = levelForTestVariableIndex
                    SubtreeWithLesserLevelsForSameTestVariableIndex = subtreeWithLesserLevelsForSameTestVariableIndex
                    SubtreeWithGreaterLevelsForSameTestVariableIndex = subtreeWithGreaterLevelsForSameTestVariableIndex
                    SubtreeForGreaterIndices = subtreeForGreaterIndices
                } ->
                    match partialTestVectorRepresentation with
                        [] ->
                            raise (PreconditionViolationException "Attempt to remove a partial test vector representation that is a prefix of a previous one.")
                      | head :: tail ->
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
                            match compare head levelForTestVariableIndex with
                                -1 -> 
                                    let partialResult =
                                        subtreeWithLesserLevelsForSameTestVariableIndex.Remove partialTestVectorRepresentation
                                    match partialResult with
                                        None ->
                                            None
                                      | Some (modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                              , removedPartialTestVector) ->
                                            Some (buildResultSubtreeWithPruningOfDegenerateLinearSubtrees modifiedSubtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                          subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                          subtreeForGreaterIndices
                                                  , removedPartialTestVector)
                              | 0 -> 
                                    let partialResult =
                                        subtreeForGreaterIndices.Remove tail
                                    match partialResult with
                                        None ->
                                            None
                                      | Some (modifiedSubtreeForGreaterIndices
                                              , removedPartialTestVector) ->
                                            Some (buildResultSubtreeWithPruningOfDegenerateLinearSubtrees subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                          subtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                          modifiedSubtreeForGreaterIndices
                                                  , (levelForTestVariableIndex :: removedPartialTestVector))
                              | 1 -> 
                                    let partialResult =
                                        subtreeWithGreaterLevelsForSameTestVariableIndex.Remove partialTestVectorRepresentation
                                    match partialResult with
                                        None ->
                                            None
                                      | Some (modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex
                                              , removedPartialTestVector) ->
                                            Some (buildResultSubtreeWithPruningOfDegenerateLinearSubtrees subtreeWithLesserLevelsForSameTestVariableIndex
                                                                                                          modifiedSubtreeWithGreaterLevelsForSameTestVariableIndex
                                                                                                          subtreeForGreaterIndices
                                                  , removedPartialTestVector)
                              | _ -> raise (InternalAssertionViolationException "'compare' function violated postcondition.") 
        
        static member Empty =
            LeafNode
            
        member this.MergeOrAdd partialTestVectorRepresentation =
            if Map.is_empty partialTestVectorRepresentation
            then raise (PreconditionViolationException "Attempt to add an empty partial test vector representation.")
            let partialTestVectorRepresentation =
                MergedPartialTestVectorRepresentations.FillOutPartialTestVectorWithWildcards partialTestVectorRepresentation
            match this.Remove partialTestVectorRepresentation with
                Some (thisWithoutMergeCandidate
                      , mergeCandidate) ->
                    let mergedPartialTestVectorRepresentation =
                        MergedPartialTestVectorRepresentations.Merge mergeCandidate partialTestVectorRepresentation
                    thisWithoutMergeCandidate.Add mergedPartialTestVectorRepresentation
              | None ->
                    this.Add partialTestVectorRepresentation              
            
            
            

        
