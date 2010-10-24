module SageSerpent.Infrastructure.BargainBasement

    open System.Collections.Generic
    open System

//    let rec CrossProduct sequences =
//        match sequences with
//            [] -> [[]]
//          | head :: tail -> let crossProductOfTail = CrossProduct tail
//                            [for itemInHead in head do
//                                for itemInCrossProductOfTail in crossProductOfTail do
//                                    yield itemInHead :: itemInCrossProductOfTail] 
    
                                    
    let rec CrossProductWithCommonSuffix commonSuffix lists =
        match lists with
            [] -> [commonSuffix]
          | head :: tail -> let crossProductOfTail = CrossProductWithCommonSuffix commonSuffix tail
                            head
                            |> List.map (fun itemInHead ->
                                            crossProductOfTail
                                            |> List.map (fun itemInCrossProductOfTail ->
                                                            itemInHead :: itemInCrossProductOfTail))
                            |> List.concat
                            
    let CrossProduct lists =
        CrossProductWithCommonSuffix [] lists

    let rec MergeSortedListsAllowingDuplicates first second =
        match first, second with
            _, [] ->
                first
          | [], _ ->
                second
          | headFromFirst :: tailFromFirst, headFromSecond :: _ when headFromFirst < headFromSecond ->
                headFromFirst :: MergeSortedListsAllowingDuplicates tailFromFirst second
          | headFromFirst :: _, headFromSecond :: tailFromSecond when headFromFirst > headFromSecond ->
                headFromSecond :: MergeSortedListsAllowingDuplicates first tailFromSecond
          | headFromFirst :: tailFromFirst, headFromSecond :: tailFromSecond ->
                headFromFirst :: headFromSecond :: MergeSortedListsAllowingDuplicates tailFromFirst tailFromSecond


    let CountDuplicatesInSortedList list =
        let rec produceItemCountPairs list =
            match list with
                [] ->
                    []
              | head :: next :: tail when head = next ->
                    match produceItemCountPairs (next :: tail) with
                        (duplicatedItem, count) :: partialResultTail -> (duplicatedItem, count + 1u) :: partialResultTail
                      | _ -> raise (InternalAssertionViolationException "The partial result in this situation should be non-empty.")
              | head :: tail ->
                    (head, 1u) :: produceItemCountPairs tail
        produceItemCountPairs list
        
        
    let AssociatedValues association =
        (association :> IDictionary<'Key, 'Value>).Values
        
    let rec BreakOff sizeOfFirstPart listBeingBroken =
        match sizeOfFirstPart
              , listBeingBroken with
            0u
            , _ ->
                []
                , listBeingBroken
          | _
            , head :: tail ->
                let partialFirstPart
                    , secondPart =
                        BreakOff (sizeOfFirstPart - 1u) tail
                head :: partialFirstPart
                , secondPart
          | _ -> raise (PreconditionViolationException "Attempt to break off more elements than present in list.")
    

    let rec ChopUpList listBeingChopped spans =
        match spans with
            [] ->
                []
          | span :: tail ->
                let section
                    , remainder =
                        BreakOff span listBeingChopped
                section :: ChopUpList remainder tail                 
        
    let Memoize computation =
        let cache =
            System.Collections.Generic.Dictionary ()
        fun input ->
            if cache.ContainsKey input
            then cache.[input]
            else let result =
                    computation input
                 cache.Add (input, result)
                 result
                 
    let DeferredDefault defaultComputation optional =
        match optional with
            Some _ ->
                optional
          | None ->
                defaultComputation () 
                
    type private TypePreservingFunction<'X> =
        delegate of 'X -> 'X
                
    let IdentityFunctionDelegate =
        (TypePreservingFunction (fun x -> x)) :> Delegate 
        
    let PartitionSizeIntoSectionsOfRandomNonZeroLength size
                                                       numberOfSubgroups
                                                       randomBehaviour =
        if size = 0u
        then raise (PreconditionViolationException "Must have at least one item to start with for subgroups to have non-zero length.")                                                         
        match numberOfSubgroups with
            0u ->
                raise (PreconditionViolationException "Must have at least one subgroup to place items into.")
          | 1u ->
                [ size ]
          | _ ->
                if numberOfSubgroups > size
                then raise (PreconditionViolationException "Number of subgroups must be at most the number of items.")
                let potentialPartitionPoints
                    = [1u .. size - 1u]
                let chosenPartitionPoints =
                    (randomBehaviour: RandomBehaviour).ChooseSeveralOf potentialPartitionPoints (numberOfSubgroups - 1u)
                    |> Seq.sort
                seq {yield 0u
                     yield! chosenPartitionPoints
                     yield size}
                |> Seq.pairwise
                |> Seq.map (function lesserPartitionPoint
                                     , greaterPartitionPoint -> greaterPartitionPoint - lesserPartitionPoint)
                |> List.ofSeq
                 
                 
    let PartitionItemsIntoSubgroupsOfRandomNonZeroLength items
                                                         numberOfSubgroups
                                                         randomBehaviour =
        let numberOfItems =
            uint32 (List.length items)
        let spans =
            PartitionSizeIntoSectionsOfRandomNonZeroLength numberOfItems
                                                           numberOfSubgroups
                                                           randomBehaviour
        ChopUpList items spans
        
    let Flip f x y =
        f y x   
        
    let Curry f x y =
        f (x, y)
        
    let Uncurry f (x, y) =
        f x y
                
    let NumberOfPermutations originalSize permutationSize =
        if permutationSize > originalSize
        then 0u
        else let numberOfItemsLeftOutOfPermutation =
                originalSize - permutationSize
             let rec productOfPartialResultAndNumberOfSubpermutations originalSize partialResult =
                if originalSize = numberOfItemsLeftOutOfPermutation
                then partialResult
                else productOfPartialResultAndNumberOfSubpermutations (originalSize - 1u) (originalSize * partialResult)
             productOfPartialResultAndNumberOfSubpermutations originalSize 1u
        
    let Factorial x =
        NumberOfPermutations x x
        
    let NumberOfCombinations originalSize combinationSize =
        NumberOfPermutations originalSize combinationSize
        / Factorial combinationSize
        
    let IsSorted<'a when 'a: comparison> =
            Seq.pairwise
            >> Seq.forall (fun (lhs: 'a
                                , rhs)
                            -> lhs < rhs)
        
    let MappingAvoidingIndices sortedIndicesToAvoid =
        if List.length sortedIndicesToAvoid = 0
        then raise (PreconditionViolationException "Must have at least one index to avoid.")
        // TODO: reinstate the precondition when I've got a workaround / bug-fix / explanation of the
        // strange type error that otherwise occurs.
        //if not (IsSorted sortedIndicesToAvoid)
        //then raise (PreconditionViolationException "Indices to avoid must be presented in ascending order.")
        let sortedAssociationBetweenIndicesAndIncrementsToApply =
            let arrangeDeferredAssociations (runningIncrement
                                             , deferredActionsForPredecessors)
                                            indexToAvoid =
                runningIncrement + 1u
                , (fun associationBuiltSoFar ->
                    deferredActionsForPredecessors ((indexToAvoid - runningIncrement
                                                    , runningIncrement + 1u)
                                                      :: associationBuiltSoFar))
            (sortedIndicesToAvoid
             |> List.fold arrangeDeferredAssociations (0u, (fun result -> result))
             |> snd) []
            |> Map.ofList   // This has the effect of eliminating all but the last entry for a group of associations
                            // for consecutive indices. Otherwise the associations for lesser indices in the group
                            // would just map onto the next higher index, which we are also trying to avoid.
            |> Map.toArray
        let remapIndex index =
            let foundIndex =
                Array.BinarySearch (sortedAssociationBetweenIndicesAndIncrementsToApply,
                                    (index
                                     , 0u),
                                    {
                                        new IComparer<UInt32 * UInt32> with
                                            member this.Compare (first, second) =
                                                compare (fst first) (fst second)
                                    })
            let incrementToApplyToIndex =
                if foundIndex >= 0
                then snd sortedAssociationBetweenIndicesAndIncrementsToApply.[foundIndex]
                else let foundIndex =
                            ~~~ foundIndex
                     if foundIndex > 0
                     then snd sortedAssociationBetweenIndicesAndIncrementsToApply.[foundIndex - 1]
                     else 0u
            index + incrementToApplyToIndex
        remapIndex
        
        