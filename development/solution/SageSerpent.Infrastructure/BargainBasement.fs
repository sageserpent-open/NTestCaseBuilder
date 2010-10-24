#light

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
                |> List.of_seq
                 
                 
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
