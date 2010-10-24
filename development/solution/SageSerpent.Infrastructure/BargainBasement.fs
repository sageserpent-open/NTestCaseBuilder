#light

module SageSerpent.Infrastructure.BargainBasement

    open System.Collections.Generic

//    let rec CrossProduct sequences =
//        match sequences with
//            [] -> [[]]
//          | head :: tail -> let crossProductOfTail = CrossProduct tail
//                            [for itemInHead in head do
//                                for itemInCrossProductOfTail in crossProductOfTail do
//                                    yield itemInHead :: itemInCrossProductOfTail] 
    
                                    
    let rec CrossProduct lists =
        match lists with
            [] -> [[]]
          | head :: tail -> let crossProductOfTail = CrossProduct tail
                            head
                            |> List.map (fun itemInHead ->
                                            crossProductOfTail
                                            |> List.map (fun itemInCrossProductOfTail ->
                                                            itemInHead :: itemInCrossProductOfTail))
                            |> List.concat


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
