#light

module SageSerpent.Infrastructure.BargainBasement

//let rec CrossProduct sequences =
//    match sequences with
//        [] -> [[]]
//      | head :: tail -> let crossProductOfTail = CrossProduct tail
//                        [for itemInHead in head do
//                            for itemInCrossProductOfTail in crossProductOfTail do
//                                yield itemInHead :: itemInCrossProductOfTail] 
//
                                
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
            