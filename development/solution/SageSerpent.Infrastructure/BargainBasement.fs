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

