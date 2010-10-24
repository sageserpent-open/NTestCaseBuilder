#light

//let crossProduct sequences =
//    let workingCrossProduct = Array.zero_create (Seq.length sequences)
//    let rec unfoldCrossProduct sequences index =
//        match sequences with
//            [] -> [List.of_array workingCrossProduct]
//          | head :: tail -> [for itemInHead in head do
//                                workingCrossProduct.[int32 index] <- itemInHead
//                                yield! unfoldCrossProduct tail (index + 1u)]
//    unfoldCrossProduct sequences 0u
//
//printf "%A\n" (crossProduct [[1;2]; [-9]; [0;8;8]])

let crossProduct lists =
    let workingCrossProduct = Array.zero_create (List.length lists)
    let rec unfoldCrossProduct lists index =
        match lists with
            [] -> [List.of_array workingCrossProduct]
          | head :: tail -> head
                            |> List.map (fun itemInHead -> workingCrossProduct.[int32 index] <- itemInHead;
                                                           unfoldCrossProduct tail (index + 1u))
                            |> List.concat
    unfoldCrossProduct lists 0u

printf "%A\n" (crossProduct [[1;2]; [-9]; [0;8;8]])

()