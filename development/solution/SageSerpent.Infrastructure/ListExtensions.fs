module SageSerpent.Infrastructure.ListExtensions

    let rec private crossProductWithCommonSuffix commonSuffix lists =
        match lists with
            [] -> [commonSuffix]
          | head :: tail -> let crossProductOfTail = crossProductWithCommonSuffix commonSuffix tail
                            head
                            |> List.map (fun itemInHead ->
                                            crossProductOfTail
                                            |> List.map (fun itemInCrossProductOfTail ->
                                                            itemInHead :: itemInCrossProductOfTail))
                            |> List.concat

    let rec private mergeSortedListsAllowingDuplicates first second =
        match first, second with
            _, [] ->
                first
          | [], _ ->
                second
          | headFromFirst :: tailFromFirst, headFromSecond :: _ when headFromFirst < headFromSecond ->
                headFromFirst :: mergeSortedListsAllowingDuplicates tailFromFirst second
          | headFromFirst :: _, headFromSecond :: tailFromSecond when headFromFirst > headFromSecond ->
                headFromSecond :: mergeSortedListsAllowingDuplicates first tailFromSecond
          | headFromFirst :: tailFromFirst, headFromSecond :: tailFromSecond ->
                headFromFirst :: headFromSecond :: mergeSortedListsAllowingDuplicates tailFromFirst tailFromSecond

    let private countDuplicatesInSortedList list =
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

    let rec private breakOff sizeOfFirstPart listBeingBroken =
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
                        breakOff (sizeOfFirstPart - 1u) tail
                head :: partialFirstPart
                , secondPart
          | _ -> raise (PreconditionViolationException "Attempt to break off more elements than present in list.")

    let rec private chopUpList listBeingChopped spans =
            match spans with
                [] ->
                    []
              | span :: tail ->
                    let section
                        , remainder =
                        breakOff span listBeingChopped
                    section :: chopUpList remainder tail

    type Microsoft.FSharp.Collections.List<'X> with
        static member CrossProductWithCommonSuffix commonSuffix lists =
            crossProductWithCommonSuffix commonSuffix lists

        static member CrossProduct lists =
            crossProductWithCommonSuffix [] lists

        static member MergeSortedListsAllowingDuplicates first second =
            mergeSortedListsAllowingDuplicates first second

        static member CountDuplicatesInSortedList list =
            countDuplicatesInSortedList list

        member this.BreakOff sizeOfFirstPart =
            breakOff sizeOfFirstPart this

        member this.ChopUpList spans =
            chopUpList this spans