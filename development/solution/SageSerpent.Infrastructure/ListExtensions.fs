module SageSerpent.Infrastructure.ListExtensions
    let enumerateTreeOfCrossProductPrefixes reverseOfCommonPrefix
                                            sequences
                                            commonSuffix =
        let rec enumerateTreeOfCrossProductPrefixes reverseOfCommonPrefix
                                                    sequences =
            match sequences with
                [] ->
                    raise (InternalAssertionViolationException "This case should not occur - there is a guard in the calling function, and the following pattern's logic prevents recursion from getting to the empty list case.")
              | [ singleton ] ->
                    seq
                        {
                            for item in singleton do
                                yield item :: reverseOfCommonPrefix
                                      |> List.fold (fun suffixOfAListInFullCrossProduct
                                                        itemBeingPrepended ->
                                                        itemBeingPrepended :: suffixOfAListInFullCrossProduct)
                                                   commonSuffix
                        }
              | head :: tail ->
                    Seq.delay (fun () ->
                                seq
                                    {
                                        for item in head do
                                            yield! enumerateTreeOfCrossProductPrefixes (item :: reverseOfCommonPrefix)
                                                                                       tail
                                    })
        enumerateTreeOfCrossProductPrefixes reverseOfCommonPrefix
                                            sequences
    let private crossProductWithCommonSuffix commonSuffix
                                             sequences =
        match sequences with
            [] ->
                Seq.singleton commonSuffix
          | _ ->
                enumerateTreeOfCrossProductPrefixes []
                                                    sequences
                                                    commonSuffix

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



    let Chunk chunkSize sequenceBeingChunked =
        if 0u = chunkSize
        then
            raise (PreconditionViolationException "Chunk size must be non-zero.")
        seq
            {
                let mutableBuffer =
                    Array.zeroCreate (int32 chunkSize)
                let mutableIndex = ref 0
                for item in sequenceBeingChunked do
                    let bufferIndex =
                        !mutableIndex % (int32 chunkSize)
                    mutableBuffer.[bufferIndex] <- item
                    if chunkSize = uint32 bufferIndex + 1u
                    then
                        yield mutableBuffer
                              |> List.ofArray
                    mutableIndex := !mutableIndex + 1
                let bufferIndex =
                    !mutableIndex % (int32 chunkSize)
                if 0 <> bufferIndex
                then
                    yield mutableBuffer.[0 .. bufferIndex - 1]
                          |> List.ofArray
            }

    open RandomExtensions

    type Microsoft.FSharp.Collections.List<'X> with
        static member CrossProductWithCommonSuffix commonSuffix
                                                   sequences =
            crossProductWithCommonSuffix commonSuffix
                                         sequences

        static member CrossProduct sequences =
            crossProductWithCommonSuffix [] sequences

        static member DecorrelatedCrossProduct sequences
                                               (randomBehaviour: System.Random) =
            let maximumNumberOfSplits =
                5
            let rec crossProductSubsequencesFrom reverseOfCommonPrefix
                                                 existingInversePermutationForEachCrossProductTerm
                                                 arrays
                                                 numberOfSplits =
                let terminateRecursion () =
                    let existingInversePermutationForEachCrossProductTerm =
                        Array.ofList existingInversePermutationForEachCrossProductTerm
                    [
                        seq
                            {
                                for permutedCrossProductTerm in enumerateTreeOfCrossProductPrefixes reverseOfCommonPrefix
                                                                                                    arrays
                                                                                                    [] do
                                    yield List.permute (fun index ->
                                                            existingInversePermutationForEachCrossProductTerm.[index])
                                                       permutedCrossProductTerm
                            }
                    ]
                match arrays with
                    headArray :: ((_ :: _) as nonEmptyTailArrays)  when numberOfSplits < maximumNumberOfSplits ->
                        let numberOfItemsInHeadArray =
                            Array.length headArray
                        match numberOfItemsInHeadArray with
                            0 ->
                                []
                          | 1 ->
                                crossProductSubsequencesFrom (headArray.[0] :: reverseOfCommonPrefix)
                                                             existingInversePermutationForEachCrossProductTerm
                                                             nonEmptyTailArrays
                                                             numberOfSplits
                          | _ ->
                                let numberOfSplits =
                                    1 + numberOfSplits
                                let startOfSecondHalfOfSplit =
                                    numberOfItemsInHeadArray / 2
                                let firstHalfOfSplit =
                                    headArray.[0 .. startOfSecondHalfOfSplit - 1]
                                let arraysResultingFromFirstHalfOfSplit =
                                    firstHalfOfSplit :: nonEmptyTailArrays
                                let crossProductSubsequencesFromFirstHalfOfSplit =
                                    crossProductSubsequencesFrom reverseOfCommonPrefix
                                                                 existingInversePermutationForEachCrossProductTerm
                                                                 arraysResultingFromFirstHalfOfSplit
                                                                 numberOfSplits
                                let secondHalfOfSplit =
                                    headArray.[startOfSecondHalfOfSplit .. numberOfItemsInHeadArray - 1]
                                let arraysResultingFromSecondHalfOfSplit =
                                    secondHalfOfSplit :: nonEmptyTailArrays
                                let forwardPermutedArraysResultingFromSecondHalfOfSplit
                                    , foo =
                                    List.zip arraysResultingFromSecondHalfOfSplit
                                            (existingInversePermutationForEachCrossProductTerm
                                             |> Seq.skip reverseOfCommonPrefix.Length
                                             |> List.ofSeq)
//                                    |> randomBehaviour.Shuffle
//                                    |> List.ofArray
                                    |> List.unzip
                                let inversePermutationForEachCrossProductTermToApplyToSecondHalfOfSplit =
                                    [
                                        yield! existingInversePermutationForEachCrossProductTerm
                                               |> Seq.take reverseOfCommonPrefix.Length
                                        yield! foo
                                    ]
                                let crossProductSubsequencesFromSecondHalfOfSplit =
                                    crossProductSubsequencesFrom reverseOfCommonPrefix
                                                                 inversePermutationForEachCrossProductTermToApplyToSecondHalfOfSplit
                                                                 arraysResultingFromSecondHalfOfSplit
                                                                 numberOfSplits
                                [
                                    yield! crossProductSubsequencesFromFirstHalfOfSplit
                                    yield! crossProductSubsequencesFromSecondHalfOfSplit
                                ]
                  | _ ->
                        terminateRecursion ()
            let arrays =
                sequences
                |> List.map Array.ofSeq
            let crossProductSubsequences =
                crossProductSubsequencesFrom []
                                             (List.init arrays.Length
                                                        (fun index ->
                                                            index))
                                             arrays
                                             0
            randomBehaviour.PickAlternatelyFrom crossProductSubsequences

        static member MergeSortedListsAllowingDuplicates first second =
            mergeSortedListsAllowingDuplicates first second

        static member CountDuplicatesInSortedList list =
            countDuplicatesInSortedList list

        member this.BreakOff sizeOfFirstPart =
            breakOff sizeOfFirstPart this

        member this.ChopUpList spans =
            chopUpList this spans
