module SageSerpent.Infrastructure.ListExtensions
    open RandomExtensions

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

    let private decorrelatedCrossProductWithCommonSuffix commonSuffix
                                                         sequences
                                                         (randomBehaviour: System.Random) =
        let arrays =
            sequences
            |> List.map Array.ofSeq
        let numberOfArrays =
            arrays.Length
        let maximumNumberOfSplits =
            5
        let rec crossProductSubsequencesFrom reverseOfCommonPrefix
                                             (existingInversePermutationForEachCrossProductTerm: array<_>)
                                             arrays
                                             numberOfSplits =
            let terminateRecursion () =
                [
                    seq
                        {
                            for permutedCrossProductTerm in enumerateTreeOfCrossProductPrefixes reverseOfCommonPrefix
                                                                                                arrays
                                                                                                commonSuffix do
                                yield List.permute (fun index ->
                                                        if numberOfArrays <= index
                                                        then
                                                            index
                                                        else
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
                            let lengthOfCommonPrefix =
                                reverseOfCommonPrefix
                                |> List.length
                            let forwardPermutedArraysResultingFromSecondHalfOfSplit
                                , inversePermutationForEachCrossProductTermExcludingCommonPrefix =
                                let maximumIndexOfInversePermutation =
                                    (existingInversePermutationForEachCrossProductTerm
                                    |> Array.length)
                                    - 1
                                Seq.zip arraysResultingFromSecondHalfOfSplit
                                        existingInversePermutationForEachCrossProductTerm.[lengthOfCommonPrefix .. maximumIndexOfInversePermutation]
                                |> randomBehaviour.Shuffle
                                |> List.ofArray
                                |> List.unzip
                            let inversePermutationForEachCrossProductTermInCommonPrefix =
                                    existingInversePermutationForEachCrossProductTerm.[0 .. lengthOfCommonPrefix - 1]
                            let inversePermutationForEachCrossProductTermToApplyToSecondHalfOfSplit =
                                [|
                                    yield! inversePermutationForEachCrossProductTermInCommonPrefix
                                    yield! inversePermutationForEachCrossProductTermExcludingCommonPrefix
                                |]
                            let crossProductSubsequencesFromSecondHalfOfSplit =
                                crossProductSubsequencesFrom reverseOfCommonPrefix
                                                             inversePermutationForEachCrossProductTermToApplyToSecondHalfOfSplit
                                                             forwardPermutedArraysResultingFromSecondHalfOfSplit
                                                             numberOfSplits
                            [
                                yield! crossProductSubsequencesFromFirstHalfOfSplit
                                yield! crossProductSubsequencesFromSecondHalfOfSplit
                            ]
                | _ ->
                    terminateRecursion ()
        let crossProductSubsequences =
            crossProductSubsequencesFrom []
                                         (Array.init numberOfArrays
                                                     (fun index ->
                                                        index))
                                         arrays
                                         0
        randomBehaviour.PickAlternatelyFrom crossProductSubsequences

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

    let RoundRobinPickFrom (sequences: List<seq<_>>) =
        let onlyNonEmptyFrom =
            List.filter (LazyList.isEmpty >> not)
            >> Array.ofList
        let pickAnItemFromNonEmptyLazyLists nonEmptyLazyLists =
            let numberOfLazyLists =
                Array.length nonEmptyLazyLists
            match numberOfLazyLists with
                0 ->
                    None
              | _ ->
                    let pickedItems
                        , lazyListsPickedFrom =
                        List.init numberOfLazyLists
                                    (fun sourceIndex ->
                                        match nonEmptyLazyLists.[sourceIndex] with
                                            LazyList.Cons (pickedItem
                                                            , tailFromPickedLazyList) ->
                                                pickedItem
                                                , tailFromPickedLazyList
                                          | _ ->
                                                raise (InternalAssertionViolationException "At this point the lazy list should be guaranteed to be non-empty."))
                        |> List.unzip
                    Some (pickedItems
                          , lazyListsPickedFrom
                            |> onlyNonEmptyFrom)

        seq
            {
                for pickedItemsChunk in Seq.unfold pickAnItemFromNonEmptyLazyLists
                                                   (sequences
                                                    |> List.map (LazyList.ofSeq)
                                                    |> onlyNonEmptyFrom) do
                    yield! pickedItemsChunk
            }

    type Microsoft.FSharp.Collections.List<'X> with
        static member CrossProductWithCommonSuffix commonSuffix
                                                   sequences =
            crossProductWithCommonSuffix commonSuffix
                                         sequences

        static member CrossProduct sequences =
            crossProductWithCommonSuffix []
                                         sequences

        static member DecorrelatedCrossProductWithCommonSuffix randomBehaviour
                                                               commonSuffix
                                                               sequences =
            decorrelatedCrossProductWithCommonSuffix commonSuffix
                                                     sequences
                                                     randomBehaviour

        static member DecorrelatedCrossProduct randomBehaviour
                                               sequences =
            decorrelatedCrossProductWithCommonSuffix []
                                                     sequences
                                                     randomBehaviour

        static member MergeSortedListsAllowingDuplicates first second =
            mergeSortedListsAllowingDuplicates first second

        static member CountDuplicatesInSortedList list =
            countDuplicatesInSortedList list

        member this.BreakOff sizeOfFirstPart =
            breakOff sizeOfFirstPart this

        member this.ChopUpList spans =
            chopUpList this spans
