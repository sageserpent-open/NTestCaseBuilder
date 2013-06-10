[<System.Runtime.CompilerServices.Extension>]
module SageSerpent.Infrastructure.RandomExtensions
    open System

    type Random with
        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("ChooseAnyNumberFromZeroToOneLessThan")>]
        member this.ChooseAnyNumberFromZeroToOneLessThan exclusiveLimit =
            exclusiveLimit
            |> int32
            |> this.Next
            |> uint32

        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("ChooseAnyNumberFromOneTo")>]
        member this.ChooseAnyNumberFromOneTo inclusiveLimit =
            this.ChooseAnyNumberFromZeroToOneLessThan inclusiveLimit + 1u

        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("HeadsItIs")>]
        member this.HeadsItIs () =
            this.ChooseAnyNumberFromZeroToOneLessThan 2u = 0u

        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("ChooseSeveralOf")>]
        member this.ChooseSeveralOf ((candidates: #seq<_>), numberToChoose) =
            let numberOfCandidates =
                Seq.length candidates
            if numberToChoose > uint32 numberOfCandidates
            then
                raise (PreconditionViolationException "Insufficient number of candidates to satisfy number to choose.")
            else
                let candidateArray =
                    Array.ofSeq candidates
                for numberOfCandidatesAlreadyChosen in 0 .. (int32 numberToChoose) - 1 do
                    let chosenCandidateIndex =
                        numberOfCandidatesAlreadyChosen + int32 (this.ChooseAnyNumberFromZeroToOneLessThan (uint32 (numberOfCandidates - numberOfCandidatesAlreadyChosen)))
                    if numberOfCandidatesAlreadyChosen < chosenCandidateIndex
                    then
                        let chosenCandidate =
                            candidateArray.[chosenCandidateIndex]
                        candidateArray.[chosenCandidateIndex] <- candidateArray.[numberOfCandidatesAlreadyChosen]
                        candidateArray.[numberOfCandidatesAlreadyChosen] <- chosenCandidate
                candidateArray.[0 .. (int32 numberToChoose) - 1]

        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("ChooseOneOf")>]
        member this.ChooseOneOf (candidates: #seq<_>) =
            (this.ChooseSeveralOf (candidates, 1u)).[0]

        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("Shuffle")>]
        member this.Shuffle (items: #seq<_>) =
            let result =
                C5.ArrayList ()
            result.AddAll items
            result.Shuffle this
            result.ToArray ()

        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("PickAlternatelyFrom")>]
        member this.PickAlternatelyFrom (sequences: List<seq<_>>) =
            let onlyNonEmptyFrom =
                List.filter (LazyList.isEmpty >> not)
            let pickAnItemFromNonEmptyLazyLists nonEmptyLazyLists =
                let numberOfLazyLists =
                    Array.length nonEmptyLazyLists
                match numberOfLazyLists with
                    0 ->
                        None
                  | _ ->
                        let sliceStartInclusiveIndex =
                            this.ChooseAnyNumberFromZeroToOneLessThan (uint32 numberOfLazyLists)
                            |> int32
                        let sliceEndExclusiveIndex =
                            this.ChooseAnyNumberFromZeroToOneLessThan (uint32 numberOfLazyLists)
                            |> int32
                        let sliceLength =
                            numberOfLazyLists
                            - ((numberOfLazyLists + sliceStartInclusiveIndex -  sliceEndExclusiveIndex) % numberOfLazyLists)
                        let translateIndex sourceIndex =
                            (sourceIndex + sliceStartInclusiveIndex) % numberOfLazyLists
                        let intraSlicePermutationDestinationIndices =
                            seq
                                {
                                    for intraSliceIndex in 0 .. sliceLength - 1 do
                                        yield translateIndex intraSliceIndex
                                }
                            |> this.Shuffle
                        let pickedItems
                            , lazyListsPickedFrom =
                            [
                                for sourceIndex in 0 .. sliceLength - 1 do
                                    yield match nonEmptyLazyLists.[intraSlicePermutationDestinationIndices.[sourceIndex]] with
                                            LazyList.Cons (pickedItem
                                                            , tailFromPickedLazyList) ->
                                                pickedItem
                                                , tailFromPickedLazyList
                                          | _ ->
                                                raise (InternalAssertionViolationException "At this point the lazy list should be guaranteed to be non-empty.")
                            ]
                            |> List.unzip
                        let unchangedLazyLists =
                            [
                                for sourceIndex in sliceLength .. numberOfLazyLists - 1 do
                                    yield nonEmptyLazyLists.[translateIndex sourceIndex]
                            ]
                        Some (pickedItems
                              , [|
                                    yield! lazyListsPickedFrom
                                           |> onlyNonEmptyFrom
                                    yield! unchangedLazyLists
                                |])
            seq
                {
                    for pickedItemsChunk in Seq.unfold pickAnItemFromNonEmptyLazyLists
                                                       (sequences
                                                        |> List.map (LazyList.ofSeq)
                                                        |> onlyNonEmptyFrom
                                                        |> Array.ofList) do
                        yield! pickedItemsChunk
                }
