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
                >> Array.ofList
            let pickAnItemFromNonEmptySequences nonEmptyLazyLists =
                let numberOfLazyLists =
                    Array.length nonEmptyLazyLists
                match numberOfLazyLists with
                    0 ->
                        None
                  | _ ->
                        let chosenLazyListIndex =
                            this.ChooseAnyNumberFromZeroToOneLessThan (uint32 numberOfLazyLists)
                            |> int32
                        match nonEmptyLazyLists.[chosenLazyListIndex] with
                            LazyList.Cons (pickedItem
                                           , tailFromChosenLazyList) ->
                            let remainingLazyLists =
                                [
                                    for index in 0 .. numberOfLazyLists - 1 do
                                        if chosenLazyListIndex = index
                                        then
                                            yield tailFromChosenLazyList
                                        else
                                            yield nonEmptyLazyLists.[index]
                                ]
                                |> onlyNonEmptyFrom
                            Some (pickedItem
                                  , remainingLazyLists)
                          | _ ->
                                raise (InternalAssertionViolationException "At this point the lazy list should be gauranteed to be non-empty.")
            Seq.unfold pickAnItemFromNonEmptySequences
                       (sequences
                        |> List.map (LazyList.ofSeq)
                        |> onlyNonEmptyFrom)
