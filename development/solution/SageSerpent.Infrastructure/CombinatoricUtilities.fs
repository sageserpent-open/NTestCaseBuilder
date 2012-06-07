module SageSerpent.Infrastructure.CombinatoricUtilities

    open Microsoft.FSharp.Core.Operators.Checked

    open System

    /// <summary>Given a list of non-negative integer limits, create lists of contributions that sum up to a total
    /// such that each contribution cannot exceed its corresponding limit. The contribution lists are returned
    /// within a list: if the total is too high to be met given the limits, the result is an empty list.</summary>

    let rec ChooseContributionsToMeetTotal contributionLimits total =
        match contributionLimits with
            [] -> []
            | head :: [] -> if head >= total
                            then [[total]]
                            else []
            | head :: tail -> [for contributionFromHead in 0u .. (min head total) do
                                let resultFromTail = ChooseContributionsToMeetTotal tail (total - contributionFromHead)
                                if not resultFromTail.IsEmpty
                                then yield! List.map (function item -> contributionFromHead::item) resultFromTail]

    /// <summary>Given a list of non-negative integer limits, create lists of contributions that sum up to a total
    /// such that each contribution cannot exceed its corresponding limit: this calculation is repeated for
    /// a range of totals from zero up to and including the limit. Each result for a given total is placed
    /// into a map that associates totals with non-empty result lists.</summary>

    let rec ChooseContributionsToMeetTotalsUpToLimit contributionLimits limit =
        match contributionLimits with
            [] -> Map.empty
            | head :: [] -> Map.ofList [for contributionFromHead in 0u .. (min head limit) do
                                            yield (contributionFromHead, [[contributionFromHead]])]
            | head :: tail -> let resultFromTail = ChooseContributionsToMeetTotalsUpToLimit tail limit
                              Map.ofList [for total in 0u .. limit do
                                            let resultForTotal =
                                                [for contributionFromHead in 0u .. (min head total) do
                                                    let totalRequiredFromTail = total - contributionFromHead
                                                    if resultFromTail.ContainsKey totalRequiredFromTail
                                                    then yield! List.map (function item -> contributionFromHead::item) resultFromTail.[totalRequiredFromTail]]
                                            if not resultForTotal.IsEmpty
                                            then yield (total, resultForTotal)]


    /// <summary>Creates a sequence of combinations of items, each combination having the
    /// given size. The order of the items chosen by a combination is a sub-sequence of the
    /// original item sequence. It is permissible to either request too many items, in which
    /// case an empty sequence results, or to request zero items, in which case a single empty
    /// combination is generated.</summary>
    let rec GenerateCombinationsOfGivenSizePreservingOrder size items =
        if 0u = size
        then Seq.singleton []
        else match items with
                head :: tail ->
                    let combinationsIncludingHead =
                        GenerateCombinationsOfGivenSizePreservingOrder (size - 1u) tail
                        |> Seq.map (fun combinationFromTail ->
                                            head :: combinationFromTail)
                    let combinationsExcludingHead =
                        GenerateCombinationsOfGivenSizePreservingOrder size tail
                    Seq.append combinationsIncludingHead combinationsExcludingHead
               | _ ->
                    Seq.empty

    /// <summary>This is an optimised form of 'GenerateCombinationsOfGivenSizePreservingOrder'
    /// that creates just the specific sorted combination picked out from all the possible
    /// sorted combinations by 'indexOfCombination'. So:-
    ///     GenerateCombinationOfGivenSizePreservingOrder size items indexOfCombination =
    ///         GenerateCombinationsOfGivenSizePreservingOrder size items
    ///         |> Seq.nth indexOfCombination
    /// However, the actual implementation does not create any combinations other than
    /// the one requested.</summary>
    let GenerateCombinationOfGivenSizePreservingOrder size items indexOfCombination =
        let numberOfItems =
            Array.length items
            |> uint32
        if BargainBasement.NumberOfCombinations numberOfItems size <= indexOfCombination
        then raise (PreconditionViolationException "'indexOfCombination' is too large: there are not enough combinations to support this choice.")
        let rec generateCombinationOfIndicesOfGivenSizePreservingOrder size
                                                                       startingIndex
                                                                       indexOfCombination =
            if 0u = size
            then []
            else let sizeLessOne =
                     size - 1u
                 let numberOfItemsLessOne =
                     numberOfItems - 1u
                 let startingIndexPlusOne =
                     startingIndex + 1u
                 let numberOfItemsToChooseFromAfterEitherPickingOrDiscardingTheOneAtTheStartingIndex =
                     numberOfItems - startingIndexPlusOne
                 let numberOfCombinationsIncludingHead =
                     BargainBasement.NumberOfCombinations numberOfItemsToChooseFromAfterEitherPickingOrDiscardingTheOneAtTheStartingIndex sizeLessOne
                 if numberOfCombinationsIncludingHead > indexOfCombination
                 then startingIndex :: generateCombinationOfIndicesOfGivenSizePreservingOrder sizeLessOne
                                                                                              startingIndexPlusOne
                                                                                              indexOfCombination
                 else generateCombinationOfIndicesOfGivenSizePreservingOrder size
                                                                             startingIndexPlusOne
                                                                             (indexOfCombination - numberOfCombinationsIncludingHead)
        let combinationOfIndicesToPickOutAt =
            generateCombinationOfIndicesOfGivenSizePreservingOrder size
                                                                   0u
                                                                   indexOfCombination
        [for indexToPickItemAt in combinationOfIndicesToPickOutAt do
         yield items.[int32 indexToPickItemAt]]





