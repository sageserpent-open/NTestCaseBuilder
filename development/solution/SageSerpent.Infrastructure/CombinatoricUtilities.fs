module SageSerpent.Infrastructure.CombinatoricUtilities

    open Microsoft.FSharp.Core.Operators.Checked
    
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
                head :: tail when size > 0u ->
                    let combinationsIncludingHead =
                        GenerateCombinationsOfGivenSizePreservingOrder (size - 1u) tail
                        |> Seq.map (fun combinationFromTail ->
                                            head :: combinationFromTail)
                    let combinationsExcludingHead =
                        GenerateCombinationsOfGivenSizePreservingOrder size tail
                    Seq.append combinationsIncludingHead combinationsExcludingHead
               | _ ->
                    Seq.empty
                                    