#light

module SageSerpent.Infrastructure.CombinatoricUtilities

    open Microsoft.FSharp.Collections
    
    open Microsoft.FSharp.Core.Operators.Checked
    
    /// <summary>Given a list of non-negative integer limits, create lists of contributions that sum up to a total
    /// such that each contribution cannot exceed its corresponding limit. The contribution lists are returned
    /// within a list: if the total is too high to be met given the limits, the result is an empty list.</summary>

    let rec chooseContributionsToMeetTotal contributionLimits total =
        match contributionLimits with
            [] -> []
            | head::[] -> if head >= total
                          then [[total]]
                          else []
            | head::tail -> [for contributionFromHead in [0u..(min head total)] do
                                let resultFromTail = chooseContributionsToMeetTotal tail (total - contributionFromHead)
                                if not resultFromTail.IsEmpty
                                then yield! List.map (function item -> contributionFromHead::item) resultFromTail]
                            
    /// <summary>Given a list of non-negative integer limits, create lists of contributions that sum up to a total
    /// such that each contribution cannot exceed its corresponding limit: this calculation is repeated for
    /// a range of totals from zero up to and including the limit. Each result for a given total is placed
    /// into a map that associates totals with non-empty result lists.</summary>
                                
    let rec chooseContributionsToMeetTotalsUpToLimit contributionLimits limit =
        match contributionLimits with
            [] -> Map.empty
            | head::[] -> Map.of_list [for contributionFromHead in [0u..(min head limit)] do
                                            yield (contributionFromHead, [[contributionFromHead]])]
            | head::tail -> let resultFromTail = chooseContributionsToMeetTotalsUpToLimit tail limit
                            Map.of_list [for total in [0u..limit] do
                                            let resultForTotal = [for contributionFromHead in [0u..(min head total)] do
                                                                    let totalRequiredFromTail = total - contributionFromHead
                                                                    if resultFromTail.ContainsKey totalRequiredFromTail
                                                                    then yield! List.map (function item -> contributionFromHead::item) resultFromTail.[totalRequiredFromTail]]
                                            if not resultForTotal.IsEmpty
                                            then yield (total, resultForTotal)]
                            
                                

    
                                    