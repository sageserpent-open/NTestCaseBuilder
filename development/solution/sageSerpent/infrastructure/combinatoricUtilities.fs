#light

module SageSerpent.Infrastructure.CombinatoricUtilities

    open Microsoft.FSharp.Collections

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
                            
                                
                                
    let rec chooseContributionsToMeetTotalsUpToLimit contributionLimits limit =
        match contributionLimits with
            [] -> Map.Empty ()
            | head::[] -> Map.FromList [for contributionFromHead in [0u..(min head limit)] do
                                            yield (contributionFromHead, [[contributionFromHead]])]
            | head::tail -> let resultFromTail = chooseContributionsToMeetTotalsUpToLimit tail limit
                            Map.FromList [for total in [0u..limit] do
                                            let resultForTotal = [for contributionFromHead in [0u..(min head total)] do
                                                                    let totalRequiredFromTail = total - contributionFromHead
                                                                    if resultFromTail.ContainsKey totalRequiredFromTail
                                                                    then yield! List.map (function item -> contributionFromHead::item) resultFromTail.[totalRequiredFromTail]]
                                            if not resultForTotal.IsEmpty
                                            then yield (total, resultForTotal)]
                            
                                

    
                                    