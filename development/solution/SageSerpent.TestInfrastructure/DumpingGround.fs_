#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    type Node =
            TestVariableNode of seq<Object>
          | InterleavingNode of seq<Node>
          | SynthesizingNode of seq<Node>
            member this.CombinationsOfTestLevelsOfStrengthUpToAndIncluding strength =
                let rec walkTree node strength indexForLeftmostTestVariable =
                    if strength = 0
                    then raise (PreconditionViolationException "Strength must be non-zero.")
                    else match node with
                            TestVariableNode levels ->
                                [levels
                                 |> Seq.map (fun level -> Map.of_list [(1u, level)])]
                                , indexForLeftmostTestVariable
                                
                          | InterleavingNode subtreeRootNodes ->
                                let rec joinPairsAtEachStrength first second =
                                    match first, second with
                                        _, [] -> first
                                      | [], _ -> second
                                      | headFromFirst::tailFromFirst, headFromSecond::tailFromSecond ->
                                            (Seq.append headFromFirst headFromSecond)::(joinPairsAtEachStrength tailFromFirst tailFromSecond)
                                let mergeCombinationsFromSubtree (partialResult, indexForLeftmostTestVariable) subtreeRootNode =
                                    let combinationsFromSubtree, maximumTestVariableIndexFromSubtree =
                                        (walkTree subtreeRootNode strength indexForLeftmostTestVariable)
                                    let mergedResult = joinPairsAtEachStrength partialResult combinationsFromSubtree
                                    let maximumTestVariableIndex = indexForLeftmostTestVariable + maximumTestVariableIndexFromSubtree
                                    mergedResult, maximumTestVariableIndex                                
                                subtreeRootNodes
                                |> Seq.fold mergeCombinationsFromSubtree ([], indexForLeftmostTestVariable)
                            
                          | SynthesizingNode subtreeRootNodes ->
                                let gatherCombinationsFromSubtree (partialResult, indexForLeftmostTestVariable) subtreeRootNode =
                                    let combinationsFromSubtree, maximumTestVariableIndexFromSubtree =
                                        (walkTree subtreeRootNode strength indexForLeftmostTestVariable)
                                    let appendedResult = combinationsFromSubtree::partialResult
                                    let maximumTestVariableIndex = indexForLeftmostTestVariable + maximumTestVariableIndexFromSubtree
                                    appendedResult, maximumTestVariableIndex
                                let resultsFromSubtrees, maximumTestVariableIndex =
                                    subtreeRootNodes
                                    |> Seq.fold gatherCombinationsFromSubtree ([], indexForLeftmostTestVariable)
                                let maximumStrengthsFromSubtrees =
                                    resultsFromSubtrees
                                    |> List.map (fun combinationsAtEachStrength -> uint32 (List.length combinationsAtEachStrength))
                                let overallStrength = List.max maximumStrengthsFromSubtrees
                                let resultsFromSubtrees = List.map List.to_array resultsFromSubtrees
                                let distributionsOfStrengthsOverSubtreesAtEachTotalStrength =
                                    CombinatoricUtilities.ChooseContributionsToMeetTotalsUpToLimit maximumStrengthsFromSubtrees overallStrength
                                let addInCombinationsForGivenStrength partialResult strength distributions =
                                    let addInCombinationsForAGivenDistribution partialResult distribution =
                                        let combinationsBySubtree =
                                            (List.zip distribution resultsFromSubtrees)
                                            |> List.map (function strength, resultFromSubtree -> resultFromSubtree.[int32 strength])
                                        let joinMaps first second =
                                            Seq.append (Map.to_seq first) (Map.to_seq second)
                                            |> Map.of_seq
                                        let combinationsBuiltFromCrossProduct =
                                            (BargainBasement.CrossProduct combinationsBySubtree)
                                            |> List.map (List.reduce_left joinMaps)
                                        Seq.append combinationsBuiltFromCrossProduct partialResult
                                    (distributions |> Seq.fold addInCombinationsForAGivenDistribution Seq.empty)::partialResult
                                let combinations =
                                    distributionsOfStrengthsOverSubtreesAtEachTotalStrength |> Map.fold_left addInCombinationsForGivenStrength []
                                combinations, maximumTestVariableIndex
                fst (walkTree this strength, 0u)
            
                                