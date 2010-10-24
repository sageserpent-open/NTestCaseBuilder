#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System.Collections.Generic
    open System
    open SageSerpent.Infrastructure

    type Node =
            TestVariableNode of seq<Object>
          | InterleavingNode of seq<Node>
          | SynthesizingNode of seq<Node>
            member this.CombinationsOfTestLevelsGroupedByStrengthUpToAndIncluding strength =
                let rec walkTree node strength indexForLeftmostTestVariable =
                    if strength = 0u
                    then raise (PreconditionViolationException "Strength must be non-zero.")
                    else match node with
                            TestVariableNode levels ->
                                [levels
                                 |> Seq.map (fun level -> Map.of_list [(indexForLeftmostTestVariable, level)])]
                                , indexForLeftmostTestVariable + 1u
                                
                          | InterleavingNode subtreeRootNodes ->
                                let rec joinPairsAtEachStrength first second =
                                    match first, second with
                                        _, [] -> first
                                      | [], _ -> second
                                      | headFromFirst::tailFromFirst, headFromSecond::tailFromSecond ->
                                            (Seq.append headFromFirst headFromSecond)::(joinPairsAtEachStrength tailFromFirst tailFromSecond)
                                let mergeCombinationsFromSubtree (previouslyMergedCombinations, indexForLeftmostTestVariable) subtreeRootNode =
                                    let combinationsGroupedByStrengthFromSubtree, maximumTestVariableIndexFromSubtree =
                                        (walkTree subtreeRootNode strength indexForLeftmostTestVariable)
                                    let mergedCombinationsGroupedByStrength =
                                        joinPairsAtEachStrength previouslyMergedCombinations combinationsGroupedByStrengthFromSubtree
                                    mergedCombinationsGroupedByStrength, maximumTestVariableIndexFromSubtree                                
                                subtreeRootNodes
                                |> Seq.fold mergeCombinationsFromSubtree ([], indexForLeftmostTestVariable)
                            
                          | SynthesizingNode subtreeRootNodes ->
                                let gatherCombinationsFromSubtree (prevouslyGatheredCombinations, indexForLeftmostTestVariable) subtreeRootNode =
                                    let combinationsGroupedByStrengthFromSubtree, maximumTestVariableIndexFromSubtree =
                                        (walkTree subtreeRootNode strength indexForLeftmostTestVariable)
                                    let gatheredCombinationsGroupedBySubtreeAndThenByStrength =
                                        combinationsGroupedByStrengthFromSubtree::prevouslyGatheredCombinations
                                    gatheredCombinationsGroupedBySubtreeAndThenByStrength, maximumTestVariableIndexFromSubtree
                                // Using 'fold' causes 'resultsFromSubtrees' to be built up in reverse to the subtree sequence,
                                // and this reversal propagates consistently through the code below. The only place it could
                                // cause a problem is where the maps are joined, but because the map joining is commutative *and*
                                // because the test variable indices are already correctly calculated, it doesn't matter.
                                let combinationsGroupedBySubtreeAndThenByStrength, maximumTestVariableIndex =
                                    subtreeRootNodes
                                    |> Seq.fold gatherCombinationsFromSubtree ([], indexForLeftmostTestVariable)
                                let maximumStrengthsFromSubtrees =
                                    combinationsGroupedBySubtreeAndThenByStrength
                                    |> List.map (fun combinationsForOneSubtreeGroupedByStrength
                                                    -> uint32 (List.length combinationsForOneSubtreeGroupedByStrength))
                                let combinationsGroupedBySubtreeAndThenByStrength =
                                    combinationsGroupedBySubtreeAndThenByStrength
                                    |> List.map List.to_array
                                // Remove the key for zero; we are not interested in zero strength entries, as they yield no combinations!
                                let distributionsOfStrengthsOverSubtreesAtEachTotalStrength =
                                    Map.remove 0u (CombinatoricUtilities.ChooseContributionsToMeetTotalsUpToLimit maximumStrengthsFromSubtrees strength)
                                let addInCombinationsForGivenStrength strength distributions partialResult =
                                    let addInCombinationsForAGivenDistribution partialResult distribution =
                                        let combinationsBySubtree =
                                            (List.zip distribution combinationsGroupedBySubtreeAndThenByStrength)
                                            |> List.map (function strength, resultFromSubtree ->
                                                                    if strength > 0u
                                                                    then resultFromSubtree.[int32 (strength - 1u)]
                                                                    else Seq.singleton Map.empty)
                                        let joinMaps first second =
                                            let keys map =
                                                Set.of_seq (seq { for key in (map:> IDictionary<'Key, 'Value>).Keys do yield key })
                                            if not (Set.intersect (keys first) (keys second)).IsEmpty
                                            then raise (InternalAssertionViolationException
                                                            "Maps from test variable indices to levels contributed by separate subtrees should not share common keys.")
                                            else Seq.append (Map.to_seq first) (Map.to_seq second)
                                                 |> Map.of_seq
                                        let combinationsBuiltFromCrossProduct =
                                            (BargainBasement.CrossProduct combinationsBySubtree)
                                            |> List.map (List.reduce_left joinMaps)
                                        Seq.append combinationsBuiltFromCrossProduct partialResult
                                    (distributions |> Seq.fold addInCombinationsForAGivenDistribution Seq.empty)::partialResult
                                let combinationsGroupedByStrength =
                                    Map.fold_right addInCombinationsForGivenStrength distributionsOfStrengthsOverSubtreesAtEachTotalStrength []
                                combinationsGroupedByStrength, maximumTestVariableIndex
                fst (walkTree this strength 0u)
            
                                