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
            member this.TestVectorRepresentationsGroupedByStrengthUpToAndIncluding strength =
                let rec walkTree node strength indexForLeftmostTestVariable =
                    if strength = 0u
                    then raise (PreconditionViolationException "Strength must be non-zero.")
                    else match node with
                            TestVariableNode levels ->
                                [levels
                                 |> Seq.map (fun level -> [(indexForLeftmostTestVariable, level)])]
                                , indexForLeftmostTestVariable + 1u
                                
                          | InterleavingNode subtreeRootNodes ->
                                let rec joinPairsAtEachStrength first second =
                                    match first, second with
                                        _, [] -> first
                                      | [], _ -> second
                                      | headFromFirst::tailFromFirst, headFromSecond::tailFromSecond ->
                                            (Seq.append headFromFirst headFromSecond)::(joinPairsAtEachStrength tailFromFirst tailFromSecond)
                                let mergeTestVectorRepresentationsFromSubtree (previouslyMergedTestVectorRepresentations, indexForLeftmostTestVariable) subtreeRootNode =
                                    let testVectorRepresentationsGroupedByStrengthFromSubtree, maximumTestVariableIndexFromSubtree =
                                        (walkTree subtreeRootNode strength indexForLeftmostTestVariable)
                                    let mergedTestVectorRepresentationsGroupedByStrength =
                                        joinPairsAtEachStrength previouslyMergedTestVectorRepresentations testVectorRepresentationsGroupedByStrengthFromSubtree
                                    mergedTestVectorRepresentationsGroupedByStrength, maximumTestVariableIndexFromSubtree                                
                                subtreeRootNodes
                                |> Seq.fold mergeTestVectorRepresentationsFromSubtree ([], indexForLeftmostTestVariable)
                            
                          | SynthesizingNode subtreeRootNodes ->
                                let gatherTestVectorRepresentationsFromSubtree (prevouslyGatheredTestVectorRepresentations, indexForLeftmostTestVariable) subtreeRootNode =
                                    let testVectorRepresentationsGroupedByStrengthFromSubtree, maximumTestVariableIndexFromSubtree =
                                        (walkTree subtreeRootNode strength indexForLeftmostTestVariable)
                                    let gatheredTestVectorRepresentationsGroupedBySubtreeAndThenByStrength =
                                        testVectorRepresentationsGroupedByStrengthFromSubtree::prevouslyGatheredTestVectorRepresentations
                                    gatheredTestVectorRepresentationsGroupedBySubtreeAndThenByStrength, maximumTestVariableIndexFromSubtree
                                // Using 'fold' causes 'resultsFromSubtrees' to be built up in reverse to the subtree sequence,
                                // and this reversal propagates consistently through the code below. The only place it could
                                // cause a problem is where the maps are joined, but because the map joining is commutative *and*
                                // because the test variable indices are already correctly calculated, it doesn't matter.
                                let testVectorRepresentationsGroupedBySubtreeAndThenByStrength, maximumTestVariableIndex =
                                    subtreeRootNodes
                                    |> Seq.fold gatherTestVectorRepresentationsFromSubtree ([], indexForLeftmostTestVariable)
                                let maximumStrengthsFromSubtrees =
                                    testVectorRepresentationsGroupedBySubtreeAndThenByStrength
                                    |> List.map (fun testVectorRepresentationsForOneSubtreeGroupedByStrength
                                                    -> uint32 (List.length testVectorRepresentationsForOneSubtreeGroupedByStrength))
                                let testVectorRepresentationsGroupedBySubtreeAndThenByStrength =
                                    testVectorRepresentationsGroupedBySubtreeAndThenByStrength
                                    |> List.map List.to_array
                                // Remove the key for zero; we are not interested in zero strength entries, as they yield no testVectorRepresentations!
                                let distributionsOfStrengthsOverSubtreesAtEachTotalStrength =
                                    Map.remove 0u (CombinatoricUtilities.ChooseContributionsToMeetTotalsUpToLimit maximumStrengthsFromSubtrees strength)
                                let addInTestVectorRepresentationsForGivenStrength strength distributions partialResult =
                                    let addInTestVectorRepresentationsForAGivenDistribution partialResult distribution =
                                        let testVectorRepresentationsBySubtree =
                                            (List.zip distribution testVectorRepresentationsGroupedBySubtreeAndThenByStrength)
                                            |> List.map (function strength, resultFromSubtree ->
                                                                    if strength > 0u
                                                                    then resultFromSubtree.[int32 (strength - 1u)]
                                                                    else Seq.singleton [])
                                        let joinTestVectorRepresentations first second =
                                            List.append first second
                                        let testVectorRepresentationsBuiltFromCrossProduct =
                                            (BargainBasement.CrossProduct testVectorRepresentationsBySubtree)
                                            |> List.map (List.reduce_left joinTestVectorRepresentations)
                                        Seq.append testVectorRepresentationsBuiltFromCrossProduct partialResult
                                    (distributions |> Seq.fold addInTestVectorRepresentationsForAGivenDistribution Seq.empty)::partialResult
                                let testVectorRepresentationsGroupedByStrength =
                                    Map.fold_right addInTestVectorRepresentationsForGivenStrength distributionsOfStrengthsOverSubtreesAtEachTotalStrength []
                                testVectorRepresentationsGroupedByStrength, maximumTestVariableIndex
                let testVectorRepresentationsGroupedByStrength, _ = walkTree this strength 0u
                let expressTestVectorRepresentationAsMap testVectorRepresentation =
                    let result = Map.of_list testVectorRepresentation
                    if result.Count = testVectorRepresentation.Length
                    then result
                    else raise (PreconditionViolationException "Found a test vector representation that had colliding test variable keys.")
                testVectorRepresentationsGroupedByStrength
                |> List.map (fun testVectorRepresentations -> testVectorRepresentations
                                                              |> Seq.map expressTestVectorRepresentationAsMap)
            
                                