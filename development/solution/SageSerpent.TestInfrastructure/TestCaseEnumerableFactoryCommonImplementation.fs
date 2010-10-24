namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    type internal TestCaseEnumerableFactoryCommonImplementation (node: Node) =
        member this.Node = node
        
        interface ITestCaseEnumerableFactory with
            override this.CreateEnumerable desiredStrength =
                match node.PruneTree with
                    Some prunedNode ->
                        let partialTestVectors
                            , associationFromTestVariableIndexToNumberOfItsLevels =
                            prunedNode.PartialTestVectorRepresentationsGroupedByStrengthUpToAndIncluding desiredStrength
                        List.iter (fun partialTestVectors ->
                                        Seq.iter (fun partialTestVector -> printf "Partial test vector: %A\n" partialTestVector) partialTestVectors) partialTestVectors
                            
                        let mergedPartialTestVectorRepresentations =
                            // Do a fold back so that high strength combinations get in there first. Hopefully the lesser strength combinations
                            // should have a greater chance of finding an earlier, larger vector to merge with this way. 
                            List.foldBack (fun partialTestVectorsAtTheSameStrength
                                             mergedPartialTestVectorRepresentations ->
                                                partialTestVectorsAtTheSameStrength
                                                |> Seq.fold (fun mergedPartialTestVectorRepresentations
                                                                 partialTestVector ->
                                                                    (mergedPartialTestVectorRepresentations: MergedPartialTestVectorRepresentations<_>).MergeOrAdd partialTestVector)
                                                            mergedPartialTestVectorRepresentations)
                                            partialTestVectors
                                            MergedPartialTestVectorRepresentations.initial
                        let randomBehaviour =
                            RandomBehaviour 0
                        Seq.iter (fun mergedPartialTestVectorRepresentation -> printf "Merged partial test vector representation: %A\n" mergedPartialTestVectorRepresentation) mergedPartialTestVectorRepresentations
                        let sequenceOfFinalValues =
                            seq {for mergedPartialTestVector in mergedPartialTestVectorRepresentations do
                                    yield prunedNode.FillOutPartialTestVectorRepresentation randomBehaviour
                                                                                            associationFromTestVariableIndexToNumberOfItsLevels
                                                                                            mergedPartialTestVector
                                          |> prunedNode.CreateFinalValueFrom}
                        sequenceOfFinalValues :> IEnumerable
                  | None ->
                        Seq.empty :> IEnumerable
            override this.MaximumStrength =
                match node.PruneTree with
                    Some prunedNode ->
                        prunedNode.MaximumStrengthOfTestVariableCombination
                  | None ->
                        0u
