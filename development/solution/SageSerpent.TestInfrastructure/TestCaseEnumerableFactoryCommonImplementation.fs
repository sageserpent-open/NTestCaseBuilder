﻿namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    type internal TestCaseEnumerableFactoryCommonImplementation (node: Node<IComparable>) =
        member this.Node = node
        
        interface ITestCaseEnumerableFactory with
            override this.CreateEnumerable desiredStrength =
                match node.PruneTree with
                    Some prunedNode ->
                        let mergedPartialTestVectorRepresentations =
                            let partialTestVectors =
                                prunedNode.PartialTestVectorRepresentationsGroupedByStrengthUpToAndIncluding desiredStrength
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
                        let sequenceOfFinalValues =
                            seq {for mergedPartialTestVector in mergedPartialTestVectorRepresentations do
                                    yield prunedNode.FillOutPartialTestVectorRepresentation randomBehaviour mergedPartialTestVector
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
