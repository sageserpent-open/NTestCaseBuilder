namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    type internal TestCaseEnumerableFactoryCommonImplementation(node: Node) =
        member this.Node = node
        
        interface ITestCaseEnumerableFactory with
            override this.CreateEnumerable desiredStrength =
                let mergedPartialTestVectorRepresentations =
                    let partialTestVectors =
                        node.PartialTestVectorRepresentationsGroupedByStrengthUpToAndIncluding desiredStrength
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
                            yield node.FillOutPartialTestVectorRepresentation randomBehaviour mergedPartialTestVector
                                  |> node.CreateFinalValueFrom}
                sequenceOfFinalValues :> IEnumerable
            override this.MaximumStrength =
                node.MaximumStrengthOfTestVariableCombination

