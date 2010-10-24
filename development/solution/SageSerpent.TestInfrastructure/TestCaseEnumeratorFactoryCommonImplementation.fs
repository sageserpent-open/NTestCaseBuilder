#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    [<AbstractClass>]
    type TestCaseEnumeratorFactoryCommonImplementation () as this =
        interface ITestCaseEnumeratorFactory with
            override this.CreateEnumerator desiredStrength =
                let node =
                    (this :> INodeWrapper).Node
                let mergedPartialTestVectorRepresentations =
                    let partialTestVectors =
                        node.PartialTestVectorRepresentationsGroupedByStrengthUpToAndIncluding desiredStrength
                    // Do a fold right so that high strength combinations get in there first. Hopefully the lesser strength combinations
                    // should have a greater chance of finding an earlier, larger vector to merge with this way. 
                    List.fold_right (fun partialTestVectorsAtTheSameStrength
                                         mergedPartialTestVectorRepresentations ->
                                            partialTestVectorsAtTheSameStrength
                                            |> Seq.fold (fun mergedPartialTestVectorRepresentations
                                                             partialTestVector ->
                                                                (mergedPartialTestVectorRepresentations: MergedPartialTestVectorRepresentations<_>).MergeOrAdd partialTestVector)
                                                        mergedPartialTestVectorRepresentations)
                                    partialTestVectors
                                    MergedPartialTestVectorRepresentations.initial
                let randomBehaviour = RandomBehaviour 0
                let sequenceOfFinalValues =
                    seq { for mergedPartialTestVector in mergedPartialTestVectorRepresentations do
                            yield node.FillOutPartialTestVectorRepresentation randomBehaviour mergedPartialTestVector
                                  |> node.CreateFinalValueFrom }
                (sequenceOfFinalValues :> IEnumerable).GetEnumerator ()
            override this.MaximumStrength =
                (this:> INodeWrapper).Node.MaximumStrengthOfTestVariableCombination

