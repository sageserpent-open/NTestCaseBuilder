module SageSerpent.TestInfrastructure.InterleavedTestCaseEnumerableFactory

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    let Create (sequenceOfFactoriesProvidingSubsequencesToInterleave: seq<TestCaseEnumerableFactory>) =
        if Seq.isEmpty sequenceOfFactoriesProvidingSubsequencesToInterleave
        then raise (PreconditionViolationException "Must provide at least one alternative.")
        let node =
            InterleavingNode (sequenceOfFactoriesProvidingSubsequencesToInterleave
                              |> List.ofSeq
                              |> List.map (fun factory
                                            -> factory.Node))
        TestCaseEnumerableFactory node

