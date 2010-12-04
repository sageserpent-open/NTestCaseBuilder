namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    open SageSerpent.Infrastructure
    
    type InterleavedTestCaseEnumerableFactory =
        static member Create (sequenceOfFactoriesProvidingSubsequencesToInterleave: seq<TestCaseEnumerableFactory>) =
            if Seq.isEmpty sequenceOfFactoriesProvidingSubsequencesToInterleave
            then
                raise (PreconditionViolationException "Must provide at least one alternative.")
            let node =
                InterleavingNode (sequenceOfFactoriesProvidingSubsequencesToInterleave
                                  |> List.ofSeq
                                  |> List.map (fun factory
                                                -> factory.Node))
            TypedTestCaseEnumerableFactory<_> node
            :> TestCaseEnumerableFactory

        static member Create (sequenceOfFactoriesProvidingSubsequencesToInterleave: seq<TypedTestCaseEnumerableFactory<'TestCase>>) =
            if Seq.isEmpty sequenceOfFactoriesProvidingSubsequencesToInterleave
            then
                raise (PreconditionViolationException "Must provide at least one alternative.")
            let node =
                InterleavingNode (sequenceOfFactoriesProvidingSubsequencesToInterleave
                                  |> List.ofSeq
                                  |> List.map (fun factory
                                                -> factory.Node))
            TypedTestCaseEnumerableFactory<'TestCase> node
