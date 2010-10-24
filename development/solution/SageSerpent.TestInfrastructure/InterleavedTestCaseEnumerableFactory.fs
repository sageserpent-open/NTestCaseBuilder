module SageSerpent.TestInfrastructure.InterleavedTestCaseEnumerableFactory

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    let Create (sequenceOfFactoriesProvidingSubsequencesToInterleave: seq<ITestCaseEnumerableFactory>) =
        if Seq.isEmpty sequenceOfFactoriesProvidingSubsequencesToInterleave
        then raise (PreconditionViolationException "Must provide at least one alternative.")
        let node =
            InterleavingNode (sequenceOfFactoriesProvidingSubsequencesToInterleave
                              |> Seq.map (fun factory
                                            -> (factory :?> TestCaseEnumerableFactoryCommonImplementation).Node))
        TestCaseEnumerableFactoryCommonImplementation node :> ITestCaseEnumerableFactory

