#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    type InterleavedTestCaseEnumeratorFactory (sequenceOfFactoriesProvidingSubsequencesToInterleave: seq<ITestCaseEnumeratorFactory>) =
        inherit TestCaseEnumeratorFactoryCommonImplementation ()
        do if Seq.is_empty sequenceOfFactoriesProvidingSubsequencesToInterleave
           then raise (PreconditionViolationException "Must provide at least one alternative.")
        let node =
            InterleavingNode (sequenceOfFactoriesProvidingSubsequencesToInterleave
                              |> Seq.map (fun factory
                                            -> factory.Node))
        interface INodeWrapper with
            override this.Node =
                node

