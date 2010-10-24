#light

module SageSerpent.TestInfrastructure.InterleavedTestCaseEnumeratorFactory

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    let Create (sequenceOfFactoriesProvidingSubsequencesToInterleave: seq<ITestCaseEnumeratorFactory>) =
        if Seq.is_empty sequenceOfFactoriesProvidingSubsequencesToInterleave
        then raise (PreconditionViolationException "Must provide at least one alternative.")
        let node =
            InterleavingNode (sequenceOfFactoriesProvidingSubsequencesToInterleave
                              |> Seq.map (fun factory
                                            -> factory.Node))
        {
            new TestCaseEnumeratorFactoryCommonImplementation ()                                       
                interface INodeWrapper with
                    override this.Node =
                        node
        } :> ITestCaseEnumeratorFactory

