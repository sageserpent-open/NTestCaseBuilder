#light

module SageSerpent.TestInfrastructure.InterleavedTestCaseEnumerableFactory

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    let Create (sequenceOfFactoriesProvidingSubsequencesToInterleave: seq<ITestCaseEnumerableFactory>) =
        if Seq.is_empty sequenceOfFactoriesProvidingSubsequencesToInterleave
        then raise (PreconditionViolationException "Must provide at least one alternative.")
        let node =
            InterleavingNode (sequenceOfFactoriesProvidingSubsequencesToInterleave
                              |> Seq.map (fun factory
                                            -> factory.Node))
        {
            new TestCaseEnumerableFactoryCommonImplementation ()                                       
                interface INodeWrapper with
                    override this.Node =
                        node
        } :> ITestCaseEnumerableFactory

