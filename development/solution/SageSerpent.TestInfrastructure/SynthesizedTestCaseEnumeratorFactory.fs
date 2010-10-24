#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    type SynthesizedTestCaseEnumeratorFactory (sequenceOfFactoriesProvidingInputsToSynthesis: seq<ITestCaseEnumeratorFactory>,
                                               synthesisDelegate: Delegate) =
        // TODO - add a precondition that checks the arity of the synthesis closure against the number of factories provided.
        inherit TestCaseEnumeratorFactoryCommonImplementation ()
        do if Seq.is_empty sequenceOfFactoriesProvidingInputsToSynthesis
           then raise (PreconditionViolationException "Must provide at least one component.")
        let node =
            SynthesizingNode ((sequenceOfFactoriesProvidingInputsToSynthesis
                              |> Seq.map (fun factory
                                            -> factory.Node))
                              , synthesisDelegate)
        interface INodeWrapper with
            override this.Node =
                node
