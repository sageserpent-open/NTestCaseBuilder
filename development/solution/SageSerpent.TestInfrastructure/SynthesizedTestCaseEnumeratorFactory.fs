#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    type SynthesizedTestCaseEnumeratorFactory (sequenceOfFactoriesProvidingInputsToSynthesis: seq<ITestCaseEnumeratorFactory>,
                                               synthesisDelegate: Delegate) =
        inherit TestCaseEnumeratorFactoryCommonImplementation ()
        do if Seq.is_empty sequenceOfFactoriesProvidingInputsToSynthesis
           then raise (PreconditionViolationException "Must provide at least one component.")
        let delegateInternalMethodArity =
            (synthesisDelegate.Method.GetParameters ()).Length
        let numberOfArgumentsRequiredByDelegate =
            if synthesisDelegate.Target <> null
            then delegateInternalMethodArity - 1
            else delegateInternalMethodArity
        do if numberOfArgumentsRequiredByDelegate <> Seq.length sequenceOfFactoriesProvidingInputsToSynthesis
           then raise (PreconditionViolationException "Delegate for synthesis takes the wrong number of arguments: it should match the number of factories provided.")
        let node =
            SynthesizingNode ((sequenceOfFactoriesProvidingInputsToSynthesis
                              |> Seq.map (fun factory
                                            -> factory.Node))
                              , synthesisDelegate)
        interface INodeWrapper with
            override this.Node =
                node
