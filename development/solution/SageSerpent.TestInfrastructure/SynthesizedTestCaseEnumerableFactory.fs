#light

module SageSerpent.TestInfrastructure.SynthesizedTestCaseEnumerableFactory

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    let Create (sequenceOfFactoriesProvidingInputsToSynthesis: seq<ITestCaseEnumerableFactory>)
               (synthesisDelegate: Delegate) =
        if Seq.is_empty sequenceOfFactoriesProvidingInputsToSynthesis
        then raise (PreconditionViolationException "Must provide at least one component.")
        let delegateInternalMethodArity =
            (synthesisDelegate.Method.GetParameters ()).Length
        let numberOfArgumentsRequiredByDelegate =
            if synthesisDelegate.Target <> null
            then delegateInternalMethodArity - 1
            else delegateInternalMethodArity
        if numberOfArgumentsRequiredByDelegate <> Seq.length sequenceOfFactoriesProvidingInputsToSynthesis
        then raise (PreconditionViolationException "Delegate for synthesis takes the wrong number of arguments: it should match the number of factories provided.")
        let node =
            SynthesizingNode ((sequenceOfFactoriesProvidingInputsToSynthesis
                              |> Seq.map (fun factory
                                            -> factory.Node))
                              , synthesisDelegate)
        {
            new TestCaseEnumerableFactoryCommonImplementation ()
                interface INodeWrapper with
                    override this.Node =
                        node
        } :> ITestCaseEnumerableFactory
