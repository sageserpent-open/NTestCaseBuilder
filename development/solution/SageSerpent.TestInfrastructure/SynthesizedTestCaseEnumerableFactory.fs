module SageSerpent.TestInfrastructure.SynthesizedTestCaseEnumerableFactory

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    let Create (sequenceOfFactoriesProvidingInputsToSynthesis: seq<ITestCaseEnumerableFactory>)
               (synthesisDelegate: Delegate) =
        if Seq.isEmpty sequenceOfFactoriesProvidingInputsToSynthesis
        then raise (PreconditionViolationException "Must provide at least one component.")
        let delegateInternalMethodArity =
            (synthesisDelegate.Method.GetParameters ()).Length
        let numberOfArgumentsRequiredByDelegate =
            match synthesisDelegate.Target with
                null -> delegateInternalMethodArity
              | _ -> delegateInternalMethodArity - 1
        if numberOfArgumentsRequiredByDelegate <> Seq.length sequenceOfFactoriesProvidingInputsToSynthesis
        then raise (PreconditionViolationException "Delegate for synthesis takes the wrong number of arguments: it should match the number of factories provided.")
        let node =
            SynthesizingNode ((sequenceOfFactoriesProvidingInputsToSynthesis
                              |> Seq.map (fun factory
                                            -> (factory :?> TestCaseEnumerableFactoryCommonImplementation).Node))
                              , synthesisDelegate)
        TestCaseEnumerableFactoryCommonImplementation node :> ITestCaseEnumerableFactory
