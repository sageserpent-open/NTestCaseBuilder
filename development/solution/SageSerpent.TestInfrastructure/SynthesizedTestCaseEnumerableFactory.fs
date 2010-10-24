module SageSerpent.TestInfrastructure.SynthesizedTestCaseEnumerableFactory

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    let Create (sequenceOfFactoriesProvidingInputsToSynthesis: seq<ITestCaseEnumerableFactory>)
               (synthesisDelegate: Delegate) =
        if Seq.isEmpty sequenceOfFactoriesProvidingInputsToSynthesis
        then raise (PreconditionViolationException "Must provide at least one component.")               
        let node =
            SynthesizingNode ((sequenceOfFactoriesProvidingInputsToSynthesis
                              |> Seq.map (fun factory
                                            -> (factory :?> TestCaseEnumerableFactoryCommonImplementation).Node))
                              , synthesisDelegate)
        TestCaseEnumerableFactoryCommonImplementation node :> ITestCaseEnumerableFactory
