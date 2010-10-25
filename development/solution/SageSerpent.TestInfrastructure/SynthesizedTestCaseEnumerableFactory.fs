module SageSerpent.TestInfrastructure.SynthesizedTestCaseEnumerableFactory

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    let Create (sequenceOfFactoriesProvidingInputsToSynthesis: seq<TestCaseEnumerableFactory>)
               (synthesisDelegate: Delegate) =
        if Seq.isEmpty sequenceOfFactoriesProvidingInputsToSynthesis
        then raise (PreconditionViolationException "Must provide at least one component.")               
        let node =
            SynthesizingNode ((sequenceOfFactoriesProvidingInputsToSynthesis
                              |> List.ofSeq
                              |> List.map (fun factory
                                            -> factory.Node))
                              , synthesisDelegate)
        TestCaseEnumerableFactory node
