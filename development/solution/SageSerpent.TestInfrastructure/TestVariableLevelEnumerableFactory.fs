module SageSerpent.TestInfrastructure.TestVariableLevelEnumerableFactory

    open System.Collections
    open System

    let Create levels =
        let weaklyTypedLevels =
            levels
            |> Seq.map box
        let node =
            SageSerpent.TestInfrastructure.TestVariableNode weaklyTypedLevels
        TestCaseEnumerableFactoryCommonImplementation node :> ITestCaseEnumerableFactory

        