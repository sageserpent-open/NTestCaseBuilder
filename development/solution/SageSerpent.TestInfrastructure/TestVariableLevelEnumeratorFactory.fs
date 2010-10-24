#light

module SageSerpent.TestInfrastructure.TestVariableLevelEnumeratorFactory

    open System.Collections
    open System

    let Create levels =
        let weaklyTypedLevels =
            levels
            |> Seq.map box
        let node =
            SageSerpent.TestInfrastructure.TestVariableNode weaklyTypedLevels
        {
            new TestCaseEnumeratorFactoryCommonImplementation ()
                interface INodeWrapper with
                    override this.Node = node
        } :> ITestCaseEnumeratorFactory

        