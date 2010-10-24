#light

module SageSerpent.TestInfrastructure.TestVariableLevelEnumerableFactory

    open System.Collections
    open System

    let Create levels =
        let weaklyTypedLevels =
            levels
            |> Seq.map box
        let node =
            SageSerpent.TestInfrastructure.TestVariableNode weaklyTypedLevels
        {
            new TestCaseEnumerableFactoryCommonImplementation ()
                interface INodeWrapper with
                    override this.Node = node
        } :> ITestCaseEnumerableFactory

        