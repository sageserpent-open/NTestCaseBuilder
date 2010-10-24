#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System

    type TestVariableLevelEnumeratorFactory (levels: seq<Object>) =
        inherit TestCaseEnumeratorFactoryCommonImplementation ()
        let node =
            SageSerpent.TestInfrastructure.TestVariableNode levels
        interface INodeWrapper with
            override this.Node = node

        