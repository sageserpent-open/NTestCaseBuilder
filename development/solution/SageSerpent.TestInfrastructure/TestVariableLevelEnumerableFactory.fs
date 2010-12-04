namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    
    type TestVariableLevelEnumerableFactory =
        static member Create (levels: seq<'TestCase>) =
            let weaklyTypedLevels =
                levels
                |> Seq.map (fun level -> box level)
                |> Array.ofSeq
            let node =
                SageSerpent.TestInfrastructure.TestVariableNode weaklyTypedLevels
            TypedTestCaseEnumerableFactory<'TestCase> node
