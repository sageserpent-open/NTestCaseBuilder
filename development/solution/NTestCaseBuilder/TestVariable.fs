namespace NTestCaseBuilder

    open System.Collections
    open System
    open NodeExtensions

    type TestVariable =
        /// <summary>Constructor function that creates an instance of TypedTestCaseEnumerableFactory&lt;'TestCase&gt;.</summary>
        /// <remarks>The resulting factory represents a single test variable.</remarks>
        /// <param name="levels">A sequence of test variable levels that can either be yielded in their
        /// own right as simple test cases by the factory or combined / interleaved by parent factories.</param>
        /// <returns>The constructed factory.</returns>
        static member Create (levels: seq<'TestCase>) =
            let weaklyTypedLevels =
                levels
                |> Seq.map (fun level -> box level)
                |> Array.ofSeq
            let node =
                TestVariableNode weaklyTypedLevels
            TypedFactory<'TestCase> node
