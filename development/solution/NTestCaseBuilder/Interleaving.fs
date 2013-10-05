namespace NTestCaseBuilder

    open System.Collections
    open System
    open SageSerpent.Infrastructure
    open NodeExtensions

    type Interleaving =
        /// <summary>Constructor function that creates an instance of Factory.</summary>
        /// <remarks>The resulting factory yields a sequence of test cases contributed by all of the subsequences that would
        /// be yielded by the child factories used to construct it. These subsequences are not actually interleaved in a systematic
        /// order such as round-robin: rather the name denotes that the contributed test cases are mixed up from different
        /// subsequences rather than combined.</remarks>
        /// <param name="sequenceOfFactoriesProvidingSubsequencesToInterleave">A sequence of factories whose test cases are interleaved
        /// into the sequence yielded by the constructed factory.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="Factory">Type of constructed factory.</seealso>
        static member Create (sequenceOfFactoriesProvidingSubsequencesToInterleave: seq<Factory>) =
            if Seq.isEmpty sequenceOfFactoriesProvidingSubsequencesToInterleave
            then
                raise (PreconditionViolationException "Must provide at least one alternative.")
            let node =
                InterleavingNode (sequenceOfFactoriesProvidingSubsequencesToInterleave
                                  |> List.ofSeq
                                  |> List.map (fun factory
                                                -> factory.Node))
            TypedFactory<_> node
            :> Factory

        /// <summary>Constructor function that creates an instance of TypedFactory&lt;'TestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of test cases contributed by all of the subsequences that would
        /// be yielded by the child factories used to construct it. These subsequences are not actually interleaved in a systematic
        /// order such as round-robin: rather the name denotes that the contributed test cases are mixed up from different
        /// subsequences rather than combined.</remarks>
        /// <param name="sequenceOfFactoriesProvidingSubsequencesToInterleave">A sequence of factories whose test cases are interleaved
        /// into the sequence yielded by the constructed factory.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TypedFactory&lt;'TestCase&gt;">Type of constructed factory.</seealso>
        static member Create (sequenceOfFactoriesProvidingSubsequencesToInterleave: seq<TypedFactory<'TestCase>>) =
            if Seq.isEmpty sequenceOfFactoriesProvidingSubsequencesToInterleave
            then
                raise (PreconditionViolationException "Must provide at least one alternative.")
            let node =
                InterleavingNode (sequenceOfFactoriesProvidingSubsequencesToInterleave
                                  |> List.ofSeq
                                  |> List.map (fun factory
                                                -> factory.Node))
            TypedFactory<'TestCase> node
