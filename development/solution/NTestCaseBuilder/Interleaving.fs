namespace NTestCaseBuilder

    open System.Collections
    open System
    open SageSerpent.Infrastructure
    open NodeExtensions

    type Interleaving =
        /// <summary>Constructor function that creates an instance of IFactory.</summary>
        /// <remarks>The resulting factory yields a sequence of test cases contributed by all of the subsequences that would
        /// be yielded by the child factories used to construct it. These subsequences are not actually interleaved in a systematic
        /// order such as round-robin: rather the name denotes that the contributed test cases are mixed up from different
        /// subsequences rather than combined.</remarks>
        /// <param name="sequenceOfFactoriesProvidingSubsequencesToInterleave">A sequence of factories whose test cases are interleaved
        /// into the sequence yielded by the constructed factory.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="IFactory">Type of constructed factory.</seealso>
        static member Create (sequenceOfFactoriesProvidingSubsequencesToInterleave: seq<IFactory>) =
            if Seq.isEmpty sequenceOfFactoriesProvidingSubsequencesToInterleave
            then
                raise (PreconditionViolationException "Must provide at least one alternative.")
            let node =
                InterleavingNode (sequenceOfFactoriesProvidingSubsequencesToInterleave
                                  |> List.ofSeq
                                  |> List.map (fun factory
                                                -> (factory :?> INodeWrapper).Node))
            TypedFactoryImplementation<_> node
            :> IFactory

        /// <summary>Constructor function that creates an instance of ITypedFactory&lt;'TestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of test cases contributed by all of the subsequences that would
        /// be yielded by the child factories used to construct it. These subsequences are not actually interleaved in a systematic
        /// order such as round-robin: rather the name denotes that the contributed test cases are mixed up from different
        /// subsequences rather than combined.</remarks>
        /// <param name="sequenceOfFactoriesProvidingSubsequencesToInterleave">A sequence of factories whose test cases are interleaved
        /// into the sequence yielded by the constructed factory.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="ITypedFactory&lt;'TestCase&gt;">Type of constructed factory.</seealso>
        static member Create (sequenceOfFactoriesProvidingSubsequencesToInterleave: seq<ITypedFactory<'TestCase>>) =
            if Seq.isEmpty sequenceOfFactoriesProvidingSubsequencesToInterleave
            then
                raise (PreconditionViolationException "Must provide at least one alternative.")
            let node =
                InterleavingNode (sequenceOfFactoriesProvidingSubsequencesToInterleave
                                  |> List.ofSeq
                                  |> List.map (fun factory
                                                -> (factory :?> INodeWrapper).Node))
            TypedFactoryImplementation<'TestCase> node
            :> ITypedFactory<_>
