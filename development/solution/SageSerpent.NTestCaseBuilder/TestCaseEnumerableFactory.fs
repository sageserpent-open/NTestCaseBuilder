namespace SageSerpent.NTestCaseBuilder

    open System.Collections
    open System.Collections.Generic
    open System
    open System.Runtime.Serialization.Formatters.Binary
    open System.IO
    open System.Numerics
    open SageSerpent.Infrastructure
    open SageSerpent.Infrastructure.RandomExtensions
    open SageSerpent.Infrastructure.IEnumerableExtensions

    module TestCaseEnumerableFactoryDetail =
        let baseSize =
            bigint (int32 Byte.MaxValue) + 1I

        let serialize (fullTestVector: FullTestVector) =
            let binaryFormatter = BinaryFormatter ()
            use memoryStream = new MemoryStream ()
            binaryFormatter.Serialize (memoryStream, fullTestVector)
            let byteArray =
                memoryStream.GetBuffer ()
            let byteArrayRepresentedByBigInteger =
                byteArray
                |> Seq.fold (fun representationOfPreviousBytes
                                 byte ->
                                 bigint (int32 byte) + representationOfPreviousBytes * baseSize)
                            1I  // NOTE: start with one rather than the obvious choice of zero: this avoids losing leading zero-bytes which
                                // from the point of view of building a number to the base of 'baseSize' are redundant.
            byteArrayRepresentedByBigInteger

        let deserialize byteArrayRepresentedByBigInteger =
            let binaryFormatter = BinaryFormatter ()
            let byteArray =
                byteArrayRepresentedByBigInteger
                |> Seq.unfold (fun representationOfBytes ->
                                if 1I = representationOfBytes   // NOTE: see comment in implementation of 'serialize'.
                                then
                                    None
                                else
                                    Some (byte (int32 (representationOfBytes % baseSize))
                                          , representationOfBytes / baseSize))
                |> Array.ofSeq
                |> Array.rev
            use memoryStream = new MemoryStream (byteArray)
            binaryFormatter.Deserialize memoryStream
            |> unbox
            : FullTestVector

        let makeDescriptionOfReproductionString fullTestVector =
            String.Format ("Encoded text for reproduction of test case follows on next line as C# string:\n\"{0}\"",
                           (serialize fullTestVector).ToString())
    open TestCaseEnumerableFactoryDetail

    type TestCaseReproductionException (fullTestVector
                                        , innerException) =
        inherit Exception (makeDescriptionOfReproductionString fullTestVector
                           , innerException)

    /// <summary>A factory that can create an enumerable sequence of test cases used in turn to drive a parameterised
    /// unit test. Each test case is presented as a parameter to repeated calls to the parameterised unit test. The test
    /// cases are configured in terms of 'test variables', each of which can be set a particular 'level'; a given
    /// assignment of a level to each test variable then determines the composition of a test case.</summary>

    /// <seealso cref="TestVariableLevelEnumerableFactory.Create">Construction method.</seealso>
    /// <seealso cref="SynthesizedTestCaseEnumerableFactory.Create">Construction method.</seealso>
    /// <seealso cref="InterleavedTestCaseEnumerableFactory.Create">Construction method.</seealso>

    /// <remarks>Test case enumerable factories form a composite data structure, where a factory is the root
    /// of a tree of simpler factories, and can itself be part of a larger tree (or even part of several trees,
    /// as sharing is permitted). Ultimately a valid tree of test case enumerable factories will have leaf node
    /// factories based on sequences of test variable levels: each leaf node factory produces a trivial
    /// sequence of test cases that are just the levels of its own test variable.

    /// Factories that are internal nodes in the tree belong to two types. The first type is a synthesizing
    /// factory: it creates a sequence of synthesized test cases, where each synthesized test case is created
    /// from several simpler input test cases taken from across distinct sequences provided by corresponding
    /// subtrees of the synthesizing factory. The second type is an interleaving factory: it creates a sequence
    /// by mixing the sequences provided by the subtrees of the interleaving factory.

    /// The crucial idea is that for any factory at the head of some subtree, then given a test variable
    /// combination strength, a sequence of test cases can be created that satisfies the property that every
    /// combination of that strength of levels from distinct test variables represented by the leaves exists
    /// in at least one of the test cases in the sequence, provided the following conditions are met:-
    /// 1. The number of distinct test variables is at least the requested combination strength (obviously).
    /// 2. For any combination in question above, the test variables that contribute the levels being combined
    /// all do so via paths up to the head of the tree that fuse together at synthesizing factories and *not*
    /// at interleaving factories.

    /// So for example, creating a sequence of strength two for a tree composed solely of synthesizing factories
    /// for the internal nodes and test variable leaf nodes would guarantee all-pairs coverage for all of the
    /// test variables - any pair of levels taken from distinct test variables would be found in at least one
    /// test case somewhere in the sequence.</remarks>

    /// <remarks>1. Levels belonging to the same test variable are never combined together via any of the
    /// internal factory nodes.</remarks>
    /// <remarks>2. Sharing a single sequence of test variable levels between different leaf node factories
    /// in a tree (or sharing tree node factories to create a DAG) does not affect the strength guarantees:
    /// any sharing of levels or nodes is 'expanded' to create the same effect as an equivalent tree structure
    /// containing duplicated test variables and levels.</remarks>
    /// <remarks>3. If for a combination of levels from distinct test variables, the test variables that
    /// contribute the levels being combined all do so via paths up to the head of the tree that fuse together
    /// at interleaving factories, then that combination *will not* occur in any test case in any sequence
    /// generated by the head of the tree.</remarks>
    /// <remarks>4. If some test variables can only be combined in strengths less than the requested strength (due
    /// to interleaving between some of the test variables), then the sequence will contain combinations of these
    /// variables in the maximum possible strength.</remarks>
    /// <remarks>5. If there are insufficient test variables to make up the combination strength, or there are
    /// enough test variables but not enough that are combined by synthesizing factories, then the sequence
    /// will 'do its best' by creating combinations of up to the highest strength possible, falling short of the
    /// requested strength.</remarks>
    /// <remarks>6. A factory only makes guarantees as to the strength of combination of levels that contribute
    /// via synthesis to a final test case: so if for example a synthesizing factory causes 'collisions' to occur
    /// whereby several distinct combinations of simpler test cases all create the same output test case, then no
    /// attempt will be made to work around this behaviour and try alternative combinations that satisfy the
    /// strength requirements but create fewer collisions.</remarks>
    [<AbstractClass>]
    type TestCaseEnumerableFactory (node: Node) =
        member internal this.Node = node

        /// <summary>Creates an enumerable sequence of test cases during execution and repeatedly executes
        /// a parameterised unit test for each test case in the sequence.</summary>
        /// <remarks>If an exception is thrown during execution of the unit test for some test case in the sequence, then this is
        /// wrapped as an inner exception into an instance of TestCaseReproductionException. The test case reproduction exception
        /// also contains a description of a text token (denoted by a string literal) that can be cut and pasted into a call to
        /// the ExecuteParameterisedUnitTestForReproducedTestCase method to reproduce the failing test case without going over
        /// the other successful test cases.</remarks>
        /// <param name="maximumDesiredStrength">Maximum strength of test variable combinations that must be covered by the sequence.</param>
        /// <param name="parameterisedUnitTest">A call to this delegate runs a unit test over the test case passed in as the single parameter.</param>
        /// <seealso cref="ExecuteParameterisedUnitTestForReproducedTestCase"/>
        abstract ExecuteParameterisedUnitTestForAllTestCases: UInt32 * Action<Object> -> UInt32

        /// <summary>Executes a parameterised unit test for some specific test case described by a reproduction string.</summary>
        /// <remarks>The reproduction string is obtained by catching (or examining via a debugger) a test case reproduction exeception
        /// obtained while running ExecuteParameterisedUnitTestForAllTestCases.</remarks>
        /// <param name="parameterisedUnitTest">A call to this delegate runs a unit test over the test case passed in as the single parameter.</param>
        /// <param name="reproductionString">Encoded text that reproduces the test case: this is obtained by inspection of a test case reproduction
        /// exception when running a failing test via ExecuteParameterisedUnitTestForAllTestCases.</param>
        abstract ExecuteParameterisedUnitTestForReproducedTestCase: Action<Object> * String -> Unit

        /// <summary>Creates an emumerable sequence of test cases that provides a guarantee of strength of
        /// combination of test variables across the entire sequence.</summary>
        /// <param name="maximumDesiredStrength">Maximum strength of test variable combinations that must be covered by the sequence.</param>
        /// <returns>A sequence of test cases typed as a non-generic IEnumerable.</returns>
        abstract CreateEnumerable: UInt32 -> IEnumerable

        /// <value>The maximum strength of combination of test variables that the factory can make guarantees about in a call to CreateEnumerable.</value>
        abstract MaximumStrength: UInt32

    /// <summary>This extends the API provided by TestCaseEnumerableFactory to deal with test cases of a specific type given
    /// by the type parameter TestCase.</summary>
    /// <seealso cref="TestCaseEnumerableFactory">The core API provided by the baseclass.</seealso>
    type TypedTestCaseEnumerableFactory<'TestCase> (node: Node) =
        inherit TestCaseEnumerableFactory (node)

        default this.ExecuteParameterisedUnitTestForAllTestCases (maximumDesiredStrength
                                                                  , parameterisedUnitTest: Action<Object>) =
            this.ExecuteParameterisedUnitTestForAllTypedTestCasesWorkaroundForDelegateNonCovariance (maximumDesiredStrength
                                                                                                     , parameterisedUnitTest.Invoke)

        default this.ExecuteParameterisedUnitTestForReproducedTestCase (parameterisedUnitTest: Action<Object>
                                                                        , reproductionString) =
            this.ExecuteParameterisedUnitTestForReproducedTypedTestCaseWorkaroundForDelegateNonCovariance (parameterisedUnitTest.Invoke
                                                                                                           , reproductionString)

        default this.CreateEnumerable maximumDesiredStrength =
            this.CreateTypedEnumerable maximumDesiredStrength
            :> IEnumerable

        default this.MaximumStrength =
            match this.Node.PruneTree with
                Some prunedNode ->
                    prunedNode.MaximumStrengthOfTestVariableCombination
              | None ->
                    0u

        member this.CreateTypedEnumerable maximumDesiredStrength =
            this.CreateEnumerableOfTypedTestCaseAndItsFullTestVector maximumDesiredStrength
            |> Seq.map fst

        member private this.ExecuteParameterisedUnitTestForAllTypedTestCasesWorkaroundForDelegateNonCovariance (maximumDesiredStrength
                                                                                                                , parameterisedUnitTest) =
            let mutable count = 0u
            for testCase
                , fullTestVector in this.CreateEnumerableOfTypedTestCaseAndItsFullTestVector maximumDesiredStrength do
                try
                    parameterisedUnitTest testCase
                    count <- count + 1u
                with
                    anyException ->
                        raise (TestCaseReproductionException (fullTestVector
                                                              , anyException))
            count

        member this.ExecuteParameterisedUnitTestForAllTypedTestCases (maximumDesiredStrength
                                                                      , parameterisedUnitTest: Action<'TestCase>) =
            this.ExecuteParameterisedUnitTestForAllTypedTestCasesWorkaroundForDelegateNonCovariance (maximumDesiredStrength
                                                                                                     , parameterisedUnitTest.Invoke)

        member private this.ExecuteParameterisedUnitTestForReproducedTypedTestCaseWorkaroundForDelegateNonCovariance (parameterisedUnitTest
                                                                                                                      , reproductionString) =
            match this.Node.PruneTree with
                Some prunedNode ->
                    let fullTestVector =
                        BigInteger.Parse reproductionString
                        |> deserialize
                    let finalValueCreator =
                        prunedNode.FinalValueCreator ()
                    let testCase =
                        finalValueCreator fullTestVector: 'TestCase
                    parameterisedUnitTest testCase
              | None ->
                    ()

        member this.ExecuteParameterisedUnitTestForReproducedTypedTestCase (parameterisedUnitTest: Action<'TestCase>
                                                                            , reproductionString) =
            this.ExecuteParameterisedUnitTestForReproducedTypedTestCaseWorkaroundForDelegateNonCovariance (parameterisedUnitTest.Invoke
                                                                                                           , reproductionString)

        member private this.CreateEnumerableOfTypedTestCaseAndItsFullTestVector maximumDesiredStrength =
            match this.Node.PruneTree with
                Some prunedNode ->
                    let associationFromStrengthToPartialTestVectorRepresentations
                        , associationFromTestVariableIndexToNumberOfItsLevels =
                        prunedNode.AssociationFromStrengthToPartialTestVectorRepresentations maximumDesiredStrength
                    let overallNumberOfTestVariables =
                        prunedNode.CountTestVariables
                    let randomBehaviour =
                        Random 0
                    let sequenceOfFinalValues =
                        let partialTestVectorsInOrderOfDecreasingStrength = // Order by decreasing strength so that high strength vectors get in there
                                                                            // first. Hopefully the lesser strength vectors should have a greater chance
                                                                            // of finding an earlier, larger vector to merge with this way.
                            (seq
                                {
                                    for partialTestVectorsAtTheSameStrength in associationFromStrengthToPartialTestVectorRepresentations
                                                                               |> Seq.sortBy (fun keyValuePair -> - (int32 keyValuePair.Key))
                                                                               |> Seq.map (fun keyValuePair -> keyValuePair.Value) do
                                        yield! partialTestVectorsAtTheSameStrength
                                })

                        let lazilyProduceMergedPartialTestVectors mergedPartialTestVectorRepresentations
                                                                  partialTestVectors =
                            seq
                                {
                                    let locallyModifiedMergedPartialTestVectorRepresentations =
                                        ref mergedPartialTestVectorRepresentations

                                    for partialTestVector in partialTestVectors do
                                        match (!locallyModifiedMergedPartialTestVectorRepresentations: MergedPartialTestVectorRepresentations<_>).MergeOrAdd partialTestVector with
                                            updatedMergedPartialTestVectorRepresentationsWithFullTestCaseVectorSuppressed
                                            , Some resultingFullTestCaseVector ->
                                                yield resultingFullTestCaseVector

                                                locallyModifiedMergedPartialTestVectorRepresentations := updatedMergedPartialTestVectorRepresentationsWithFullTestCaseVectorSuppressed
                                          | updatedMergedPartialTestVectorRepresentations
                                            , None ->
                                                locallyModifiedMergedPartialTestVectorRepresentations := updatedMergedPartialTestVectorRepresentations

                                    yield! (!locallyModifiedMergedPartialTestVectorRepresentations).EnumerationOfMergedTestVectors false
                                }

                        let lazilyProducedMergedPartialTestVectors =
                            lazilyProduceMergedPartialTestVectors (MergedPartialTestVectorRepresentations.Initial overallNumberOfTestVariables)
                                                                  partialTestVectorsInOrderOfDecreasingStrength

                        let finalValueCreator =
                            prunedNode.FinalValueCreator ()
                        seq
                            {
                                for mergedPartialTestVector in lazilyProducedMergedPartialTestVectors  do
                                    let fullTestVector =
                                        prunedNode.FillOutPartialTestVectorRepresentation associationFromTestVariableIndexToNumberOfItsLevels
                                                                                          mergedPartialTestVector
                                                                                          randomBehaviour
                                    yield (finalValueCreator fullTestVector: 'TestCase)
                                           , fullTestVector
                            }
                    sequenceOfFinalValues

              | None ->
                    Seq.empty

