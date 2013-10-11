namespace NTestCaseBuilder

    open System.Collections
    open System
    open Microsoft.FSharp.Core
    open SageSerpent.Infrastructure
    open SageSerpent.Infrastructure.OptionWorkflow
    open SageSerpent.Infrastructure.CombinatoricUtilities
    open BargainBasement
    open NodeExtensions

    module SynthesisDetail =
        let mediateFinalValueCreatorType (createFinalValueFrom: List<FullTestVector> -> 'CreatorViewOfSynthesizedTestCase) =
            match createFinalValueFrom
                    |> box with
                :? (List<FullTestVector> -> 'CallerViewOfSynthesizedTestCase) as finalValueCreatorWithAgreedTypeSignature ->
                    finalValueCreatorWithAgreedTypeSignature
                    // We have to mediate between the expected precise type of the result and the actual type
                    // that is hidden by the abstracting interface 'IFixedCombinationOfSubtreeNodesForSynthesis'
                    // - client code must ensure that the types are the same. By doing this to a function value
                    // rather than to individual test cases produced by applying the function, we can take an
                    // up-front one-off performance hit and then get a precisely-typed function that doesn't
                    // have any internal (un)boxing in its own implementation: so the same function value can
                    // then be repeatedly applied to lots of full test variable vectors by the caller.
                | _ ->
                    createFinalValueFrom >> box >> unbox
                        // Plan B: if the caller wants the synthesized test case typed other than
                        // 'CreatorViewOfSynthesizedTestCase' then let's do a conversion via
                        // 'Object'. In practice the desired type will turn out to be 'Object' anyway,
                        // so the unbox operation will be trivial.

    open SynthesisDetail

    /// <summary>Low-level F#-specific API for collecting together child factories to construct a synthesized Factory.</summary>
    /// <seealso cref="Synthesis">Higher-level facade API that encapsulates usage of this class: try this first.</seealso>
    /// <seealso cref="FixedCombinationOfFactoriesForSynthesis">Cooperating class from low-level F#-specific API</seealso>
    type SynthesisInputs<'SynthesisFunction, 'SynthesizedTestCase> =
        {
            Prune: unit -> Option<SynthesisInputs<'SynthesisFunction, 'SynthesizedTestCase>>
            ContinuationToApplyResultsFromAllButRightmostFactory: 'SynthesisFunction -> List<FullTestVector> -> 'SynthesizedTestCase
            NodesInRightToLeftOrder: List<Node>
        }

        static member StartWithLeftmostFactory (leftmostFactory: TypedFactory<'TestCaseFromLeftmostFactory>) =
            let nodeFromLeftmostFactory =
                (leftmostFactory :?> NodeWrapper).Node
            let rec createSingletonCombination (nodeFromLeftmostFactory: Node) =
                {
                    Prune =
                        fun () ->
                            nodeFromLeftmostFactory.PruneTree
                            |> Option.map createSingletonCombination
                    ContinuationToApplyResultsFromAllButRightmostFactory =
                        fun synthesisFunction
                            slicesOfFullTestVectorInRightToLeftOrder ->
                                match slicesOfFullTestVectorInRightToLeftOrder with
                                    [ sliceOfFullTestVector ] ->
                                        (nodeFromLeftmostFactory.FinalValueCreator ()
                                                                                   sliceOfFullTestVector: 'TestCaseFromLeftmostFactory)
                                        |> synthesisFunction
                                  | _ ->
                                        raise (PreconditionViolationException "The rightmost factory expects a single slice of the full test vector in the list of slices.")
                    NodesInRightToLeftOrder =
                        [ nodeFromLeftmostFactory ]
                }
            createSingletonCombination nodeFromLeftmostFactory

        static member AddFactoryToTheRight (combinationOfAllOtherFactories,
                                            rightmostFactory: TypedFactory<'TestCaseFromRightmostFactory>) =
            let nodeFromRightmostFactory =
                (rightmostFactory :?> NodeWrapper).Node
            let rec createCombinationWithExtraRightmostNode (nodeFromRightmostFactory: Node)
                                                             combinationOfAllOtherFactories =
                {
                    Prune =
                        fun () ->
                            optionWorkflow
                                {
                                    let! prunedNodeFromRightmostFactory =
                                        nodeFromRightmostFactory.PruneTree
                                    let! prunedCombinationOfAllOtherFactories =
                                        combinationOfAllOtherFactories.Prune ()
                                    return createCombinationWithExtraRightmostNode prunedNodeFromRightmostFactory
                                                                                   prunedCombinationOfAllOtherFactories
                                }
                    ContinuationToApplyResultsFromAllButRightmostFactory =
                        fun synthesisFunction
                            slicesOfFullTestVectorInRightToLeftOrder ->
                                match slicesOfFullTestVectorInRightToLeftOrder with
                                    sliceOfFullTestVectorForRightmostFactory :: remainingSlicesOfFullTestVectorForAllButRightmostFactory ->
                                        let synthesisFunctionPartiallyAppliedToResultsFromAllButRightmostFactory =
                                            combinationOfAllOtherFactories.ContinuationToApplyResultsFromAllButRightmostFactory synthesisFunction
                                                                                                                                remainingSlicesOfFullTestVectorForAllButRightmostFactory
                                        (nodeFromRightmostFactory.FinalValueCreator ()
                                                                                    sliceOfFullTestVectorForRightmostFactory: 'TestCaseFromRightmostFactory)
                                        |> synthesisFunctionPartiallyAppliedToResultsFromAllButRightmostFactory
                                  | _ ->
                                        raise (PreconditionViolationException "The rightmost factory expects a head slice of the full test vector in the list of slices.")
                    NodesInRightToLeftOrder =
                        nodeFromRightmostFactory :: combinationOfAllOtherFactories.NodesInRightToLeftOrder
                }
            createCombinationWithExtraRightmostNode nodeFromRightmostFactory
                                                    combinationOfAllOtherFactories

    /// <summary>Low-level F#-specific API for bundling together a collection of child factories and a synthesis function to construct a synthesized Factory.</summary>
    /// <seealso cref="Synthesis">Higher-level facade API that encapsulates usage of this class: try this first.</seealso>
    /// <seealso cref="SynthesisInputs">Cooperating class from low-level F#-specific API</seealso>
    and FixedCombinationOfFactoriesForSynthesis<'SynthesisFunction, 'SynthesizedTestCase>
            (heterogenousCombinationOfFactoriesForSynthesis: SynthesisInputs<'SynthesisFunction, 'SynthesizedTestCase>
             , synthesisFunction) =
        let nodes =
            List.rev heterogenousCombinationOfFactoriesForSynthesis.NodesInRightToLeftOrder

        let createFinalValueFrom =
            List.rev
            >> heterogenousCombinationOfFactoriesForSynthesis.ContinuationToApplyResultsFromAllButRightmostFactory synthesisFunction

        interface IFixedCombinationOfSubtreeNodesForSynthesis with
            member this.Prune =
                optionWorkflow
                    {
                        let! prunedHeterogenousCombinationOfFactoriesForSynthesis =
                            heterogenousCombinationOfFactoriesForSynthesis.Prune ()
                        return FixedCombinationOfFactoriesForSynthesis (prunedHeterogenousCombinationOfFactoriesForSynthesis,
                                                                        synthesisFunction)
                               :> IFixedCombinationOfSubtreeNodesForSynthesis
                    }

            member this.Nodes =
                nodes
                |> Array.ofList

            member this.FinalValueCreator (): List<FullTestVector> -> 'CallerViewOfSynthesizedTestCase =
                mediateFinalValueCreatorType createFinalValueFrom

            member this.IsSubtreeZeroCost _ =
                false

            member this.IsSubtreeHiddenFromFilters _ =
                false

    type UnaryDelegate<'Argument, 'Result> =
        delegate of 'Argument -> 'Result

    type BinaryDelegate<'ArgumentOne, 'ArgumentTwo, 'Result> =
        delegate of 'ArgumentOne * 'ArgumentTwo -> 'Result

    type TernaryDelegate<'ArgumentOne, 'ArgumentTwo, 'ArgumentThree, 'Result> =
        delegate of 'ArgumentOne * 'ArgumentTwo * 'ArgumentThree -> 'Result

    type QuatenaryDelegate<'ArgumentOne, 'ArgumentTwo, 'ArgumentThree, 'ArgumentFour, 'Result> =
        delegate of 'ArgumentOne * 'ArgumentTwo * 'ArgumentThree * 'ArgumentFour -> 'Result

    type QuintenaryDelegate<'ArgumentOne, 'ArgumentTwo, 'ArgumentThree, 'ArgumentFour, 'ArgumentFive, 'Result> =
        delegate of 'ArgumentOne * 'ArgumentTwo * 'ArgumentThree * 'ArgumentFour * 'ArgumentFive -> 'Result

    type SequenceCondensation<'Item, 'Result> =
        delegate of seq<'Item> -> 'Result

    type Permutation<'Item> =
        delegate of seq<'Item> -> List<'Item>

    type Synthesis =
        /// <summary>Constructor function that creates an instance of Factory.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <remarks>The synthesis delegate is passed in weakly-typed form to allow synthesis functions of various
        /// arities and type signatures to be used with the same construction function: consistency is checked at runtime;
        /// any violations result in an exception being thrown. This approach is consistent with the use of the weakly-typed
        /// factory API of Factory used for the child factories.</remarks>
        /// <param name="sequenceOfFactoriesProvidingInputsToSynthesis">A sequence of factories whose test cases form inputs
        /// for synthesis.</param>
        /// <param name="synthesisDelegate">Delegate in weakly-typed form used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="Factory">Type of constructed factory.</seealso>
        static member Create (sequenceOfFactoriesProvidingInputsToSynthesis: #seq<Factory>,
                              synthesisDelegate: Delegate) =
            if Seq.isEmpty sequenceOfFactoriesProvidingInputsToSynthesis
            then
                raise (PreconditionViolationException "Must provide at least one component.")
            let node =
                let subtreeRootNodes =
                    sequenceOfFactoriesProvidingInputsToSynthesis
                    |> List.ofSeq
                    |> List.map (fun factory
                                    -> (factory :?> NodeWrapper).Node)
                Node.CreateSynthesizingNode subtreeRootNodes
                                            synthesisDelegate
            TypedFactoryImplementation<_> node
            :> Factory

        /// <summary>Constructor function that creates an instance of TypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a curried synthesis function
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <param name="fixedCombinationOfFactoriesForSynthesis">Parameter object from low-level F#-specific API that bundles together the child factories and synthesis function.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TypedFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        /// <seealso cref="FixedCombinationOfFactoriesForSynthesis">Cooperating class from low-level F#-specific API.</seealso>
        static member inline Create (fixedCombinationOfFactoriesForSynthesis: FixedCombinationOfFactoriesForSynthesis<_, 'SynthesizedTestCase>) =
            TypedFactoryImplementation<'SynthesizedTestCase> (fixedCombinationOfFactoriesForSynthesis
                                                              :> IFixedCombinationOfSubtreeNodesForSynthesis
                                                              |> SynthesizingNode)
            :> TypedFactory<_>

        // TODO: re-implement with special-case implementations of 'IFixedCombinationOfSubtreeNodesForSynthesis';
        // we don't need the full API using 'SynthesisInputs<_, _>' to do these tuple cases.

        /// <summary>Constructor function that creates an instance of TypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of an input test case taken from the sequence yielded by the the child factory used to construct the factory.
        /// The input test case is converted by means of a delegate that takes it as a parameter and synthesises an output test case.</remarks>
        /// <param name="synthesisDelegate">Delegate used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TypedFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (factory: TypedFactory<_>,
                              synthesisDelegate: UnaryDelegate<_, 'SynthesizedTestCase>) =
            let singletonCombinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.StartWithLeftmostFactory factory
            let fixedCombinationOfFactoriesForSynthesis =
                FixedCombinationOfFactoriesForSynthesis (singletonCombinationOfFactoriesForSynthesis
                                                         , synthesisDelegate.Invoke)
            Synthesis.Create fixedCombinationOfFactoriesForSynthesis
            :> TypedFactory<_>

        /// <summary>Constructor function that creates an instance of TypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <param name="synthesisDelegate">Delegate used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TypedFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (factoryOne: TypedFactory<_>,
                              factoryTwo: TypedFactory<_>,
                              synthesisDelegate: BinaryDelegate<_, _, _>) =
            let singletonCombinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.StartWithLeftmostFactory factoryOne
            let combinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.AddFactoryToTheRight (singletonCombinationOfFactoriesForSynthesis,
                                                                                           factoryTwo)
            let fixedCombinationOfFactoriesForSynthesis =
                FixedCombinationOfFactoriesForSynthesis (combinationOfFactoriesForSynthesis
                                                         , FuncConvert.FuncFromTupled<_, _, 'SynthesizedTestCase> synthesisDelegate.Invoke)
            Synthesis.Create fixedCombinationOfFactoriesForSynthesis
            :> TypedFactory<_>

        /// <summary>Constructor function that creates an instance of TypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <param name="synthesisDelegate">Delegate used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TypedFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (factoryOne: TypedFactory<_>,
                              factoryTwo: TypedFactory<_>,
                              factoryThree: TypedFactory<_>,
                              synthesisDelegate: TernaryDelegate<_, _, _, _>) =
            let singletonCombinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.StartWithLeftmostFactory factoryOne
            let combinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.AddFactoryToTheRight(singletonCombinationOfFactoriesForSynthesis,
                                                                                          factoryTwo)
            let combinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.AddFactoryToTheRight(combinationOfFactoriesForSynthesis,
                                                                                          factoryThree)
            let fixedCombinationOfFactoriesForSynthesis =
                FixedCombinationOfFactoriesForSynthesis (combinationOfFactoriesForSynthesis
                                                         , FuncConvert.FuncFromTupled<_, _, _, 'SynthesizedTestCase> synthesisDelegate.Invoke)
            Synthesis.Create fixedCombinationOfFactoriesForSynthesis
            :> TypedFactory<_>

        /// <summary>Constructor function that creates an instance of TypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <param name="synthesisDelegate">Delegate used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TypedFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (factoryOne: TypedFactory<_>,
                              factoryTwo: TypedFactory<_>,
                              factoryThree: TypedFactory<_>,
                              factoryFour: TypedFactory<_>,
                              synthesisDelegate: QuatenaryDelegate<_, _, _, _, _>) =
            let singletonCombinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.StartWithLeftmostFactory factoryOne
            let combinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.AddFactoryToTheRight(singletonCombinationOfFactoriesForSynthesis,
                                                                                          factoryTwo)
            let combinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.AddFactoryToTheRight(combinationOfFactoriesForSynthesis,
                                                                                          factoryThree)
            let combinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.AddFactoryToTheRight(combinationOfFactoriesForSynthesis,
                                                                                          factoryFour)
            let fixedCombinationOfFactoriesForSynthesis =
                FixedCombinationOfFactoriesForSynthesis (combinationOfFactoriesForSynthesis
                                                         , FuncConvert.FuncFromTupled<_, _, _, _, 'SynthesizedTestCase> synthesisDelegate.Invoke)
            Synthesis.Create fixedCombinationOfFactoriesForSynthesis
            :> TypedFactory<_>

        /// <summary>Constructor function that creates an instance of TypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <param name="synthesisDelegate">Delegate used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TypedFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (factoryOne: TypedFactory<_>,
                              factoryTwo: TypedFactory<_>,
                              factoryThree: TypedFactory<_>,
                              factoryFour: TypedFactory<_>,
                              factoryFive: TypedFactory<_>,
                              synthesisDelegate: QuintenaryDelegate<_, _, _, _, _, _>) =
            let singletonCombinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.StartWithLeftmostFactory factoryOne
            let combinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.AddFactoryToTheRight(singletonCombinationOfFactoriesForSynthesis,
                                                                                          factoryTwo)
            let combinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.AddFactoryToTheRight(combinationOfFactoriesForSynthesis,
                                                                                          factoryThree)
            let combinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.AddFactoryToTheRight(combinationOfFactoriesForSynthesis,
                                                                                          factoryFour)
            let combinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.AddFactoryToTheRight(combinationOfFactoriesForSynthesis,
                                                                                          factoryFive)
            let fixedCombinationOfFactoriesForSynthesis =
                FixedCombinationOfFactoriesForSynthesis (combinationOfFactoriesForSynthesis
                                                         , FuncConvert.FuncFromTupled<_, _, _, _, _, 'SynthesizedTestCase> synthesisDelegate.Invoke)
            Synthesis.Create fixedCombinationOfFactoriesForSynthesis
            :> TypedFactory<_>

        /// <summary>Constructor function that creates an instance of TypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases, each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them a sequence and synthesises an output test case.</remarks>
        /// <remarks>This is strongly typed - and as the condensation delegate takes a single sequence that bundles up the
        /// input test cases, it also imposes homogenity of the nominal type across the input test cases used in the
        /// synthesis; this approach avoids worrying about consistency of the arity of the actual delegate implementation,
        /// trading off some flexibility of the input test case types.</remarks>
        /// <remarks>There is another overload that does almost exactly the same thing; that overload
        /// adds the choice of whether to apply permutations across the inputs. This overload is provided as an
        /// alternative to implementing an optional parameter whose default switches the permutation
        /// behaviour off; doing it this way means that C# clients don't require a reference to 'FSharp.Core.dll'
        /// that would have been caused if we used the "?parameter" form.</remarks>
        /// <param name="sequenceOfFactoriesProvidingInputsToSynthesis">A sequence of factories whose test cases form inputs
        /// for synthesis.</param>
        /// <param name="condensation">Delegate in strongly-typed form used to synthesize the output test cases from the input
        /// test cases passed together to it as a list.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TypedFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (sequenceOfFactoriesProvidingInputsToSynthesis: #seq<TypedFactory<'TestCaseListElement>>,
                              condensation: SequenceCondensation<'TestCaseListElement, 'SynthesizedTestCase>) =
            let subtreeRootNodesFromExplicitFactories =
                sequenceOfFactoriesProvidingInputsToSynthesis
                |> List.ofSeq
                |> List.map (fun factory
                                -> (factory :?> NodeWrapper).Node)
            let fixedCombinationOfSubtreeNodesForSynthesis =
                let rec fixedCombinationOfSubtreeNodesForSynthesis subtreeRootNodes =
                    {
                        new IFixedCombinationOfSubtreeNodesForSynthesis with
                            member this.Prune =
                                Node.PruneAndCombine subtreeRootNodes
                                                     fixedCombinationOfSubtreeNodesForSynthesis

                            member this.Nodes =
                                subtreeRootNodes
                                |> Array.ofList

                            member this.FinalValueCreator () =
                                fun slicesOfFullTestVector ->
                                    let resultsFromSubtrees =
                                        List.zip subtreeRootNodes
                                                 slicesOfFullTestVector
                                        |> List.map (fun (subtreeRootNode
                                                          , sliceOfFullTestVectorCorrespondingToSubtree) ->
                                                        subtreeRootNode.FinalValueCreator () sliceOfFullTestVectorCorrespondingToSubtree)
                                    condensation.Invoke resultsFromSubtrees
                                |> mediateFinalValueCreatorType

                            member this.IsSubtreeZeroCost _ =
                                false

                            member this.IsSubtreeHiddenFromFilters _ =
                                false
                    }
                fixedCombinationOfSubtreeNodesForSynthesis subtreeRootNodesFromExplicitFactories
            let node =
                fixedCombinationOfSubtreeNodesForSynthesis
                |> SynthesizingNode
            TypedFactoryImplementation<'SynthesizedTestCase> node
            :> TypedFactory<_>

        /// <summary>Constructor function that creates an instance of TypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases, each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them a sequence and synthesises an output test case.</remarks>
        /// <remarks>This is strongly typed - and as the condensation delegate takes a single sequence that bundles up the
        /// input test cases, it also imposes homogenity of the nominal type across the input test cases used in the
        /// synthesis; this approach avoids worrying about consistency of the arity of the actual delegate implementation,
        /// trading off some flexibility of the input test case types.</remarks>
        /// <remarks>There is an option to apply a permutation to the input test cases before they are submitted to
        /// the condensation delegate, in other words, the input tests cases are reordered before being processed. The
        /// choice of permutation that is used to generate each output test case is systematically varied as another
        /// implicit test variable that contributes to the synthesis; this is dealt with by locally increasing the
        /// strength for the synthesizing factory (but not for its child factories), so that the implicit variable does
        /// not have to compete with the explicitly supplied test variable level factories for the strength guarantees
        /// documented for Factory.</remarks>
        /// <remarks>There is another overload that does almost exactly the same thing; that overload does
        /// not permit the option of applying permutations across the inputs.</remarks>
        /// <param name="sequenceOfFactoriesProvidingInputsToSynthesis">A sequence of factories whose test cases form inputs
        /// for synthesis.</param>
        /// <param name="condensation">Delegate in strongly-typed form used to synthesize the output test cases from the input
        /// test cases passed together to it as a list.</param>
        /// <param name="permuteInputs">Whether to permute the input test cases prior to submitting them to the condensation delegate.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TypedFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (sequenceOfFactoriesProvidingInputsToSynthesis: #seq<TypedFactory<'TestCaseListElement>>,
                              condensation: SequenceCondensation<'TestCaseListElement, 'SynthesizedTestCase>,
                              permuteInputs: Boolean) =
            (if permuteInputs
             then
                let intermediateFactory =
                    Synthesis.CreateWithPermutation sequenceOfFactoriesProvidingInputsToSynthesis
                let permuteAndCondense (unshuffledResultsFromSubtrees
                                        , shuffle: Permutation<'TestCaseListElement>) =
                    shuffle.Invoke unshuffledResultsFromSubtrees
                    |> condensation.Invoke

                Synthesis.Create (intermediateFactory,
                                  UnaryDelegate(permuteAndCondense))
             else
                Synthesis.Create (sequenceOfFactoriesProvidingInputsToSynthesis,
                                  condensation))
            :> TypedFactory<_>

        /// <summary>Constructor function that creates an instance of TypedFactory&lt;List&lt;'TestCaseListElement&gt; * Permutation&lt;'Something&gt;&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases, each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined into a sequence that forms
        /// an output test case. At the same time, a permutation function is created that can shuffle the output test case
        /// (or any other sequence of the same size). The idea is to allow subsequent transformation of the output test cases
        /// item-by-item followed by applying the permutations to shuffle the transformed test cases.</remarks>
        /// <remarks>The choice of permutation that is used to generate each output test case is systematically varied
        /// as another implicit test variable that contributes to the synthesis; it is therefore subject to the strength
        /// guarantees documented for Factory.</remarks>
        /// <param name="sequenceOfFactoriesProvidingInputsToSynthesis">A sequence of factories whose test cases form inputs
        /// for synthesis.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TypedFactory&lt;Tuple&lt;List&lt;'TestCaseListElement&gt;, Permutation&lt;'Something&gt;&gt;&gt;">Type of constructed factory.</seealso>
        static member CreateWithPermutation<'TestCaseListElement, 'Something> (sequenceOfFactoriesProvidingInputsToSynthesis: seq<TypedFactory<'TestCaseListElement>>): TypedFactory<List<'TestCaseListElement> * Permutation<'Something>> =
            let subtreeRootNodesFromExplicitFactories =
                sequenceOfFactoriesProvidingInputsToSynthesis
                |> List.ofSeq
                |> List.map (fun factory
                                -> (factory :?> NodeWrapper).Node)
            let fixedCombinationOfSubtreeNodesForSynthesis =
                let subtreeRootNodesIncludingImplicitFactoryForPermutation =
                    let numberOfExplicitlySuppliedFactories =
                        Seq.length sequenceOfFactoriesProvidingInputsToSynthesis
                    let numberOfPermutations =
                        BargainBasement.Factorial numberOfExplicitlySuppliedFactories
                    let additionalFactoryForPermutations =
                        TestVariable.Create [0 .. numberOfPermutations - 1]
                    [
                        yield (additionalFactoryForPermutations :?> NodeWrapper).Node
                        yield! subtreeRootNodesFromExplicitFactories
                    ]
                let rec fixedCombinationOfSubtreeNodesForSynthesis subtreeRootNodes =
                    {
                        new IFixedCombinationOfSubtreeNodesForSynthesis with
                            member this.Prune =
                                Node.PruneAndCombine subtreeRootNodes
                                                     fixedCombinationOfSubtreeNodesForSynthesis

                            member this.Nodes =
                                subtreeRootNodes
                                |> Array.ofList

                            member this.FinalValueCreator () =
                                fun slicesOfFullTestVector ->
                                    let implicitSubtreeRootNodeForPermutation
                                        :: explicitSubtreeRootNodes =
                                        subtreeRootNodes
                                    let sliceOfFullTestVectorForPermutation
                                        :: slicesOfFullTestVectorForExplicitSubtrees =
                                        slicesOfFullTestVector
                                    let permutationIndex =
                                        implicitSubtreeRootNodeForPermutation.FinalValueCreator () sliceOfFullTestVectorForPermutation
                                        |> unbox
                                    let unshuffledResultsFromSubtrees =
                                        List.zip explicitSubtreeRootNodes
                                                 slicesOfFullTestVectorForExplicitSubtrees
                                        |> List.map (fun (subtreeRootNode
                                                          , sliceOfFullTestVectorCorrespondingToSubtree) ->
                                                        subtreeRootNode.FinalValueCreator () sliceOfFullTestVectorCorrespondingToSubtree: 'TestCaseListElement)
                                    let shuffle itemsToPermute =
                                        GeneratePermutation itemsToPermute
                                                            permutationIndex
                                    unshuffledResultsFromSubtrees
                                    , Permutation<'Something>(List.ofSeq >> shuffle)
                                |> mediateFinalValueCreatorType

                            member this.IsSubtreeZeroCost subtreeIndex =
                                0 = subtreeIndex

                            member this.IsSubtreeHiddenFromFilters subtreeIndex =
                                0 = subtreeIndex
                    }
                fixedCombinationOfSubtreeNodesForSynthesis subtreeRootNodesIncludingImplicitFactoryForPermutation

            let node =
                fixedCombinationOfSubtreeNodesForSynthesis
                |> SynthesizingNode
            TypedFactoryImplementation<List<'TestCaseListElement> * Permutation<'Something>> node
            :> TypedFactory<_>