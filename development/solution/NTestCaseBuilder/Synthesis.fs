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

        let extractNodeFrom (factory: IFactory) =
            (factory :?> INodeWrapper).Node

    open SynthesisDetail

    /// <summary>Low-level F#-specific API for collecting together child factories to construct a synthesized IFactory.</summary>
    /// <seealso cref="Synthesis">Higher-level facade API that encapsulates usage of this class: try this first.</seealso>
    /// <seealso cref="FixedCombinationOfFactoriesForSynthesis">Cooperating class from low-level F#-specific API</seealso>
    type SynthesisInputs<'SynthesisFunction, 'SynthesizedTestCase> =
        {
            ApplyResultsFromAllFactories: 'SynthesisFunction -> List<FullTestVector> -> List<Node> -> 'SynthesizedTestCase * List<FullTestVector>
            NodesInRightToLeftOrder: List<Node>
        }

        static member StartWithLeftmostFactory (leftmostFactory: ITypedFactory<'TestCaseFromThisFactory>) =
            let nodeFromLeftmostFactory =
                extractNodeFrom leftmostFactory
            let rec createSingletonCombination (nodeFromLeftmostFactory: Node) =
                {
                    ApplyResultsFromAllFactories =
                        fun synthesisFunction
                            slicesOfFullTestVector
                            nodesInRightToLeftOrder ->
                                match slicesOfFullTestVector
                                      , nodesInRightToLeftOrder with
                                    sliceOfFullTestVectorForThisFactory :: slicesOfFullTestVectorForFactoriesToTheRight
                                    , [ nodeFromThisFactory] ->
                                        (nodeFromThisFactory.FinalValueCreator () sliceOfFullTestVectorForThisFactory: 'TestCaseFromThisFactory)
                                        |> synthesisFunction
                                        , slicesOfFullTestVectorForFactoriesToTheRight
                                  | _ ->
                                        raise (PreconditionViolationException "Must have at least one slice of the full test vector and exactly one node to create a value from it.")
                    NodesInRightToLeftOrder =
                        [ nodeFromLeftmostFactory ]
                }
            createSingletonCombination nodeFromLeftmostFactory

        static member AddFactoryToTheRight (combinationOfAllOtherFactories,
                                            rightmostFactory: ITypedFactory<'TestCaseFromThisFactory>) =
            let nodeFromRightmostFactory =
                extractNodeFrom rightmostFactory
            let rec createCombinationWithExtraRightmostNode (nodeFromRightmostFactory: Node)
                                                             combinationOfAllOtherFactories =
                {
                    ApplyResultsFromAllFactories =
                        fun synthesisFunction
                            slicesOfFullTestVector
                            nodesInRightToLeftOrder ->
                                match nodesInRightToLeftOrder with
                                    nodeFromThisFactory :: nodesFromFactoriesToTheLeftInRightToLeftOrder ->
                                        let synthesisFunctionPartiallyAppliedToResultsFromAllFactoriesToTheLeft
                                            , remainingSlicesOfFullTestVectorForThisAndFactoriesToTheRight =
                                            combinationOfAllOtherFactories.ApplyResultsFromAllFactories synthesisFunction
                                                                                                                                slicesOfFullTestVector
                                                                                                                                nodesFromFactoriesToTheLeftInRightToLeftOrder

                                        match remainingSlicesOfFullTestVectorForThisAndFactoriesToTheRight with
                                            sliceOfFullTestVectorForThisFactory :: slicesOfFullTestVectorForFactoriesToTheRight ->
                                                (nodeFromThisFactory.FinalValueCreator () sliceOfFullTestVectorForThisFactory: 'TestCaseFromThisFactory)
                                                |> synthesisFunctionPartiallyAppliedToResultsFromAllFactoriesToTheLeft
                                                , slicesOfFullTestVectorForFactoriesToTheRight
                                          | _ ->
                                                raise (PreconditionViolationException "Missing at least one slice of the full test vector on the right.")
                                  | _ ->
                                        raise (PreconditionViolationException "Missing at least one node on the right.")
                    NodesInRightToLeftOrder =
                        nodeFromRightmostFactory :: combinationOfAllOtherFactories.NodesInRightToLeftOrder
                }
            createCombinationWithExtraRightmostNode nodeFromRightmostFactory
                                                    combinationOfAllOtherFactories

    /// <summary>Low-level F#-specific API for bundling together a collection of child factories and a synthesis function to construct a synthesized IFactory.</summary>
    /// <seealso cref="Synthesis">Higher-level facade API that encapsulates usage of this class: try this first.</seealso>
    /// <seealso cref="SynthesisInputs">Cooperating class from low-level F#-specific API</seealso>
    and FixedCombinationOfFactoriesForSynthesis<'SynthesisFunction, 'SynthesizedTestCase>
            (nodesInRightToLeftOrder: List<Node>,
             applyResultsFromAllFactories,
             synthesisFunction) =

        let createFinalValueFrom fullTestVector =
            applyResultsFromAllFactories synthesisFunction
                                         fullTestVector
                                         nodesInRightToLeftOrder
            |> fst

        new (heterogenousCombinationOfFactoriesForSynthesis: SynthesisInputs<'SynthesisFunction, 'SynthesizedTestCase>,
             synthesisFunction) =
             FixedCombinationOfFactoriesForSynthesis(heterogenousCombinationOfFactoriesForSynthesis.NodesInRightToLeftOrder,
                                                     heterogenousCombinationOfFactoriesForSynthesis.ApplyResultsFromAllFactories,
                                                     synthesisFunction)

        interface IFixedCombinationOfSubtreeNodesForSynthesis with
            member this.Prune (deferralBudget,
                               numberOfDeferralsSpent) =
                Node.PruneAndCombine (nodesInRightToLeftOrder
                                      |> List.rev)
                                     (fun prunedNodeList ->
                                        FixedCombinationOfFactoriesForSynthesis((prunedNodeList
                                                                                 |> List.rev),
                                                                                applyResultsFromAllFactories,
                                                                                synthesisFunction)
                                        :> IFixedCombinationOfSubtreeNodesForSynthesis)
                                     deferralBudget
                                     numberOfDeferralsSpent

            member this.Nodes =
                nodesInRightToLeftOrder
                |> List.rev
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
        /// <summary>Constructor function that creates an instance of IFactory.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <remarks>The synthesis delegate is passed in weakly-typed form to allow synthesis functions of various
        /// arities and type signatures to be used with the same construction function: consistency is checked at runtime;
        /// any violations result in an exception being thrown. This approach is consistent with the use of the weakly-typed
        /// factory API of IFactory used for the child factories.</remarks>
        /// <param name="sequenceOfFactoriesProvidingInputsToSynthesis">A sequence of factories whose test cases form inputs
        /// for synthesis.</param>
        /// <param name="synthesisDelegate">Delegate in weakly-typed form used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="IFactory">Type of constructed factory.</seealso>
        static member Create (sequenceOfFactoriesProvidingInputsToSynthesis: #seq<IFactory>,
                              synthesisDelegate: Delegate) =
            if Seq.isEmpty sequenceOfFactoriesProvidingInputsToSynthesis
            then
                raise (PreconditionViolationException "Must provide at least one component.")
            let node =
                let subtreeRootNodes =
                    sequenceOfFactoriesProvidingInputsToSynthesis
                    |> List.ofSeq
                    |> List.map extractNodeFrom
                Node.CreateSynthesizingNode subtreeRootNodes
                                            synthesisDelegate
            TypedFactoryImplementation<_> node
            :> IFactory

        /// <summary>Constructor function that creates an instance of ITypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a curried synthesis function
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <param name="fixedCombinationOfFactoriesForSynthesis">Parameter object from low-level F#-specific API that bundles together the child factories and synthesis function.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="ITypedFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        /// <seealso cref="FixedCombinationOfFactoriesForSynthesis">Cooperating class from low-level F#-specific API.</seealso>
        static member inline Create (fixedCombinationOfFactoriesForSynthesis: IFixedCombinationOfSubtreeNodesForSynthesis) =
            TypedFactoryImplementation<'SynthesizedTestCase> (fixedCombinationOfFactoriesForSynthesis
                                                              |> SynthesizingNode)
            :> ITypedFactory<_>

        // TODO: re-implement with special-case implementations of 'IFixedCombinationOfSubtreeNodesForSynthesis';
        // we don't need the full API using 'SynthesisInputs<_, _>' to do these tuple cases.

        /// <summary>Constructor function that creates an instance of ITypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of an input test case taken from the sequence yielded by the the child factory used to construct the factory.
        /// The input test case is converted by means of a delegate that takes it as a parameter and synthesises an output test case.</remarks>
        /// <param name="synthesisDelegate">Delegate used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="ITypedFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (factory: ITypedFactory<_>,
                              synthesisDelegate: UnaryDelegate<_, 'SynthesizedTestCase>) =
            let singletonCombinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.StartWithLeftmostFactory factory
            let fixedCombinationOfFactoriesForSynthesis =
                FixedCombinationOfFactoriesForSynthesis (singletonCombinationOfFactoriesForSynthesis
                                                         , synthesisDelegate.Invoke)
            Synthesis.Create fixedCombinationOfFactoriesForSynthesis
            : ITypedFactory<'SynthesizedTestCase>

        /// <summary>Constructor function that creates an instance of ITypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <param name="synthesisDelegate">Delegate used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="ITypedFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (factoryOne: ITypedFactory<'InputTestCase1>,
                              factoryTwo: ITypedFactory<'InputTestCase2>,
                              synthesisDelegate: BinaryDelegate<'InputTestCase1, 'InputTestCase2, 'SynthesizedTestCase>) =
            let rec fixedCombinationOfSubtreeNodesForSynthesis node1
                                                               node2 =
                {
                    new IFixedCombinationOfSubtreeNodesForSynthesis with
                        member this.Prune (deferralBudget,
                                           numberOfDeferralsSpent) =
                            Node.PruneAndCombine [node1; node2]
                                                 (function [node1; node2] ->
                                                            fixedCombinationOfSubtreeNodesForSynthesis node1
                                                                                                       node2)
                                                 deferralBudget
                                                 numberOfDeferralsSpent

                        member this.Nodes =
                            [|node1; node2|]

                        member this.FinalValueCreator () =
                            function [sliceOfFullTestVector1; sliceOfFullTestVector2] ->
                                    let invocationArgument1 =
                                        node1.FinalValueCreator () sliceOfFullTestVector1
                                    let invocationArgument2 =
                                        node2.FinalValueCreator () sliceOfFullTestVector2
                                    synthesisDelegate.Invoke(invocationArgument1, invocationArgument2)
                            |> mediateFinalValueCreatorType
                        member this.IsSubtreeZeroCost _ =
                            false

                        member this.IsSubtreeHiddenFromFilters _ =
                            false
                }
            let fixedCombinationsOfNodesForSynthesis =
                fixedCombinationOfSubtreeNodesForSynthesis (extractNodeFrom factoryOne)
                                                           (extractNodeFrom factoryTwo)
            Synthesis.Create fixedCombinationsOfNodesForSynthesis
            : ITypedFactory<'SynthesizedTestCase>

        /// <summary>Constructor function that creates an instance of ITypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <param name="synthesisDelegate">Delegate used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="ITypedFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (factoryOne: ITypedFactory<_>,
                              factoryTwo: ITypedFactory<_>,
                              factoryThree: ITypedFactory<_>,
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
            : ITypedFactory<'SynthesizedTestCase>

        /// <summary>Constructor function that creates an instance of ITypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <param name="synthesisDelegate">Delegate used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="ITypedFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (factoryOne: ITypedFactory<_>,
                              factoryTwo: ITypedFactory<_>,
                              factoryThree: ITypedFactory<_>,
                              factoryFour: ITypedFactory<_>,
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
            : ITypedFactory<'SynthesizedTestCase>

        /// <summary>Constructor function that creates an instance of ITypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <param name="synthesisDelegate">Delegate used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="ITypedFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (factoryOne: ITypedFactory<_>,
                              factoryTwo: ITypedFactory<_>,
                              factoryThree: ITypedFactory<_>,
                              factoryFour: ITypedFactory<_>,
                              factoryFive: ITypedFactory<_>,
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
            : ITypedFactory<'SynthesizedTestCase>

        /// <summary>Constructor function that creates an instance of ITypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
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
        /// <seealso cref="ITypedFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (sequenceOfFactoriesProvidingInputsToSynthesis: #seq<ITypedFactory<'TestCaseListElement>>,
                              condensation: SequenceCondensation<'TestCaseListElement, 'SynthesizedTestCase>) =
            let subtreeRootNodesFromExplicitFactories =
                sequenceOfFactoriesProvidingInputsToSynthesis
                |> List.ofSeq
                |> List.map extractNodeFrom
            let fixedCombinationOfSubtreeNodesForSynthesis =
                let rec fixedCombinationOfSubtreeNodesForSynthesis subtreeRootNodes =
                    {
                        new IFixedCombinationOfSubtreeNodesForSynthesis with
                            member this.Prune (deferralBudget,
                                               numberOfDeferralsSpent) =
                                Node.PruneAndCombine subtreeRootNodes
                                                     fixedCombinationOfSubtreeNodesForSynthesis
                                                     deferralBudget
                                                     numberOfDeferralsSpent

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
            Synthesis.Create fixedCombinationOfSubtreeNodesForSynthesis
            : ITypedFactory<'SynthesizedTestCase>

        /// <summary>Constructor function that creates an instance of ITypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases, each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. An output test case is the concatenation of the input
        /// test cases it is synthesized from.</remarks>
        /// <remarks>This is strongly typed - and as the output is the concatenation of input test cases,
        /// it also imposes homogenity of the nominal type across the input test cases used in the
        /// synthesis.</remarks>
        /// <remarks>There is another overload that does almost exactly the same thing; that overload
        /// adds the choice of whether to apply permutations across the inputs. This overload is provided as an
        /// alternative to implementing an optional parameter whose default switches the permutation
        /// behaviour off; doing it this way means that C# clients don't require a reference to 'FSharp.Core.dll'
        /// that would have been caused if we used the "?parameter" form.</remarks>
        /// <param name="sequenceOfFactoriesProvidingInputsToSynthesis">A sequence of factories whose test cases are concatenated.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="ITypedFactory&lt;'TestCaseListElement&gt;">Type of constructed factory.</seealso>
        static member Create (sequenceOfFactoriesProvidingInputsToSynthesis: #seq<ITypedFactory<'TestCaseListElement>>) =
            Synthesis.Create (sequenceOfFactoriesProvidingInputsToSynthesis,
                              SequenceCondensation(BargainBasement.Identity))

        /// <summary>Constructor function that creates an instance of ITypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
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
        /// documented for IFactory.</remarks>
        /// <remarks>There is another overload that does almost exactly the same thing; that overload does
        /// not permit the option of applying permutations across the inputs.</remarks>
        /// <param name="sequenceOfFactoriesProvidingInputsToSynthesis">A sequence of factories whose test cases form inputs
        /// for synthesis.</param>
        /// <param name="condensation">Delegate in strongly-typed form used to synthesize the output test cases from the input
        /// test cases passed together to it as a list.</param>
        /// <param name="permuteInputs">Whether to permute the input test cases prior to submitting them to the condensation delegate.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="ITypedFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (sequenceOfFactoriesProvidingInputsToSynthesis: #seq<ITypedFactory<'TestCaseListElement>>,
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

        /// <summary>Constructor function that creates an instance of ITypedFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases, each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. An output test case is the concatenation of the input
        /// test cases it is synthesized from.</remarks>
        /// <remarks>This is strongly typed - and as the output is the concatenation of input test cases,
        /// it also imposes homogenity of the nominal type across the input test cases used in the
        /// synthesis.</remarks>
        /// <remarks>There is an option to apply a permutation to the input test cases before they are concatenated,
        /// in other words, the input tests cases are reordered before being processed. The choice of permutation
        /// that is used to generate each output test case is systematically varied as another implicit test variable
        /// that contributes to the synthesis; this is dealt with by locally increasing the strength for the
        /// synthesizing factory (but not for its child factories), so that the implicit variable does not have to
        /// compete with the explicitly supplied test variable level factories for the strength guarantees
        /// documented for IFactory.</remarks>
        /// <remarks>There is another overload that does almost exactly the same thing; that overload does
        /// not permit the option of applying permutations across the inputs.</remarks>
        /// <param name="sequenceOfFactoriesProvidingInputsToSynthesis">A sequence of factories whose test cases are concatenated.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="ITypedFactory&lt;'TestCaseListElement&gt;">Type of constructed factory.</seealso>
        static member Create (sequenceOfFactoriesProvidingInputsToSynthesis: #seq<ITypedFactory<'TestCaseListElement>>,
                              permuteInputs: Boolean) =
            Synthesis.Create (sequenceOfFactoriesProvidingInputsToSynthesis,
                              SequenceCondensation(BargainBasement.Identity),
                              permuteInputs)

        /// <summary>Constructor function that creates an instance of ITypedFactory&lt;List&lt;'TestCaseListElement&gt; * Permutation&lt;'Something&gt;&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases, each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined into a sequence that forms
        /// an output test case. At the same time, a permutation function is created that can shuffle the output test case
        /// (or any other sequence of the same size). The idea is to allow subsequent transformation of the output test cases
        /// item-by-item followed by applying the permutations to shuffle the transformed test cases.</remarks>
        /// <remarks>The choice of permutation that is used to generate each output test case is systematically varied
        /// as another implicit test variable that contributes to the synthesis; it is therefore subject to the strength
        /// guarantees documented for IFactory.</remarks>
        /// <param name="sequenceOfFactoriesProvidingInputsToSynthesis">A sequence of factories whose test cases form inputs
        /// for synthesis.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="ITypedFactory&lt;Tuple&lt;List&lt;'TestCaseListElement&gt;, Permutation&lt;'Something&gt;&gt;&gt;">Type of constructed factory.</seealso>
        static member CreateWithPermutation<'TestCaseListElement, 'Something> (sequenceOfFactoriesProvidingInputsToSynthesis: seq<ITypedFactory<'TestCaseListElement>>): ITypedFactory<List<'TestCaseListElement> * Permutation<'Something>> =
            let subtreeRootNodesFromExplicitFactories =
                sequenceOfFactoriesProvidingInputsToSynthesis
                |> List.ofSeq
                |> List.map extractNodeFrom
            let fixedCombinationOfSubtreeNodesForSynthesis =
                let subtreeRootNodesIncludingImplicitFactoryForPermutation =
                    let numberOfExplicitlySuppliedFactories =
                        Seq.length sequenceOfFactoriesProvidingInputsToSynthesis
                    let numberOfPermutations =
                        BargainBasement.Factorial numberOfExplicitlySuppliedFactories
                    let additionalFactoryForPermutations =
                        TestVariable.Create [0 .. numberOfPermutations - 1]
                    [
                        yield extractNodeFrom additionalFactoryForPermutations
                        yield! subtreeRootNodesFromExplicitFactories
                    ]
                let rec fixedCombinationOfSubtreeNodesForSynthesis subtreeRootNodes =
                    {
                        new IFixedCombinationOfSubtreeNodesForSynthesis with
                            member this.Prune (deferralBudget,
                                               numberOfDeferralsSpent) =
                                Node.PruneAndCombine subtreeRootNodes
                                                     fixedCombinationOfSubtreeNodesForSynthesis
                                                     deferralBudget
                                                     numberOfDeferralsSpent

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

            Synthesis.Create fixedCombinationOfSubtreeNodesForSynthesis
            : ITypedFactory<List<'TestCaseListElement> * Permutation<'Something>>