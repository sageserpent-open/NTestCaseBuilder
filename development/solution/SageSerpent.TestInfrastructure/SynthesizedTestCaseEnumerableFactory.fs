namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    open Microsoft.FSharp.Core
    open SageSerpent.Infrastructure
    open SageSerpent.Infrastructure.OptionWorkflow
    open BargainBasement
    
    /// <summary>Low-level F#-specific API for collecting together child factories to construct a synthesized TestCaseEnumerableFactory.</summary>
    /// <seealso cref="SynthesizedTestCaseEnumerableFactory">Higher-level facade API that encapsulates usage of this class: try this first.</seealso>
    /// <seealso cref="FixedCombinationOfFactoriesForSynthesis">Cooperating class from low-level F#-specific API</seealso>
    type SynthesisInputs<'SynthesisFunction, 'SynthesizedTestCase> =
        {
            Prune: unit -> Option<SynthesisInputs<'SynthesisFunction, 'SynthesizedTestCase>>
            ContinuationToApplyResultsFromAllButRightmostFactory: 'SynthesisFunction -> List<FullTestVector> -> 'SynthesizedTestCase
            NodesInRightToLeftOrder: List<Node>
        }
        
        static member StartWithLeftmostFactory (leftmostFactory: TypedTestCaseEnumerableFactory<'TestCaseFromLeftmostFactory>) =
            let nodeFromLeftmostFactory =
                leftmostFactory.Node
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
                                            rightmostFactory: TypedTestCaseEnumerableFactory<'TestCaseFromRightmostFactory>) =
            let nodeFromRightmostFactory =
                rightmostFactory.Node
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
            
    /// <summary>Low-level F#-specific API for bundling together a collection of child factories and a synthesis function to construct a synthesized TestCaseEnumerableFactory.</summary>
    /// <seealso cref="SynthesizedTestCaseEnumerableFactory">Higher-level facade API that encapsulates usage of this class: try this first.</seealso>
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
                
            member this.FinalValueCreator (): List<FullTestVector> -> 'CallerViewOfSynthesizedTestCase =
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
                            // Plan B: if the caller wants the synthesized test case typed other than 'SynthesizedTestCase'
                            // then let's do a conversion via 'Object'. In practice the desired type will be 'Object' and
                            // the unbox operation will be trivial.
                
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
                
    type SynthesizedTestCaseEnumerableFactory =
        /// <summary>Constructor function that creates an instance of TestCaseEnumerableFactory.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <remarks>The synthesis delegate is passed in weakly-typed form to allow synthesis functions of various
        /// arities and type signatures to be used with the same construction function: consistency is checked at runtime;
        /// any violations result in an exception being thrown. This approach is consistent with the use of the weakly-typed
        /// factory API of TestCaseEnumerableFactory used for the child factories.</remarks>
        /// <param name="sequenceOfFactoriesProvidingInputsToSynthesis">A sequence of factories whose test cases form inputs
        /// for synthesis.</param>
        /// <param name="synthesisDelegate">Delegate in weakly-typed form used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TestCaseEnumerableFactory">Type of constructed factory.</seealso>
        static member Create (sequenceOfFactoriesProvidingInputsToSynthesis: seq<TestCaseEnumerableFactory>,
                              synthesisDelegate: Delegate) =
            if Seq.isEmpty sequenceOfFactoriesProvidingInputsToSynthesis
            then
                raise (PreconditionViolationException "Must provide at least one component.")  
            let node =
                let subtreeRootNodes =
                    sequenceOfFactoriesProvidingInputsToSynthesis
                    |> List.ofSeq
                    |> List.map (fun factory
                                    -> factory.Node)
                Node.CreateSynthesizingNode subtreeRootNodes
                                            synthesisDelegate
            TypedTestCaseEnumerableFactory<_> node
            :> TestCaseEnumerableFactory
            
        /// <summary>Constructor function that creates an instance of TypedTestCaseEnumerableFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a curried synthesis function
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <param name="fixedCombinationOfFactoriesForSynthesis">Parameter object from low-level F#-specific API that bundles together the child factories and synthesis function.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TypedTestCaseEnumerableFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        /// <seealso cref="FixedCombinationOfFactoriesForSynthesis">Cooperating class from low-level F#-specific API.</seealso>
        static member inline Create (fixedCombinationOfFactoriesForSynthesis: FixedCombinationOfFactoriesForSynthesis<_, 'SynthesizedTestCase>) =
            TypedTestCaseEnumerableFactory<'SynthesizedTestCase> (fixedCombinationOfFactoriesForSynthesis
                                                                  :> IFixedCombinationOfSubtreeNodesForSynthesis
                                                                  |> SynthesizingNode)
   
        // TODO: re-implement with special-case implementations of 'IFixedCombinationOfSubtreeNodesForSynthesis';
        // we don't need the full API using 'SynthesisInputs<_, _>' to do these tuple cases.
    
        /// <summary>Constructor function that creates an instance of TypedTestCaseEnumerableFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of an input test case taken from the sequence yielded by the the child factory used to construct the factory.
        /// The input test case is converted by means of a delegate that takes it as a parameter and synthesises an output test case.</remarks>
        /// <param name="synthesisDelegate">Delegate used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TypedTestCaseEnumerableFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (factory: TypedTestCaseEnumerableFactory<_>,
                              synthesisDelegate: UnaryDelegate<_, 'SynthesizedTestCase>) =
            let singletonCombinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.StartWithLeftmostFactory factory
            let fixedCombinationOfFactoriesForSynthesis =
                FixedCombinationOfFactoriesForSynthesis (singletonCombinationOfFactoriesForSynthesis
                                                         , synthesisDelegate.Invoke)
            SynthesizedTestCaseEnumerableFactory.Create fixedCombinationOfFactoriesForSynthesis
                                       
        /// <summary>Constructor function that creates an instance of TypedTestCaseEnumerableFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <param name="synthesisDelegate">Delegate used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TypedTestCaseEnumerableFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (factoryOne: TypedTestCaseEnumerableFactory<_>,
                              factoryTwo: TypedTestCaseEnumerableFactory<_>,
                              synthesisDelegate: BinaryDelegate<_, _, _>) =
            let singletonCombinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.StartWithLeftmostFactory factoryOne
            let combinationOfFactoriesForSynthesis =
                SynthesisInputs<_, _>.AddFactoryToTheRight (singletonCombinationOfFactoriesForSynthesis,
                                                                                           factoryTwo)
            let fixedCombinationOfFactoriesForSynthesis =
                FixedCombinationOfFactoriesForSynthesis (combinationOfFactoriesForSynthesis
                                                         , FuncConvert.FuncFromTupled<_, _, 'SynthesizedTestCase> synthesisDelegate.Invoke)
            SynthesizedTestCaseEnumerableFactory.Create fixedCombinationOfFactoriesForSynthesis
                                       
        /// <summary>Constructor function that creates an instance of TypedTestCaseEnumerableFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <param name="synthesisDelegate">Delegate used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TypedTestCaseEnumerableFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (factoryOne: TypedTestCaseEnumerableFactory<_>,
                              factoryTwo: TypedTestCaseEnumerableFactory<_>,
                              factoryThree: TypedTestCaseEnumerableFactory<_>,
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
            SynthesizedTestCaseEnumerableFactory.Create fixedCombinationOfFactoriesForSynthesis
            
        /// <summary>Constructor function that creates an instance of TypedTestCaseEnumerableFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <param name="synthesisDelegate">Delegate used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TypedTestCaseEnumerableFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (factoryOne: TypedTestCaseEnumerableFactory<_>,
                              factoryTwo: TypedTestCaseEnumerableFactory<_>,
                              factoryThree: TypedTestCaseEnumerableFactory<_>,
                              factoryFour: TypedTestCaseEnumerableFactory<_>,
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
            SynthesizedTestCaseEnumerableFactory.Create fixedCombinationOfFactoriesForSynthesis

        /// <summary>Constructor function that creates an instance of TypedTestCaseEnumerableFactory&lt;'SynthesizedTestCase&gt;.</summary>
        /// <remarks>The resulting factory yields a sequence of output test cases each of which is synthesized
        /// out of a combination of input test cases taken from across the sequences yielded by the the child
        /// factories used to construct the factory. The input test cases are combined by means of a delegate
        /// that takes them as parameters and synthesises an output test case.</remarks>
        /// <param name="synthesisDelegate">Delegate used to synthesize the output test cases.</param>
        /// <returns>The constructed factory.</returns>
        /// <seealso cref="TypedTestCaseEnumerableFactory&lt;'SynthesizedTestCase&gt;">Type of constructed factory.</seealso>
        static member Create (factoryOne: TypedTestCaseEnumerableFactory<_>,
                              factoryTwo: TypedTestCaseEnumerableFactory<_>,
                              factoryThree: TypedTestCaseEnumerableFactory<_>,
                              factoryFour: TypedTestCaseEnumerableFactory<_>,
                              factoryFive: TypedTestCaseEnumerableFactory<_>,
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
            SynthesizedTestCaseEnumerableFactory.Create fixedCombinationOfFactoriesForSynthesis
