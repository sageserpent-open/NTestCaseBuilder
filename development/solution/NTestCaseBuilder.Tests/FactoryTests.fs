namespace NTestCaseBuilder.Tests

    open NUnit.Framework

    open SageSerpent.Infrastructure
    open SageSerpent.Infrastructure.ListExtensions
    open SageSerpent.Infrastructure.RandomExtensions
    open SageSerpent.Infrastructure.IEnumerableExtensions
    open SageSerpent.Infrastructure.OptionWorkflow
    open SageSerpent.Infrastructure.OptionExtensions
    open NTestCaseBuilder
    open System
    open System.Windows.Forms
    open System.Collections.Generic
    open System.Text.RegularExpressions
    open System.Linq
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Linq.QuotationEvaluation
    open Microsoft.FSharp.Reflection
    open System.Reflection.Emit
    open System.Reflection

    type TestVariableLevel =
        Int32 * Option<Int32> // Test variable index, optional distinguishing index for value.

    module CodeGeneration =
        let private ModuleBuilder =
            let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly (AssemblyName "SageSerpent.GeneratedAtRuntime",
                                                                                 AssemblyBuilderAccess.Run)
            assemblyBuilder.DefineDynamicModule "ModuleForCodeGeneratedAtRuntime"

        let NAryDelegateTypeBuilder<'X> (arity: Int32) =
            let typeOfItemsBeingCondensed =
                typeof<'X>

            let arityAsString =
                arity.ToString ()

            let typeAttributes =
                TypeAttributes.Class
                ||| TypeAttributes.Public
                ||| TypeAttributes.Sealed
                ||| TypeAttributes.AnsiClass
                ||| TypeAttributes.AutoClass

            let typeBuilder =
                ModuleBuilder.DefineType ("TupleCondensationDelegateTypeFor" + arityAsString + "ArgumentsOfType" + typeOfItemsBeingCondensed.Name,
                                          typeAttributes)

            typeBuilder.SetParent (typeof<MulticastDelegate>)

            let constructorArgumentTypes =
                [| typeof<Object>; typeof<IntPtr> |]

            let constructorAttributes =
                MethodAttributes.RTSpecialName
                ||| MethodAttributes.HideBySig
                ||| MethodAttributes.Public

            let constructorBuilder =
                typeBuilder.DefineConstructor (constructorAttributes,
                                               CallingConventions.Standard,
                                               constructorArgumentTypes)

            constructorBuilder.SetImplementationFlags (MethodImplAttributes.Runtime
                                                       ||| MethodImplAttributes.IL
                                                       ||| MethodImplAttributes.Managed)


            let invokeMethodArgumentTypes =
                Array.init (int arity) (fun _ -> typeOfItemsBeingCondensed)

            let invokeMethodAttributes =
                MethodAttributes.Public
                ||| MethodAttributes.HideBySig
                ||| MethodAttributes.NewSlot
                ||| MethodAttributes.Virtual

            let invokeMethodName = "Invoke"

            let invokeMethodBuilder =
                typeBuilder.DefineMethod (invokeMethodName,
                                          invokeMethodAttributes,
                                          typeOfItemsBeingCondensed,
                                          invokeMethodArgumentTypes)

            invokeMethodBuilder.SetImplementationFlags (MethodImplAttributes.Runtime
                                                        ||| MethodImplAttributes.IL
                                                        ||| MethodImplAttributes.Managed)

            typeBuilder.CreateType ()

        let NAryCondensationDelegateBuilder arity
                                            delegateTypeBuilder
                                            (listCondensation: list<'X> -> 'X) =
            let typeOfItemsBeingCondensed =
                typeof<'X>

            let arguments = [for index in 1 .. arity do
                                yield Var ("argument" + index.ToString ()
                                           , typeOfItemsBeingCondensed
                                           , false)]

            let listConsConstructor =
                match <@ Unchecked.defaultof<'X> :: [] @> with
                    NewUnionCase (unionCaseConstructor
                                  , _)
                        -> unionCaseConstructor
                   | _
                        -> raise (PreconditionViolationException "This case should not occur.")

            let listEmptyConstructor =
                match <@ []: list<'X> @> with
                    NewUnionCase (unionCaseConstructor
                                  , _)
                        -> unionCaseConstructor
                   | _
                        -> raise (PreconditionViolationException "This case should not occur.")

            let expressionForLadderSequenceBuildingUpList =
                List.foldBack (fun argument
                                   expressionForLadderSequenceBuildingUpList ->
                                    let argumentExpression =
                                        Expr.Var argument
                                    Expr.NewUnionCase (listConsConstructor
                                                       , [argumentExpression; expressionForLadderSequenceBuildingUpList]))
                              arguments
                              (Expr.NewUnionCase (listEmptyConstructor
                                                  , []))

            let condensationAppliedToList = Expr.Application (<@ listCondensation @>
                                                              , expressionForLadderSequenceBuildingUpList)

            let delegateType = delegateTypeBuilder arity

            let expressionForDelegate = Expr.NewDelegate (delegateType
                                                          , arguments
                                                          , condensationAppliedToList)

            expressionForDelegate.CompileUntyped () ():?> Delegate  // Need to apply this one more time, because the underlying expression
                                                                    // actually *creates* a delegate: remember, functional languages *evaluate*
                                                                    // function definitions.

    module StuffForFsCheckAdaptedExample =
        type Tree = Leaf of Int32 | Branch of Tree * Tree

    open StuffForFsCheckAdaptedExample

    type FactoryConstructors<'Factory, 'TestCase> =
        {
            FactoryWithFilterFrom: 'Factory -> LevelCombinationFilter -> 'Factory
            TestVariableFrom: List<'TestCase> -> 'Factory
            SingletonFrom: 'TestCase -> 'Factory
            SynthesisFrom: 'Factory [] -> (List<'TestCase> -> 'TestCase) -> Boolean -> 'Factory
            InterleavingFrom: List<'Factory> -> 'Factory
            DeferralFrom: 'Factory -> 'Factory
            ApplyTagTo: 'Factory -> Int32 -> 'Factory
        }

    type PermutationCreation =
        No
      | Yes
      | Randomly


    [<TestFixture>]
    type FactoryTestFixture () =
        let maximumCombinationStrength = 5
        let maximumNumberOfNonZeroCombinationStrengthSubtrees = 4
        let maximumNumberOfTestLevels = 3
        let maximumNumberOfAncestorFactoriesAllowingFairChanceOfInterleaving = 2;
        let maximumNumberOfDeferrals = 5

        let delegateTypeBuilder =
            BargainBasement.Memoize (CodeGeneration.NAryDelegateTypeBuilder<List<TestVariableLevel>>)

        let weaklyTypedFactoryConstructors =
            {
                FactoryWithFilterFrom =
                    (fun (factory: IFactory)
                         filter ->
                         factory.WithFilter filter)
                TestVariableFrom =
                    (fun testVariableLevels ->
                        TestVariable.Create testVariableLevels
                        :> IFactory)
                SingletonFrom =
                    (fun testVariableLevel ->
                        Singleton.Create testVariableLevel
                        :> IFactory)
                SynthesisFrom =
                    (fun permutedSubtrees
                         undoShuffleAndConcatenateContributedLevels
                         permuteInputs ->
                        if permuteInputs
                        then
                            raise (LogicErrorException "Can't permute inputs with the weakly-typed API - this indicates a bug in the test's logic.")
                        let nAryCondensationDelegate =
                            CodeGeneration.NAryCondensationDelegateBuilder permutedSubtrees.Length
                                                                           delegateTypeBuilder
                                                                           (undoShuffleAndConcatenateContributedLevels: List<List<TestVariableLevel>> -> List<TestVariableLevel>)

                        Synthesis.Create (permutedSubtrees,
                                          nAryCondensationDelegate))
                InterleavingFrom =
                    (fun subtrees ->
                        Interleaving.Create subtrees)
                DeferralFrom =
                    (fun factory ->
                        Deferral.Create (fun () ->
                                            factory))
                ApplyTagTo =
                    (fun factory
                         tag ->
                        factory.WithTag tag)
            }

        let stronglyTypedFactoryConstructors =
            {
                FactoryWithFilterFrom =
                    (fun (factory: ITypedFactory<_>)
                         filter ->
                         factory.WithFilter filter)
                TestVariableFrom =
                    (fun testVariableLevels ->
                        TestVariable.Create testVariableLevels)
                SingletonFrom =
                    (fun testVariableLevel ->
                        Singleton.Create testVariableLevel)
                SynthesisFrom =
                    (fun permutedSubtrees
                         undoShuffleAndConcatenateContributedLevels
                         permuteInputs ->
                        match Array.length permutedSubtrees with
                            1 when not permuteInputs ->
                                Synthesis.Create (permutedSubtrees.[0],
                                                  Func<_, _>(fun inputTestCase ->
                                                                undoShuffleAndConcatenateContributedLevels [inputTestCase]))
                          | 2 when not permuteInputs ->
                                Synthesis.Create (permutedSubtrees.[0],
                                                  permutedSubtrees.[1],
                                                  Func<_, _, _>(fun firstInputTestCase
                                                                    secondInputTestCase ->
                                                                        undoShuffleAndConcatenateContributedLevels [firstInputTestCase; secondInputTestCase]))
                          | 3 when not permuteInputs ->
                                let trivialCombinationOfFactoriesForSynthesis =
                                    SynthesisInputs<_, _>.StartWithLeftmostFactory permutedSubtrees.[0]
                                let combinationOfFactoriesForSynthesis =
                                    SynthesisInputs<_, _>.AddFactoryToTheRight(trivialCombinationOfFactoriesForSynthesis,
                                                                               permutedSubtrees.[1])
                                let combinationOfFactoriesForSynthesis =
                                    SynthesisInputs<_, _>.AddFactoryToTheRight(combinationOfFactoriesForSynthesis,
                                                                               permutedSubtrees.[2])
                                let fixedCombinationOfFactoriesForSynthesis =
                                    FixedCombinationOfFactoriesForSynthesis (combinationOfFactoriesForSynthesis
                                                                             , (fun firstInputTestCase
                                                                                    secondInputTestCase
                                                                                    thirdInputTestCase ->
                                                                                    undoShuffleAndConcatenateContributedLevels [firstInputTestCase; secondInputTestCase; thirdInputTestCase]))
                                Synthesis.Create fixedCombinationOfFactoriesForSynthesis                               
                          | _ ->
                                Synthesis.Create (permutedSubtrees,
                                                  SequenceCondensation(List.ofSeq >> undoShuffleAndConcatenateContributedLevels),
                                                                       permuteInputs))
                InterleavingFrom =
                    (fun subtrees ->
                        Interleaving.Create subtrees)
                DeferralFrom =
                    (fun factory ->
                        Deferral.Create (fun () ->
                                            factory))
                ApplyTagTo =
                    (fun factory
                         tag ->
                        factory.WithTag tag)
            }

        let testVariableEncodedIndicesPass testVariableLevelEncodedIndices
                                           numberOfTestVariablesInTargetNonSingletonTestVariableCombination =
            Seq.length testVariableLevelEncodedIndices <> numberOfTestVariablesInTargetNonSingletonTestVariableCombination
                // Can only exclude combinations that involve
                // *all* the target non-singleton test variables.
            || testVariableLevelEncodedIndices
                |> Seq.exists (fun testVariableLevelIndex ->
                                1 <> testVariableLevelIndex)    // Excluding a level index of one makes sure that there is at least one alternative
                                                                // combination of levels for the target non-singleton test variables - namely all
                                                                // the level indices being zero.

        let constructFactoryWithAccompanyingTestVariableCombinations factoryConstructors
                                                                     randomBehaviour
                                                                     allowDuplicatedLevels
                                                                     allowSynthesisToPermuteInputs
                                                                     ensureContiguousSortedTestVariableIndicesAfterPruning
                                                                     allowDeferrals =
            // NOTE: the logic that follows is written so that 'allowSynthesisToPermuteInputs' can be set to either true or false without changing
            // the fundamental structure of the generated tree (given all the other parameters are the same, including the underlying mutable state
            // of 'randomBehaviour'). The tree of factories so generated will have the same structure - the only differences being the presence or
            // absence of permutation on some of the synthesizing factories. Also note that one should not infer that the test cases generated by the
            // factories are simply related by permutation - the tests have to take that statement into account.
            let combinationStrength =
                (randomBehaviour: Random).ChooseAnyNumberFromOneTo maximumCombinationStrength
            let rec tagAndThenDecorateWithDeferralIfRequired combinationStrength
                                                             testVariableIndexToLevelsMapping
                                                             tagToPreorderDepthFirstIndexMapping
                                                             numberOfAncestorFactories
                                                             allowEmptyValueNodes
                                                             mustHavePermutingSynthesisInTree =
                let indexForLeftmostTestVariable =
                    (testVariableIndexToLevelsMapping: Map<_, _>).Count
                let taggedAsEven =
                    (0 = indexForLeftmostTestVariable % 2)
                let preorderDepthFirstIndex =
                    Map.tryFind taggedAsEven
                                tagToPreorderDepthFirstIndexMapping
                    |> BargainBasement.Flip defaultArg
                                            0
                let tag =
                    2 * preorderDepthFirstIndex
                    + if taggedAsEven
                      then
                        0
                      else
                        1
                let factory
                    , testVariableCombination
                    , testVariableIndexToLevelsMapping
                    , tagToPreorderDepthFirstIndexMapping
                    , permutationExample
                    , targetNonSingletonTestVariableCombinations =
                    constructFactoryWithAccompanyingTestVariableCombinations combinationStrength
                                                                             testVariableIndexToLevelsMapping
                                                                             (Map.add taggedAsEven
                                                                                      (1 + preorderDepthFirstIndex)
                                                                                      tagToPreorderDepthFirstIndexMapping)
                                                                             numberOfAncestorFactories
                                                                             allowEmptyValueNodes
                                                                             mustHavePermutingSynthesisInTree
                let taggedFactory =
                    factoryConstructors.ApplyTagTo factory
                                                   tag
                if allowDeferrals
                   && numberOfAncestorFactories < maximumNumberOfDeferrals
                   && randomBehaviour.HeadsItIs()
                then
                    factoryConstructors.DeferralFrom taggedFactory
                else
                    taggedFactory
                , testVariableCombination
                , testVariableIndexToLevelsMapping
                , tagToPreorderDepthFirstIndexMapping
                , permutationExample
                , targetNonSingletonTestVariableCombinations
            and constructFactoryWithAccompanyingTestVariableCombinations combinationStrength
                                                                         testVariableIndexToLevelsMapping
                                                                         tagToPreorderDepthFirstIndexMapping
                                                                         numberOfAncestorFactories
                                                                         allowEmptyValueNodes
                                                                         mustHavePermutingSynthesisInTree =
                let indexForLeftmostTestVariable =
                    (testVariableIndexToLevelsMapping: Map<_, _>).Count
                let considerAddingFilterToFactory factory
                                                  testVariableIndexToLevelsMapping
                                                  feasibleTestVariableCombination
                                                  targetNonSingletonTestVariableCombinations =
                    optionWorkflow
                        {
                            let! feasibleTestVariableCombination =
                                feasibleTestVariableCombination
                            let targetNonSingletonTestVariableCombination =
                                feasibleTestVariableCombination
                                |> Set.filter (fun testVariableIndex ->
                                                (testVariableIndexToLevelsMapping: Map<_, _>).[testVariableIndex]
                                                 |> List.head
                                                 |> List.head
                                                 |> snd
                                                 |> Option.isSome)
                            let numberOfTestVariablesInTargetNonSingletonTestVariableCombination =
                                Set.count targetNonSingletonTestVariableCombination
                            match randomBehaviour.ChooseAnyNumberFromOneTo 4 with
                                1 when 1 < numberOfTestVariablesInTargetNonSingletonTestVariableCombination ->
                                    let additionalFilterForThisFactory (dictionary: IDictionary<Int32, Int32 * Object>) =
                                        let relevantTestVariableLevelEncodedIndices =
                                            seq
                                                {
                                                    for KeyValue (relativeTestVariableIndexPassedExplicitlyToFilter
                                                                  , (testVariableLevelIndex
                                                                     , testVariableLevelValue)) in dictionary do
                                                        match (unbox testVariableLevelValue): List<TestVariableLevel> with
                                                            [testVariableIndexForNonSingletonTestVariable
                                                             , Some testVariableLevelEncodedIndex] ->
                                                                if ensureContiguousSortedTestVariableIndicesAfterPruning
                                                                then
                                                                    let relativeTestVariableIndex =
                                                                        testVariableIndexForNonSingletonTestVariable - indexForLeftmostTestVariable
                                                                    let shouldBeTrue =
                                                                        relativeTestVariableIndexPassedExplicitlyToFilter
                                                                         = relativeTestVariableIndex
                                                                    Assert.IsTrue shouldBeTrue
                                                                if not allowDuplicatedLevels
                                                                then
                                                                    let shouldBeTrue =
                                                                        testVariableLevelIndex = testVariableLevelEncodedIndex
                                                                    Assert.IsTrue shouldBeTrue
                                                                if targetNonSingletonTestVariableCombination.Contains testVariableIndexForNonSingletonTestVariable
                                                                then
                                                                    yield testVariableLevelIndex        
                                                          | [testVariableIndexForNonSingletonTestVariable
                                                             , None] ->
                                                                Assert.Fail ("Level value is from a singleton test variable - this is not permitted.")
                                                          | _ ->
                                                                Assert.Fail ("Level value passed to filter is not well-formed.")
                                                }
                                        testVariableEncodedIndicesPass relevantTestVariableLevelEncodedIndices
                                                                       numberOfTestVariablesInTargetNonSingletonTestVariableCombination
                                    return factoryConstructors.FactoryWithFilterFrom factory
                                                                                     (LevelCombinationFilter additionalFilterForThisFactory)
                                           , targetNonSingletonTestVariableCombination :: targetNonSingletonTestVariableCombinations
                              | _ ->
                                    return! optionWorkflow.Zero ()
                            }
                        |> BargainBasement.Flip defaultArg
                                                (factory
                                                 , targetNonSingletonTestVariableCombinations)
                match randomBehaviour.ChooseAnyNumberFromOneTo 3 with
                    1 when combinationStrength = 1
                            && not mustHavePermutingSynthesisInTree ->
                        if randomBehaviour.HeadsItIs ()
                        then
                            let levelCountForTestVariableIntroducedHere =
                                if allowEmptyValueNodes
                                   && not mustHavePermutingSynthesisInTree
                                   // NOTE: the second part of the conjunction ensures that if a permutation
                                   // example is needed, then recursion will eventually make one.
                                   && not ensureContiguousSortedTestVariableIndicesAfterPruning
                                then
                                    randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (1 + maximumNumberOfTestLevels)
                                else
                                    randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestLevels
                            let testVariableLevels =
                                let privateRandomBehaviourThatDoesNotPerturbTheMainOne =
                                    Random (randomBehaviour.Next())
                                if allowDuplicatedLevels
                                then
                                        (Array.init levelCountForTestVariableIntroducedHere
                                                    (fun _ ->
                                                        if privateRandomBehaviourThatDoesNotPerturbTheMainOne.HeadsItIs ()
                                                        then
                                                            0
                                                        else
                                                            1)
                                        |> Array.scan (+) 0).[0 .. levelCountForTestVariableIntroducedHere - 1]
                                        |> List.ofArray
                                        |> List.map (fun level -> [(indexForLeftmostTestVariable, Some level)])

                                else
                                        [ for level in Enumerable.Range(0, levelCountForTestVariableIntroducedHere) do
                                            yield [(indexForLeftmostTestVariable, Some level)] ]
                            factoryConstructors.TestVariableFrom testVariableLevels
                            , if testVariableLevels.IsEmpty
                              then
                                None
                              else
                                Set.singleton indexForLeftmostTestVariable
                                |> Some
                            , Map.add indexForLeftmostTestVariable
                                      testVariableLevels
                                      testVariableIndexToLevelsMapping
                            , tagToPreorderDepthFirstIndexMapping
                            , None
                            , []
                        else
                            let testVariableLevel =
                                [(indexForLeftmostTestVariable, None)]: List<TestVariableLevel>
                            let testVariableLevels =
                                [ testVariableLevel ]
                            factoryConstructors.SingletonFrom testVariableLevel
                            , Set.singleton indexForLeftmostTestVariable
                              |> Some
                            , Map.add indexForLeftmostTestVariable
                                      testVariableLevels
                                      testVariableIndexToLevelsMapping
                            , tagToPreorderDepthFirstIndexMapping
                            , None
                            , []
                  | _ ->
                    if randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (2 * maximumNumberOfAncestorFactoriesAllowingFairChanceOfInterleaving)
                       < max maximumNumberOfAncestorFactoriesAllowingFairChanceOfInterleaving
                             numberOfAncestorFactories
                    then
                        let numberOfSubtrees =
                            randomBehaviour.ChooseAnyNumberFromOneTo combinationStrength
                        let nonZeroCombinationStrengthsForSubtrees =
                            BargainBasement.PartitionSizeIntoSectionsOfRandomNonZeroLength combinationStrength
                                                                                           numberOfSubtrees
                                                                                           randomBehaviour

                        let permuteInputsAndForbidPruning =
                            let oddsAgainstPermuting =
                                5
                                / numberOfSubtrees
                            mustHavePermutingSynthesisInTree
                            && match randomBehaviour.ChooseAnyNumberFromOneTo (1 + oddsAgainstPermuting) with
                                1 ->
                                    true
                              | _ ->
                                    false

                        let permuteInputs =
                            permuteInputsAndForbidPruning
                            || match randomBehaviour.ChooseAnyNumberFromOneTo 5 with
                                1 ->
                                    true
                              | _ ->
                                    false

                        let pairsOfWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice =
                            let choices =
                                if mustHavePermutingSynthesisInTree
                                then
                                    [
                                        yield allowEmptyValueNodes
                                              , true
                                        for _ in [2 .. numberOfSubtrees] do
                                            yield false // The siblings of a subtree containing the permutation example musn't be
                                                        // prunable, as that might make this subtree (and thus the permutation example)
                                                        // prunable too.
                                                  , false
                                    ]
                                else
                                    [
                                        for _ in [1 .. numberOfSubtrees] do
                                            yield allowEmptyValueNodes
                                                  , false
                                    ]
                            randomBehaviour.Shuffle choices
                            |> List.ofArray

                        let triplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice =
                            List.zip nonZeroCombinationStrengthsForSubtrees
                                     pairsOfWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice
                            |> List.map (fun (strength
                                              , (whetherToAllowEmptyValueNode
                                                 , mustHavePermutingSynthesisInTree)) ->
                                              strength
                                              , whetherToAllowEmptyValueNode
                                              , mustHavePermutingSynthesisInTree)

                        let rec createSubtrees triplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice
                                               testVariableIndexToLevelsMapping
                                               tagToPreorderDepthFirstIndexMapping
                                               permutationExtentsForSubtrees =
                            match triplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice with
                                [] ->
                                    []
                                    , []
                                    , testVariableIndexToLevelsMapping
                                    , tagToPreorderDepthFirstIndexMapping
                                    , permutationExtentsForSubtrees
                                    , []
                              | (subtreeCombinationStrength
                                 , allowEmptyValueNodeInSubtree
                                 , mustHavePermutingSynthesisInTreeInSubtree) :: tailTriplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice ->
                                    let subtree
                                        , testVariableCombinationFromSubtree
                                        , testVariableIndexToLevelsMappingFromSubtree
                                        , tagToPreorderDepthFirstIndexMappingFromSubtree
                                        , permutationExtentFromSubtree
                                        , targetNonSingletonTestVariableCombinationsFromSubtree =
                                        tagAndThenDecorateWithDeferralIfRequired subtreeCombinationStrength
                                                                                 testVariableIndexToLevelsMapping
                                                                                 tagToPreorderDepthFirstIndexMapping
                                                                                 (numberOfAncestorFactories + 1)
                                                                                 (allowEmptyValueNodeInSubtree
                                                                                  && not permuteInputsAndForbidPruning)
                                                                                 (mustHavePermutingSynthesisInTreeInSubtree
                                                                                  && not permuteInputsAndForbidPruning)
                                    let remainingSubtrees
                                        , testVariableCombinationsFromRemainingSubtrees
                                        , testVariableIndexToLevelsMappingFromRemainingSubtrees
                                        , tagToPreorderDepthFirstIndexMappingFromRemainingSubtrees
                                        , permutationExtentsFromRemainingSubtrees
                                        , targetNonSingletonTestVariableCombinationsFromRemainingSubtrees =
                                        createSubtrees tailTriplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice
                                                       testVariableIndexToLevelsMappingFromSubtree
                                                       tagToPreorderDepthFirstIndexMappingFromSubtree
                                                       permutationExtentsForSubtrees
                                    subtree :: remainingSubtrees
                                    , testVariableCombinationFromSubtree :: testVariableCombinationsFromRemainingSubtrees
                                    , testVariableIndexToLevelsMappingFromRemainingSubtrees
                                    , tagToPreorderDepthFirstIndexMappingFromRemainingSubtrees
                                    , permutationExtentFromSubtree :: permutationExtentsFromRemainingSubtrees
                                    , List.append targetNonSingletonTestVariableCombinationsFromSubtree targetNonSingletonTestVariableCombinationsFromRemainingSubtrees
                        let subtrees
                            , testVariableCombinationsFromSubtrees
                            , testVariableIndexToLevelsMappingFromSubtrees
                            , tagToPreorderDepthFirstIndexMappingFromSubtrees
                            , permutationExtentsForSubtrees
                            , targetNonSingletonTestVariableCombinationsFromSubtrees =
                            createSubtrees triplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice
                                           testVariableIndexToLevelsMapping
                                           tagToPreorderDepthFirstIndexMapping
                                           []
                        let shuffledSubtrees    // NOTE: to make it clear, this has *nothing* to do with 'allowSynthesisToPermuteInputs';
                                                // rather this is the setup for a separate, explicit *inverse* permutation that is always
                                                // part of the synthesis function and which is therefore always applied. The idea is to
                                                // apply a non-trivial computation for synthesis that would nevertheless boil down to doing
                                                // a concatenation of the inputs (thus making the rest of the test logic simpler)
                                                // - it turns out that way because the subtrees are also permuted with the corresponding
                                                // forward permutation prior to being used for synthesis, so the result is just a plain
                                                // concatenation.
                                                // This came first, the support for testing implicit permutations came much later. What's
                                                // more, when implicit permutations are enabled, the additional permuting activity really
                                                // will reorder the contributions from the inputs.
                            , permutationInvertingShuffle =
                            let subtreesAndLabellingIndices =
                                List.zip subtrees [0 .. subtrees.Length - 1]
                            (if ensureContiguousSortedTestVariableIndicesAfterPruning
                                then
                                    subtreesAndLabellingIndices
                                    |> Array.ofList
                                else
                                    randomBehaviour.Shuffle subtreesAndLabellingIndices)
                            |> Array.unzip

                        let undoShuffleAndConcatenateContributedLevels listOfLevels =
                            let inverseShuffledListsOfLevels =
                                listOfLevels
                                |> List.permute (fun inputIndex -> permutationInvertingShuffle.[inputIndex])
                            List.concat inverseShuffledListsOfLevels

                        let testVariableCombination =
                            testVariableCombinationsFromSubtrees
                            |> List.fold (fun partialTestVariableCombination
                                              testVariableCombinationFromSubtree ->
                                                optionWorkflow
                                                    {
                                                        let! partialTestVariableCombination =
                                                            partialTestVariableCombination
                                                        let! testVariableCombinationFromSubtree =
                                                            testVariableCombinationFromSubtree
                                                        return Set.union partialTestVariableCombination
                                                                         testVariableCombinationFromSubtree
                                                    })
                                         (Some Set.empty)

                        let permutationExample =
                            if permuteInputsAndForbidPruning
                            then
                                testVariableCombinationsFromSubtrees
                                |> List.fold (fun permutationExample
                                                  testVariableCombinationFromSubtree ->
                                                    optionWorkflow
                                                        {
                                                            let! permutationExample =
                                                                permutationExample
                                                            let! testVariableCombinationFromSubtree =
                                                                testVariableCombinationFromSubtree
                                                            let chosenTestVariable =
                                                                randomBehaviour.ChooseOneOf testVariableCombinationFromSubtree
                                                            return Set.add chosenTestVariable
                                                                           permutationExample
                                                        })
                                             (Some Set.empty)
                            else
                                None

                        let permutationExampleChoices =
                            permutationExample :: permutationExtentsForSubtrees
                            |> Option<_>.GetFromMany

                        let chosenPermutationExample =
                            if permutationExampleChoices.IsEmpty
                            then
                                None
                            else
                                randomBehaviour.ChooseOneOf permutationExampleChoices
                                |> Some

                        let factory =
                            factoryConstructors.SynthesisFrom shuffledSubtrees
                                                              undoShuffleAndConcatenateContributedLevels
                                                              (permuteInputs
                                                               && match allowSynthesisToPermuteInputs with
                                                                    No ->
                                                                        false
                                                                  | Yes ->
                                                                        true
                                                                  | Randomly ->
                                                                        randomBehaviour.HeadsItIs ())

                        let factory
                            , targetNonSingletonTestVariableCombinations =
                            considerAddingFilterToFactory factory
                                                          testVariableIndexToLevelsMappingFromSubtrees
                                                          testVariableCombination
                                                          targetNonSingletonTestVariableCombinationsFromSubtrees

                        factory
                        , testVariableCombination
                        , testVariableIndexToLevelsMappingFromSubtrees
                        , tagToPreorderDepthFirstIndexMappingFromSubtrees
                        , chosenPermutationExample
                        , targetNonSingletonTestVariableCombinations
                    else
                        let numberOfSubtrees =
                            randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfNonZeroCombinationStrengthSubtrees
                        let rec chooseCombinationStrengths numberOfSubtrees =
                            if numberOfSubtrees > combinationStrength
                                  then seq { yield! seq { 1 .. combinationStrength }
                                             yield! chooseCombinationStrengths (numberOfSubtrees - combinationStrength) }
                                  else seq { yield! randomBehaviour.ChooseSeveralOf (seq { 1 .. combinationStrength - 1 }, numberOfSubtrees - 1)
                                             yield combinationStrength }
                        let combinationStrengths =
                            chooseCombinationStrengths numberOfSubtrees
                            |> randomBehaviour.Shuffle
                            |> List.ofArray

                        let whetherToAllowEmptyValueNodeChoices =
                            let halfNumberOfSubtreesRoundedDown =
                                numberOfSubtrees / 2
                                // Rounding down causes the next binding to favour the case of *not* flipping 'allowEmptyValueNodes'
                                // - that way, if we have a trivial interleave with just a single subtree, 'allowEmptyValueNodes' will
                                // not be flipped.
                            Seq.append (List.replicate halfNumberOfSubtreesRoundedDown (not allowEmptyValueNodes))
                                       (List.replicate (numberOfSubtrees - halfNumberOfSubtreesRoundedDown) allowEmptyValueNodes)
                            |> randomBehaviour.Shuffle
                            |> List.ofArray

                        let mustHavePermutingSynthesisInTreeChoices =
                            let choices =
                                [
                                    yield mustHavePermutingSynthesisInTree
                                    for _ in [2 .. numberOfSubtrees] do
                                        yield false
                                ]
                            randomBehaviour.Shuffle choices
                            |> List.ofArray

                        let rec createSubtrees triplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice
                                               testVariableIndexToLevelsMapping
                                               tagToPreorderDepthFirstIndexMapping
                                               permutationExtentsForSubtrees =
                            match triplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice with
                                [] ->
                                    []
                                    , []
                                    , testVariableIndexToLevelsMapping
                                    , tagToPreorderDepthFirstIndexMapping
                                    , permutationExtentsForSubtrees
                                    , []
                              | (subtreeCombinationStrength
                                 , allowEmptyValueNodeInSubtree
                                 , mustHavePermutingSynthesisInTreeInSubtree) :: tailTriplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice ->
                                    let subtree
                                        , testVariableCombinationFromSubtree
                                        , testVariableIndexToLevelsMappingFromSubtree
                                        , tagToPreorderDepthFirstIndexMappingFromSubtree
                                        , permutationExtentFromSubtree
                                        , targetNonSingletonTestVariableCombinationsFromSubtree =
                                        tagAndThenDecorateWithDeferralIfRequired subtreeCombinationStrength
                                                                                 testVariableIndexToLevelsMapping
                                                                                 tagToPreorderDepthFirstIndexMapping
                                                                                 (numberOfAncestorFactories + 1)
                                                                                 allowEmptyValueNodeInSubtree
                                                                                 mustHavePermutingSynthesisInTreeInSubtree
                                    let remainingSubtrees
                                        , testVariableCombinationsFromRemainingSubtrees
                                        , testVariableIndexToLevelsMappingFromRemainingSubtrees
                                        , tagToPreorderDepthFirstIndexMappingFromRemainingSubtrees
                                        , permutationExtentsFromRemainingSubtrees
                                        , targetNonSingletonTestVariableCombinationsFromRemainingSubtrees =
                                        createSubtrees tailTriplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice
                                                       testVariableIndexToLevelsMappingFromSubtree
                                                       tagToPreorderDepthFirstIndexMappingFromSubtree
                                                       permutationExtentsForSubtrees
                                    subtree :: remainingSubtrees
                                    , testVariableCombinationFromSubtree :: testVariableCombinationsFromRemainingSubtrees
                                    , testVariableIndexToLevelsMappingFromRemainingSubtrees
                                    , tagToPreorderDepthFirstIndexMappingFromRemainingSubtrees
                                    , permutationExtentFromSubtree :: permutationExtentsFromRemainingSubtrees
                                    , List.append targetNonSingletonTestVariableCombinationsFromSubtree targetNonSingletonTestVariableCombinationsFromRemainingSubtrees
                        let subtrees
                            , testVariableCombinationsFromSubtrees
                            , testVariableIndexToLevelsMappingFromSubtrees
                            , tagToPreorderDepthFirstIndexMappingFromSubtrees
                            , permutationExtentsForSubtrees
                            , targetNonSingletonTestVariableCombinationsFromSubtrees =
                            createSubtrees (List.zip3 combinationStrengths
                                                      whetherToAllowEmptyValueNodeChoices
                                                      mustHavePermutingSynthesisInTreeChoices)
                                           testVariableIndexToLevelsMapping
                                           tagToPreorderDepthFirstIndexMapping
                                           []
                        let achievableTestVariableCombinationsFromSubtrees =
                            testVariableCombinationsFromSubtrees
                            |> Option<_>.GetFromMany
                        let chosenTestVariableCombination =
                            if List.isEmpty achievableTestVariableCombinationsFromSubtrees
                            then
                                None
                            else
                                randomBehaviour.ChooseOneOf achievableTestVariableCombinationsFromSubtrees
                                |> Some

                        let permutationExampleChoices =
                            permutationExtentsForSubtrees
                            |> Option<_>.GetFromMany

                        let chosenPermutationExample =
                            if permutationExampleChoices.IsEmpty
                            then
                                None
                            else
                                randomBehaviour.ChooseOneOf permutationExampleChoices
                                |> Some

                        let factory =
                            factoryConstructors.InterleavingFrom subtrees

                        let factory
                            , targetNonSingletonTestVariableCombinations =
                            considerAddingFilterToFactory factory
                                                          testVariableIndexToLevelsMappingFromSubtrees
                                                          chosenTestVariableCombination
                                                          targetNonSingletonTestVariableCombinationsFromSubtrees

                        factory
                        , chosenTestVariableCombination
                        , testVariableIndexToLevelsMappingFromSubtrees
                        , tagToPreorderDepthFirstIndexMappingFromSubtrees
                        , chosenPermutationExample
                        , targetNonSingletonTestVariableCombinations

            let factory
                , testVariableCombination
                , testVariableIndexToLevelsMapping
                , _
                , permutationExample
                , targetNonSingletonTestVariableCombinations =
                tagAndThenDecorateWithDeferralIfRequired combinationStrength
                                                         Map.empty
                                                         Map.empty
                                                         0
                                                         false
                                                         true
            factory
            , testVariableCombination.Value
            , testVariableIndexToLevelsMapping
            , permutationExample
            , targetNonSingletonTestVariableCombinations

        let randomBehaviourSeed = 23

        let overallTestRepeats = 100

        let isSortedByTestVariableIndex =
            Seq.map fst
            >> IEnumerable<_>.IsSorted

        let partitionTestVariablesIntoThoseWithLevelsAndSingletons testVariableCombination
                                                                   testVariableIndexToLevelsMapping =
            Set.partition (fun testVariableIndex ->
                            Map.find testVariableIndex testVariableIndexToLevelsMapping
                            |> (fun levels ->
                                    match levels with
                                        [[_, None]] -> false
                                      | _ -> true)) testVariableCombination

        let createTreesAndHandOffToTest testHandoff =
            let randomBehaviour = Random randomBehaviourSeed
            for _ in 1 .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let factory
                    , testVariableCombination
                    , testVariableIndexToLevelsMapping
                    , permutationExample
                    , targetNonSingletonTestVariableCombinations =
                    constructFactoryWithAccompanyingTestVariableCombinations weaklyTypedFactoryConstructors
                                                                             randomBehaviour
                                                                             false
                                                                             No
                                                                             false
                                                                             true
                let factory =
                    factory.WithDeferralBudgetOf maximumNumberOfDeferrals
                let maximumStrength =
                    randomBehaviour.ChooseAnyNumberFromOneTo factory.MaximumStrength
                let testVariableCombinationConformingToMaximumStrength =
                    if testVariableCombination.Count <= maximumStrength
                    then testVariableCombination
                    else randomBehaviour.ChooseSeveralOf (testVariableCombination, maximumStrength)
                         |> Set.ofArray
                let testCaseEnumerable =
                    factory.CreateEnumerable maximumStrength
                testHandoff testCaseEnumerable
                            testVariableCombinationConformingToMaximumStrength
                            testVariableIndexToLevelsMapping
                            targetNonSingletonTestVariableCombinations
                            maximumStrength
                            randomBehaviour

        [<Test>]
        member this.TestCoverageOfImplicitPermutationsIsComplete () =
            let randomBehaviour = Random randomBehaviourSeed
            for _ in 1 .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let sharedSeed =
                    randomBehaviour.Next()
                let copiedRandomBehaviourOne =
                    Random sharedSeed
                let copiedRandomBehaviourTwo =
                    Random sharedSeed
                let factory
                    , _
                    , testVariableIndexToLevelsMapping
                    , Some permutationExample
                    , _ =
                    constructFactoryWithAccompanyingTestVariableCombinations stronglyTypedFactoryConstructors
                                                                             copiedRandomBehaviourOne
                                                                             false
                                                                             Yes
                                                                             false
                                                                             false
                let unpermutedFactory
                    , _
                    , _
                    , _
                    , _ =
                    constructFactoryWithAccompanyingTestVariableCombinations stronglyTypedFactoryConstructors
                                                                             copiedRandomBehaviourTwo
                                                                             false
                                                                             No
                                                                             false
                                                                             false

                printf "Permutation example: %A\n" permutationExample

                let maximumStrength =
                    randomBehaviour.ChooseAnyNumberFromOneTo factory.MaximumStrength
                let testCaseEnumerable =
                    factory.CreateEnumerable maximumStrength


                let testCases =
                    factory.CreateEnumerable maximumStrength

                let unpermutedTestCases =
                    unpermutedFactory.CreateEnumerable maximumStrength

                let slice testCases =
                    testCases
                    |> Seq.map (List.filter (fun (testVariableIndex
                                                  , _) ->
                                                permutationExample.Contains testVariableIndex))
                    |> Set.ofSeq    // Eliminate duplicates caused by slicing throwing away distinguishing state.
                    |> Set.remove List.empty

                let slicedTestCases =
                    slice testCases

                let slicedUnpermutedTestCases =
                    slice unpermutedTestCases

                let chosenSlicedTestCaseToSelectTestVariables =
                    randomBehaviour.ChooseOneOf slicedTestCases

                let numberOfTestVariablesInSliceThatPermutationCoverageIsGuaranteedFor =
                    let numberOfTestVariablesInPermutationExample =
                        permutationExample
                        |> Set.count
                    (maximumStrength
                     |> max 1)
                    |> min numberOfTestVariablesInPermutationExample

                let chosenTestVariablesToCheckPermutationCoverageFor =
                    let testVariablesToChooseFrom =
                        chosenSlicedTestCaseToSelectTestVariables
                        |> List.map fst
                    if numberOfTestVariablesInSliceThatPermutationCoverageIsGuaranteedFor
                       < List.length chosenSlicedTestCaseToSelectTestVariables
                    then
                        randomBehaviour.ChooseSeveralOf (testVariablesToChooseFrom,
                                                         numberOfTestVariablesInSliceThatPermutationCoverageIsGuaranteedFor)
                        :> seq<_>
                    else
                        testVariablesToChooseFrom
                        :> seq<_>
                    |> Set.ofSeq

                let fineSlice slicedTestCases =
                    slicedTestCases
                    |> Set.map (List.filter (fun (testVariableIndex
                                                  , _) ->
                                                chosenTestVariablesToCheckPermutationCoverageFor.Contains testVariableIndex))
                    |> Set.remove List.empty
                    |> Set.filter (fun testCase ->
                                    List.length testCase
                                     = numberOfTestVariablesInSliceThatPermutationCoverageIsGuaranteedFor)
                    // Again, more elimination of duplicates caused by throwing away distinguishing state.
                    // Furthermore, eliminate any test cases whose finely sliced test variables form proper
                    // subsets of the fine slice - this can happen when some of the test variables in the
                    // permutation example are individually bracketed by interleaves - so they can drop out
                    // of a test case.

                let finelySlicedTestCases =
                    fineSlice slicedTestCases

                let finelySlicedUnpermutedTestCases =
                    fineSlice slicedUnpermutedTestCases

                let finelySlicedTestCasesGroupedByTestLevelCombinations =
                    finelySlicedTestCases
                    |> Set.fold (fun finelySlicedTestCasesGroupedByTestLevelCombinations
                                     finelySlicedTestCase ->
                                        let groupKey =
                                            finelySlicedTestCase
                                            |> Set.ofList
                                        let groupWithFinelySlicedTestCaseAdded =
                                            defaultArg (finelySlicedTestCasesGroupedByTestLevelCombinations
                                                        |> Map.tryFind groupKey
                                                        |> Option.bind (Set.add finelySlicedTestCase
                                                                        >> Some))
                                                       (Set.singleton finelySlicedTestCase)
                                        finelySlicedTestCasesGroupedByTestLevelCombinations
                                        |> Map.add groupKey
                                                   groupWithFinelySlicedTestCaseAdded)
                                Map.empty

                let shouldBeTrue =
                    let groupKeysFromFinelySlicedTestCases =
                        (finelySlicedTestCasesGroupedByTestLevelCombinations :> IDictionary<_, _>).Keys
                        |> Set.ofSeq
                    let finelySlicedUnpermutedTestCasesAsKeys =
                        finelySlicedUnpermutedTestCases
                        |> Set.map Set.ofList
//                    printf "%A\n" (((testCases |> Seq.map Set.ofList |> Set.ofSeq) - (unpermutedTestCases |> Seq.map Set.ofList |> Set.ofSeq)) |> Set.toArray)
                    groupKeysFromFinelySlicedTestCases = finelySlicedUnpermutedTestCasesAsKeys
                Assert.IsTrue shouldBeTrue

                let numberOfPermutationsExpected =
                    numberOfTestVariablesInSliceThatPermutationCoverageIsGuaranteedFor
                    |> BargainBasement.Factorial

                for _
                    , group in finelySlicedTestCasesGroupedByTestLevelCombinations
                               |> Map.toSeq do
                    let numberOfPermutationsOfFineSlice =
                        Set.count group
                    let shouldBeTrue =
                        numberOfPermutationsOfFineSlice = numberOfPermutationsExpected
                    Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestThatZeroStrengthResultsInAnEmptyTestCaseSequence () =
            let randomBehaviour = Random randomBehaviourSeed
            for _ in 1 .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let sharedSeed =
                    randomBehaviour.Next()
                let factory
                    , _
                    , _
                    , _
                    , _ =
                    constructFactoryWithAccompanyingTestVariableCombinations stronglyTypedFactoryConstructors
                                                                             randomBehaviour
                                                                             false
                                                                             Yes
                                                                             false
                                                                             false

                let shouldBeTrue =
                    seq
                        {
                            for testCase in factory.CreateEnumerable 0 do
                                yield testCase :> IComparable
                        }
                    |> Seq.length = 0
                Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestCorrectOrderingOfLevelsFromDistinctTestVariablesInEachOutputTestCase () =
            let testHandoff (testCaseEnumerable: System.Collections.IEnumerable)
                            _
                            _
                            _
                            _
                            _ =
                for testCase in testCaseEnumerable do
                    let testCase = unbox<List<TestVariableLevel>> testCase
                    let shouldBeTrue = isSortedByTestVariableIndex testCase
                    Assert.IsTrue shouldBeTrue
            createTreesAndHandOffToTest testHandoff

        [<Test>]
        member this.TestCoverageOfNCombinationsOfVariableLevelsInFinalResultsIsComplete () =
            let testHandoff (testCaseEnumerable: System.Collections.IEnumerable)
                            testVariableCombination
                            testVariableIndexToLevelsMapping
                            targetNonSingletonTestVariableCombinations
                            maximumStrength
                            _ =
                let testVariableIndexToLevelsMappingForTestVariableCombination =
                    testVariableCombination
                    |> Set.toList
                    |> List.map (fun testVariable ->
                                    testVariable
                                    , Map.find testVariable testVariableIndexToLevelsMapping)

                let filter testLevelCombination =
                    targetNonSingletonTestVariableCombinations
                    |> List.forall (fun targetNonSingletonTestVariableCombination ->
                                        let testLevelEncodedIndices =
                                            testLevelCombination
                                            |> Seq.filter (fst >> (fun testVariableIndex ->
                                                                        Set.contains testVariableIndex
                                                                                     targetNonSingletonTestVariableCombination))
                                            |> Seq.map (snd >> Option.get)  // NOTE: this is protected, because the test variable has a level due to the previous filter.
                                        testVariableEncodedIndicesPass testLevelEncodedIndices
                                                                       (Set.count targetNonSingletonTestVariableCombination))

                let combinationsOfTestLevels =
                    testVariableIndexToLevelsMappingForTestVariableCombination
                    |> List.map (snd >> List.head)
                    |> List.CrossProduct
                    |> Seq.map Set.ofList
                    |> Set.ofSeq

                let expectedCombinationsOfTestLevels
                    , forbiddenCombinationsOfTestLevels =
                    combinationsOfTestLevels
                    |> Set.partition filter

                let testCases =
                    seq {for testCase in testCaseEnumerable do
                            yield unbox<List<TestVariableLevel>> testCase
                                  |> Set.ofList}

                let unaccountedExpectedCombinationsOfTestLevels =
                    Seq.fold (fun expectedCombinationsOfTestLevels
                                  testCase ->
                                    let expectedCombinationsOfTestLevelsSatisfiedByTestCase =
                                        expectedCombinationsOfTestLevels
                                        |> Set.filter (fun expectedTestLevelCombination ->
                                                            (testCase: Set<_>).IsSupersetOf expectedTestLevelCombination)
                                    expectedCombinationsOfTestLevels - expectedCombinationsOfTestLevelsSatisfiedByTestCase)
                             expectedCombinationsOfTestLevels
                             testCases

                let shouldBeTrue =
                    unaccountedExpectedCombinationsOfTestLevels
                    |> Set.isEmpty
                if not shouldBeTrue
                then
                    printf "Missing: %A\n" unaccountedExpectedCombinationsOfTestLevels
                    printf "expectedCombinationsOfTestLevels: %A\n" expectedCombinationsOfTestLevels
                    printf "forbiddenCombinationsOfTestLevels: %A\n" forbiddenCombinationsOfTestLevels
                    printf "targetNonSingletonTestVariableCombinations: %A\n" targetNonSingletonTestVariableCombinations
                    printf "maximumStrength: %A\n" maximumStrength
                    printf "testVariableCombination: %A\n" testVariableCombination
                Assert.IsTrue shouldBeTrue

                let unwantedForbiddenCombinationsOfTestLevels =
                    Seq.fold (fun unwantedForbiddenCombinationsOfTestLevels
                                  testCase ->
                                    let unwantedForbiddenCombinationsOfTestLevelsSatisfiedByTestCase =
                                        forbiddenCombinationsOfTestLevels
                                        |> Set.filter (fun forbiddenTestLevelCombination ->
                                                            (testCase: Set<_>).IsSupersetOf forbiddenTestLevelCombination)
                                    unwantedForbiddenCombinationsOfTestLevels + unwantedForbiddenCombinationsOfTestLevelsSatisfiedByTestCase)
                             Set.empty
                             testCases

                let shouldBeTrue =
                    unwantedForbiddenCombinationsOfTestLevels
                    |> Set.isEmpty
                Assert.IsTrue shouldBeTrue

            createTreesAndHandOffToTest testHandoff

        [<Test>]
        member this.TestThatDuplicatedLevelValuesAreTreatedAsDistinctWhenMakingTestCases () =
            let randomBehaviour = Random randomBehaviourSeed
            for _ in 1 .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let sharedSeed =
                    randomBehaviour.Next()
                let copiedRandomBehaviourOne =
                    Random sharedSeed
                let copiedRandomBehaviourTwo =
                    Random sharedSeed
                let factory
                    , _
                    , _
                    , _
                    , _ = constructFactoryWithAccompanyingTestVariableCombinations weaklyTypedFactoryConstructors
                                                                                   copiedRandomBehaviourOne
                                                                                   false
                                                                                   No
                                                                                   false
                                                                                   false
                let factoryBasedOnDuplicateLevels
                    , _
                    , _
                    , _
                    , _ = constructFactoryWithAccompanyingTestVariableCombinations weaklyTypedFactoryConstructors
                                                                                   copiedRandomBehaviourTwo
                                                                                   true
                                                                                   No
                                                                                   false
                                                                                   false
                let maximumStrength =
                    factory.MaximumStrength
                let shouldBeTrue =
                    maximumStrength = factoryBasedOnDuplicateLevels.MaximumStrength
                Assert.IsTrue shouldBeTrue
                let comparer = HashIdentity.Structural<_>
                let bagCombinationsOfTestVariableIndicesDisregardingLevels testCases =
                    let combinationsOfTestVariableIndices =
                        testCases
                        |> Seq.map (List.map fst)
                    let result =
                        C5.HashBag<_>(comparer)
                    result.AddAll combinationsOfTestVariableIndices
                    result
                let testCaseSequence (factory: IFactory) =
                    seq {for testCase in factory.CreateEnumerable(maximumStrength) do
                            yield unbox<List<TestVariableLevel>> testCase}
                let shouldBeTrue = (testCaseSequence factory
                                    |> bagCombinationsOfTestVariableIndicesDisregardingLevels).UnsequencedEquals
                                     (testCaseSequence factoryBasedOnDuplicateLevels
                                      |> bagCombinationsOfTestVariableIndicesDisregardingLevels)
                Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestCoverageOfHighestCombinationsOfVariableLevelsInFinalResultsIsOptimal () =
            let randomBehaviour = Random randomBehaviourSeed
            for _ in 1 .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let factory
                    , _
                    , testVariableIndexToLevelsMapping
                    , _
                    , _ =
                    constructFactoryWithAccompanyingTestVariableCombinations weaklyTypedFactoryConstructors
                                                                             randomBehaviour
                                                                             false
                                                                             No
                                                                             false
                                                                             false
                let maximumStrength =
                    factory.MaximumStrength
                let testCaseEnumerable =
                    factory.CreateEnumerable maximumStrength
                let testCasesOfMaximumStrength =
                    [for testCase in testCaseEnumerable do
                        let testCase =
                            unbox<List<TestVariableLevel>> testCase
                            |> Set.ofList
                        if testCase.Count = maximumStrength
                        then yield testCase]

                let omittedTestCase =
                    (randomBehaviour: Random).ChooseOneOf testCasesOfMaximumStrength

                let combinationsCorrespondingToOmittedTestCase =
                    CombinatoricUtilities.GenerateCombinationsOfGivenSizePreservingOrder maximumStrength
                                                                                         (omittedTestCase
                                                                                          |> Set.toList)

                    |> Seq.map Set.ofList
                    |> Set.ofSeq

                let testCasesExceptTheOmittedOne =
                    seq {for testCase in testCaseEnumerable do
                            let testCase = unbox<List<TestVariableLevel>> testCase
                                           |> Set.ofList
                            if testCase <> omittedTestCase
                            then yield testCase}

                let unaccountedCombinationsOfTestLevels =
                    Seq.fold (fun expectedCombinationsOfTestLevels
                                  testCase ->
                                    let expectedCombinationsOfTestLevelsSatisfiedByTestCase =
                                        expectedCombinationsOfTestLevels
                                        |> Set.filter (fun testLevelCombination ->
                                                            (testCase: Set<_>).IsSupersetOf testLevelCombination)
                                    expectedCombinationsOfTestLevels - expectedCombinationsOfTestLevelsSatisfiedByTestCase)
                             combinationsCorrespondingToOmittedTestCase
                             testCasesExceptTheOmittedOne

                let shouldBeTrue =
                    unaccountedCombinationsOfTestLevels
                    |> Set.isEmpty
                    |> not
                Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestReproductionOfSpecificTestCases () =
            let randomBehaviour = Random randomBehaviourSeed
            for _ in 1 .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let factoryWithoutExplicitDeferralBudget
                    , _
                    , _
                    , _
                    , _ =
                    constructFactoryWithAccompanyingTestVariableCombinations stronglyTypedFactoryConstructors
                                                                             randomBehaviour
                                                                             false
                                                                             Randomly
                                                                             false
                                                                             true
                let factoryWithDeferralBudget =
                    factoryWithoutExplicitDeferralBudget.WithDeferralBudgetOf maximumNumberOfDeferrals
                let randomStrength =
                    randomBehaviour.ChooseAnyNumberFromOneTo factoryWithDeferralBudget.MaximumStrength
                let randomTestCase =
                    seq
                        {
                            for testCase in factoryWithDeferralBudget.CreateEnumerable randomStrength do
                                yield testCase
                        }
                    |> randomBehaviour.ChooseOneOf
                let failOnSpecificTestCase testCase =
                    if randomTestCase = testCase
                    then
                        raise (LogicErrorException (testCase.ToString ()))
                let shouldBeNone =
                    try factoryWithDeferralBudget.ExecuteParameterisedUnitTestForAllTestCases (randomStrength
                                                                                               , Action<_>(failOnSpecificTestCase))
                        |> Some with
                        :? TestCaseReproductionException as testCaseReproductionException ->
                            let innerException =
                                testCaseReproductionException.InnerException
                            let descriptionOfReproductionString =
                                testCaseReproductionException.Message
                            let reproductionString =
                                let regex =
                                    Regex @"^[^\""]*""([^\""]*)""$"
                                let groupsFromMatch =
                                    (regex.Match descriptionOfReproductionString).Groups
                                Assert.AreEqual (2
                                                 , groupsFromMatch.Count)
                                groupsFromMatch.[1].Value
                            let verifyBugReproductionUsing (factory: ITypedFactory<_>) =
                                let shouldBeNone =
                                    try factory.ExecuteParameterisedUnitTestForReproducedTestCase (Action<_>(failOnSpecificTestCase)
                                                                                                   , reproductionString)
                                        |> Some with
                                        reproducedException ->
                                            let shouldBeTrue =
                                                innerException.Message = reproducedException.Message
                                            Assert.IsTrue shouldBeTrue
                                            None
                                Assert.IsTrue shouldBeNone.IsNone
                            verifyBugReproductionUsing factoryWithoutExplicitDeferralBudget
                            verifyBugReproductionUsing factoryWithDeferralBudget
                            None
                      | _ ->
                            Assert.Fail "No other exception should have been thrown."
                            None
                Assert.IsTrue shouldBeNone.IsNone
                let neverFail =
                    ignore
                try factoryWithDeferralBudget.ExecuteParameterisedUnitTestForAllTestCases (randomStrength
                                                                                           , Action<Object>(neverFail))
                    |> ignore with
                    :? TestCaseReproductionException as testCaseReproductionException ->
                        Assert.Fail "Should not be throwing this specific kind of exception if the unit test succeeded."
                  | _ ->
                        Assert.Fail "No other exception should have been thrown."

        [<Test>]
        member this.SmokeTestFiltersIndirectly () =
            let randomBehaviour = Random randomBehaviourSeed
            for _ in 1 .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let typedFactory
                    , _
                    , _
                    , _
                    , _ =
                    constructFactoryWithAccompanyingTestVariableCombinations stronglyTypedFactoryConstructors
                                                                             randomBehaviour
                                                                             true
                                                                             Randomly
                                                                             true
                                                                             false
                for strength in 0 .. typedFactory.MaximumStrength do
                    for _ in typedFactory.CreateEnumerable strength do
                        ()

        [<Test>]
        member this.TestConsistencyOfWeaklyAndStronglyTypedApis () =
            let randomBehaviour = Random randomBehaviourSeed
            for _ in 1 .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let sharedSeed =
                    randomBehaviour.Next()
                let copiedRandomBehaviourOne =
                    Random sharedSeed
                let copiedRandomBehaviourTwo =
                    Random sharedSeed
                let weaklyTypedFactory
                    , _
                    , _
                    , _
                    , _ =
                    constructFactoryWithAccompanyingTestVariableCombinations weaklyTypedFactoryConstructors
                                                                             copiedRandomBehaviourOne
                                                                             false
                                                                             No
                                                                             false
                                                                             true
                let weaklyTypedFactory =
                    weaklyTypedFactory.WithDeferralBudgetOf maximumNumberOfDeferrals
                let stronglyTypedFactory
                    , _
                    , _
                    , _
                    , _ =
                    constructFactoryWithAccompanyingTestVariableCombinations stronglyTypedFactoryConstructors
                                                                             copiedRandomBehaviourTwo
                                                                             false
                                                                             No
                                                                             false
                                                                             true
                let stronglyTypedFactory =
                    stronglyTypedFactory.WithDeferralBudgetOf maximumNumberOfDeferrals
                let shouldBeTrue =
                    weaklyTypedFactory.MaximumStrength = stronglyTypedFactory.MaximumStrength
                for strength in 0 .. weaklyTypedFactory.MaximumStrength do
                    let testCasesFromWeaklyTypedFactory =
                        [for testCase in weaklyTypedFactory.CreateEnumerable strength do
                            yield testCase]
                    let testCasesFromStronglyTypedFactory =
                        stronglyTypedFactory.CreateEnumerable strength
                        |> List.ofSeq
                    let shouldBeTrue =
                        List.zip testCasesFromWeaklyTypedFactory
                                 testCasesFromStronglyTypedFactory
                        |> List.forall (fun (weaklyTypedTestCase
                                             , stronglyTypedTestCase) ->
                                            unbox weaklyTypedTestCase = stronglyTypedTestCase)
                        // NOTE: 'List.zip' also checks equality of list lengths as a precondition.
                    Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.SmokeTestForProfiling () =
            let inline buildSynthesizingFactory (levelGroupsForEachOfTheTestVariables: seq<#seq<'HasAddition>>) =
                let delegateTypeBuilder =
                    BargainBasement.Memoize (CodeGeneration.NAryDelegateTypeBuilder<'HasAddition>)
                let inline condensation items =
                    List.reduce (+)
                                items
                let nAryCondensationDelegate =
                        CodeGeneration.NAryCondensationDelegateBuilder (Seq.length levelGroupsForEachOfTheTestVariables)
                                                                       delegateTypeBuilder
                                                                       (condensation: List<'HasAddition> -> 'HasAddition)
                let testVariableFactories =
                    Seq.map (fun levelGroup ->
                                TestVariable.Create levelGroup
                                :> IFactory)
                            levelGroupsForEachOfTheTestVariables
                Synthesis.Create (testVariableFactories,
                                  nAryCondensationDelegate)
            let numberOfStringVariables = 3
            let stringSynthesizer = // Progressive Rock, yeah!
                let numberOfStringLevels = 10
                buildSynthesizingFactory (seq
                                            {
                                                for _ in 1 .. numberOfStringVariables do
                                                    yield Seq.init numberOfStringLevels
                                                                   (fun item ->
                                                                        item.ToString ())
                                            })
            let numberOfIntegerVariables = 4
            let integerSynthesizer =
                let numberOfIntegerLevels = 3
                buildSynthesizingFactory (seq
                                            {
                                                for _ in 1 .. numberOfIntegerVariables do
                                                    yield Enumerable.Range(0, numberOfIntegerLevels)
                                            })
            let interleavingSynthesizer =
                Interleaving.Create [stringSynthesizer; integerSynthesizer]
            for combinationStrength in 1 .. max numberOfStringVariables numberOfIntegerVariables do
                let numberOfCases =
                    Seq.length (interleavingSynthesizer.CreateEnumerable combinationStrength
                                :?> seq<_>)
                printfn "Number of cases generated at combination strength %d is: %d" combinationStrength numberOfCases

        [<Test>]
        member this.MeasureScalingOfSynthesis () =
            let numberOfLevels =
                10
            let testVariableFactory =
                TestVariable.Create (seq {
                                            for item in 1 .. numberOfLevels do
                                                yield item
                                         })
            let numberOfVariables =
                35
            let testVariableFactories =
                List.replicate numberOfVariables
                               testVariableFactory
            let factoriesWithIncreasingNumberOfTestVariables =
                List.scan (fun factoryWithSeveralTestVariables
                               testVariableFactory ->
                            Synthesis.Create (testVariableFactory,
                                              factoryWithSeveralTestVariables,
                                              (fun head
                                                   tail ->
                                                    head :: tail)))
                          (Singleton.Create [])
                          testVariableFactories
            let combinationStrength =
                2
            factoriesWithIncreasingNumberOfTestVariables
            |> List.iteri (fun numberOfTestVariables
                               factoryWithSeveralTestVariables ->
                            let numberOfTestCases =
                                factoryWithSeveralTestVariables.ExecuteParameterisedUnitTestForAllTestCases(combinationStrength, Action<List<Int32>>(ignore))
                            printf "Number of test variables: %A, number of test cases: %A.\n"
                                   numberOfTestVariables
                                   numberOfTestCases)


        [<Test>]
        member this.MeasureScalingOfSynthesisOfInterleavesOfGroupsOfSynthesis () =
            let numberOfLevels =
                1
            let testVariableFactory =
                TestVariable.Create (seq {
                                            for item in 1 .. numberOfLevels do
                                                yield item
                                         })
            let numberOfVariables =
                5
            let testVariableFactories =
                List.replicate numberOfVariables
                               testVariableFactory
            let groupOfTestVariableFactories =
                List.fold (fun factoryWithSeveralTestVariables
                               testVariableFactory ->
                            Synthesis.Create (testVariableFactory,
                                              factoryWithSeveralTestVariables,
                                              (fun head
                                                   tail ->
                                                    head :: tail)))
                          (Singleton.Create [])
                          testVariableFactories
            let numberOfInterleaves = 4
            let groupInterleaveFactory =
                Interleaving.Create (seq {
                                            for _ in 1 .. numberOfInterleaves do
                                                yield groupOfTestVariableFactories
                                         })
            let numberOfGroups =
                5
            let interleavedFactories =
                List.replicate numberOfGroups
                               testVariableFactory
            let factoriesWithIncreasingNumberOfGroupInterleaves =
                List.scan (fun factoryWithGroupInterleaves
                               testVariableFactory ->
                            Synthesis.Create (groupInterleaveFactory,
                                              factoryWithGroupInterleaves,
                                              (fun head
                                                   tail ->
                                                    head :: tail)))
                          (Singleton.Create [])
                          interleavedFactories
            let combinationStrength =
                2
            factoriesWithIncreasingNumberOfGroupInterleaves
            |> List.iteri (fun numberOfGroupInterleaves
                               factoryWithGroupInterleaves ->
                            let numberOfTestCases =
                                factoryWithGroupInterleaves.ExecuteParameterisedUnitTestForAllTestCases(combinationStrength, Action<List<List<Int32>>>(ignore))
                            printf "Number of test variables: %A, number of test cases: %A.\n"
                                   (numberOfGroupInterleaves * numberOfVariables)
                                   numberOfTestCases)

        [<Test>]
        member this.StressCopiousUseOfInterleavesMotivatedByBuildingLogicalExpressionTestCases () =
            let maximumNumberOfOperands =
                3

            let rec expressionFactory maximumSubexpressionNesting
                                      expectSuccess =
                let trivialCaseFactory =
                    expectSuccess.ToString ()
                    |> Singleton.Create

                if 1 = maximumSubexpressionNesting
                then
                    trivialCaseFactory
                else
                    let irrefutableSubexpressionFactory =
                        expressionFactory (maximumSubexpressionNesting - 1)

                    let alwaysSucceedsFactory =
                        irrefutableSubexpressionFactory true

                    let alwaysFailsFactory =
                        irrefutableSubexpressionFactory false

                    let refutableSubexpressionFactory =
                        Interleaving.Create [alwaysSucceedsFactory; alwaysFailsFactory]

                    let rec conjunctionOperandsFactory maximumNumberOfOperands =
                        let reduction leadingOperand
                                      remainingOperands =
                            leadingOperand + " AND " + remainingOperands
                        let lastOperandPairFactory =
                            (if expectSuccess
                             then
                                [(alwaysSucceedsFactory, alwaysSucceedsFactory)]
                             else
                                [(alwaysFailsFactory, alwaysFailsFactory); (alwaysFailsFactory, alwaysSucceedsFactory); (alwaysSucceedsFactory, alwaysFailsFactory)])
                            |> List.map (fun (lhsOperandFactory
                                              , rhsOperandFactory) ->
                                              Synthesis.Create(lhsOperandFactory,
                                                               rhsOperandFactory,
                                                               reduction))
                            |> Interleaving.Create
                        if 2 < maximumNumberOfOperands
                        then
                            let remainingOperandsFactory =
                                conjunctionOperandsFactory (maximumNumberOfOperands - 1)
                            let moreThanTwoOperandsFactory =
                                if expectSuccess
                                then
                                    Synthesis.Create (alwaysSucceedsFactory,
                                                      remainingOperandsFactory,
                                                      reduction)
                                else
                                    Synthesis.Create (refutableSubexpressionFactory,
                                                      remainingOperandsFactory,
                                                      reduction)
                            Interleaving.Create [lastOperandPairFactory; moreThanTwoOperandsFactory]
                        else
                            lastOperandPairFactory

                    let rec disjunctionOperandsFactory maximumNumberOfOperands =
                        let reduction leadingOperand
                                      remainingOperands =
                            leadingOperand + " OR " + remainingOperands
                        let lastOperandPairFactory =
                            (if expectSuccess
                             then
                                [(alwaysSucceedsFactory, alwaysSucceedsFactory); (alwaysFailsFactory, alwaysSucceedsFactory); (alwaysSucceedsFactory, alwaysFailsFactory)]
                             else
                                [(alwaysFailsFactory, alwaysFailsFactory)])
                            |> List.map (fun (lhsOperandFactory
                                              , rhsOperandFactory) ->
                                              Synthesis.Create(lhsOperandFactory,
                                                               rhsOperandFactory,
                                                               reduction))
                            |> Interleaving.Create
                        if 2 < maximumNumberOfOperands
                        then
                            let remainingOperandsFactory =
                                disjunctionOperandsFactory (maximumNumberOfOperands - 1)
                            let moreThanTwoOperandsFactory =
                                if expectSuccess
                                then
                                    Synthesis.Create (refutableSubexpressionFactory,
                                                      remainingOperandsFactory,
                                                      reduction)
                                else
                                    Synthesis.Create (alwaysFailsFactory,
                                                      remainingOperandsFactory,
                                                      reduction)
                            Interleaving.Create [lastOperandPairFactory; moreThanTwoOperandsFactory]
                        else
                            lastOperandPairFactory

                    let negationOperandFactory =
                        Synthesis.Create (irrefutableSubexpressionFactory (not expectSuccess),
                                          fun negativeExpression ->
                                            "NOT " + negativeExpression)

                    let bracket subexpressionsFactory =
                        Synthesis.Create (subexpressionsFactory,
                                          fun logicalOperatorWithPluralOperands ->
                                            "(" + logicalOperatorWithPluralOperands + ")")

                    let conjunctionFactory =
                        conjunctionOperandsFactory maximumNumberOfOperands
                        |> bracket

                    let disjunctionFactory =
                        disjunctionOperandsFactory maximumNumberOfOperands
                        |> bracket

                    let negationFactory =
                        bracket negationOperandFactory

                    let singularSubexpressionCases =
                        [trivialCaseFactory; negationFactory]
                    let pluralSubexpressionCases =
                        [conjunctionFactory; disjunctionFactory]

                    (if 1 < maximumNumberOfOperands
                     then
                        List.append singularSubexpressionCases
                                    pluralSubexpressionCases
                     else
                        singularSubexpressionCases)
                    |> Interleaving.Create

            let maximumSubexpressionNesting =
                3

            let expressionFactory =
                [true; false]
                |> List.map (fun expectSuccess ->
                                let expressionFactory =
                                    expressionFactory maximumSubexpressionNesting
                                                      expectSuccess
                                Synthesis.Create (expressionFactory,
                                                  (fun expression ->
                                                    (if expectSuccess
                                                        then
                                                            "Expect success: "
                                                        else
                                                            "Expect failure: ") + expression)))
                |> Interleaving.Create

            let combinationStrength =
                2

            let numberOfTestCases =
                100

            for testCase in expressionFactory.CreateEnumerable combinationStrength
                            |> Seq.take numberOfTestCases do
                printf "%A\n" testCase

        [<Test>]
        member this.AdaptedFsCheckExample() =
            let rec createFactory (): ITypedFactory<_> =
                Interleaving.Create [
                                        Synthesis.Create(TestVariable.Create [0 .. 3], Leaf);
                                        Synthesis.Create(Deferral.Create createFactory, Deferral.Create createFactory, Func<_, _, _>(fun x y -> Branch (x,y)))
                                    ]

            let maximumDepthOfTree =
                3

            let combinationStrength = 2

            let numberOfTestCasesGenerated =
                (createFactory().WithDeferralBudgetOf maximumDepthOfTree).ExecuteParameterisedUnitTestForAllTestCases(combinationStrength, Action<Tree>(fun tree -> printf "Tree: %A\n" tree))

            printf "Exercised %A test cases." numberOfTestCasesGenerated