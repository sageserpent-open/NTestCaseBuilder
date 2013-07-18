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

    type FactoryConstructors<'Factory, 'TestCase> =
        {
            TestCaseEnumerableFactoryWithFilterFrom: 'Factory -> LevelCombinationFilter -> 'Factory
            TestVariableLevelEnumerableFactoryFrom: List<'TestCase> -> 'Factory
            SingletonTestCaseEnumerableFactoryFrom: 'TestCase -> 'Factory
            SynthesizedTestCaseEnumerableFactoryFrom: 'Factory [] -> (List<'TestCase> -> 'TestCase) -> Boolean -> 'Factory
            InterleavedTestCaseEnumerableFactoryFrom: List<'Factory> -> 'Factory
        }

    type PermutationCreation =
        No
      | Yes
      | Randomly


    [<TestFixture>]
    type TestCaseEnumerableFactoryTestFixture () =
        let maximumCombinationStrength = 5
        let maximumNumberOfNonZeroCombinationStrengthSubtrees = 4
        let maximumNumberOfTestLevels = 3
        let maximumNumberOfAncestorFactoriesAllowingFairChanceOfInterleaving = 2;

        let delegateTypeBuilder =
            BargainBasement.Memoize (CodeGeneration.NAryDelegateTypeBuilder<List<TestVariableLevel>>)

        let weaklyTypedFactoryConstructors =
            {
                TestCaseEnumerableFactoryWithFilterFrom =
                    (fun (factory: TestCaseEnumerableFactory)
                         filter ->
                         factory.WithFilter filter)
                TestVariableLevelEnumerableFactoryFrom =
                    (fun testVariableLevels ->
                        TestVariableLevelEnumerableFactory.Create testVariableLevels
                        :> TestCaseEnumerableFactory)
                SingletonTestCaseEnumerableFactoryFrom =
                    (fun testVariableLevel ->
                        SingletonTestCaseEnumerableFactory.Create testVariableLevel
                            :> TestCaseEnumerableFactory)
                SynthesizedTestCaseEnumerableFactoryFrom =
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

                        SynthesizedTestCaseEnumerableFactory.Create (permutedSubtrees,
                                                                     nAryCondensationDelegate))
                InterleavedTestCaseEnumerableFactoryFrom =
                    (fun subtrees ->
                        InterleavedTestCaseEnumerableFactory.Create subtrees)
            }

        let stronglyTypedFactoryConstructors =
            {
                TestCaseEnumerableFactoryWithFilterFrom =
                    (fun (factory: TypedTestCaseEnumerableFactory<_>)
                         filter ->
                         factory.WithFilterTyped filter)
                TestVariableLevelEnumerableFactoryFrom =
                    (fun testVariableLevels ->
                        TestVariableLevelEnumerableFactory.Create testVariableLevels)
                SingletonTestCaseEnumerableFactoryFrom =
                    (fun testVariableLevel ->
                        SingletonTestCaseEnumerableFactory.Create testVariableLevel)
                SynthesizedTestCaseEnumerableFactoryFrom =
                    (fun permutedSubtrees
                         undoShuffleAndConcatenateContributedLevels
                         permuteInputs ->
                        match Array.length permutedSubtrees with
                            1 when not permuteInputs ->
                                SynthesizedTestCaseEnumerableFactory.Create (permutedSubtrees.[0],
                                                                             UnaryDelegate(fun inputTestCase ->
                                                                                            undoShuffleAndConcatenateContributedLevels [inputTestCase]))
                          | 2 when not permuteInputs ->
                                SynthesizedTestCaseEnumerableFactory.Create (permutedSubtrees.[0],
                                                                             permutedSubtrees.[1],
                                                                             BinaryDelegate(fun firstInputTestCase
                                                                                                secondInputTestCase ->
                                                                                                undoShuffleAndConcatenateContributedLevels [firstInputTestCase; secondInputTestCase]))
                          | _ ->
                                SynthesizedTestCaseEnumerableFactory.Create (permutedSubtrees,
                                                                             SequenceCondensation(List.ofSeq >> undoShuffleAndConcatenateContributedLevels),
                                                                             permuteInputs))
                InterleavedTestCaseEnumerableFactoryFrom =
                    (fun subtrees ->
                        InterleavedTestCaseEnumerableFactory.Create subtrees)
            }

        let constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations factoryConstructors
                                                                                       randomBehaviour
                                                                                       allowDuplicatedLevels
                                                                                       allowSynthesisToPermuteInputs =
            // NOTE: the logic that follows is written so that 'allowSynthesisToPermuteInputs' can be set to either true or false without changing
            // the fundamental structure of the generated tree (given all the other parameters are the same, including the underlying mutable state
            // of 'randomBehaviour'). That the tree of factories generated will have the same structure - the only differences being the presence or
            // absence of permutation on some of the synthesizing factories. Also note that one should not infer that the test cases generated by the
            // factories are simply related by permutation - the tests have to take that statement into account.
            let combinationStrength =
                (randomBehaviour: Random).ChooseAnyNumberFromOneTo maximumCombinationStrength
            let rec constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations combinationStrength
                                                                                               testVariableIndexToLevelsMapping
                                                                                               numberOfAncestorFactories
                                                                                               allowEmptyValueNodes
                                                                                               mustHavePermutingSynthesisInTree =
                let indexForLeftmostTestVariable =
                    (testVariableIndexToLevelsMapping: Map<_, _>).Count
                let considerAddingFilterToFactory factory
                                                  feasibleTestVariableCombination
                                                  filters =
                    optionWorkflow
                        {
                            let! feasibleTestVariableCombination =
                                feasibleTestVariableCombination
                            let numberOfTestVariablesInFeasibleCombination =
                                Set.count feasibleTestVariableCombination
                            match randomBehaviour.ChooseAnyNumberFromOneTo 4 with
                                1 when 1 < numberOfTestVariablesInFeasibleCombination ->
                                    let relativeTestVariableIndicesMonitoredByFilter =
                                        let numberOfTestVariablesMonitoredByFilter =
                                            1 + randomBehaviour.ChooseAnyNumberFromOneTo (numberOfTestVariablesInFeasibleCombination - 1)
                                        randomBehaviour.ChooseSeveralOf (feasibleTestVariableCombination,
                                                                         numberOfTestVariablesMonitoredByFilter)
                                        |> Array.map (fun testVariableIndex ->
                                                        testVariableIndex - indexForLeftmostTestVariable)
                                        |> Set.ofArray
                                    let additionalFilterForThisFactory (dictionary: IDictionary<Int32, Int32 * Object>) =
                                        let relevantTestVariableLevelIndices =
                                            seq
                                                {
                                                    for KeyValue (relativeTestVariableIndex
                                                                    , (testVariableLevelIndex
                                                                       , _)) in dictionary do
                                                        if relativeTestVariableIndicesMonitoredByFilter.Contains relativeTestVariableIndex
                                                        then
                                                            yield testVariableLevelIndex
                                                }
                                        relevantTestVariableLevelIndices
                                        |> Seq.groupBy BargainBasement.Identity
                                        |> Seq.forall (function _
                                                                , groupOfTheSameTestVariableLevelIndicesOriginatingFromDifferentTestVariables ->
                                                                    1 = Seq.length groupOfTheSameTestVariableLevelIndicesOriginatingFromDifferentTestVariables)
                                    return factoryConstructors.TestCaseEnumerableFactoryWithFilterFrom factory
                                                                                                       (LevelCombinationFilter additionalFilterForThisFactory)
                                           , (indexForLeftmostTestVariable
                                              , additionalFilterForThisFactory) :: filters
                              | _ ->
                                    return! optionWorkflow.Zero ()
                            }
                        |> BargainBasement.Flip defaultArg
                                                (factory
                                                 , filters)
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
                                then
                                    randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (1 + maximumNumberOfTestLevels)
                                else
                                    randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestLevels
                            let testVariableLevels =
                                let privateRandomBehaviourThatDoesNotPerturbTheMainOne =
                                    Random (randomBehaviour.Next())
                                if allowDuplicatedLevels
                                then
                                        List.replicate levelCountForTestVariableIntroducedHere ()
                                        |> List.scan (fun previousLevel _ ->
                                                        if privateRandomBehaviourThatDoesNotPerturbTheMainOne.HeadsItIs ()
                                                        then previousLevel
                                                        else 1 + previousLevel)
                                                     0
                                        |> List.tail
                                        |> List.map (fun level -> [(indexForLeftmostTestVariable, Some level)])

                                else
                                        [ for level in 1 .. levelCountForTestVariableIntroducedHere do
                                            yield [(indexForLeftmostTestVariable, Some level)] ]
                            factoryConstructors.TestVariableLevelEnumerableFactoryFrom testVariableLevels
                            , if testVariableLevels.IsEmpty
                              then
                                None
                              else
                                Set.singleton indexForLeftmostTestVariable
                                |> Some
                            , if testVariableLevels.IsEmpty
                              then
                                testVariableIndexToLevelsMapping
                              else
                                Map.add indexForLeftmostTestVariable
                                        testVariableLevels
                                        testVariableIndexToLevelsMapping
                            , None
                            , []
                        else
                            let testVariableLevel =
                                [(indexForLeftmostTestVariable, None)]: List<TestVariableLevel>
                            let testVariableLevels =
                                [ testVariableLevel ]
                            factoryConstructors.SingletonTestCaseEnumerableFactoryFrom testVariableLevel
                            , Set.singleton indexForLeftmostTestVariable
                              |> Some
                            , Map.add indexForLeftmostTestVariable
                                      testVariableLevels
                                      testVariableIndexToLevelsMapping
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
                                               permutationExtentsForSubtrees =
                            match triplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice with
                                [] ->
                                    []
                                    , []
                                    , testVariableIndexToLevelsMapping
                                    , permutationExtentsForSubtrees
                                    , []
                              | (subtreeCombinationStrength
                                 , allowEmptyValueNodeInSubtree
                                 , mustHavePermutingSynthesisInTreeInSubtree) :: tailTriplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice ->
                                    let subtree
                                        , testVariableCombinationFromSubtree
                                        , testVariableIndexToLevelsMappingFromSubtree
                                        , permutationExtentFromSubtree
                                        , filtersFromSubtree =
                                        constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations subtreeCombinationStrength
                                                                                                                   testVariableIndexToLevelsMapping
                                                                                                                   (numberOfAncestorFactories + 1)
                                                                                                                   (allowEmptyValueNodeInSubtree
                                                                                                                    && not permuteInputsAndForbidPruning)
                                                                                                                   (mustHavePermutingSynthesisInTreeInSubtree
                                                                                                                    && not permuteInputsAndForbidPruning)
                                    let remainingSubtrees
                                        , testVariableCombinationsFromRemainingSubtrees
                                        , testVariableIndexToLevelsMappingFromRemainingSubtrees
                                        , permutationExtentsFromRemainingSubtrees
                                        , filtersFromRemainingSubtrees =
                                        createSubtrees tailTriplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice
                                                       testVariableIndexToLevelsMappingFromSubtree
                                                       permutationExtentsForSubtrees
                                    subtree :: remainingSubtrees
                                    , testVariableCombinationFromSubtree :: testVariableCombinationsFromRemainingSubtrees
                                    , testVariableIndexToLevelsMappingFromRemainingSubtrees
                                    , permutationExtentFromSubtree :: permutationExtentsFromRemainingSubtrees
                                    , List.append filtersFromSubtree filtersFromRemainingSubtrees
                        let subtrees
                            , testVariableCombinationsFromSubtrees
                            , testVariableIndexToLevelsMappingFromSubtrees
                            , permutationExtentsForSubtrees
                            , filtersFromSubtrees =
                            createSubtrees triplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice
                                           testVariableIndexToLevelsMapping
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
                            randomBehaviour.Shuffle (List.zip subtrees [0 .. subtrees.Length - 1])
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
                            factoryConstructors.SynthesizedTestCaseEnumerableFactoryFrom shuffledSubtrees
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
                            , filters =
                            considerAddingFilterToFactory factory
                                                          testVariableCombination
                                                          filtersFromSubtrees

                        factory
                        , testVariableCombination
                        , testVariableIndexToLevelsMappingFromSubtrees
                        , chosenPermutationExample
                        , filters
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
                                // Rounding down causes the next binding to favour the cause of *not* flipping 'allowEmptyValueNodes'
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
                                               permutationExtentsForSubtrees =
                            match triplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice with
                                [] ->
                                    []
                                    , []
                                    , testVariableIndexToLevelsMapping
                                    , permutationExtentsForSubtrees
                                    , []
                              | (subtreeCombinationStrength
                                 , allowEmptyValueNodeInSubtree
                                 , mustHavePermutingSynthesisInTreeInSubtree) :: tailTriplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice ->
                                    let subtree
                                        , testVariableCombinationFromSubtree
                                        , testVariableIndexToLevelsMappingFromSubtree
                                        , permutationExtentFromSubtree
                                        , filtersFromSubtree =
                                        constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations subtreeCombinationStrength
                                                                                                                   testVariableIndexToLevelsMapping
                                                                                                                   (numberOfAncestorFactories + 1)
                                                                                                                   allowEmptyValueNodeInSubtree
                                                                                                                   mustHavePermutingSynthesisInTreeInSubtree
                                    let remainingSubtrees
                                        , testVariableCombinationsFromRemainingSubtrees
                                        , testVariableIndexToLevelsMappingFromRemainingSubtrees
                                        , permutationExtentsFromRemainingSubtrees
                                        , filtersFromRemainingSubtrees =
                                        createSubtrees tailTriplesOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoiceAndMustHavePermutingSynthesisInTreeChoice
                                                       testVariableIndexToLevelsMappingFromSubtree
                                                       permutationExtentsForSubtrees
                                    subtree :: remainingSubtrees
                                    , testVariableCombinationFromSubtree :: testVariableCombinationsFromRemainingSubtrees
                                    , testVariableIndexToLevelsMappingFromRemainingSubtrees
                                    , permutationExtentFromSubtree :: permutationExtentsFromRemainingSubtrees
                                    , List.append filtersFromSubtree filtersFromRemainingSubtrees
                        let subtrees
                            , testVariableCombinationsFromSubtrees
                            , testVariableIndexToLevelsMappingFromSubtrees
                            , permutationExtentsForSubtrees
                            , filtersFromSubtrees =
                            createSubtrees (List.zip3 combinationStrengths
                                                      whetherToAllowEmptyValueNodeChoices
                                                      mustHavePermutingSynthesisInTreeChoices)
                                           testVariableIndexToLevelsMapping
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
                            factoryConstructors.InterleavedTestCaseEnumerableFactoryFrom subtrees

                        let factory
                            , filters =
                            considerAddingFilterToFactory factory
                                                          chosenTestVariableCombination
                                                          filtersFromSubtrees

                        factory
                        , chosenTestVariableCombination
                        , testVariableIndexToLevelsMappingFromSubtrees
                        , chosenPermutationExample
                        , filtersFromSubtrees

            let testCaseEnumerableFactory
                , testVariableCombination
                , testVariableIndexToLevelsMapping
                , permutationExample
                , filters =
                constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations combinationStrength
                                                                                           Map.empty
                                                                                           0
                                                                                           false
                                                                                           true
            testCaseEnumerableFactory
            , testVariableCombination.Value
            , testVariableIndexToLevelsMapping
            , permutationExample
            , filters

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
                let testCaseEnumerableFactory
                    , testVariableCombination
                    , testVariableIndexToLevelsMapping
                    , permutationExample
                    , filters =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations weaklyTypedFactoryConstructors
                                                                                               randomBehaviour
                                                                                               false
                                                                                               No
                let maximumStrength =
                    randomBehaviour.ChooseAnyNumberFromOneTo testCaseEnumerableFactory.MaximumStrength
                let testVariableCombinationConformingToMaximumStrength =
                    if testVariableCombination.Count <= maximumStrength
                    then testVariableCombination
                    else randomBehaviour.ChooseSeveralOf (testVariableCombination, maximumStrength)
                         |> Set.ofArray
                let testCaseEnumerable =
                    testCaseEnumerableFactory.CreateEnumerable maximumStrength
                testHandoff testCaseEnumerable
                            testVariableCombinationConformingToMaximumStrength
                            testVariableIndexToLevelsMapping
                            filters
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
                let testCaseEnumerableFactory
                    , testVariableCombination
                    , testVariableIndexToLevelsMapping
                    , Some permutationExample
                    , _ =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations stronglyTypedFactoryConstructors
                                                                                               copiedRandomBehaviourOne
                                                                                               false
                                                                                               Yes
                let unpermutedTestCaseEnumerableFactory
                    , _
                    , _
                    , _
                    , _ =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations stronglyTypedFactoryConstructors
                                                                                               copiedRandomBehaviourTwo
                                                                                               false
                                                                                               No

                printf "Permutation example: %A\n" permutationExample

                let maximumStrength =
                    randomBehaviour.ChooseAnyNumberFromOneTo testCaseEnumerableFactory.MaximumStrength
                let testCaseEnumerable =
                    testCaseEnumerableFactory.CreateEnumerable maximumStrength


                let testCases =
                    testCaseEnumerableFactory.CreateTypedEnumerable maximumStrength

                let unpermutedTestCases =
                    unpermutedTestCaseEnumerableFactory.CreateTypedEnumerable maximumStrength

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
                let testCaseEnumerableFactory
                    , _
                    , _
                    , _
                    , _ =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations stronglyTypedFactoryConstructors
                                                                                               randomBehaviour
                                                                                               false
                                                                                               Yes

                let shouldBeTrue =
                    seq
                        {
                            for testCase in testCaseEnumerableFactory.CreateEnumerable 0 do
                                yield testCase :?> IComparable
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
                            filters
                            _
                            _ =
                let testVariableIndexToLevelsMappingForTestVariableCombination =
                    testVariableCombination
                    |> Set.toList
                    |> List.map (fun testVariable ->
                                    testVariable
                                    , Map.find testVariable testVariableIndexToLevelsMapping)

                let combinedFilter testVariableCombination =
                    filters
                    |> List.forall (fun (indexOfLeftmostTestVariableCoveredByFilter
                                         , filterDelegate) ->
                                        testVariableCombination
                                        |> Seq.filter (snd >> Option.isSome)
                                        |> Seq.map (fun (testVariableIndex
                                                         , (Some indexOfTestVariableLevel) as testVariableLevel) ->
                                                        testVariableIndex - indexOfLeftmostTestVariableCoveredByFilter
                                                        , (indexOfTestVariableLevel
                                                           , box [testVariableIndex
                                                                  , testVariableLevel]))
                                        |> Map.ofSeq
                                        :> IDictionary<_, _>
                                        |> filterDelegate)

                let combinationsOfTestLevels =
                    testVariableIndexToLevelsMappingForTestVariableCombination
                    |> List.filter (snd >> List.isEmpty >> not)
                    |> List.map (snd >> List.head)
                    |> List.CrossProduct
                    |> Seq.map Set.ofList
                    |> Set.ofSeq

                let expectedCombinationsOfTestLevels
                    , forbiddenCombinationsOfTestLevels =
                    combinationsOfTestLevels
                    |> Set.partition combinedFilter

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
                let testCaseEnumerableFactory
                    , _
                    , _
                    , _
                    , _ = constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations weaklyTypedFactoryConstructors
                                                                                                     copiedRandomBehaviourOne
                                                                                                     false
                                                                                                     No
                let testCaseEnumerableFactoryBasedOnDuplicateLevels
                    , _
                    , _
                    , _
                    , _ = constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations weaklyTypedFactoryConstructors
                                                                                                     copiedRandomBehaviourTwo
                                                                                                     true
                                                                                                     No
                let maximumStrength =
                    testCaseEnumerableFactory.MaximumStrength
                let shouldBeTrue =
                    maximumStrength = testCaseEnumerableFactoryBasedOnDuplicateLevels.MaximumStrength
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
                let testCaseSequence (testCaseEnumerableFactory: TestCaseEnumerableFactory) =
                    seq {for testCase in testCaseEnumerableFactory.CreateEnumerable(maximumStrength) do
                            yield unbox<List<TestVariableLevel>> testCase}
                let shouldBeTrue = (testCaseSequence testCaseEnumerableFactory
                                    |> bagCombinationsOfTestVariableIndicesDisregardingLevels).UnsequencedEquals
                                     (testCaseSequence testCaseEnumerableFactoryBasedOnDuplicateLevels
                                       |> bagCombinationsOfTestVariableIndicesDisregardingLevels)
                Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestCoverageOfHighestCombinationsOfVariableLevelsInFinalResultsIsOptimal () =
            let randomBehaviour = Random randomBehaviourSeed
            for _ in 1 .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let testCaseEnumerableFactory
                    , testVariableCombination
                    , testVariableIndexToLevelsMapping
                    , _
                    , _ =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations weaklyTypedFactoryConstructors
                                                                                               randomBehaviour
                                                                                               false
                                                                                               No
                let maximumStrength =
                    testCaseEnumerableFactory.MaximumStrength
                let testCaseEnumerable =
                    testCaseEnumerableFactory.CreateEnumerable maximumStrength
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
                let testCaseEnumerableFactory
                    , _
                    , _
                    , _
                    , _ =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations stronglyTypedFactoryConstructors
                                                                                               randomBehaviour
                                                                                               false
                                                                                               Randomly
                let randomStrength =
                    randomBehaviour.ChooseAnyNumberFromOneTo testCaseEnumerableFactory.MaximumStrength
                let randomTestCase =
                    seq
                        {
                            for testCase in testCaseEnumerableFactory.CreateEnumerable randomStrength do
                                yield testCase
                        }
                    |> randomBehaviour.ChooseOneOf
                let failOnSpecificTestCase testCase =
                    if randomTestCase = testCase
                    then
                        raise (LogicErrorException (testCase.ToString ()))
                let shouldBeNone =
                    try testCaseEnumerableFactory.ExecuteParameterisedUnitTestForAllTestCases (randomStrength
                                                                                               , Action<Object>(failOnSpecificTestCase))
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
                            let shouldBeNone =
                                try testCaseEnumerableFactory.ExecuteParameterisedUnitTestForReproducedTestCase (Action<Object>(failOnSpecificTestCase)
                                                                                                                 , reproductionString)
                                    |> Some with
                                    reproducedException ->
                                        let shouldBeTrue =
                                            innerException.Message = reproducedException.Message
                                        Assert.IsTrue shouldBeTrue
                                        None
                            Assert.IsTrue shouldBeNone.IsNone
                            None
                      | _ ->
                            Assert.Fail "No other exception should have been thrown."
                            None
                Assert.IsTrue shouldBeNone.IsNone
                let neverFail =
                    ignore
                try testCaseEnumerableFactory.ExecuteParameterisedUnitTestForAllTestCases (randomStrength
                                                                                           , Action<Object>(neverFail))
                    |> ignore with
                    :? TestCaseReproductionException as testCaseReproductionException ->
                        Assert.Fail "Should not be throwing this specific kind of exception if the unit test succeeded."
                  | _ ->
                        Assert.Fail "No other exception should have been thrown."

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
                let weaklyTypedTestCaseEnumerableFactory
                    , _
                    , _
                    , _
                    , _ =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations weaklyTypedFactoryConstructors
                                                                                               copiedRandomBehaviourOne
                                                                                               false
                                                                                               No
                let stronglyTypedTestCaseEnumerableFactory
                    , _
                    , _
                    , _
                    , _ =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations stronglyTypedFactoryConstructors
                                                                                               copiedRandomBehaviourTwo
                                                                                               false
                                                                                               No
                let shouldBeTrue =
                    weaklyTypedTestCaseEnumerableFactory.MaximumStrength = stronglyTypedTestCaseEnumerableFactory.MaximumStrength
                for strength in 0 .. weaklyTypedTestCaseEnumerableFactory.MaximumStrength do
                    let testCasesFromWeaklyTypedFactory =
                        [for testCase in weaklyTypedTestCaseEnumerableFactory.CreateEnumerable strength do
                            yield testCase]
                    let testCasesFromStronglyTypedFactory =
                        stronglyTypedTestCaseEnumerableFactory.CreateTypedEnumerable strength
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
                                TestVariableLevelEnumerableFactory.Create levelGroup
                                :> TestCaseEnumerableFactory)
                            levelGroupsForEachOfTheTestVariables
                SynthesizedTestCaseEnumerableFactory.Create (testVariableFactories,
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
                InterleavedTestCaseEnumerableFactory.Create [stringSynthesizer; integerSynthesizer]
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
                TestVariableLevelEnumerableFactory.Create (seq {
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
                            SynthesizedTestCaseEnumerableFactory.Create (testVariableFactory,
                                                                         factoryWithSeveralTestVariables,
                                                                         (fun head
                                                                             tail ->
                                                                             head :: tail)))
                          (SingletonTestCaseEnumerableFactory.Create [])
                          testVariableFactories
            let combinationStrength =
                2
            factoriesWithIncreasingNumberOfTestVariables
            |> List.iteri (fun numberOfTestVariables
                               factoryWithSeveralTestVariables ->
                            let numberOfTestCases =
                                factoryWithSeveralTestVariables.ExecuteParameterisedUnitTestForAllTypedTestCases(combinationStrength, Action<_>(ignore))
                            printf "Number of test variables: %A, number of test cases: %A.\n"
                                   numberOfTestVariables
                                   numberOfTestCases)


        [<Test>]
        member this.MeasureScalingOfSynthesisOfInterleavesOfGroupsOfSynthesis () =
            let numberOfLevels =
                1
            let testVariableFactory =
                TestVariableLevelEnumerableFactory.Create (seq {
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
                            SynthesizedTestCaseEnumerableFactory.Create (testVariableFactory,
                                                                         factoryWithSeveralTestVariables,
                                                                         (fun head
                                                                             tail ->
                                                                             head :: tail)))
                          (SingletonTestCaseEnumerableFactory.Create [])
                          testVariableFactories
            let numberOfInterleaves = 4
            let groupInterleaveFactory =
                InterleavedTestCaseEnumerableFactory.Create (seq {
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
                            SynthesizedTestCaseEnumerableFactory.Create (groupInterleaveFactory,
                                                                         factoryWithGroupInterleaves,
                                                                         (fun head
                                                                             tail ->
                                                                             head :: tail)))
                          (SingletonTestCaseEnumerableFactory.Create [])
                          interleavedFactories
            let combinationStrength =
                2
            factoriesWithIncreasingNumberOfGroupInterleaves
            |> List.iteri (fun numberOfGroupInterleaves
                               factoryWithGroupInterleaves ->
                            let numberOfTestCases =
                                factoryWithGroupInterleaves.ExecuteParameterisedUnitTestForAllTypedTestCases(combinationStrength, Action<_>(ignore))
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
                    |> SingletonTestCaseEnumerableFactory.Create

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
                        InterleavedTestCaseEnumerableFactory.Create [alwaysSucceedsFactory; alwaysFailsFactory]

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
                                              SynthesizedTestCaseEnumerableFactory.Create(lhsOperandFactory,
                                                                                          rhsOperandFactory,
                                                                                          reduction))
                            |> InterleavedTestCaseEnumerableFactory.Create
                        if 2 < maximumNumberOfOperands
                        then
                            let remainingOperandsFactory =
                                conjunctionOperandsFactory (maximumNumberOfOperands - 1)
                            let moreThanTwoOperandsFactory =
                                if expectSuccess
                                then
                                    SynthesizedTestCaseEnumerableFactory.Create (alwaysSucceedsFactory,
                                                                                 remainingOperandsFactory,
                                                                                 reduction)
                                else
                                    SynthesizedTestCaseEnumerableFactory.Create (refutableSubexpressionFactory,
                                                                                 remainingOperandsFactory,
                                                                                 reduction)
                            InterleavedTestCaseEnumerableFactory.Create [lastOperandPairFactory; moreThanTwoOperandsFactory]
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
                                              SynthesizedTestCaseEnumerableFactory.Create(lhsOperandFactory,
                                                                                          rhsOperandFactory,
                                                                                          reduction))
                            |> InterleavedTestCaseEnumerableFactory.Create
                        if 2 < maximumNumberOfOperands
                        then
                            let remainingOperandsFactory =
                                disjunctionOperandsFactory (maximumNumberOfOperands - 1)
                            let moreThanTwoOperandsFactory =
                                if expectSuccess
                                then
                                    SynthesizedTestCaseEnumerableFactory.Create (refutableSubexpressionFactory,
                                                                                 remainingOperandsFactory,
                                                                                 reduction)
                                else
                                    SynthesizedTestCaseEnumerableFactory.Create (alwaysFailsFactory,
                                                                                 remainingOperandsFactory,
                                                                                 reduction)
                            InterleavedTestCaseEnumerableFactory.Create [lastOperandPairFactory; moreThanTwoOperandsFactory]
                        else
                            lastOperandPairFactory

                    let negationOperandFactory =
                        SynthesizedTestCaseEnumerableFactory.Create (irrefutableSubexpressionFactory (not expectSuccess),
                                                                     fun negativeExpression ->
                                                                       "NOT " + negativeExpression)

                    let bracket subexpressionsFactory =
                        SynthesizedTestCaseEnumerableFactory.Create (subexpressionsFactory,
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
                    |> InterleavedTestCaseEnumerableFactory.Create

            let maximumSubexpressionNesting =
                3

            let expressionFactory =
                [true; false]
                |> List.map (fun expectSuccess ->
                                let expressionFactory =
                                    expressionFactory maximumSubexpressionNesting
                                                      expectSuccess
                                SynthesizedTestCaseEnumerableFactory.Create (expressionFactory,
                                                                             (fun expression ->
                                                                                (if expectSuccess
                                                                                 then
                                                                                    "Expect success: "
                                                                                 else
                                                                                    "Expect failure: ") + expression)))
                |> InterleavedTestCaseEnumerableFactory.Create

            let combinationStrength =
                2

            let numberOfTestCases =
                100

            for testCase in expressionFactory.CreateTypedEnumerable combinationStrength
                            |> Seq.take numberOfTestCases do
                printf "%A\n" testCase
