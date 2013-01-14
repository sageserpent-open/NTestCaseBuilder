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
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Linq.QuotationEvaluation
    open Microsoft.FSharp.Reflection
    open System.Reflection.Emit
    open System.Reflection

    type TestVariableLevel =
        UInt32 * Option<UInt32> // Test variable index, optional distinguishing index for value.

    module CodeGeneration =
        let private ModuleBuilder =
            let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly (AssemblyName "SageSerpent.GeneratedAtRuntime",
                                                                                 AssemblyBuilderAccess.Run)
            assemblyBuilder.DefineDynamicModule "ModuleForCodeGeneratedAtRuntime"

        let NAryDelegateTypeBuilder<'X> (arity: UInt32) =
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

            let arguments = [for index in 1u .. arity do
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
            TestVariableLevelEnumerableFactoryFrom: List<'TestCase> -> 'Factory
            SingletonTestCaseEnumerableFactoryFrom: 'TestCase -> 'Factory
            SynthesizedTestCaseEnumerableFactoryFrom: 'Factory [] -> (List<'TestCase> -> 'TestCase) -> Boolean -> 'Factory
            InterleavedTestCaseEnumerableFactoryFrom: List<'Factory> -> 'Factory
        }


    [<TestFixture>]
    type TestCaseEnumerableFactoryTestFixture () =
        let maximumCombinationStrength = 6u
        let maximumNumberOfNonZeroCombinationStrengthSubtrees = 4u
        let maximumNumberOfZeroCombinationStrengthSubtrees = 2u
        let maximumNumberOfTestLevels = 3u
        let maximumNumberOfAncestorFactoriesAllowingFairChanceOfInterleaving = 2u;

        let delegateTypeBuilder =
            BargainBasement.Memoize (CodeGeneration.NAryDelegateTypeBuilder<List<TestVariableLevel>>)

        let weaklyTypedFactoryConstructors =
            {
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
                            CodeGeneration.NAryCondensationDelegateBuilder (uint32 permutedSubtrees.Length)
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
            let combinationStrength =
                (randomBehaviour: Random).ChooseAnyNumberFromOneTo maximumCombinationStrength
            let rec constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations combinationStrength
                                                                                               testVariableIndexToLevelsMapping
                                                                                               numberOfAncestorFactories
                                                                                               allowEmptyValueNodes
                                                                                               mustHavePermutingSynthesisInTree
                                                                                               isChildOfAPermutation =
                match randomBehaviour.ChooseAnyNumberFromOneTo 3u with
                    1u when combinationStrength = 1u
                            && not mustHavePermutingSynthesisInTree ->
                        let indexForLeftmostTestVariable =
                            uint32 (testVariableIndexToLevelsMapping: Map<_, _>).Count
                        if randomBehaviour.HeadsItIs ()
                        then
                            let levelCountForTestVariableIntroducedHere =
                                if allowEmptyValueNodes
                                   && not isChildOfAPermutation
                                then
                                    randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (1u + maximumNumberOfTestLevels)
                                else
                                    randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestLevels
                            let testVariableLevels =
                                let privateRandomBehaviourThatDoesNotPerturbTheMainOne =
                                    Random (randomBehaviour.Next())
                                if allowDuplicatedLevels
                                then
                                        List.init (int32 levelCountForTestVariableIntroducedHere) (fun _ -> 0u)
                                        |> List.scan (fun previousLevel _ ->
                                                        if privateRandomBehaviourThatDoesNotPerturbTheMainOne.HeadsItIs ()
                                                        then previousLevel
                                                        else 1u + previousLevel)
                                                    0u
                                        |> List.tail
                                        |> List.map (fun level -> [(indexForLeftmostTestVariable, Some level)])

                                else
                                        [ for level in 1u .. levelCountForTestVariableIntroducedHere do
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
                  | _ ->
                    if randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (2u * maximumNumberOfAncestorFactoriesAllowingFairChanceOfInterleaving)
                       < max maximumNumberOfAncestorFactoriesAllowingFairChanceOfInterleaving
                             numberOfAncestorFactories
                    then
                        let numberOfSubtrees =
                            randomBehaviour.ChooseAnyNumberFromOneTo combinationStrength
                        let nonZeroCombinationStrengthsForSubtrees =
                            BargainBasement.PartitionSizeIntoSectionsOfRandomNonZeroLength combinationStrength
                                                                                           numberOfSubtrees
                                                                                           randomBehaviour
                        let permuteInputs =
                            allowSynthesisToPermuteInputs
                            && randomBehaviour.HeadsItIs ()


                        let rec createSubtrees combinationStrengthsForSubtrees
                                               testVariableIndexToLevelsMapping
                                               permutationExtentsForSubtrees =
                            match combinationStrengthsForSubtrees with
                                [] ->
                                    []
                                    , []
                                    , testVariableIndexToLevelsMapping
                                    , permutationExtentsForSubtrees
                              | headCombinationStrength :: tail ->
                                    let subtree
                                        , testVariableCombinationFromSubtree
                                        , testVariableIndexToLevelsMappingFromSubtree
                                        , permutationExtentFromSubtree =
                                        constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations headCombinationStrength
                                                                                                                   testVariableIndexToLevelsMapping
                                                                                                                   (numberOfAncestorFactories + 1u)
                                                                                                                   allowEmptyValueNodes
                                                                                                                   (mustHavePermutingSynthesisInTree
                                                                                                                    && (not permuteInputs))
                                                                                                                   (isChildOfAPermutation
                                                                                                                    || permuteInputs)
                                    let remainingSubtrees
                                        , testVariableCombinationsFromRemainingSubtrees
                                        , testVariableIndexToLevelsMappingFromRemainingSubtrees
                                        , permutationExtentsFromRemainingSubtrees =
                                        createSubtrees tail
                                                       testVariableIndexToLevelsMappingFromSubtree
                                                       permutationExtentsForSubtrees
                                    subtree :: remainingSubtrees
                                    , testVariableCombinationFromSubtree :: testVariableCombinationsFromRemainingSubtrees
                                    , testVariableIndexToLevelsMappingFromRemainingSubtrees
                                    , permutationExtentFromSubtree :: permutationExtentsFromRemainingSubtrees
                        let subtrees
                            , testVariableCombinationsFromSubtrees
                            , testVariableIndexToLevelsMappingFromSubtrees
                            , permutationExtentsForSubtrees =
                            createSubtrees nonZeroCombinationStrengthsForSubtrees
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

                        let permutationExtent =
                            if permuteInputs
                            then
                                let indexForLeftmostTestVariableOfExtent =
                                    uint32 (testVariableIndexToLevelsMapping: Map<_, _>).Count
                                let onePastIndexForRightmostTestVariableOfExtent =
                                    uint32 (testVariableIndexToLevelsMappingFromSubtrees: Map<_, _>).Count
                                Some ((indexForLeftmostTestVariableOfExtent
                                       , onePastIndexForRightmostTestVariableOfExtent)
                                      , (Array.length shuffledSubtrees
                                         |> uint32))
                            else
                                None

                        let permutationExtentsChoices =
                            permutationExtent :: permutationExtentsForSubtrees
                            |> Option<_>.GetFromMany

                        let chosenPermutationExtent =
                            if permutationExtentsChoices.IsEmpty
                            then
                                None
                            else
                                randomBehaviour.ChooseOneOf permutationExtentsChoices
                                |> Some

                        factoryConstructors.SynthesizedTestCaseEnumerableFactoryFrom shuffledSubtrees
                                                                                     undoShuffleAndConcatenateContributedLevels
                                                                                     permuteInputs
                        , testVariableCombination
                        , testVariableIndexToLevelsMappingFromSubtrees
                        , chosenPermutationExtent
                    else
                        let numberOfSubtrees =
                            randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfNonZeroCombinationStrengthSubtrees
                        let rec chooseCombinationStrengths numberOfSubtrees =
                            if numberOfSubtrees > combinationStrength
                                  then seq { yield! seq { 1u .. combinationStrength }
                                             yield! chooseCombinationStrengths (numberOfSubtrees - combinationStrength) }
                                  else seq { yield! randomBehaviour.ChooseSeveralOf (seq { 1u .. combinationStrength - 1u }, numberOfSubtrees - 1u)
                                             yield combinationStrength }
                        let combinationStrengths =
                            chooseCombinationStrengths numberOfSubtrees
                            |> randomBehaviour.Shuffle
                            |> List.ofArray

                        let whetherToAllowEmptyValueNodeChoices =
                            let halfNumberOfSubtreesRoundedDown =
                                numberOfSubtrees / 2u
                            Seq.append (Seq.init (int32 halfNumberOfSubtreesRoundedDown) (fun _ -> not allowEmptyValueNodes))
                                       (Seq.init (int32 (numberOfSubtrees - halfNumberOfSubtreesRoundedDown)) (fun _ -> allowEmptyValueNodes))
                            |> randomBehaviour.Shuffle
                            |> List.ofArray

                        let rec createSubtrees pairsOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice
                                               testVariableIndexToLevelsMapping
                                               permutationExtentsForSubtrees =
                            match pairsOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice with
                                [] ->
                                    []
                                    , []
                                    , testVariableIndexToLevelsMapping
                                    , permutationExtentsForSubtrees
                              | headPairOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice :: tailPairsOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice ->
                                    let subtree
                                        , testVariableCombinationFromSubtree
                                        , testVariableIndexToLevelsMappingFromSubtree
                                        , permutationExtentFromSubtree =
                                        constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations (fst headPairOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice)
                                                                                                                   testVariableIndexToLevelsMapping
                                                                                                                   (numberOfAncestorFactories + 1u)
                                                                                                                   (snd headPairOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice)
                                                                                                                   mustHavePermutingSynthesisInTree
                                                                                                                   isChildOfAPermutation
                                    let remainingSubtrees
                                        , testVariableCombinationsFromRemainingSubtrees
                                        , testVariableIndexToLevelsMappingFromRemainingSubtrees
                                        , permutationExtentsFromRemainingSubtrees =
                                        createSubtrees tailPairsOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice
                                                       testVariableIndexToLevelsMappingFromSubtree
                                                       permutationExtentsForSubtrees
                                    subtree :: remainingSubtrees
                                    , testVariableCombinationFromSubtree :: testVariableCombinationsFromRemainingSubtrees
                                    , testVariableIndexToLevelsMappingFromRemainingSubtrees
                                    , permutationExtentFromSubtree :: permutationExtentsFromRemainingSubtrees
                        let subtrees
                            , testVariableCombinationsFromSubtrees
                            , testVariableIndexToLevelsMappingFromSubtrees
                            , permutationExtentsForSubtrees =
                            createSubtrees (List.zip combinationStrengths whetherToAllowEmptyValueNodeChoices)
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

                        let permutationExtentsChoices =
                            permutationExtentsForSubtrees
                            |> Option<_>.GetFromMany

                        let chosenPermutationExtent =
                            if permutationExtentsChoices.IsEmpty
                            then
                                None
                            else
                                randomBehaviour.ChooseOneOf permutationExtentsChoices
                                |> Some

                        factoryConstructors.InterleavedTestCaseEnumerableFactoryFrom subtrees
                        , chosenTestVariableCombination
                        , testVariableIndexToLevelsMappingFromSubtrees
                        , chosenPermutationExtent

            let testCaseEnumerableFactory
                , testVariableCombination
                , testVariableIndexToLevelsMapping
                , permutationExtent =
                constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations combinationStrength
                                                                                           Map.empty
                                                                                           0u
                                                                                           false
                                                                                           allowSynthesisToPermuteInputs
                                                                                           false
            testCaseEnumerableFactory
            , testVariableCombination.Value
            , testVariableIndexToLevelsMapping
            , permutationExtent

        let randomBehaviourSeed = 23

        let overallTestRepeats = 100u

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
            for _ in 1u .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let testCaseEnumerableFactory
                    , testVariableCombination
                    , testVariableIndexToLevelsMapping
                    , permutationExtent =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations weaklyTypedFactoryConstructors
                                                                                               randomBehaviour
                                                                                               false
                                                                                               false
                let maximumStrength =
                    randomBehaviour.ChooseAnyNumberFromOneTo testCaseEnumerableFactory.MaximumStrength
                let testVariableCombinationConformingToMaximumStrength =
                    if uint32 testVariableCombination.Count <= maximumStrength
                    then testVariableCombination
                    else randomBehaviour.ChooseSeveralOf (testVariableCombination, maximumStrength)
                         |> Set.ofArray
                let testCaseEnumerable =
                    testCaseEnumerableFactory.CreateEnumerable maximumStrength
                testHandoff testCaseEnumerable
                            testVariableCombinationConformingToMaximumStrength
                            testVariableIndexToLevelsMapping
                            maximumStrength
                            randomBehaviour

        [<Test>]
        member this.TestCoverageOfImplicitPermutationsIsComplete () =
            let randomBehaviour = Random randomBehaviourSeed
            for _ in 1u .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let testCaseEnumerableFactory
                    , testVariableCombination
                    , testVariableIndexToLevelsMapping
                    , Some permutationExtent =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations stronglyTypedFactoryConstructors
                                                                                               randomBehaviour
                                                                                               false
                                                                                               true
                let maximumStrength =
                    randomBehaviour.ChooseAnyNumberFromOneTo testCaseEnumerableFactory.MaximumStrength
                let testCaseEnumerable =
                    testCaseEnumerableFactory.CreateEnumerable maximumStrength


                let testCases =
                    testCaseEnumerableFactory.CreateTypedEnumerable maximumStrength

                let lhsTestVariableFromPermutationExtent =
                    (fst >> fst) permutationExtent

                let onePastRhsTestVariableFromPermutationExtent =
                    (fst >> snd) permutationExtent

                let slicedTestCases =
                    testCases
                    |> Seq.map (List.filter (fun (testVariableIndex
                                                  , _) ->
                                                lhsTestVariableFromPermutationExtent <= testVariableIndex
                                                && testVariableIndex < onePastRhsTestVariableFromPermutationExtent))
                    |> Set.ofSeq   // Eliminate duplicates caused by slicing throwing away distinguishing state.
                    |> Set.remove List.empty

                let chosenSlicedTestCaseToSelectTestVariables =
                    randomBehaviour.ChooseOneOf slicedTestCases

                let numberOfTestVariablesInSliceThatPermutationCoverageIsGuaranteedFor =
                    let numberOfTestVariablesCoveredByPermutationExtent =
                        onePastRhsTestVariableFromPermutationExtent - lhsTestVariableFromPermutationExtent
                    (maximumStrength
                     |> max 1u) - 1u    // Subtract one to make room for the implicit permutation
                                        // test variable - this is what this test is checking.
                    |> min numberOfTestVariablesCoveredByPermutationExtent

                let chosenTestVariablesToCheckPermutationCoverageFor =
                    let testVariablesToChooseFrom =
                        chosenSlicedTestCaseToSelectTestVariables
                        |> List.map fst
                    if numberOfTestVariablesInSliceThatPermutationCoverageIsGuaranteedFor
                       < (List.length chosenSlicedTestCaseToSelectTestVariables
                          |> uint32)
                    then
                        randomBehaviour.ChooseSeveralOf (testVariablesToChooseFrom,
                                                         numberOfTestVariablesInSliceThatPermutationCoverageIsGuaranteedFor)
                        :> seq<_>
                    else
                        testVariablesToChooseFrom
                        :> seq<_>
                    |> Set.ofSeq

                let finelySlicedTestCases =
                    slicedTestCases
                    |> Set.map (List.filter (fun (testVariableIndex
                                                  , _) ->
                                                chosenTestVariablesToCheckPermutationCoverageFor.Contains testVariableIndex))
                    |> Set.remove List.empty
                    // Again, more elimination of duplicates caused by throwing away distinguishing state.

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

                let numberOfPermutationsExpected =
                    snd permutationExtent
                    |> BargainBasement.Factorial

                for _
                    , group in finelySlicedTestCasesGroupedByTestLevelCombinations
                               |> Map.toSeq do
                    let numberOfPermutationsOfFineSlice =
                        Set.count group
                        |> uint32
                    let shouldBeTrue =
                        numberOfPermutationsOfFineSlice >= numberOfPermutationsExpected // NOTE: this is possible, because we may be permuting
                                                                                        // subtrees that themselves contain finer-grained permutations.
                    Assert.IsTrue shouldBeTrue
                    let shouldBeTrue =
                        0u = numberOfPermutationsOfFineSlice % numberOfPermutationsExpected
                    Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestThatZeroStrengthResultsInAnEmptyTestCaseSequence () =
            let randomBehaviour = Random randomBehaviourSeed
            for _ in 1u .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let sharedSeed =
                    randomBehaviour.Next()
                let testCaseEnumerableFactory
                    , _
                    , _
                    , _ =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations stronglyTypedFactoryConstructors
                                                                                               randomBehaviour
                                                                                               false
                                                                                               true

                let shouldBeTrue =
                    seq
                        {
                            for testCase in testCaseEnumerableFactory.CreateEnumerable 0u do
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
                            _
                            _ =
                let testVariableIndexToLevelsMappingForTestVariableCombination =
                    testVariableCombination
                    |> Set.toList
                    |> List.map (fun testVariable ->
                                    testVariable
                                    , Map.find testVariable testVariableIndexToLevelsMapping)

                let expectedCombinationsOfTestLevels =
                    testVariableIndexToLevelsMappingForTestVariableCombination
                    |> List.filter (snd >> List.isEmpty >> not)
                    |> List.map (snd >> List.head)
                    |> List.CrossProduct
                    |> Seq.map Set.ofList
                    |> Set.ofSeq

                let testCases =
                    seq {for testCase in testCaseEnumerable do
                            yield unbox<List<TestVariableLevel>> testCase
                                  |> Set.ofList}

                let unaccountedCombinationsOfTestLevels =
                    Seq.fold (fun expectedCombinationsOfTestLevels
                                  testCase ->
                                    let expectedCombinationsOfTestLevelsSatisfiedByTestCase =
                                        expectedCombinationsOfTestLevels
                                        |> Set.filter (fun testLevelCombination ->
                                                            (testCase: Set<_>).IsSupersetOf testLevelCombination)
                                    expectedCombinationsOfTestLevels - expectedCombinationsOfTestLevelsSatisfiedByTestCase)
                             expectedCombinationsOfTestLevels
                             testCases

                let shouldBeTrue =
                    unaccountedCombinationsOfTestLevels
                    |> Set.count
                     = 0
                Assert.IsTrue shouldBeTrue

            createTreesAndHandOffToTest testHandoff

        [<Test>]
        member this.TestThatDuplicatedLevelValuesAreTreatedAsDistinctWhenMakingTestCases () =
            let randomBehaviour = Random randomBehaviourSeed
            for _ in 1u .. overallTestRepeats do
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
                    = constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations weaklyTypedFactoryConstructors
                                                                                                 copiedRandomBehaviourOne
                                                                                                 false
                                                                                                 false
                let testCaseEnumerableFactoryBasedOnDuplicateLevels
                    , _
                    , _
                    , _
                    = constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations weaklyTypedFactoryConstructors
                                                                                                 copiedRandomBehaviourTwo
                                                                                                 true
                                                                                                 false
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
            for _ in 1u .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let testCaseEnumerableFactory
                    , testVariableCombination
                    , testVariableIndexToLevelsMapping
                    , _ =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations weaklyTypedFactoryConstructors
                                                                                               randomBehaviour
                                                                                               false
                                                                                               false
                let maximumStrength =
                    testCaseEnumerableFactory.MaximumStrength
                let testCaseEnumerable =
                    testCaseEnumerableFactory.CreateEnumerable maximumStrength
                let testCasesOfMaximumStrength =
                    [for testCase in testCaseEnumerable do
                        let testCase =
                            unbox<List<TestVariableLevel>> testCase
                            |> Set.ofList
                        if uint32 testCase.Count = maximumStrength
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
                    |> Set.count
                     > 0
                Assert.IsTrue shouldBeTrue

        [<Test>]
        member this.TestReproductionOfSpecificTestCases () =
            let randomBehaviour = Random randomBehaviourSeed
            for _ in 1u .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let testCaseEnumerableFactory
                    , _
                    , _
                    , _ =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations stronglyTypedFactoryConstructors
                                                                                               randomBehaviour
                                                                                               false
                                                                                               true
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
            for _ in 1u .. overallTestRepeats do
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
                    , _ =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations weaklyTypedFactoryConstructors
                                                                                               copiedRandomBehaviourOne
                                                                                               false
                                                                                               false
                let stronglyTypedTestCaseEnumerableFactory
                    , _
                    , _
                    , _ =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations stronglyTypedFactoryConstructors
                                                                                               copiedRandomBehaviourTwo
                                                                                               false
                                                                                               false
                let shouldBeTrue =
                    weaklyTypedTestCaseEnumerableFactory.MaximumStrength = stronglyTypedTestCaseEnumerableFactory.MaximumStrength
                for strength in 0u .. weaklyTypedTestCaseEnumerableFactory.MaximumStrength do
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
                        CodeGeneration.NAryCondensationDelegateBuilder (Seq.length levelGroupsForEachOfTheTestVariables
                                                                        |> uint32)
                                                                       delegateTypeBuilder
                                                                       (condensation: List<'HasAddition> -> 'HasAddition)
                let testVariableFactories =
                    Seq.map (fun levelGroup ->
                                TestVariableLevelEnumerableFactory.Create levelGroup
                                :> TestCaseEnumerableFactory)
                            levelGroupsForEachOfTheTestVariables
                SynthesizedTestCaseEnumerableFactory.Create (testVariableFactories,
                                                             nAryCondensationDelegate)
            let numberOfStringVariables = 3u
            let stringSynthesizer = // Progressive Rock, yeah!
                let numberOfStringLevels = 10u
                buildSynthesizingFactory (seq
                                            {
                                                for _ in 1u .. numberOfStringVariables do
                                                    yield Seq.init (numberOfStringLevels
                                                                    |> int32)
                                                                   (fun item ->
                                                                        item.ToString ())
                                            })
            let numberOfIntegerVariables = 4u
            let integerSynthesizer =
                let numberOfIntegerLevels = 3u
                buildSynthesizingFactory (seq
                                            {
                                                for _ in 1u .. numberOfIntegerVariables do
                                                    yield Seq.init (numberOfIntegerLevels
                                                                    |> int32)
                                                                   (fun item ->
                                                                        item)
                                            })
            let interleavingSynthesizer =
                InterleavedTestCaseEnumerableFactory.Create [stringSynthesizer; integerSynthesizer]
            for combinationStrength in 1u .. max numberOfStringVariables numberOfIntegerVariables do
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
                List.init numberOfVariables
                          (fun _ ->
                            testVariableFactory)
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
                2u
            factoriesWithIncreasingNumberOfTestVariables
            |> List.iteri (fun numberOfTestVariables
                               factoryWithSeveralTestVariables ->
                            let numberOfTestCases =
                                factoryWithSeveralTestVariables.ExecuteParameterisedUnitTestForAllTypedTestCases(combinationStrength, Action<_>(ignore))
                            printf "Number of test variables: %A, number of test cases: %A.\n"
                                   numberOfTestVariables
                                   ((int32) numberOfTestCases))


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
                List.init numberOfVariables
                          (fun _ ->
                            testVariableFactory)
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
                List.init numberOfGroups
                          (fun _ ->
                            testVariableFactory)
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
                2u
            factoriesWithIncreasingNumberOfGroupInterleaves
            |> List.iteri (fun numberOfGroupInterleaves
                               factoryWithGroupInterleaves ->
                            let numberOfTestCases =
                                factoryWithGroupInterleaves.ExecuteParameterisedUnitTestForAllTypedTestCases(combinationStrength, Action<_>(ignore))
                            printf "Number of test variables: %A, number of test cases: %A.\n"
                                   (numberOfGroupInterleaves * numberOfVariables)
                                   ((int32) numberOfTestCases))