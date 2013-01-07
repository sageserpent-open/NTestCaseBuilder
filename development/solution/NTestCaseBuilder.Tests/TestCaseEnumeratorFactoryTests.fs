namespace NTestCaseBuilder.Tests

    open NUnit.Framework

    open SageSerpent.Infrastructure
    open SageSerpent.Infrastructure.ListExtensions
    open SageSerpent.Infrastructure.RandomExtensions
    open SageSerpent.Infrastructure.IEnumerableExtensions
    open SageSerpent.Infrastructure.OptionWorkflow
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

    [<TestFixture>]
    type TestCaseEnumerableFactoryTestFixture () =
        let maximumCombinationStrength = 6u
        let maximumNumberOfNonZeroCombinationStrengthSubtrees = 4u
        let maximumNumberOfZeroCombinationStrengthSubtrees = 2u
        let maximumNumberOfTestLevels = 3u
        let maximumNumberOfAncestorFactoriesAllowingFairChanceOfInterleaving = 2u;

        let delegateTypeBuilder =
            BargainBasement.Memoize (CodeGeneration.NAryDelegateTypeBuilder<List<TestVariableLevel>>)

        let constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations randomBehaviour
                                                                                       allowDuplicatedLevels =
            let combinationStrength =
                (randomBehaviour: Random).ChooseAnyNumberFromOneTo maximumCombinationStrength
            let rec constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations combinationStrength
                                                                                               testVariableIndexToLevelsMapping
                                                                                               numberOfAncestorFactories
                                                                                               allowEmptyValueNodes =
                match randomBehaviour.ChooseAnyNumberFromOneTo 3u with
                    1u when combinationStrength = 1u ->
                        let indexForLeftmostTestVariable =
                            uint32 (testVariableIndexToLevelsMapping: Map<_, _>).Count
                        if randomBehaviour.HeadsItIs ()
                        then
                            let levelCountForTestVariableIntroducedHere =
                                if allowEmptyValueNodes
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
                            TestVariableLevelEnumerableFactory.Create testVariableLevels
                            :> TestCaseEnumerableFactory
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
                        else
                            let testVariableLevel =
                                [(indexForLeftmostTestVariable, None)]: List<TestVariableLevel>
                            let testVariableLevels =
                                [ testVariableLevel ]
                            SingletonTestCaseEnumerableFactory.Create testVariableLevel
                            :> TestCaseEnumerableFactory
                            , Set.singleton indexForLeftmostTestVariable
                              |> Some
                            , Map.add indexForLeftmostTestVariable
                                      testVariableLevels
                                      testVariableIndexToLevelsMapping
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

                        let rec createSubtrees combinationStrengthsForSubtrees
                                               testVariableIndexToLevelsMapping =
                            match combinationStrengthsForSubtrees with
                                [] ->
                                    []
                                    , []
                                    , testVariableIndexToLevelsMapping
                              | headCombinationStrength :: tail ->
                                    let subtree
                                        , testVariableCombinationFromSubtree
                                        , testVariableIndexToLevelsMappingFromSubtree =
                                        constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations headCombinationStrength
                                                                                                                   testVariableIndexToLevelsMapping
                                                                                                                   (numberOfAncestorFactories + 1u)
                                                                                                                   allowEmptyValueNodes
                                    let remainingSubtrees
                                        , testVariableCombinationsFromRemainingSubtrees
                                        , testVariableIndexToLevelsMappingFromRemainingSubtrees =
                                        createSubtrees tail
                                                       testVariableIndexToLevelsMappingFromSubtree
                                    subtree :: remainingSubtrees
                                    , testVariableCombinationFromSubtree :: testVariableCombinationsFromRemainingSubtrees
                                    , testVariableIndexToLevelsMappingFromRemainingSubtrees
                        let subtrees
                            , testVariableCombinationsFromSubtrees
                            , testVariableIndexToLevelsMappingFromSubtrees =
                            createSubtrees nonZeroCombinationStrengthsForSubtrees
                                           testVariableIndexToLevelsMapping
                        let permutedSubtrees
                            , inversePermutation =
                            randomBehaviour.Shuffle (List.zip subtrees [0 .. subtrees.Length - 1])
                            |> Array.unzip

                        let undoEffectsOfPermutationOnOrderOfAndConcatenateContributedLevels listOfLevels =
                            let inversePermutedListsOfLevels =
                                listOfLevels
                                |> List.permute (fun inputIndex -> inversePermutation.[inputIndex])
                            List.concat inversePermutedListsOfLevels

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

                        let nAryCondensationDelegate =
                            CodeGeneration.NAryCondensationDelegateBuilder (uint32 permutedSubtrees.Length)
                                                                           delegateTypeBuilder
                                                                           (undoEffectsOfPermutationOnOrderOfAndConcatenateContributedLevels: List<List<TestVariableLevel>> -> List<TestVariableLevel>)

                        SynthesizedTestCaseEnumerableFactory.Create (permutedSubtrees,
                                                                     nAryCondensationDelegate)
                        , testVariableCombination
                        , testVariableIndexToLevelsMappingFromSubtrees
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
                                               testVariableIndexToLevelsMapping =
                            match pairsOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice with
                                [] ->
                                    []
                                    , []
                                    , testVariableIndexToLevelsMapping
                              | headPairOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice :: tailPairsOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice ->
                                    let subtree
                                        , testVariableCombinationFromSubtree
                                        , testVariableIndexToLevelsMappingFromSubtree =
                                        constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations (fst headPairOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice)
                                                                                                                   testVariableIndexToLevelsMapping
                                                                                                                   (numberOfAncestorFactories + 1u)
                                                                                                                   (snd headPairOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice)
                                    let remainingSubtrees
                                        , testVariableCombinationsFromRemainingSubtrees
                                        , testVariableIndexToLevelsMappingFromRemainingSubtrees =
                                        createSubtrees tailPairsOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice
                                                       testVariableIndexToLevelsMappingFromSubtree
                                    subtree :: remainingSubtrees
                                    , testVariableCombinationFromSubtree :: testVariableCombinationsFromRemainingSubtrees
                                    , testVariableIndexToLevelsMappingFromRemainingSubtrees
                        let subtrees
                            , testVariableCombinationsFromSubtrees
                            , testVariableIndexToLevelsMappingFromSubtrees =
                            createSubtrees (List.zip combinationStrengths whetherToAllowEmptyValueNodeChoices)
                                           testVariableIndexToLevelsMapping
                        let achievableTestVariableCombinationsFromSubtrees =
                            testVariableCombinationsFromSubtrees
                            |> List.filter Option.isSome
                        let chosenTestVariableCombination =
                            if List.isEmpty achievableTestVariableCombinationsFromSubtrees
                            then
                                None
                            else
                                randomBehaviour.ChooseOneOf achievableTestVariableCombinationsFromSubtrees

                        InterleavedTestCaseEnumerableFactory.Create subtrees
                        , chosenTestVariableCombination
                        , testVariableIndexToLevelsMappingFromSubtrees

            let testCaseEnumerableFactory
                , testVariableCombination
                , testVariableIndexToLevelsMapping =
                constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations combinationStrength
                                                                                           Map.empty
                                                                                           0u
                                                                                           false
            testCaseEnumerableFactory
            , testVariableCombination.Value
            , testVariableIndexToLevelsMapping

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
                    , testVariableIndexToLevelsMapping =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations randomBehaviour false
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
                    = constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations copiedRandomBehaviourOne false
                let testCaseEnumerableFactoryBasedOnDuplicateLevels
                    , _
                    , _
                    = constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations copiedRandomBehaviourTwo true
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
                    , testVariableIndexToLevelsMapping =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations randomBehaviour false
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
                    , _ =
                    constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations randomBehaviour false
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
        member this.SmokeTestForProfiling () =
            let inline buildSynthesizingFactory (levelGroupsForEachOfTheTestVariables: seq<#seq<'HasAddition>>) =
                let delegateTypeBuilder =
                    BargainBasement.Memoize (CodeGeneration.NAryDelegateTypeBuilder<'HasAddition>)
                let inline condensation items =
                    List.reduce (fun left right ->
                                        left + right)
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
                25
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