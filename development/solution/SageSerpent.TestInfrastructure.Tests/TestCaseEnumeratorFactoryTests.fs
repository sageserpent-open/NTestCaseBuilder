namespace SageSerpent.TestInfrastructure.Tests

    open NUnit.Framework
    
    open SageSerpent.Infrastructure
    open SageSerpent.TestInfrastructure
    open System
    open System.Windows.Forms
    open System.Drawing
    open System.Collections.Generic
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Quotations
    open Microsoft.FSharp.Quotations.Patterns
    open Microsoft.FSharp.Linq.QuotationEvaluation
    open Microsoft.FSharp.Reflection
    open System.Reflection.Emit
    open System.Reflection
    
    type TestVariableLevel =
        UInt32 * UInt32 // Test variable index, value multiplicity of levels for a given test variable.
        
    module CodeGeneration =
        let private ModuleBuilder =
            let assemblyBuilder = AppDomain.CurrentDomain.DefineDynamicAssembly (AssemblyName "SageSerpent.GeneratedAtRuntime",
                                                                                 AssemblyBuilderAccess.Run)
            assemblyBuilder.DefineDynamicModule "ModuleForCodeGeneratedAtRuntime"
            
        let NAryDelegateTypeBuilder<'X> (arity: UInt32) =
            let arityAsString = arity.ToString ()

            let typeAttributes =
                TypeAttributes.Class
                ||| TypeAttributes.Public
                ||| TypeAttributes.Sealed
                ||| TypeAttributes.AnsiClass
                ||| TypeAttributes.AutoClass
                
            let typeBuilder =
                ModuleBuilder.DefineType ("TupleCondensationDelegateTypeFor" + arityAsString + "Arguments",
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
                Array.init (int arity) (fun _ -> typeof<'X>)

            let invokeMethodAttributes =
                MethodAttributes.Public
                ||| MethodAttributes.HideBySig
                ||| MethodAttributes.NewSlot
                ||| MethodAttributes.Virtual

            let invokeMethodName = "Invoke"

            let invokeMethodBuilder =
                typeBuilder.DefineMethod (invokeMethodName,
                                          invokeMethodAttributes,
                                          typeof<'X>,
                                          invokeMethodArgumentTypes)

            invokeMethodBuilder.SetImplementationFlags (MethodImplAttributes.Runtime
                                                        ||| MethodImplAttributes.IL
                                                        ||| MethodImplAttributes.Managed)

            typeBuilder.CreateType ()
                                                    
        let NAryCondensationDelegateBuilder arity
                                            delegateTypeBuilder
                                            (listCondensation: List<'X> -> 'X) =         
            let arguments = [for index in 1u .. arity do
                                yield Var ("argument" + index.ToString ()
                                           , typeof<'X>
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
            BargainBasement.Memoize (CodeGeneration.NAryDelegateTypeBuilder<list<TestVariableLevel>>)
                
        let constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations randomBehaviour =
            let combinationStrength = 
                (randomBehaviour: RandomBehaviour).ChooseAnyNumberFromOneTo maximumCombinationStrength
            let rec constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations combinationStrength
                                                                                               testVariableIndexToCountMapping
                                                                                               numberOfAncestorFactories
                                                                                               allowEmptyValueNodes =
                match randomBehaviour.ChooseAnyNumberFromOneTo 3u with
                    1u when combinationStrength = 1u ->
                        let indexForLeftmostTestVariable =
                            uint32 (testVariableIndexToCountMapping: Map<_, _>).Count
                        let levelCountForTestVariableIntroducedHere =
                            if allowEmptyValueNodes
                            then    randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (1u + maximumNumberOfTestLevels)
                            else    randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestLevels
                        TestVariableLevelEnumerableFactory.Create (seq { for levelNumber in 1u .. levelCountForTestVariableIntroducedHere do
                                                                            yield [(indexForLeftmostTestVariable, levelNumber)] })
                        , Set.singleton indexForLeftmostTestVariable
                        , Map.add indexForLeftmostTestVariable
                                  levelCountForTestVariableIntroducedHere
                                  testVariableIndexToCountMapping
                  | _ when combinationStrength = 0u ->
                        SingletonTestCaseEnumerableFactory.Create ([]: List<TestVariableLevel>)
                        , Set.empty
                        , testVariableIndexToCountMapping 
                  | _ ->
                    if randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (2u * maximumNumberOfAncestorFactoriesAllowingFairChanceOfInterleaving)
                       < max maximumNumberOfAncestorFactoriesAllowingFairChanceOfInterleaving
                             numberOfAncestorFactories
                    then    let numberOfSubtrees =
                                randomBehaviour.ChooseAnyNumberFromOneTo combinationStrength
                            let nonZeroCombinationStrengthsForSubtrees =
                                BargainBasement.PartitionSizeIntoSectionsOfRandomNonZeroLength combinationStrength
                                                                                               numberOfSubtrees
                                                                                               randomBehaviour
                            let numberOfZeroCombinationStrengthSubtrees =
                                randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumNumberOfZeroCombinationStrengthSubtrees
                            
                            let combinationStrengthsForSubtrees =
                                Seq.append nonZeroCombinationStrengthsForSubtrees
                                            (Seq.init (int32 numberOfZeroCombinationStrengthSubtrees) (fun _ -> 0u))
                                |> randomBehaviour.Shuffle
                                |> List.ofArray
                            
                            let rec createSubtrees combinationStrengthsForSubtrees
                                                   testVariableIndexToCountMapping =
                                match combinationStrengthsForSubtrees with
                                    [] ->
                                        []
                                        , []
                                        , testVariableIndexToCountMapping
                                  | headCombinationStrength :: tail ->
                                        let subtree
                                            , testVariableCombinationFromSubtree
                                            , testVariableIndexToCountMappingFromSubtree =
                                            constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations headCombinationStrength
                                                                                                                       testVariableIndexToCountMapping
                                                                                                                       (numberOfAncestorFactories + 1u)
                                                                                                                       allowEmptyValueNodes
                                        let remainingSubtrees
                                            , testVariableCombinationsFromRemainingSubtrees
                                            , testVariableIndexToCountMappingFromRemainingSubtrees =
                                            createSubtrees tail
                                                           testVariableIndexToCountMappingFromSubtree  
                                        subtree :: remainingSubtrees
                                        , testVariableCombinationFromSubtree :: testVariableCombinationsFromRemainingSubtrees
                                        , testVariableIndexToCountMappingFromRemainingSubtrees
                            let subtrees
                                , testVariableCombinationsFromSubtrees
                                , testVariableIndexToCountMappingFromSubtrees =
                                createSubtrees combinationStrengthsForSubtrees
                                               testVariableIndexToCountMapping
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
                                |> Set.unionMany
                                
                            let nAryCondensationDelegate =
                                CodeGeneration.NAryCondensationDelegateBuilder (uint32 permutedSubtrees.Length)
                                                                               delegateTypeBuilder
                                                                               (undoEffectsOfPermutationOnOrderOfAndConcatenateContributedLevels: List<List<TestVariableLevel>> -> List<TestVariableLevel>)
                                                                               
                            SynthesizedTestCaseEnumerableFactory.Create permutedSubtrees
                                                                        nAryCondensationDelegate
                            , testVariableCombination
                            , testVariableIndexToCountMappingFromSubtrees
                    else    let numberOfSubtrees = 
                                randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfNonZeroCombinationStrengthSubtrees
                            let rec chooseCombinationStrengths numberOfSubtrees =
                                if numberOfSubtrees > combinationStrength + 1u
                                      then seq { yield! seq { 0u .. combinationStrength } 
                                                 yield! chooseCombinationStrengths (numberOfSubtrees - (combinationStrength + 1u)) }
                                      else seq { yield! randomBehaviour.ChooseSeveralOf (seq { 0u .. combinationStrength - 1u })
                                                                                        (numberOfSubtrees - 1u)
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
                                                   testVariableIndexToCountMapping =
                                match pairsOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice with
                                    [] ->
                                        []
                                        , []
                                        , testVariableIndexToCountMapping
                                  | headPairOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice :: tailPairsOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice ->
                                        let subtree
                                            , testVariableCombinationFromSubtree
                                            , testVariableIndexToCountMappingFromSubtree =
                                            constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations (fst headPairOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice)
                                                                                                                       testVariableIndexToCountMapping
                                                                                                                       (numberOfAncestorFactories + 1u)
                                                                                                                       (snd headPairOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice)
                                        let remainingSubtrees
                                            , testVariableCombinationsFromRemainingSubtrees
                                            , testVariableIndexToCountMappingFromRemainingSubtrees =
                                            createSubtrees tailPairsOfCombinationStrengthAndWhetherToAllowEmptyValueNodeChoice
                                                           testVariableIndexToCountMappingFromSubtree  
                                        subtree :: remainingSubtrees
                                        , testVariableCombinationFromSubtree :: testVariableCombinationsFromRemainingSubtrees
                                        , testVariableIndexToCountMappingFromRemainingSubtrees
                            let subtrees
                                , testVariableCombinationsFromSubtrees
                                , testVariableIndexToCountMappingFromSubtrees =
                                createSubtrees (List.zip combinationStrengths whetherToAllowEmptyValueNodeChoices)
                                               testVariableIndexToCountMapping
                            let chosenTestVariableCombination =
                                randomBehaviour.ChooseOneOf testVariableCombinationsFromSubtrees
                                
                            InterleavedTestCaseEnumerableFactory.Create subtrees
                            , chosenTestVariableCombination
                            , testVariableIndexToCountMappingFromSubtrees
                                                                                                           
                      | _ ->
                        raise (InternalAssertionViolationException "This case should never occur!")
            constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations combinationStrength
                                                                                       Map.empty
                                                                                       0u
                                                                                       false
            
        let randomBehaviourSeed = 23
        
        let overallTestRepeats = 30u
        
        let isSortedByTestVariableIndex =
            Seq.map fst
            >> BargainBasement.IsSorted
                                
        let createTreesAndHandOffToTest testHandoff =
            let randomBehaviour = RandomBehaviour randomBehaviourSeed
            for _ in 1u .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let testCaseEnumerableFactory
                    , testVariableCombination
                    , testVariableIndexToCountMapping
                    = constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations randomBehaviour
                let maximumStrength =
                    randomBehaviour.ChooseAnyNumberFromOneTo testCaseEnumerableFactory.MaximumStrength
                let testVariableCombinationConformingToMaximumStrength =
                    if uint32 testVariableCombination.Count <= maximumStrength
                    then testVariableCombination
                    else randomBehaviour.ChooseSeveralOf testVariableCombination
                                                         maximumStrength
                         |> Set.ofArray
                let testCaseEnumerable =
                    testCaseEnumerableFactory.CreateEnumerable maximumStrength
                testHandoff testCaseEnumerable
                            testVariableCombinationConformingToMaximumStrength
                            testVariableIndexToCountMapping
                            maximumStrength
                            randomBehaviour
        
        [<Test>]
        member this.TestCorrectOrderingOfLevelsFromDistinctTestVariablesInEachOutputTestCase () =
            let testHandoff (testCaseEnumerable: System.Collections.IEnumerable)
                            testVariableCombination
                            testVariableIndexToCountMapping
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
                            testVariableIndexToCountMapping
                            _
                            _ =
                let testVariableIndexToCountMappingForTestVariableCombination =
                    testVariableCombination
                    |> Set.toList
                    |> List.map (fun testVariable ->
                                    testVariable
                                    , Map.find testVariable testVariableIndexToCountMapping)
                            
                let expectedCombinationsOfTestLevels =
                    testVariableIndexToCountMappingForTestVariableCombination
                    |> List.map (fun (testVariableIndex, numberOfLevels) ->
                                    List.init (int numberOfLevels)
                                              (fun levelNumber ->
                                                    (testVariableIndex, 1u + uint32 levelNumber)))
                    |> BargainBasement.CrossProduct 
                    |> List.map Set.ofList
                    |> Set.ofList
                  
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
        member this.TestCoverageOfHighestCombinationsOfVariableLevelsInFinalResultsIsOptimal () =
            let randomBehaviour = RandomBehaviour randomBehaviourSeed
            for _ in 1u .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let testCaseEnumerableFactory
                    , testVariableCombination
                    , testVariableIndexToCountMapping
                    = constructTestCaseEnumerableFactoryWithAccompanyingTestVariableCombinations randomBehaviour
                let maximumStrength =
                    testCaseEnumerableFactory.MaximumStrength
                let testCaseEnumerable =
                    testCaseEnumerableFactory.CreateEnumerable maximumStrength
                let testCasesOfMaximumStrength =
                    [for testCase in testCaseEnumerable do
                        let testCase = unbox<List<TestVariableLevel>> testCase
                                       |> Set.ofList
                        if uint32 testCase.Count = maximumStrength
                        then yield testCase]
                
                let omittedTestCase =
                    (randomBehaviour: RandomBehaviour).ChooseOneOf testCasesOfMaximumStrength
                    
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
                printf "combinationsCorrespondingToOmittedTestCase.Count: %u, unaccountedCombinationsOfTestLevels.Count %u\n" combinationsCorrespondingToOmittedTestCase.Count
                                                                                                                              unaccountedCombinationsOfTestLevels.Count         
                Assert.IsTrue shouldBeTrue

