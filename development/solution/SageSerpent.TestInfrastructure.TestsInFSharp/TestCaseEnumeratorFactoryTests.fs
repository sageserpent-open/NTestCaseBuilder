#light

#nowarn "57"

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
                                yield Var ("argument" + arity.ToString ()
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
                List.fold_right (fun argument
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
    type TestCaseEnumeratorFactoryTestFixture () =
        let mutable delegatesCache = Map.empty
        
        let maximumCombinationStrength = 5u
        let maximumNumberOfSubtrees = 4u
        let maximumNumberOfTestLevels = 3u
        let maximumNumberOfAncestorFactoriesAllowingFairChanceOfInterleaving = 2u;
        
        let delegateTypeBuilder =
            BargainBasement.Memoize (CodeGeneration.NAryDelegateTypeBuilder<list<TestVariableLevel>>)
                
        let constructTestCaseEnumeratorFactoryWithAccompanyingTestVariableCombinations randomBehaviour =
            let combinationStrength = 
                (randomBehaviour: RandomBehaviour).ChooseAnyNumberFromOneTo maximumCombinationStrength
            let rec constructTestCaseEnumeratorFactoryWithAccompanyingTestVariableCombinations combinationStrength
                                                                                               testVariableIndexToCountMapping
                                                                                               numberOfAncestorFactories =
                match randomBehaviour.ChooseAnyNumberFromOneTo 3u with
                    1u when combinationStrength = 1u ->
                        let indexForLeftmostTestVariable =
                            uint32 (testVariableIndexToCountMapping: Map<_, _>).Count
                        let levelCountForTestVariableIntroducedHere =
                            randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestLevels
                        (TestVariableLevelEnumeratorFactory (seq { for levelNumber in 1u .. levelCountForTestVariableIntroducedHere do
                                                                    yield box [(indexForLeftmostTestVariable, levelNumber)] })
                         :> ITestCaseEnumeratorFactory)
                        , Set.singleton indexForLeftmostTestVariable
                        , Map.add indexForLeftmostTestVariable
                                  levelCountForTestVariableIntroducedHere
                                  testVariableIndexToCountMapping
                  | _ ->
                    if randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (2u * maximumNumberOfAncestorFactoriesAllowingFairChanceOfInterleaving)
                       < max maximumNumberOfAncestorFactoriesAllowingFairChanceOfInterleaving
                             numberOfAncestorFactories
                    then    let numberOfSubtrees =
                                randomBehaviour.ChooseAnyNumberFromOneTo combinationStrength
                            let combinationStrengthsForSubtrees =
                                BargainBasement.PartitionSizeIntoSectionsOfRandomNonZeroLength combinationStrength
                                                                                               numberOfSubtrees
                                                                                               randomBehaviour
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
                                            constructTestCaseEnumeratorFactoryWithAccompanyingTestVariableCombinations headCombinationStrength
                                                                                                                       testVariableIndexToCountMapping
                                                                                                                       (numberOfAncestorFactories + 1u)
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
                                |> Set.union_all
                                
                            let nAryCondensationDelegate =
                                CodeGeneration.NAryCondensationDelegateBuilder (uint32 permutedSubtrees.Length)
                                                                               delegateTypeBuilder
                                                                               (undoEffectsOfPermutationOnOrderOfAndConcatenateContributedLevels: List<List<TestVariableLevel> > -> List<TestVariableLevel>)
                                
                            (SynthesizedTestCaseEnumeratorFactory (permutedSubtrees,
                                                                   nAryCondensationDelegate)
                             :> ITestCaseEnumeratorFactory)
                            , testVariableCombination
                            , testVariableIndexToCountMappingFromSubtrees
                    else    let numberOfSubtrees = 
                                randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfSubtrees
                            let rec chooseCombinationStrengths numberOfSubtrees =
                                if numberOfSubtrees > combinationStrength
                                      then seq { yield! seq { 1u .. combinationStrength } 
                                                 yield! chooseCombinationStrengths (numberOfSubtrees - combinationStrength) }
                                      else seq { yield! randomBehaviour.ChooseSeveralOf (seq { 1u .. combinationStrength - 1u })
                                                                                        (numberOfSubtrees - 1u)
                                                 yield combinationStrength }
                            let combinationStrengths =
                                chooseCombinationStrengths numberOfSubtrees
                                |> randomBehaviour.Shuffle
                                |> List.of_array
                                
                            let rec createSubtrees combinationStrengths
                                                   testVariableIndexToCountMapping =
                                match combinationStrengths with
                                    [] ->
                                        []
                                        , []
                                        , testVariableIndexToCountMapping
                                  | headCombinationStrength :: tailCombinationStrengths ->
                                        let subtree
                                            , testVariableCombinationFromSubtree
                                            , testVariableIndexToCountMappingFromSubtree =
                                            constructTestCaseEnumeratorFactoryWithAccompanyingTestVariableCombinations headCombinationStrength
                                                                                                                       testVariableIndexToCountMapping
                                                                                                                       (numberOfAncestorFactories + 1u)
                                        let remainingSubtrees
                                            , testVariableCombinationsFromRemainingSubtrees
                                            , testVariableIndexToCountMappingFromRemainingSubtrees =
                                            createSubtrees tailCombinationStrengths
                                                           testVariableIndexToCountMappingFromSubtree  
                                        subtree :: remainingSubtrees
                                        , testVariableCombinationFromSubtree :: testVariableCombinationsFromRemainingSubtrees
                                        , testVariableIndexToCountMappingFromRemainingSubtrees
                            let subtrees
                                , testVariableCombinationsFromSubtrees
                                , testVariableIndexToCountMappingFromSubtrees =
                                createSubtrees combinationStrengths
                                               testVariableIndexToCountMapping
                            let chosenTestVariableCombination =
                                testVariableCombinationsFromSubtrees.[int32 (randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (uint32 testVariableCombinationsFromSubtrees.Length))]
                                
                            (InterleavedTestCaseEnumeratorFactory subtrees
                             :> ITestCaseEnumeratorFactory)
                            , chosenTestVariableCombination
                            , testVariableIndexToCountMappingFromSubtrees
                                                                                                           
                      | _ ->
                        raise (InternalAssertionViolationException "This case should never occur!")
            constructTestCaseEnumeratorFactoryWithAccompanyingTestVariableCombinations combinationStrength
                                                                                       Map.empty
                                                                                       0u
            
        let randomBehaviourSeed = 23
        
        let overallTestRepeats = 30u
        
        let isSortedByTestVariableIndex testVariableLevels =
            testVariableLevels
            |> Seq.map fst
            |> Seq.pairwise
            |> Seq.for_all (fun (lhs
                                 , rhs)
                                -> lhs < rhs)
        
        [<Test>]
        member this.TestCorrectOrderingOfLevelsFromDistinctTestVariablesInEachOutputTestCase () =
            let randomBehaviour = RandomBehaviour randomBehaviourSeed
            for _ in 1u .. overallTestRepeats do
                printf "\n\n\n******************************\n\n\n"
                let testCaseEnumeratorFactory
                    , _
                    , testVariableIndexToCountMapping
                    = constructTestCaseEnumeratorFactoryWithAccompanyingTestVariableCombinations randomBehaviour
                let numberOfTestVariables =
                    uint32 testVariableIndexToCountMapping.Count
                let maximumStrength = randomBehaviour.ChooseAnyNumberFromOneTo numberOfTestVariables
                let testCaseEnumerator = testCaseEnumeratorFactory.CreateEnumerator maximumStrength
                for testCase in
                    {
                        new Collections.IEnumerable with
                            member this.GetEnumerator () = testCaseEnumerator
                    } do
                    let testCase = unbox<List<TestVariableLevel>> testCase
                    printf "testCase: %A\n" testCase
                    let shouldBeTrue = isSortedByTestVariableIndex testCase
                    Assert.IsTrue shouldBeTrue
            
        [<Test>]
        member this.TestCoverageOfNCombinationsOfVariableLevelsInFinalResultsIsComplete () =
            () // Sort this out!!!!!!!!!!
            
        [<Test>]
        member this.TestCoverageOfNCombinationsOfVariableLevelsInFinalResultsIsOptimal () =
            () // Sort this out!!!!!!!!!!
        

