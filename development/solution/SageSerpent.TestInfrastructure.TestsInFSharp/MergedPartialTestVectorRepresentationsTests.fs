#light

namespace SageSerpent.TestInfrastructure.Tests

    open NUnit.Framework
    
    open SageSerpent.Infrastructure
    open SageSerpent.TestInfrastructure
    open System
    open System.Windows.Forms
    open System.Drawing
    open System.Collections.Generic
    open Wintellect.PowerCollections
    open Microsoft.FSharp.Collections
    
    
    [<TestFixture>]
    type MergedPartialTestVectorRepresentationsTestFixture () =
        let randomBehaviourSeed = 23
        
        let reciprocalOfProbabilityOfTerminatingRandomlyGeneratedPartOfPartialTestVector = 10u
        
        let reciprocalOfProbabilityOfNotGeneratingAnyFurtherPartialTestVectors = 30u
        
        let maximumNumberOfTestVariables = 50u
        
        let maximumNumberOfTestVectors = 50u
        
        let maximumRecursionDepth = 50u
        
        let maximumRandomWalkStep = 10u
        
        let maximumLevelDelta = 5u
        
        let overallTestRepeats = 4u
        
        let maximumNumberOfIndicesToAvoid = 4u
        
        let boxAssociatedLevel (testVariableIndex
                                , associatedLevel) =
            testVariableIndex
            , box associatedLevel
        
        let createNonOverlappingPartialTestVectorsAndHandEachOffToTest testHandoff =
            let randomBehaviour =
                RandomBehaviour randomBehaviourSeed
            let createPartialTestVectors () =
                let createShuffledUniqueLevels () =
                    Algorithms.RandomShuffle (seq {1u .. maximumNumberOfTestVectors}, randomBehaviour.UnderlyingImplementationForClientUse)
                let shuffleForEachTestVariableIndex =
                    Array.init (int32 maximumNumberOfTestVariables) (fun _ -> createShuffledUniqueLevels ())
                let numberOfTestVectors =
                    randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestVectors
                let rec fillInPartialTestVectors incompletePartialTestVectors completedPartialTestVectors =
                    match incompletePartialTestVectors with
                        [] ->
                            completedPartialTestVectors
                      | head :: tail ->
                            let forceDistinctionBetweenVectors vectorBeingExamined
                                                               (modifiedCopiesOfVectorsBeingExamined
                                                                , vectorBeingCompleted) =
                                let testVariableIndex =
                                    randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumNumberOfTestVariables
                                let shuffledLevels =
                                    shuffleForEachTestVariableIndex.[int32 testVariableIndex]
                                let numberOfCompletedPartialTestVectors =
                                    List.length completedPartialTestVectors
                                let numberOfCopiesOfVectorsBeingExamined =
                                    List.length modifiedCopiesOfVectorsBeingExamined
                                let levelForVectorBeingExamined =
                                    shuffledLevels.[int32 numberOfTestVectors - (1 + numberOfCopiesOfVectorsBeingExamined)]
                                let vectorBeingExamined =
                                    Map.add testVariableIndex (box levelForVectorBeingExamined) vectorBeingExamined
                                let levelForVectorBeingCompleted =
                                    shuffledLevels.[numberOfCompletedPartialTestVectors]
                                let vectorBeingCompleted =
                                    Map.add testVariableIndex (box levelForVectorBeingCompleted) vectorBeingCompleted
                                vectorBeingExamined :: modifiedCopiesOfVectorsBeingExamined
                                , vectorBeingCompleted
                            let incompletePartialTestVectors
                                , completedPartialTestVector =
                                List.fold_right forceDistinctionBetweenVectors tail ([], head)
                            fillInPartialTestVectors incompletePartialTestVectors (completedPartialTestVector :: completedPartialTestVectors)
                let partialTestVectors =
                    fillInPartialTestVectors (List.init (int32 numberOfTestVectors) (fun _ -> Map.empty))
                                             []
                Algorithms.RandomShuffle (partialTestVectors, randomBehaviour.UnderlyingImplementationForClientUse)
                |> List.of_array               
            for _ in 1u .. overallTestRepeats do
                testHandoff (createPartialTestVectors ())
                
        let mergeOrAddPartialTestVectors partialTestVectors initialCollection =
            partialTestVectors
            |> List.fold_left (fun (mergedPartialTestVectors: MergedPartialTestVectorRepresentations) partialTestVector ->
                                mergedPartialTestVectors.MergeOrAdd partialTestVector)
                              initialCollection
                              
        [<Test>]
        member this.TestAdditionOfUnmergeableVectorsPreservesIndividualVectors () =
            let testHandoff partialTestVectorsThatDoNotOverlap =
                let mergedPartialTestVectors =
                    mergeOrAddPartialTestVectors partialTestVectorsThatDoNotOverlap MergedPartialTestVectorRepresentations.Initial
                let shouldBeTrue =
                    Set.of_list partialTestVectorsThatDoNotOverlap = Set.of_seq mergedPartialTestVectors
                Assert.IsTrue shouldBeTrue
            createNonOverlappingPartialTestVectorsAndHandEachOffToTest testHandoff
            
        [<Test>]
        member this.TestAdditionOfPartialsOfExistingVectors () =
            let testHandoff partialTestVectorsThatDoNotOverlap =
                let mergedPartialTestVectors =
                    mergeOrAddPartialTestVectors partialTestVectorsThatDoNotOverlap MergedPartialTestVectorRepresentations.Initial
                let randomBehaviour = RandomBehaviour randomBehaviourSeed
                let mutantsOrCopiesOf partialTestVector =
                    let maximumRecursionDepth = 10u
                    let rec createMutantOrCopy recursionDepth =
                        let mutantOrCopy =
                            if randomBehaviour.HeadsItIs ()
                            then Algorithms.RandomSubset (Map.to_seq partialTestVector,
                                                          int32 (randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (uint32 partialTestVector.Count)),
                                                          randomBehaviour.UnderlyingImplementationForClientUse)
                                 |> Map.of_seq
                            else partialTestVector 
                        mutantOrCopy
                        :: if recursionDepth = maximumRecursionDepth
                              || randomBehaviour.HeadsItIs ()
                           then []
                           else createMutantOrCopy (recursionDepth + 1u)
                    createMutantOrCopy 0u
                let partialTestVectorsThatAddNoNewInformation =
                    partialTestVectorsThatDoNotOverlap
                    |> List.map mutantsOrCopiesOf
                    |> List.concat
                let remergedPartialTestVectors =
                    mergeOrAddPartialTestVectors partialTestVectorsThatAddNoNewInformation mergedPartialTestVectors
                let shouldBeTrue =
                    (Set.of_list partialTestVectorsThatDoNotOverlap).Count = (Set.of_seq remergedPartialTestVectors).Count
                Assert.IsTrue shouldBeTrue
            createNonOverlappingPartialTestVectorsAndHandEachOffToTest testHandoff
            
        [<Test>]
        member this.TestMergingOfVectorsInWithExistingPartialVectors () =
            let avoidCertainIndicesByRemapping sortedIndicesToAvoid =
                if List.length sortedIndicesToAvoid = 0
                then raise (PreconditionViolationException "Must have at least one index to avoid.")
                let sortedAssociationBetweenIndicesAndIncrementsToApply =
                    let arrangeDeferredAssociations (runningIncrement
                                                     , deferredActionsForPredecessors)
                                                    indexToAvoid =
                        runningIncrement + 1u
                        , (fun associationBuiltSoFar ->
                            deferredActionsForPredecessors ((indexToAvoid - runningIncrement
                                                            , runningIncrement + 1u)
                                                              :: associationBuiltSoFar))
                    (sortedIndicesToAvoid
                     |> List.fold_left arrangeDeferredAssociations (0u, (fun result -> result))
                     |> snd) []
                    |> List.to_array
                let remapIndex (index
                                , level) =
                    let foundIndex =
                        Array.BinarySearch (sortedAssociationBetweenIndicesAndIncrementsToApply,
                                            (index
                                             , 0u),
                                            {
                                                new IComparer<UInt32 * UInt32> with
                                                    member this.Compare (first, second) =
                                                        compare (fst first) (fst second)
                                            })
                    let incrementToApplyToIndex =
                        if foundIndex >= 0
                        then snd sortedAssociationBetweenIndicesAndIncrementsToApply.[foundIndex]
                        else let foundIndex =
                                    ~~~ foundIndex
                             if foundIndex > 0
                             then snd sortedAssociationBetweenIndicesAndIncrementsToApply.[foundIndex - 1]
                             else 0u
                    index + incrementToApplyToIndex
                    , level
                Map.to_list >> List.map remapIndex >> Map.of_list
            let randomBehaviour = RandomBehaviour randomBehaviourSeed
            let sortedIndicesToAvoid = // TODO - make this a function or a local binding to get new sets of indices for each test handoff. 
                Algorithms.RandomSubset (List.init (int32 maximumNumberOfTestVariables) (fun count -> uint32 count),
                                         int32 (randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfIndicesToAvoid),
                                         randomBehaviour.UnderlyingImplementationForClientUse)
                |> List.of_array
                |> List.sort compare
            let testHandoff partialTestVectorsThatDoNotOverlap =
                let remappedPartialTestVectors =
                    partialTestVectorsThatDoNotOverlap
                    |> List.map (avoidCertainIndicesByRemapping sortedIndicesToAvoid)
                let mergedPartialTestVectors =
                    mergeOrAddPartialTestVectors remappedPartialTestVectors MergedPartialTestVectorRepresentations.Initial
                let possiblyAddLevelsForIndices indicesToAvoid partialTestVector =
                    let chosenIndicesToAvoid =
                        Algorithms.RandomSubset (indicesToAvoid,
                                                 int32 (randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (uint32 (Seq.length sortedIndicesToAvoid) + 1u)),
                                                 randomBehaviour.UnderlyingImplementationForClientUse)
                    seq {for index in chosenIndicesToAvoid do
                            yield index, index + uint32 (randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumLevelDelta)}
                    |> (let addIndexAndLevel partialTestVector
                                             (testVariableIndex
                                              , level)
                                             =
                            Map.add testVariableIndex (box level) partialTestVector
                        Seq.fold addIndexAndLevel partialTestVector)
                let partialTestVectorsWhichMayHaveThePreviouslyAvoidedIndices =
                    remappedPartialTestVectors
                    |> List.map (possiblyAddLevelsForIndices sortedIndicesToAvoid)
                let remergedPartialTestVectors =
                    mergeOrAddPartialTestVectors partialTestVectorsWhichMayHaveThePreviouslyAvoidedIndices mergedPartialTestVectors
                let shouldBeTrue =
                    Set.of_list partialTestVectorsWhichMayHaveThePreviouslyAvoidedIndices = Set.of_seq remergedPartialTestVectors
                Assert.IsTrue shouldBeTrue
            createNonOverlappingPartialTestVectorsAndHandEachOffToTest testHandoff
            
        [<Test>]
        member this.TestVectorsAreEitherAddedOrMerged () =
            let randomBehaviour =
                RandomBehaviour randomBehaviourSeed
            for _ in 1u .. overallTestRepeats do
                let createPartialTestVectors () =
                    let rec createPartialTestVectors testVariableIndex =
                        if testVariableIndex = maximumNumberOfTestVariables
                           || randomBehaviour.ChooseAnyNumberFromOneTo reciprocalOfProbabilityOfNotGeneratingAnyFurtherPartialTestVectors = 1u
                        then []
                        else let rec chooseTestVariableIndicesAndTheirLevels recursionDepth =
                                if recursionDepth = maximumRecursionDepth
                                   || randomBehaviour.ChooseAnyNumberFromOneTo reciprocalOfProbabilityOfTerminatingRandomlyGeneratedPartOfPartialTestVector = 1u
                                then []
                                else let lowerBoundInclusive =
                                        if testVariableIndex > maximumRandomWalkStep
                                        then testVariableIndex - maximumRandomWalkStep
                                        else 0u
                                     let upperBoundInclusive =
                                        testVariableIndex + maximumRandomWalkStep
                                     let chosenAbitraryTestVariableIndex =
                                        lowerBoundInclusive + randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (upperBoundInclusive + 1u - lowerBoundInclusive)
                                     (chosenAbitraryTestVariableIndex
                                      , randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumLevelDelta)
                                     :: chooseTestVariableIndicesAndTheirLevels (recursionDepth + 1u)
                             let partialTestVector =
                                chooseTestVariableIndicesAndTheirLevels 0u
                                |> List.map boxAssociatedLevel
                                |> Map.of_list
                             partialTestVector :: createPartialTestVectors (testVariableIndex + 1u)
                    createPartialTestVectors 0u
                let partialTestVectors =
                    createPartialTestVectors ()
                let mergedPartialTestVectors =
                        mergeOrAddPartialTestVectors partialTestVectors MergedPartialTestVectorRepresentations.Initial
                let numberOfMergedPartialTestVectors =
                    Seq.length mergedPartialTestVectors
                let shouldBeTrue = numberOfMergedPartialTestVectors <= partialTestVectors.Length
                Assert.IsTrue shouldBeTrue
                let mergedPartialTestVectors =
                    Set.of_seq mergedPartialTestVectors
                let partialVectorsThatWereChanged =
                    partialTestVectors
                    |> List.filter (fun partialTestVector -> not (mergedPartialTestVectors.Contains partialTestVector))
                    |> Set.of_seq
                let checkInclusionInAtLeastOneMergedPartialTestVector partialTestVector =
                    let firstIncludesSecond first second =
                        (first
                         |> Map.to_list
                         |> Set.of_list).IsSupersetOf (second
                                                       |> Map.to_list
                                                       |> Set.of_list)
                    for mergedPartialTestVector in mergedPartialTestVectors do
                        let shouldBeTrue =
                            firstIncludesSecond mergedPartialTestVector partialTestVector
                        Assert.IsTrue shouldBeTrue
                partialVectorsThatWereChanged
                |> Seq.iter checkInclusionInAtLeastOneMergedPartialTestVector
                
        [<Test>]
        member this.TestInitialStateContainsATrivialEmptyPartialTestVector () =
            let initial =
                MergedPartialTestVectorRepresentations.Initial
            let containedPartialTestVectors =
                initial
                |> List.of_seq
            match containedPartialTestVectors with
                [ presumedToBeEmpty ] ->
                    let shouldBeTrue =
                        Map.is_empty presumedToBeEmpty
                    Assert.IsTrue shouldBeTrue
              | _ ->
                    Assert.Fail "There should be precisely one partial test vector in the initial state."
                