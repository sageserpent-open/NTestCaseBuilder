namespace SageSerpent.TestInfrastructure.Tests

    open NUnit.Framework

    open SageSerpent.Infrastructure
    open SageSerpent.TestInfrastructure
    open SageSerpent.Infrastructure.RandomExtensions
    open System
    open System.Windows.Forms
    open System.Collections.Generic
    open Microsoft.FSharp.Collections


    [<TestFixture>]
    type MergedPartialTestVectorRepresentationsTestFixture () =
        let randomBehaviourSeed = 23

        let reciprocalOfProbabilityOfTerminatingRandomlyGeneratedPartOfPartialTestVector = 10u

        let reciprocalOfProbabilityOfNotGeneratingAnyFurtherPartialTestVectors = 30u

        let maximumNumberOfTestVariables = 70u

        let maximumNumberOfTestVectors = 1000u

        let maximumRandomWalkStep = 10u

        let maximumLevelDelta = 5u

        let overallTestRepeats = 300u

        let maximumNumberOfIndicesToAvoid = 4u

        let createNonOverlappingPartialTestVectorsAndHandEachOffToTest testHandoff =
            let randomBehaviour =
                Random randomBehaviourSeed
            let createPartialTestVectors () =
                let createShuffledUniqueLevels () =
                    randomBehaviour.Shuffle (seq {1u .. maximumNumberOfTestVectors})
                let shuffleForEachTestVariableIndex =
                    Array.init (int32 maximumNumberOfTestVariables) (fun _ -> createShuffledUniqueLevels ())
                let numberOfTestVectors =
                    randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfTestVectors
                let rec fillInPartialTestVectors incompletePartialTestVectors
                                                 completedPartialTestVectors =
                    match incompletePartialTestVectors with
                        [] ->
                            raise (InternalAssertionViolationException "Base case for recursion is for *one* incomplete vector.")
                      | head :: [] ->
                            let rec ensureVectorIsNonEmpty vectorBeingCompleted =
                                let testVariableIndex =
                                    randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumNumberOfTestVariables
                                let shuffledLevels =
                                    shuffleForEachTestVariableIndex.[int32 testVariableIndex]
                                let numberOfCompletedPartialTestVectors =
                                    List.length completedPartialTestVectors
                                let levelForVectorBeingCompleted =
                                    shuffledLevels.[numberOfCompletedPartialTestVectors]
                                Map.add testVariableIndex levelForVectorBeingCompleted
                                                          (if randomBehaviour.HeadsItIs ()
                                                           then vectorBeingCompleted
                                                           else ensureVectorIsNonEmpty vectorBeingCompleted)
                            ensureVectorIsNonEmpty head
                            :: completedPartialTestVectors
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
                                    Map.add testVariableIndex levelForVectorBeingExamined vectorBeingExamined
                                let levelForVectorBeingCompleted =
                                    shuffledLevels.[numberOfCompletedPartialTestVectors]
                                let vectorBeingCompleted =
                                    Map.add testVariableIndex levelForVectorBeingCompleted vectorBeingCompleted
                                vectorBeingExamined :: modifiedCopiesOfVectorsBeingExamined
                                , vectorBeingCompleted
                            let modifiedIncompletePartialTestVectors
                                , completedPartialTestVector =
                                List.foldBack forceDistinctionBetweenVectors tail ([], head)
                            fillInPartialTestVectors modifiedIncompletePartialTestVectors
                                                     (completedPartialTestVector :: completedPartialTestVectors)
                let partialTestVectors =
                    fillInPartialTestVectors (List.init (int32 numberOfTestVectors) (fun _ -> Map.empty))
                                             []
                randomBehaviour.Shuffle partialTestVectors
                |> List.ofArray
            for _ in 1u .. overallTestRepeats do
                testHandoff (createPartialTestVectors ())

        let createOverlappingPartialTestVectorsAndHandEachOffToTest testHandoff =
            let randomBehaviour =
                Random randomBehaviourSeed
            let createPartialTestVectors () =
                let rec createPartialTestVectors testVariableIndex =
                    if testVariableIndex = maximumNumberOfTestVariables
                       || 0u < testVariableIndex
                          && randomBehaviour.ChooseAnyNumberFromOneTo reciprocalOfProbabilityOfNotGeneratingAnyFurtherPartialTestVectors = 1u
                    then []
                    else let rec chooseTestVariableIndicesAndTheirLevels recursionDepth =
                            let maximumRecursionDepth = 50u
                            if recursionDepth = maximumRecursionDepth
                               || randomBehaviour.ChooseAnyNumberFromOneTo reciprocalOfProbabilityOfTerminatingRandomlyGeneratedPartOfPartialTestVector = 1u
                            then []
                            else let lowerBoundInclusive =
                                    if testVariableIndex > maximumRandomWalkStep
                                    then testVariableIndex - maximumRandomWalkStep
                                    else 0u
                                 let upperBoundInclusive =
                                    if testVariableIndex + maximumRandomWalkStep >= maximumNumberOfTestVariables
                                    then maximumNumberOfTestVariables - 1u
                                    else testVariableIndex + maximumRandomWalkStep
                                 let chosenAbitraryTestVariableIndex =
                                    lowerBoundInclusive + randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (upperBoundInclusive + 1u - lowerBoundInclusive)
                                 (chosenAbitraryTestVariableIndex
                                  , randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumLevelDelta)
                                 :: chooseTestVariableIndicesAndTheirLevels (recursionDepth + 1u)
                         let partialTestVector =
                            chooseTestVariableIndicesAndTheirLevels 0u
                            |> Map.ofList
                         partialTestVector :: createPartialTestVectors (testVariableIndex + 1u)
                createPartialTestVectors 0u
            for _ in 1u .. overallTestRepeats do
                testHandoff (createPartialTestVectors ())

        let mergeOrAddPartialTestVectors partialTestVectors
                                         initialCollection
                                         randomBehaviour =
            let mergedPartialTestVectors
                , setOfFullTestVectors =
                partialTestVectors
                |> List.fold (fun (mergedPartialTestVectors, setOfFullTestVectors) partialTestVector ->
                                match (mergedPartialTestVectors: MergedPartialTestVectorRepresentations<_>).MergeOrAdd partialTestVector
                                                                                                                       randomBehaviour with
                                    updatedMergedPartialTestVectorRepresentationsWithFullTestCaseVectorSuppressed
                                    , Some resultingFullTestCaseVector ->
                                        updatedMergedPartialTestVectorRepresentationsWithFullTestCaseVectorSuppressed
                                        , Set.add resultingFullTestCaseVector
                                                  setOfFullTestVectors
                                  | updatedMergedPartialTestVectorRepresentations
                                    , None ->
                                        updatedMergedPartialTestVectorRepresentations
                                        , setOfFullTestVectors)
                             (initialCollection, Set.empty)
            mergedPartialTestVectors
            , (Set.ofSeq mergedPartialTestVectors
               |> Set.union setOfFullTestVectors)

        let dumpPartialTestVector partialTestVector =
            printf "[ "
            partialTestVector
            |> Map.iter (fun testVariableIndex testLevel ->
                            printf "(%A, %A) " testVariableIndex testLevel)
            printf "]\n"

        [<Test>]
        member this.TestAdditionOfUnmergeableVectorsPreservesIndividualVectors () =
            let randomBehaviour = Random randomBehaviourSeed
            let testHandoff partialTestVectorsThatDoNotOverlap =
                let mergedPartialTestVectors
                    , setOfMergedPartialTestVectors =
                    mergeOrAddPartialTestVectors partialTestVectorsThatDoNotOverlap
                                                 (MergedPartialTestVectorRepresentations.Initial maximumNumberOfTestVariables)
                                                 randomBehaviour
                let shouldBeTrue =
                    Set.ofList partialTestVectorsThatDoNotOverlap = setOfMergedPartialTestVectors
                if not shouldBeTrue
                then let originals = Set.ofList partialTestVectorsThatDoNotOverlap
                     let merged = Set.ofSeq mergedPartialTestVectors
                     let common = Set.intersect originals merged
                     printf "Only in originals:-\n"
                     (originals - common) |> Set.iter dumpPartialTestVector
                     printf "Only in merged:-\n"
                     (merged - common) |> Set.iter dumpPartialTestVector
                     printf "Common:-\n"
                     common |> Set.iter dumpPartialTestVector
                Assert.IsTrue shouldBeTrue
            createNonOverlappingPartialTestVectorsAndHandEachOffToTest testHandoff

        [<Test>]
        member this.TestAdditionOfPartialsOfExistingVectors () =
            let randomBehaviour = Random randomBehaviourSeed
            let testHandoff partialTestVectorsThatDoNotOverlap =
                let mergedPartialTestVectors
                    , _ =
                    mergeOrAddPartialTestVectors partialTestVectorsThatDoNotOverlap
                                                 (MergedPartialTestVectorRepresentations.Initial maximumNumberOfTestVariables)
                                                 randomBehaviour
                let mutantsOrCopiesOf partialTestVector =
                    let maximumRecursionDepth = 50u
                    let rec createMutantOrCopy recursionDepth =
                        let mutantOrCopy =
                            match randomBehaviour.ChooseAnyNumberFromOneTo 3u with
                                1u ->
                                    randomBehaviour.ChooseSeveralOf ((Map.toSeq partialTestVector)
                                                                     , (randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (uint32 partialTestVector.Count)))
                                 |> Map.ofSeq
                              | 2u ->
                                    Map.toSeq partialTestVector
                                    |> Seq.take (int32 (randomBehaviour.ChooseAnyNumberFromOneTo (uint32 partialTestVector.Count - 1u)))
                                    |> Map.ofSeq
                              | 3u ->
                                    partialTestVector
                              | _ ->
                                    raise (InternalAssertionViolationException "This case should never be matched.")
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
                let remergedPartialTestVectors
                    , setOfMergedPartialTestVectorsFromRemerge =
                    mergeOrAddPartialTestVectors partialTestVectorsThatAddNoNewInformation
                                                 mergedPartialTestVectors
                                                 randomBehaviour
                let shouldBeTrue =
                    (Set.ofList partialTestVectorsThatDoNotOverlap).Count = setOfMergedPartialTestVectorsFromRemerge.Count
                if not shouldBeTrue
                then let originals = Set.ofList partialTestVectorsThatDoNotOverlap
                     let remerged = Set.ofSeq remergedPartialTestVectors
                     let common = Set.intersect originals remerged
                     printf "Only in originals:-\n"
                     (originals - common) |> Set.iter dumpPartialTestVector
                     printf "Only in remerged:-\n"
                     (remerged - common) |> Set.iter dumpPartialTestVector
                     printf "Common:-\n"
                     common |> Set.iter dumpPartialTestVector
                Assert.IsTrue shouldBeTrue
            createNonOverlappingPartialTestVectorsAndHandEachOffToTest testHandoff

        [<Test>]
        member this.TestMergingOfVectorsInWithExistingPartialVectors () =
            let randomBehaviour = Random randomBehaviourSeed

            let testHandoff partialTestVectorsThatDoNotOverlap =
                let numberOfIndicesToAvoid =
                    randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfIndicesToAvoid
                let sortedIndicesToAvoid =
                    randomBehaviour.ChooseSeveralOf ((List.init (int32 maximumNumberOfTestVariables) uint32)
                                                     , numberOfIndicesToAvoid)
                    |> List.ofArray
                    |> List.sort
                let mappingAvoidingIndices =
                    BargainBasement.MappingAvoidingIndices sortedIndicesToAvoid
                let maximumNumberOfTestVariablesAfterRemapping =
                    maximumNumberOfTestVariables + numberOfIndicesToAvoid
                let rotationOffsetToAllowCoverageOfTrailingIndices =
                    randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumNumberOfIndicesToAvoid
                let rotation =
                    fun testVariableIndex -> (rotationOffsetToAllowCoverageOfTrailingIndices + testVariableIndex) % maximumNumberOfTestVariablesAfterRemapping
                let mappingAvoidingIndicesThenRotation =
                    mappingAvoidingIndices >> rotation
                let remappedPartialTestVectors =
                    partialTestVectorsThatDoNotOverlap
                    |> List.map (Map.toList >> List.map (function index
                                                                  , level ->
                                                                    mappingAvoidingIndicesThenRotation index
                                                                    , level) >> Map.ofList)
                let mergedPartialTestVectors
                    , _ =
                    mergeOrAddPartialTestVectors remappedPartialTestVectors
                                                 (MergedPartialTestVectorRepresentations.Initial maximumNumberOfTestVariablesAfterRemapping)
                                                 randomBehaviour
                let possiblyAddLevelsForIndices indicesToAdd partialTestVector =
                    let chosenIndicesToAdd =
                        randomBehaviour.ChooseSeveralOf (indicesToAdd
                                                         , (randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (uint32 (Seq.length indicesToAdd) + 1u)))
                    seq {for index in chosenIndicesToAdd do
                            yield index, index + uint32 (randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumLevelDelta)}
                    |> (let addIndexAndLevel partialTestVector
                                             (testVariableIndex
                                              , level)
                                             =
                            Map.add testVariableIndex level partialTestVector
                        Seq.fold addIndexAndLevel partialTestVector)
                let sortedIndicesToAvoidWithRotationApplied =
                    sortedIndicesToAvoid
                    |> List.map rotation
                let partialTestVectorsWhichMayHaveThePreviouslyAvoidedIndices =
                    remappedPartialTestVectors
                    |> List.map (possiblyAddLevelsForIndices sortedIndicesToAvoidWithRotationApplied)
                let remergedPartialTestVectors
                    , setOfMergedPartialTestVectorsFromRemerge =
                    mergeOrAddPartialTestVectors partialTestVectorsWhichMayHaveThePreviouslyAvoidedIndices
                                                 mergedPartialTestVectors
                                                 randomBehaviour
                let shouldBeTrue =
                    Set.ofList partialTestVectorsWhichMayHaveThePreviouslyAvoidedIndices = setOfMergedPartialTestVectorsFromRemerge
                if not shouldBeTrue
                then let originals = Set.ofList partialTestVectorsWhichMayHaveThePreviouslyAvoidedIndices
                     let remerged = Set.ofSeq remergedPartialTestVectors
                     let common = Set.intersect originals remerged
                     printf "remappedPartialTestVectors:\n"
                     Set.ofList remappedPartialTestVectors |> Set.iter dumpPartialTestVector
                     printf "mergedPartialTestVectors:\n"
                     Set.ofSeq mergedPartialTestVectors |> Set.iter dumpPartialTestVector
                     printf "Only in originals:-\n"
                     (originals - common) |> Set.iter dumpPartialTestVector
                     printf "Only in remerged:-\n"
                     (remerged - common) |> Set.iter dumpPartialTestVector
                     printf "Common:-\n"
                     common |> Set.iter dumpPartialTestVector
                     printf "Indices to avoid with rotation applied: %A\n" sortedIndicesToAvoidWithRotationApplied
                Assert.IsTrue shouldBeTrue
            createNonOverlappingPartialTestVectorsAndHandEachOffToTest testHandoff

        [<Test>]
        member this.TestVectorsAreEitherAddedOrMerged () =
            let randomBehaviour = Random randomBehaviourSeed
            let testHandoff partialTestVectors =
                let mergedPartialTestVectors
                    , setOfMergedPartialTestVectors =
                        mergeOrAddPartialTestVectors partialTestVectors
                                                     (MergedPartialTestVectorRepresentations.Initial maximumNumberOfTestVariables)
                                                     randomBehaviour
                let numberOfMergedPartialTestVectors =
                    setOfMergedPartialTestVectors.Count
                let shouldBeTrue = numberOfMergedPartialTestVectors <= partialTestVectors.Length
                Assert.IsTrue shouldBeTrue
                let partialVectorsThatWereChanged =
                    partialTestVectors
                    |> List.filter (fun partialTestVector -> not (setOfMergedPartialTestVectors.Contains partialTestVector))
                    |> Set.ofSeq
                let checkInclusionInAtLeastOneMergedPartialTestVector partialTestVector =
                    let firstIncludesSecond first second =
                        (first
                         |> Map.toList
                         |> Set.ofList).IsSupersetOf (second
                                                       |> Map.toList
                                                       |> Set.ofList)
                    let shouldBeTrue =
                        setOfMergedPartialTestVectors
                        |> Set.exists (fun mergedPartialTestVector
                                            -> firstIncludesSecond mergedPartialTestVector partialTestVector)
                    Assert.IsTrue shouldBeTrue
                partialVectorsThatWereChanged
                |> Seq.iter checkInclusionInAtLeastOneMergedPartialTestVector
            createOverlappingPartialTestVectorsAndHandEachOffToTest testHandoff

        [<Test>]
        member this.TestAdditionOfTrivialEmptyPartialTestVectorsLeavesCollectionUnchanged () =
            let randomBehaviour = Random randomBehaviourSeed
            let testHandoff partialTestVectors =
                let mergedPartialTestVectors
                    , setOfMergedPartialTestVectors =
                    mergeOrAddPartialTestVectors partialTestVectors
                                                 (MergedPartialTestVectorRepresentations.Initial maximumNumberOfTestVariables)
                                                 randomBehaviour
                let remergedPartialTestVectors
                    , setOfMergedPartialTestVectorsFromRemerge =
                    mergeOrAddPartialTestVectors [Map.empty]
                                                 mergedPartialTestVectors
                                                 randomBehaviour
                let shouldBeTrue =
                    setOfMergedPartialTestVectors = setOfMergedPartialTestVectorsFromRemerge
                Assert.IsTrue shouldBeTrue
            createOverlappingPartialTestVectorsAndHandEachOffToTest testHandoff

        [<Test>]
        member this.TestInitialStateIsEmptyAndDoesNotContainATrivialEmptyPartialTestVector () =
            for maximumNumberOfTestVariables in 0u .. maximumNumberOfTestVariables do   // NOTE: includes the boundary case of no test variables whatsover.
                let initial =
                    MergedPartialTestVectorRepresentations.Initial maximumNumberOfTestVariables
                let containedPartialTestVectors =
                    initial
                    |> List.ofSeq
                let shouldBeTrue =
                    containedPartialTestVectors.Length = 0
                Assert.IsTrue shouldBeTrue
