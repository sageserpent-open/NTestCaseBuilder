﻿namespace NTestCaseBuilder.Tests

    open NUnit.Framework

    open SageSerpent.Infrastructure
    open NTestCaseBuilder
    open SageSerpent.Infrastructure.RandomExtensions
    open System
    open System.Windows.Forms
    open System.Collections.Generic
    open Microsoft.FSharp.Collections


    [<TestFixture>]
    type SetOfMergedPathsTestFixture () =
        let passAllFilter _ =
            true

        let randomBehaviourSeed = 23

        let reciprocalOfProbabilityOfTerminatingRandomlyGeneratedPartOfPartialTestVector = 10

        let reciprocalOfProbabilityOfNotGeneratingAnyFurtherPartialTestVectors = 30

        let maximumNumberOfTestVariables = 70

        let maximumNumberOfTestVectors = 1000

        let maximumRandomWalkStep = 10

        let maximumLevelDelta = 5

        let overallTestRepeats = 300

        let maximumNumberOfIndicesToAvoid = 4

        let createNonOverlappingPartialTestVectorsAndHandEachOffToTest testHandoff =
            let randomBehaviour =
                Random randomBehaviourSeed
            let createPartialTestVectors () =
                let createShuffledUniqueLevels () =
                    randomBehaviour.Shuffle (seq {1 .. maximumNumberOfTestVectors})
                let shuffleForEachTestVariableIndex =
                    Array.init maximumNumberOfTestVariables (fun _ -> createShuffledUniqueLevels ())
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
                                    shuffleForEachTestVariableIndex.[testVariableIndex]
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
                                    shuffleForEachTestVariableIndex.[testVariableIndex]
                                let numberOfCompletedPartialTestVectors =
                                    List.length completedPartialTestVectors
                                let numberOfCopiesOfVectorsBeingExamined =
                                    List.length modifiedCopiesOfVectorsBeingExamined
                                let levelForVectorBeingExamined =
                                    shuffledLevels.[numberOfTestVectors - (1 + numberOfCopiesOfVectorsBeingExamined)]
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
                    fillInPartialTestVectors (List.replicate numberOfTestVectors Map.empty)
                                             []
                randomBehaviour.Shuffle partialTestVectors
                |> List.ofArray
            for _ in 1 .. overallTestRepeats do
                testHandoff (createPartialTestVectors ())

        let createOverlappingPartialTestVectorsAndHandEachOffToTest testHandoff =
            let randomBehaviour =
                Random randomBehaviourSeed
            let createPartialTestVectors () =
                let rec createPartialTestVectors testVariableIndex =
                    if testVariableIndex = maximumNumberOfTestVariables
                       || 0 < testVariableIndex
                          && randomBehaviour.ChooseAnyNumberFromOneTo reciprocalOfProbabilityOfNotGeneratingAnyFurtherPartialTestVectors = 1
                    then []
                    else let rec chooseTestVariableIndicesAndTheirLevels recursionDepth =
                            let maximumRecursionDepth = 50
                            if recursionDepth = maximumRecursionDepth
                               || randomBehaviour.ChooseAnyNumberFromOneTo reciprocalOfProbabilityOfTerminatingRandomlyGeneratedPartOfPartialTestVector = 1
                            then []
                            else let lowerBoundInclusive =
                                    if testVariableIndex > maximumRandomWalkStep
                                    then testVariableIndex - maximumRandomWalkStep
                                    else 0
                                 let upperBoundInclusive =
                                    if testVariableIndex + maximumRandomWalkStep >= maximumNumberOfTestVariables
                                    then maximumNumberOfTestVariables - 1
                                    else testVariableIndex + maximumRandomWalkStep
                                 let chosenAbitraryTestVariableIndex =
                                    lowerBoundInclusive + randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (upperBoundInclusive + 1 - lowerBoundInclusive)
                                 (chosenAbitraryTestVariableIndex
                                  , randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumLevelDelta)
                                 :: chooseTestVariableIndicesAndTheirLevels (recursionDepth + 1)
                         let partialTestVector =
                            chooseTestVariableIndicesAndTheirLevels 0
                            |> Map.ofList
                         partialTestVector :: createPartialTestVectors (testVariableIndex + 1)
                createPartialTestVectors 0
            let overlappingPartialTestVectors =
                createPartialTestVectors ()

            let overlappingPartialTestVectorsWithSomeExtraFullTestVectors =
                seq
                    {
                        for partialTestVector in overlappingPartialTestVectors do
                            yield partialTestVector

                            if randomBehaviour.HeadsItIs()
                            then
                                let testVariableIndices =
                                    partialTestVector
                                    |> Map.toList
                                    |> List.map fst
                                let unusedTestVariableIndices =
                                    (List.init maximumNumberOfTestVariables BargainBasement.Identity
                                     |> Set.ofList) - (testVariableIndices
                                                       |> Set.ofList)
                                let extraPadding =
                                    unusedTestVariableIndices
                                    |> Seq.map (fun testVariableIndex -> testVariableIndex, randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumNumberOfTestVariables)

                                yield partialTestVector
                                |> Map.toSeq
                                |> Seq.append extraPadding
                                |> Map.ofSeq
                    }
                |> randomBehaviour.Shuffle
                |> List.ofArray

            for _ in 1 .. overallTestRepeats do
                testHandoff overlappingPartialTestVectorsWithSomeExtraFullTestVectors


        let dumpPartialTestVector partialTestVector =
            printf "[ "
            partialTestVector
            |> Map.iter (fun testVariableIndex testLevel ->
                            printf "(%A, %A) " testVariableIndex testLevel)
            printf "]\n"

        let mergeOrAddPartialTestVectors partialTestVectors
                                         initialCollection
                                         randomBehaviour
                                         revealFullTestVectorsAgain =
            let maximumNumberOfTestVariables =
                (initialCollection: SetOfMergedPaths<_>).MaximumNumberOfTestVariables
            let shuffledDuplicatedPartialTestVectors =
                (randomBehaviour: Random).Shuffle (List.append partialTestVectors partialTestVectors)
                |> List.ofArray
            let mergedPartialTestVectors
                , setOfMergedFullTestVectors =
                shuffledDuplicatedPartialTestVectors
                |> List.fold (fun (mergedPartialTestVectors, setOfFullTestVectors) partialTestVector ->
                                match (mergedPartialTestVectors: SetOfMergedPaths<_>).MergeOrAdd partialTestVector with
                                    updatedSetOfMergedPathsWithFullTestCaseVectorSuppressed
                                    , Some resultingFullTestCaseVector ->
                                        let shouldBeTrue =
                                            not (Set.contains resultingFullTestCaseVector
                                                              setOfFullTestVectors)

                                        Assert.IsTrue shouldBeTrue

                                        updatedSetOfMergedPathsWithFullTestCaseVectorSuppressed
                                        , Set.add resultingFullTestCaseVector
                                                  setOfFullTestVectors
                                  | updatedSetOfMergedPaths
                                    , None ->
                                        updatedSetOfMergedPaths
                                        , setOfFullTestVectors)
                             (initialCollection, Set.empty)
            let isFullTestVector partialTestVector =
                let sumOfIndicesExpectedForFullTestVector =
                    maximumNumberOfTestVariables * (maximumNumberOfTestVariables + 1) / 2
                partialTestVector
                |> Map.toSeq
                |> Seq.map (fst >> (fun testVariableIndex -> 1 + testVariableIndex))   // NOTE: shift by one because otherwise the leading term for a sum of zero-relative indices would not show up in the sum!
                |> Seq.reduce (+)
                 = sumOfIndicesExpectedForFullTestVector

            let shouldBeTrue =
                setOfMergedFullTestVectors
                |> Set.forall isFullTestVector

            Assert.IsTrue shouldBeTrue

            if revealFullTestVectorsAgain
            then
                let setOfAllMergedTestVectors =
                    mergedPartialTestVectors.EnumerationOfMergedTestVectors true
                    |> Set.ofSeq

                let shouldBeTrue =
                    Set.isSubset setOfMergedFullTestVectors setOfAllMergedTestVectors
                Assert.IsTrue shouldBeTrue

                mergedPartialTestVectors
                , setOfAllMergedTestVectors
            else
                let setOfMergedPartialTestVectors =
                    mergedPartialTestVectors.EnumerationOfMergedTestVectors false
                    |> Set.ofSeq

                let shouldBeTrue =
                    Set.isEmpty (Set.intersect setOfMergedFullTestVectors setOfMergedPartialTestVectors)

                if not shouldBeTrue
                then let common = Set.intersect setOfMergedFullTestVectors setOfMergedPartialTestVectors
                     printf "setOfMergedFullTestVectors:\n"
                     setOfMergedFullTestVectors |> Set.iter dumpPartialTestVector
                     printf "setOfMergedPartialTestVectors:\n"
                     setOfMergedPartialTestVectors  |> Set.iter dumpPartialTestVector
                     printf "Only in setOfMergedFullTestVectors:-\n"
                     (setOfMergedFullTestVectors - common) |> Set.iter dumpPartialTestVector
                     printf "Only in setOfMergedPartialTestVectors:-\n"
                     (setOfMergedPartialTestVectors - common) |> Set.iter dumpPartialTestVector
                     printf "Common:-\n"
                     common |> Set.iter dumpPartialTestVector

                Assert.IsTrue shouldBeTrue

                let shouldBeTrue =
                    setOfMergedPartialTestVectors
                    |> Set.forall (isFullTestVector >> not)

                if not shouldBeTrue
                then
                    printf "Maximum number of test variables: %A\n" maximumNumberOfTestVariables
                    printf "Got at least one full test vector that is just from the enumeration:-\n"
                    setOfMergedPartialTestVectors |> Set.filter isFullTestVector |> Set.iter dumpPartialTestVector

                Assert.IsTrue shouldBeTrue

                mergedPartialTestVectors
                , (setOfMergedPartialTestVectors
                   |> Set.union setOfMergedFullTestVectors)

        [<Test>]
        member this.TestAdditionOfUnmergeableVectorsPreservesIndividualVectors () =
            let randomBehaviour = Random randomBehaviourSeed
            let testHandoff partialTestVectorsThatDoNotOverlap =
                let mergedPartialTestVectors
                    , setOfMergedPartialTestVectors =
                    mergeOrAddPartialTestVectors partialTestVectorsThatDoNotOverlap
                                                 (SetOfMergedPaths.Initial maximumNumberOfTestVariables
                                                                                                 passAllFilter)
                                                 randomBehaviour
                                                 true
                let shouldBeTrue =
                    Set.ofList partialTestVectorsThatDoNotOverlap = setOfMergedPartialTestVectors
                if not shouldBeTrue
                then let originals = Set.ofList partialTestVectorsThatDoNotOverlap
                     let merged = mergedPartialTestVectors.EnumerationOfMergedTestVectors true |> Set.ofSeq
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
                                                 (SetOfMergedPaths.Initial maximumNumberOfTestVariables
                                                                                                 passAllFilter)
                                                 randomBehaviour
                                                 true
                let mutantsOrCopiesOf partialTestVector =
                    let maximumRecursionDepth = 50
                    let rec createMutantOrCopy recursionDepth =
                        let mutantOrCopy =
                            match randomBehaviour.ChooseAnyNumberFromOneTo 3 with
                                1 ->
                                    randomBehaviour.ChooseSeveralOf ((Map.toSeq partialTestVector)
                                                                     , (randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan partialTestVector.Count))
                                 |> Map.ofSeq
                              | 2 ->
                                    Map.toSeq partialTestVector
                                    |> Seq.take (randomBehaviour.ChooseAnyNumberFromOneTo (partialTestVector.Count - 1))
                                    |> Map.ofSeq
                              | 3 ->
                                    partialTestVector
                              | _ ->
                                    raise (InternalAssertionViolationException "This case should never be matched.")
                        mutantOrCopy
                        :: if recursionDepth = maximumRecursionDepth
                              || randomBehaviour.HeadsItIs ()
                           then []
                           else createMutantOrCopy (recursionDepth + 1)
                    createMutantOrCopy 0
                let partialTestVectorsThatAddNoNewInformation =
                    partialTestVectorsThatDoNotOverlap
                    |> List.map mutantsOrCopiesOf
                    |> List.concat
                let remergedPartialTestVectors
                    , setOfMergedPartialTestVectorsFromRemerge =
                    mergeOrAddPartialTestVectors partialTestVectorsThatAddNoNewInformation
                                                 mergedPartialTestVectors
                                                 randomBehaviour
                                                 true
                let shouldBeTrue =
                    (Set.ofList partialTestVectorsThatDoNotOverlap).Count = setOfMergedPartialTestVectorsFromRemerge.Count
                if not shouldBeTrue
                then let originals = Set.ofList partialTestVectorsThatDoNotOverlap
                     let remerged = remergedPartialTestVectors.EnumerationOfMergedTestVectors true |> Set.ofSeq
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
                    randomBehaviour.ChooseSeveralOf ((List.init maximumNumberOfTestVariables BargainBasement.Identity)
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
                                                 (SetOfMergedPaths.Initial maximumNumberOfTestVariablesAfterRemapping
                                                                                                 passAllFilter)
                                                 randomBehaviour
                                                 true
                let possiblyAddLevelsForIndices indicesToAdd partialTestVector =
                    let chosenIndicesToAdd =
                        randomBehaviour.ChooseSeveralOf (indicesToAdd
                                                         , (randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (Seq.length indicesToAdd + 1)))
                    seq {for index in chosenIndicesToAdd do
                            yield index, index + randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumLevelDelta}
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
                                                 true
                let shouldBeTrue =
                    Set.ofList partialTestVectorsWhichMayHaveThePreviouslyAvoidedIndices = setOfMergedPartialTestVectorsFromRemerge
                if not shouldBeTrue
                then let originals = Set.ofList partialTestVectorsWhichMayHaveThePreviouslyAvoidedIndices
                     let remerged = remergedPartialTestVectors.EnumerationOfMergedTestVectors true |> Set.ofSeq
                     let common = Set.intersect originals remerged
                     printf "remappedPartialTestVectors:\n"
                     Set.ofList remappedPartialTestVectors |> Set.iter dumpPartialTestVector
                     printf "mergedPartialTestVectors:\n"
                     mergedPartialTestVectors.EnumerationOfMergedTestVectors true |> Set.ofSeq  |> Set.iter dumpPartialTestVector
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
                                                     (SetOfMergedPaths.Initial maximumNumberOfTestVariables
                                                                                                     passAllFilter)
                                                     randomBehaviour
                                                     true
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
                                                 (SetOfMergedPaths.Initial maximumNumberOfTestVariables
                                                                                                 passAllFilter)
                                                 randomBehaviour
                                                 true
                let remergedPartialTestVectors
                    , setOfMergedPartialTestVectorsFromRemerge =
                    mergeOrAddPartialTestVectors [Map.empty]
                                                 mergedPartialTestVectors
                                                 randomBehaviour
                                                 true
                let shouldBeTrue =
                    setOfMergedPartialTestVectors = setOfMergedPartialTestVectorsFromRemerge
                Assert.IsTrue shouldBeTrue
            createOverlappingPartialTestVectorsAndHandEachOffToTest testHandoff

        [<Test>]
        member this.TestThatNotRevealingFullTestVectorsSuppressesThemFromTheEnumerationButNotTheEarlyAccessApi () =
            let randomBehaviour = Random randomBehaviourSeed
            let testHandoff partialTestVectors =
                let commonSeedForRandomBehaviourClones =
                    randomBehaviour.Next()  // Need to clone the random behaviour instances passed to the two calls to
                                            // 'mergeOrAddPartialTestVectors', because this function shuffles the partial
                                            // test vectors internally - which would otherwise legitimately result in
                                            // different merges in the two calls, and therefore different results.
                let randomBehaviourCloneOne =
                    Random(commonSeedForRandomBehaviourClones)
                let randomBehaviourCloneTwo =
                    Random(commonSeedForRandomBehaviourClones)
                let mergedPartialTestVectors
                    , setOfMergedPartialTestVectorsRevealingFullTestVectorsInEnumeration =
                    mergeOrAddPartialTestVectors partialTestVectors
                                                 (SetOfMergedPaths.Initial maximumNumberOfTestVariables
                                                                                                 passAllFilter)
                                                 randomBehaviourCloneOne
                                                 true
                let mergedPartialTestVectors
                    , setOfMergedPartialTestVectorsNotRevealingFullTestVectorsInEnumeration =
                    mergeOrAddPartialTestVectors partialTestVectors
                                                 (SetOfMergedPaths.Initial maximumNumberOfTestVariables
                                                                                                 passAllFilter)
                                                 randomBehaviourCloneTwo
                                                 false

                let shouldBeTrue =
                    setOfMergedPartialTestVectorsRevealingFullTestVectorsInEnumeration = setOfMergedPartialTestVectorsNotRevealingFullTestVectorsInEnumeration
                    // NOTE: this looks odd, but bear in mind that the result from the helper 'mergeOrAddPartialTestVectors' also
                    // includes the early access API results - so we are saying that nothing is lost overall at this point.

                    // NOTE: the check for suppression of the full test vectors is performed in the helper 'mergeOrAddPartialTestVectors'.

                if not shouldBeTrue
                then let common = Set.intersect setOfMergedPartialTestVectorsRevealingFullTestVectorsInEnumeration setOfMergedPartialTestVectorsNotRevealingFullTestVectorsInEnumeration
                     printf "setOfMergedPartialTestVectorsRevealingFullTestVectorsInEnumeration:\n"
                     setOfMergedPartialTestVectorsRevealingFullTestVectorsInEnumeration |> Set.iter dumpPartialTestVector
                     printf "setOfMergedPartialTestVectorsNotRevealingFullTestVectorsInEnumeration:\n"
                     setOfMergedPartialTestVectorsNotRevealingFullTestVectorsInEnumeration  |> Set.iter dumpPartialTestVector
                     printf "Only in setOfMergedPartialTestVectorsRevealingFullTestVectorsInEnumeration:-\n"
                     (setOfMergedPartialTestVectorsRevealingFullTestVectorsInEnumeration - common) |> Set.iter dumpPartialTestVector
                     printf "Only in setOfMergedPartialTestVectorsNotRevealingFullTestVectorsInEnumeration:-\n"
                     (setOfMergedPartialTestVectorsNotRevealingFullTestVectorsInEnumeration - common) |> Set.iter dumpPartialTestVector
                     printf "Common:-\n"
                     common |> Set.iter dumpPartialTestVector

                Assert.IsTrue shouldBeTrue
            createOverlappingPartialTestVectorsAndHandEachOffToTest testHandoff
            createNonOverlappingPartialTestVectorsAndHandEachOffToTest testHandoff

        [<Test>]
        member this.TestInitialStateIsEmptyAndDoesNotContainATrivialEmptyPartialTestVector () =
            for maximumNumberOfTestVariables in 0 .. maximumNumberOfTestVariables do   // NOTE: includes the boundary case of no test variables whatsover.
                let initial =
                    SetOfMergedPaths.Initial maximumNumberOfTestVariables
                                                                   passAllFilter
                let containedPartialTestVectors =
                    initial.EnumerationOfMergedTestVectors true
                    |> List.ofSeq
                let shouldBeTrue =
                    containedPartialTestVectors.Length = 0
                Assert.IsTrue shouldBeTrue