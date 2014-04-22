namespace NTestCaseBuilder.Tests

    open NUnit.Framework

    open SageSerpent.Infrastructure
    open SageSerpent.Infrastructure.RandomExtensions
    open System
    open System.Collections.Generic
    open Microsoft.FSharp.Collections


    [<TestFixture>]
    type SetOfMergedPathsTestFixture () =
        let passAllFilter _ =
            true

        let randomBehaviourSeed = 23

        let reciprocalOfProbabilityOfTerminatingRandomlyGeneratedPartOfIncompletePath = 10

        let reciprocalOfProbabilityOfNotGeneratingAnyFurtherPartialPaths = 30

        let maximumNumberOfPathSteps = 70

        let maximumNumberOfPaths = 1000

        let maximumRandomWalkStep = 10

        let maximumStepDelta = 5

        let overallTestRepeats = 300

        let maximumNumberOfIndicesToAvoid = 4

        let createNonOverlappingPartialPathsAndHandEachOffToTest testHandoff =
            let randomBehaviour =
                Random randomBehaviourSeed
            let createPartialPaths () =
                let createShuffledUniqueSteps () =
                    randomBehaviour.Shuffle (seq {1 .. maximumNumberOfPaths})
                let shuffleForEachPathStepIndex =
                    Array.init maximumNumberOfPathSteps (fun _ -> createShuffledUniqueSteps ())
                let numberOfPaths =
                    randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfPaths
                let rec fillInPartialPaths incompletePartialPaths
                                                 completedPartialPaths =
                    match incompletePartialPaths with
                        [] ->
                            raise (InternalAssertionViolationException "Base case for recursion is for *one* incomplete path.")
                      | head :: [] ->
                            let rec ensurePathIsNonEmpty pathBeingCompleted =
                                let pathStepIndex =
                                    randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumNumberOfPathSteps
                                let shuffledSteps =
                                    shuffleForEachPathStepIndex.[pathStepIndex]
                                let numberOfCompletedPartialPaths =
                                    List.length completedPartialPaths
                                let stepForPathBeingCompleted =
                                    shuffledSteps.[numberOfCompletedPartialPaths]
                                Map.add pathStepIndex stepForPathBeingCompleted
                                                          (if randomBehaviour.HeadsItIs ()
                                                           then pathBeingCompleted
                                                           else ensurePathIsNonEmpty pathBeingCompleted)
                            ensurePathIsNonEmpty head
                            :: completedPartialPaths
                      | head :: tail ->
                            let forceDistinctionBetweenPaths pathBeingExamined
                                                               (modifiedCopiesOfPathsBeingExamined
                                                                , pathBeingCompleted) =
                                let pathStepIndex =
                                    randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumNumberOfPathSteps
                                let shuffledSteps =
                                    shuffleForEachPathStepIndex.[pathStepIndex]
                                let numberOfCompletedPartialPaths =
                                    List.length completedPartialPaths
                                let numberOfCopiesOfPathsBeingExamined =
                                    List.length modifiedCopiesOfPathsBeingExamined
                                let stepForPathBeingExamined =
                                    shuffledSteps.[numberOfPaths - (1 + numberOfCopiesOfPathsBeingExamined)]
                                let pathBeingExamined =
                                    Map.add pathStepIndex stepForPathBeingExamined pathBeingExamined
                                let stepForPathBeingCompleted =
                                    shuffledSteps.[numberOfCompletedPartialPaths]
                                let pathBeingCompleted =
                                    Map.add pathStepIndex stepForPathBeingCompleted pathBeingCompleted
                                pathBeingExamined :: modifiedCopiesOfPathsBeingExamined
                                , pathBeingCompleted
                            let modifiedIncompletePartialPaths
                                , completedIncompletePath =
                                List.foldBack forceDistinctionBetweenPaths tail ([], head)
                            fillInPartialPaths modifiedIncompletePartialPaths
                                                     (completedIncompletePath :: completedPartialPaths)
                let incompletePaths =
                    fillInPartialPaths (List.replicate numberOfPaths Map.empty)
                                             []
                randomBehaviour.Shuffle incompletePaths
                |> List.ofArray
            for _ in 1 .. overallTestRepeats do
                testHandoff (createPartialPaths ())

        let createOverlappingPartialPathsAndHandEachOffToTest testHandoff =
            let randomBehaviour =
                Random randomBehaviourSeed
            let createPartialPaths () =
                let rec createPartialPaths pathStepIndex =
                    if pathStepIndex = maximumNumberOfPathSteps
                       || 0 < pathStepIndex
                          && randomBehaviour.ChooseAnyNumberFromOneTo reciprocalOfProbabilityOfNotGeneratingAnyFurtherPartialPaths = 1
                    then []
                    else let rec choosePathStepIndicesAndTheirSteps recursionDepth =
                            let maximumRecursionDepth = 50
                            if recursionDepth = maximumRecursionDepth
                               || randomBehaviour.ChooseAnyNumberFromOneTo reciprocalOfProbabilityOfTerminatingRandomlyGeneratedPartOfIncompletePath = 1
                            then []
                            else let lowerBoundInclusive =
                                    if pathStepIndex > maximumRandomWalkStep
                                    then pathStepIndex - maximumRandomWalkStep
                                    else 0
                                 let upperBoundInclusive =
                                    if pathStepIndex + maximumRandomWalkStep >= maximumNumberOfPathSteps
                                    then maximumNumberOfPathSteps - 1
                                    else pathStepIndex + maximumRandomWalkStep
                                 let chosenAbitraryPathStepIndex =
                                    lowerBoundInclusive + randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (upperBoundInclusive + 1 - lowerBoundInclusive)
                                 (chosenAbitraryPathStepIndex
                                  , randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumStepDelta)
                                 :: choosePathStepIndicesAndTheirSteps (recursionDepth + 1)
                         let incompletePath =
                            choosePathStepIndicesAndTheirSteps 0
                            |> Map.ofList
                         incompletePath :: createPartialPaths (pathStepIndex + 1)
                createPartialPaths 0
            let overlappingPartialPaths =
                createPartialPaths ()

            let overlappingPartialPathsWithSomeExtraCompletePaths =
                seq
                    {
                        for incompletePath in overlappingPartialPaths do
                            yield incompletePath

                            if randomBehaviour.HeadsItIs()
                            then
                                let pathStepIndices =
                                    incompletePath
                                    |> Map.toList
                                    |> List.map fst
                                let unusedPathStepIndices =
                                    (List.init maximumNumberOfPathSteps BargainBasement.Identity
                                     |> Set.ofList) - (pathStepIndices
                                                       |> Set.ofList)
                                let extraPadding =
                                    unusedPathStepIndices
                                    |> Seq.map (fun pathStepIndex -> pathStepIndex, randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumNumberOfPathSteps)

                                yield incompletePath
                                |> Map.toSeq
                                |> Seq.append extraPadding
                                |> Map.ofSeq
                    }
                |> randomBehaviour.Shuffle
                |> List.ofArray

            for _ in 1 .. overallTestRepeats do
                testHandoff overlappingPartialPathsWithSomeExtraCompletePaths


        let dumpIncompletePath incompletePath =
            printf "[ "
            incompletePath
            |> Map.iter (fun pathStepIndex testStep ->
                            printf "(%A, %A) " pathStepIndex testStep)
            printf "]\n"

        let mergeOrAddPartialPaths incompletePaths
                                         initialCollection
                                         randomBehaviour
                                         revealCompletePathsAgain =
            let maximumNumberOfPathSteps =
                (initialCollection: SetOfMergedPaths<_>).NumberOfStepsInACompletePath
            let shuffledDuplicatedPartialPaths =
                (randomBehaviour: Random).Shuffle (List.append incompletePaths incompletePaths)
                |> List.ofArray
            let mergedPartialPaths
                , setOfMergedCompletePaths =
                shuffledDuplicatedPartialPaths
                |> List.fold (fun (mergedPartialPaths, setOfCompletePaths) incompletePath ->
                                match (mergedPartialPaths: SetOfMergedPaths<_>).MergeOrAdd incompletePath with
                                    updatedSetOfMergedPathsWithCompleteTestCasePathSuppressed
                                    , Some resultingCompleteTestCasePath ->
                                        let shouldBeTrue =
                                            not (Set.contains resultingCompleteTestCasePath
                                                              setOfCompletePaths)

                                        Assert.IsTrue shouldBeTrue

                                        updatedSetOfMergedPathsWithCompleteTestCasePathSuppressed
                                        , Set.add resultingCompleteTestCasePath
                                                  setOfCompletePaths
                                  | updatedSetOfMergedPaths
                                    , None ->
                                        updatedSetOfMergedPaths
                                        , setOfCompletePaths)
                             (initialCollection, Set.empty)
            let isCompletePath incompletePath =
                let sumOfIndicesExpectedForCompletePath =
                    maximumNumberOfPathSteps * (maximumNumberOfPathSteps + 1) / 2
                incompletePath
                |> Map.toSeq
                |> Seq.map (fst >> (fun pathStepIndex -> 1 + pathStepIndex))   // NOTE: shift by one because otherwise the leading term for a sum of zero-relative indices would not show up in the sum!
                |> Seq.reduce (+)
                 = sumOfIndicesExpectedForCompletePath

            let shouldBeTrue =
                setOfMergedCompletePaths
                |> Set.forall isCompletePath

            Assert.IsTrue shouldBeTrue

            if revealCompletePathsAgain
            then
                let setOfAllMergedPaths =
                    mergedPartialPaths.EnumerationOfMergedPaths true
                    |> Set.ofSeq

                let shouldBeTrue =
                    Set.isSubset setOfMergedCompletePaths setOfAllMergedPaths
                Assert.IsTrue shouldBeTrue

                mergedPartialPaths
                , setOfAllMergedPaths
            else
                let setOfMergedPartialPaths =
                    mergedPartialPaths.EnumerationOfMergedPaths false
                    |> Set.ofSeq

                let shouldBeTrue =
                    Set.isEmpty (Set.intersect setOfMergedCompletePaths setOfMergedPartialPaths)

                if not shouldBeTrue
                then let common = Set.intersect setOfMergedCompletePaths setOfMergedPartialPaths
                     printf "setOfMergedCompletePaths:\n"
                     setOfMergedCompletePaths |> Set.iter dumpIncompletePath
                     printf "setOfMergedPartialPaths:\n"
                     setOfMergedPartialPaths  |> Set.iter dumpIncompletePath
                     printf "Only in setOfMergedCompletePaths:-\n"
                     (setOfMergedCompletePaths - common) |> Set.iter dumpIncompletePath
                     printf "Only in setOfMergedPartialPaths:-\n"
                     (setOfMergedPartialPaths - common) |> Set.iter dumpIncompletePath
                     printf "Common:-\n"
                     common |> Set.iter dumpIncompletePath

                Assert.IsTrue shouldBeTrue

                let shouldBeTrue =
                    setOfMergedPartialPaths
                    |> Set.forall (isCompletePath >> not)

                if not shouldBeTrue
                then
                    printf "Maximum number of steps: %A\n" maximumNumberOfPathSteps
                    printf "Got at least one complete path that is just from the enumeration:-\n"
                    setOfMergedPartialPaths |> Set.filter isCompletePath |> Set.iter dumpIncompletePath

                Assert.IsTrue shouldBeTrue

                mergedPartialPaths
                , (setOfMergedPartialPaths
                   |> Set.union setOfMergedCompletePaths)

        [<Test>]
        member this.TestAdditionOfUnmergeablePathsPreservesIndividualPaths () =
            let randomBehaviour = Random randomBehaviourSeed
            let testHandoff incompletePathsThatDoNotOverlap =
                let mergedPartialPaths
                    , setOfMergedPartialPaths =
                    mergeOrAddPartialPaths incompletePathsThatDoNotOverlap
                                                 (SetOfMergedPaths.Initial maximumNumberOfPathSteps
                                                                                                 passAllFilter)
                                                 randomBehaviour
                                                 true
                let shouldBeTrue =
                    Set.ofList incompletePathsThatDoNotOverlap = setOfMergedPartialPaths
                if not shouldBeTrue
                then let originals = Set.ofList incompletePathsThatDoNotOverlap
                     let merged = mergedPartialPaths.EnumerationOfMergedPaths true |> Set.ofSeq
                     let common = Set.intersect originals merged
                     printf "Only in originals:-\n"
                     (originals - common) |> Set.iter dumpIncompletePath
                     printf "Only in merged:-\n"
                     (merged - common) |> Set.iter dumpIncompletePath
                     printf "Common:-\n"
                     common |> Set.iter dumpIncompletePath
                Assert.IsTrue shouldBeTrue
            createNonOverlappingPartialPathsAndHandEachOffToTest testHandoff

        [<Test>]
        member this.TestAdditionOfPartialsOfExistingPaths () =
            let randomBehaviour = Random randomBehaviourSeed
            let testHandoff incompletePathsThatDoNotOverlap =
                let mergedPartialPaths
                    , _ =
                    mergeOrAddPartialPaths incompletePathsThatDoNotOverlap
                                                 (SetOfMergedPaths.Initial maximumNumberOfPathSteps
                                                                                                 passAllFilter)
                                                 randomBehaviour
                                                 true
                let mutantsOrCopiesOf incompletePath =
                    let maximumRecursionDepth = 50
                    let rec createMutantOrCopy recursionDepth =
                        let mutantOrCopy =
                            match randomBehaviour.ChooseAnyNumberFromOneTo 3 with
                                1 ->
                                    randomBehaviour.ChooseSeveralOf ((Map.toSeq incompletePath)
                                                                     , (randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan incompletePath.Count))
                                 |> Map.ofSeq
                              | 2 ->
                                    Map.toSeq incompletePath
                                    |> Seq.take (randomBehaviour.ChooseAnyNumberFromOneTo (incompletePath.Count - 1))
                                    |> Map.ofSeq
                              | 3 ->
                                    incompletePath
                              | _ ->
                                    raise (InternalAssertionViolationException "This case should never be matched.")
                        mutantOrCopy
                        :: if recursionDepth = maximumRecursionDepth
                              || randomBehaviour.HeadsItIs ()
                           then []
                           else createMutantOrCopy (recursionDepth + 1)
                    createMutantOrCopy 0
                let incompletePathsThatAddNoNewInformation =
                    incompletePathsThatDoNotOverlap
                    |> List.map mutantsOrCopiesOf
                    |> List.concat
                let remergedPartialPaths
                    , setOfMergedPartialPathsFromRemerge =
                    mergeOrAddPartialPaths incompletePathsThatAddNoNewInformation
                                                 mergedPartialPaths
                                                 randomBehaviour
                                                 true
                let shouldBeTrue =
                    (Set.ofList incompletePathsThatDoNotOverlap).Count = setOfMergedPartialPathsFromRemerge.Count
                if not shouldBeTrue
                then let originals = Set.ofList incompletePathsThatDoNotOverlap
                     let remerged = remergedPartialPaths.EnumerationOfMergedPaths true |> Set.ofSeq
                     let common = Set.intersect originals remerged
                     printf "Only in originals:-\n"
                     (originals - common) |> Set.iter dumpIncompletePath
                     printf "Only in remerged:-\n"
                     (remerged - common) |> Set.iter dumpIncompletePath
                     printf "Common:-\n"
                     common |> Set.iter dumpIncompletePath
                Assert.IsTrue shouldBeTrue
            createNonOverlappingPartialPathsAndHandEachOffToTest testHandoff

        [<Test>]
        member this.TestMergingOfPathsInWithExistingPartialPaths () =
            let randomBehaviour = Random randomBehaviourSeed

            let testHandoff incompletePathsThatDoNotOverlap =
                let numberOfIndicesToAvoid =
                    randomBehaviour.ChooseAnyNumberFromOneTo maximumNumberOfIndicesToAvoid
                let sortedIndicesToAvoid =
                    randomBehaviour.ChooseSeveralOf ((List.init maximumNumberOfPathSteps BargainBasement.Identity)
                                                     , numberOfIndicesToAvoid)
                    |> List.ofArray
                    |> List.sort
                let mappingAvoidingIndices =
                    BargainBasement.MappingAvoidingIndices sortedIndicesToAvoid
                let maximumNumberOfPathStepsAfterRemapping =
                    maximumNumberOfPathSteps + numberOfIndicesToAvoid
                let rotationOffsetToAllowCoverageOfTrailingIndices =
                    randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumNumberOfIndicesToAvoid
                let rotation =
                    fun pathStepIndex -> (rotationOffsetToAllowCoverageOfTrailingIndices + pathStepIndex) % maximumNumberOfPathStepsAfterRemapping
                let mappingAvoidingIndicesThenRotation =
                    mappingAvoidingIndices >> rotation
                let remappedPartialPaths =
                    incompletePathsThatDoNotOverlap
                    |> List.map (Map.toList >> List.map (function index
                                                                  , step ->
                                                                    mappingAvoidingIndicesThenRotation index
                                                                    , step) >> Map.ofList)
                let mergedPartialPaths
                    , _ =
                    mergeOrAddPartialPaths remappedPartialPaths
                                                 (SetOfMergedPaths.Initial maximumNumberOfPathStepsAfterRemapping
                                                                                                 passAllFilter)
                                                 randomBehaviour
                                                 true
                let possiblyAddStepsForIndices indicesToAdd incompletePath =
                    let chosenIndicesToAdd =
                        randomBehaviour.ChooseSeveralOf (indicesToAdd
                                                         , (randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan (Seq.length indicesToAdd + 1)))
                    seq {for index in chosenIndicesToAdd do
                            yield index, index + randomBehaviour.ChooseAnyNumberFromZeroToOneLessThan maximumStepDelta}
                    |> (let addIndexAndStep incompletePath
                                             (pathStepIndex
                                              , step)
                                             =
                            Map.add pathStepIndex step incompletePath
                        Seq.fold addIndexAndStep incompletePath)
                let sortedIndicesToAvoidWithRotationApplied =
                    sortedIndicesToAvoid
                    |> List.map rotation
                let incompletePathsWhichMayHaveThePreviouslyAvoidedIndices =
                    remappedPartialPaths
                    |> List.map (possiblyAddStepsForIndices sortedIndicesToAvoidWithRotationApplied)
                let remergedPartialPaths
                    , setOfMergedPartialPathsFromRemerge =
                    mergeOrAddPartialPaths incompletePathsWhichMayHaveThePreviouslyAvoidedIndices
                                                 mergedPartialPaths
                                                 randomBehaviour
                                                 true
                let shouldBeTrue =
                    Set.ofList incompletePathsWhichMayHaveThePreviouslyAvoidedIndices = setOfMergedPartialPathsFromRemerge
                if not shouldBeTrue
                then let originals = Set.ofList incompletePathsWhichMayHaveThePreviouslyAvoidedIndices
                     let remerged = remergedPartialPaths.EnumerationOfMergedPaths true |> Set.ofSeq
                     let common = Set.intersect originals remerged
                     printf "remappedPartialPaths:\n"
                     Set.ofList remappedPartialPaths |> Set.iter dumpIncompletePath
                     printf "mergedPartialPaths:\n"
                     mergedPartialPaths.EnumerationOfMergedPaths true |> Set.ofSeq  |> Set.iter dumpIncompletePath
                     printf "Only in originals:-\n"
                     (originals - common) |> Set.iter dumpIncompletePath
                     printf "Only in remerged:-\n"
                     (remerged - common) |> Set.iter dumpIncompletePath
                     printf "Common:-\n"
                     common |> Set.iter dumpIncompletePath
                     printf "Indices to avoid with rotation applied: %A\n" sortedIndicesToAvoidWithRotationApplied
                Assert.IsTrue shouldBeTrue
            createNonOverlappingPartialPathsAndHandEachOffToTest testHandoff

        [<Test>]
        member this.PathsAreEitherAddedOrMerged () =
            let randomBehaviour = Random randomBehaviourSeed
            let testHandoff incompletePaths =
                let mergedPartialPaths
                    , setOfMergedPartialPaths =
                        mergeOrAddPartialPaths incompletePaths
                                                     (SetOfMergedPaths.Initial maximumNumberOfPathSteps
                                                                                                     passAllFilter)
                                                     randomBehaviour
                                                     true
                let numberOfMergedPartialPaths =
                    setOfMergedPartialPaths.Count
                let shouldBeTrue = numberOfMergedPartialPaths <= incompletePaths.Length
                Assert.IsTrue shouldBeTrue
                let incompletePathsThatWereChanged =
                    incompletePaths
                    |> List.filter (fun incompletePath -> not (setOfMergedPartialPaths.Contains incompletePath))
                    |> Set.ofSeq
                let checkInclusionInAtLeastOneMergedIncompletePath incompletePath =
                    let firstIncludesSecond first second =
                        (first
                         |> Map.toList
                         |> Set.ofList).IsSupersetOf (second
                                                       |> Map.toList
                                                       |> Set.ofList)
                    let shouldBeTrue =
                        setOfMergedPartialPaths
                        |> Set.exists (fun mergedIncompletePath
                                            -> firstIncludesSecond mergedIncompletePath incompletePath)
                    Assert.IsTrue shouldBeTrue
                incompletePathsThatWereChanged
                |> Seq.iter checkInclusionInAtLeastOneMergedIncompletePath
            createOverlappingPartialPathsAndHandEachOffToTest testHandoff

        [<Test>]
        member this.TestAdditionOfTrivialEmptyPartialPathsLeavesCollectionUnchanged () =
            let randomBehaviour = Random randomBehaviourSeed
            let testHandoff incompletePaths =
                let mergedPartialPaths
                    , setOfMergedPartialPaths =
                    mergeOrAddPartialPaths incompletePaths
                                                 (SetOfMergedPaths.Initial maximumNumberOfPathSteps
                                                                                                 passAllFilter)
                                                 randomBehaviour
                                                 true
                let remergedPartialPaths
                    , setOfMergedPartialPathsFromRemerge =
                    mergeOrAddPartialPaths [Map.empty]
                                                 mergedPartialPaths
                                                 randomBehaviour
                                                 true
                let shouldBeTrue =
                    setOfMergedPartialPaths = setOfMergedPartialPathsFromRemerge
                Assert.IsTrue shouldBeTrue
            createOverlappingPartialPathsAndHandEachOffToTest testHandoff

        [<Test>]
        member this.TestThatNotRevealingCompletePathsSuppressesThemFromTheEnumerationButNotTheEarlyAccessApi () =
            let randomBehaviour = Random randomBehaviourSeed
            let testHandoff incompletePaths =
                let commonSeedForRandomBehaviourClones =
                    randomBehaviour.Next()  // Need to clone the random behaviour instances passed to the two calls to
                                            // 'mergeOrAddPartialPaths', because this function shuffles the incomplete
                                            // paths internally - which would otherwise legitimately result in
                                            // different merges in the two calls, and therefore different results.
                let randomBehaviourCloneOne =
                    Random(commonSeedForRandomBehaviourClones)
                let randomBehaviourCloneTwo =
                    Random(commonSeedForRandomBehaviourClones)
                let mergedPartialPaths
                    , setOfMergedPartialPathsRevealingCompletePathsInEnumeration =
                    mergeOrAddPartialPaths incompletePaths
                                                 (SetOfMergedPaths.Initial maximumNumberOfPathSteps
                                                                                                 passAllFilter)
                                                 randomBehaviourCloneOne
                                                 true
                let mergedPartialPaths
                    , setOfMergedPartialPathsNotRevealingCompletePathsInEnumeration =
                    mergeOrAddPartialPaths incompletePaths
                                                 (SetOfMergedPaths.Initial maximumNumberOfPathSteps
                                                                                                 passAllFilter)
                                                 randomBehaviourCloneTwo
                                                 false

                let shouldBeTrue =
                    setOfMergedPartialPathsRevealingCompletePathsInEnumeration = setOfMergedPartialPathsNotRevealingCompletePathsInEnumeration
                    // NOTE: this looks odd, but bear in mind that the result from the helper 'mergeOrAddPartialPaths' also
                    // includes the early access API results - so we are saying that nothing is lost overall at this point.

                    // NOTE: the check for suppression of the complete paths is performed in the helper 'mergeOrAddPartialPaths'.

                if not shouldBeTrue
                then let common = Set.intersect setOfMergedPartialPathsRevealingCompletePathsInEnumeration setOfMergedPartialPathsNotRevealingCompletePathsInEnumeration
                     printf "setOfMergedPartialPathsRevealingCompletePathsInEnumeration:\n"
                     setOfMergedPartialPathsRevealingCompletePathsInEnumeration |> Set.iter dumpIncompletePath
                     printf "setOfMergedPartialPathsNotRevealingCompletePathsInEnumeration:\n"
                     setOfMergedPartialPathsNotRevealingCompletePathsInEnumeration  |> Set.iter dumpIncompletePath
                     printf "Only in setOfMergedPartialPathsRevealingCompletePathsInEnumeration:-\n"
                     (setOfMergedPartialPathsRevealingCompletePathsInEnumeration - common) |> Set.iter dumpIncompletePath
                     printf "Only in setOfMergedPartialPathsNotRevealingCompletePathsInEnumeration:-\n"
                     (setOfMergedPartialPathsNotRevealingCompletePathsInEnumeration - common) |> Set.iter dumpIncompletePath
                     printf "Common:-\n"
                     common |> Set.iter dumpIncompletePath

                Assert.IsTrue shouldBeTrue
            createOverlappingPartialPathsAndHandEachOffToTest testHandoff
            createNonOverlappingPartialPathsAndHandEachOffToTest testHandoff

        [<Test>]
        member this.TestInitialStateIsEmptyAndDoesNotContainATrivialEmptyIncompletePath () =
            for maximumNumberOfPathSteps in 0 .. maximumNumberOfPathSteps do   // NOTE: includes the boundary case of no steps whatsover.
                let initial =
                    SetOfMergedPaths.Initial maximumNumberOfPathSteps
                                                                   passAllFilter
                let containedPartialPaths =
                    initial.EnumerationOfMergedPaths true
                    |> List.ofSeq
                let shouldBeTrue =
                    containedPartialPaths.Length = 0
                Assert.IsTrue shouldBeTrue
