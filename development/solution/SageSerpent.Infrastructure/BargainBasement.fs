module SageSerpent.Infrastructure.BargainBasement

    open System.Collections.Generic
    open System
    open ListExtensions
    open RandomExtensions
    open SageSerpent.Infrastructure.IEnumerableExtensions

    let IsSorted items =
        IEnumerable<_>.IsSorted items

    let AssociatedValues association =
        (association :> IDictionary<'Key, 'Value>).Values



    let Memoize computation =
        let cache =
            System.Collections.Generic.Dictionary ()
        fun input ->
            if cache.ContainsKey input
            then cache.[input]
            else let result =
                    computation input
                 cache.Add (input, result)
                 result

    let Identity x =
        x

    type private TypePreservingFunction<'X> =
        delegate of 'X -> 'X

    let IdentityFunctionDelegate =
        TypePreservingFunction Identity :> Delegate

    let PartitionSizeIntoSectionsOfRandomNonZeroLength size
                                                       numberOfSubgroups
                                                       randomBehaviour =
        if size = 0
        then
            raise (PreconditionViolationException "Must have at least one item to start with for subgroups to have non-zero length.")
        else
            match numberOfSubgroups with
                0 ->
                    raise (PreconditionViolationException "Must have at least one subgroup to place items into.")
              | 1 ->
                    [ size ]
              | _ ->
                    if numberOfSubgroups > size
                    then
                        raise (PreconditionViolationException "Number of subgroups must be at most the number of items.")
                    else
                        let potentialPartitionPoints
                            = [1 .. size - 1]
                        let chosenPartitionPoints =
                            (randomBehaviour: Random).ChooseSeveralOf (potentialPartitionPoints, (numberOfSubgroups - 1))
                        let chosenSortedPartitionPoints =
                            chosenPartitionPoints |> Seq.sort
                        seq
                            {
                                yield 0
                                yield! chosenSortedPartitionPoints
                                yield size
                            }
                        |> Seq.pairwise
                        |> Seq.map (function lesserPartitionPoint
                                            , greaterPartitionPoint -> greaterPartitionPoint - lesserPartitionPoint)
                        |> List.ofSeq

    let PartitionItemsIntoSubgroupsOfRandomNonZeroLength items
                                                         numberOfSubgroups
                                                         randomBehaviour =
        let numberOfItems =
            List.length items
        let spans =
            PartitionSizeIntoSectionsOfRandomNonZeroLength numberOfItems
                                                           numberOfSubgroups
                                                           randomBehaviour
        items.ChopUpList spans

    let Flip f x y =
        f y x

    let NumberOfPermutations originalSize permutationSize =
        if permutationSize > originalSize
        then 0
        else let numberOfItemsLeftOutOfPermutation =
                originalSize - permutationSize
             let rec productOfPartialResultAndNumberOfSubpermutations originalSize partialResult =
                if originalSize = numberOfItemsLeftOutOfPermutation
                then partialResult
                else productOfPartialResultAndNumberOfSubpermutations (originalSize - 1) (originalSize * partialResult)
             productOfPartialResultAndNumberOfSubpermutations originalSize 1

    let Factorial x =
        NumberOfPermutations x x

    let NumberOfCombinations originalSize combinationSize =
        let unpickedSize =
            originalSize - combinationSize
        if combinationSize < unpickedSize
        then
            NumberOfPermutations originalSize combinationSize
            / Factorial combinationSize
        else
            NumberOfPermutations originalSize unpickedSize
            / Factorial unpickedSize


    let MappingAvoidingIndices sortedIndicesToAvoid =
        if Seq.length sortedIndicesToAvoid = 0
        then raise (PreconditionViolationException "Must have at least one index to avoid.")
        if not (IEnumerable<_>.IsSorted sortedIndicesToAvoid)
        then raise (PreconditionViolationException "Indices to avoid must be presented in ascending order.")
        let sortedAssociationBetweenIndicesAndIncrementsToApply =
            let arrangeDeferredAssociations (runningIncrement
                                             , deferredActionsForPredecessors)
                                            indexToAvoid =
                let nextIncrement = runningIncrement + 1
                nextIncrement
                , (fun associationBuiltSoFar ->
                    deferredActionsForPredecessors ((indexToAvoid - runningIncrement
                                                    , nextIncrement)
                                                      :: associationBuiltSoFar))
            (sortedIndicesToAvoid
             |> Seq.fold arrangeDeferredAssociations (0, (fun result -> result))
             |> snd) []
            |> Map.ofList   // This has the effect of eliminating all but the last entry for a group of associations
                            // for consecutive indices. Otherwise the associations for lesser indices in the group
                            // would just map onto the next higher index, which we are also trying to avoid.
            |> Map.toArray
        let remapIndex index =
            let foundIndex =
                Array.BinarySearch (sortedAssociationBetweenIndicesAndIncrementsToApply,
                                    (index
                                     , 0),
                                    {
                                        new IComparer<Int32 * Int32> with
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
                     else 0
            index + incrementToApplyToIndex
        remapIndex

    let MergeSortedAssociationLists joinAtSameKey lhs rhs =
        let rec mergeSortedAssociationLists lhs rhs =
            match lhs
                    , rhs with
                []
                , [] -> []
                | ((lhsHeadKey, lhsHeadValue) as lhsHead :: lhsTail)
                , ((rhsHeadKey, rhsHeadValue) as rhsHead :: rhsTail) ->
                    match compare lhsHeadKey rhsHeadKey with
                        result when result < 0 ->
                            lhsHead :: mergeSortedAssociationLists lhsTail rhs
                        | result when result > 0 ->
                            rhsHead :: mergeSortedAssociationLists lhs rhsTail
                        | _ ->
                            (lhsHeadKey, joinAtSameKey lhsHeadValue rhsHeadValue) :: mergeSortedAssociationLists lhsTail rhsTail
                | _
                , [] ->
                    lhs
                | []
                , _ ->
                    rhs
        mergeSortedAssociationLists lhs rhs

    let MergeDisjointSortedAssociationLists lhs rhs =
        MergeSortedAssociationLists (fun lhsValue rhsValue ->
                                        raise (InternalAssertionViolationException "The keys from the two disjoint association lists should not have common entries to join."))
                                    lhs
                                    rhs

    let MergeMaps joinAtSameKey lhs rhs =
        MergeSortedAssociationLists joinAtSameKey (Map.toList lhs) (Map.toList rhs)
        |> Map.ofList

    let CollectAcrossSortedAssociationLists associationLists =
        let result =
            if associationLists
               |> List.isEmpty
            then
                List.empty
            else
                associationLists
                |> List.map (List.map (fun (key
                                            , associatedItem) ->
                                        key
                                        , [associatedItem]))
                |> List.reduce (MergeSortedAssociationLists List.append)
        if result
           |> List.exists (snd >> List.isEmpty)
        then
            raise (InternalAssertionViolationException "The associated lists should always have a contribution from at least one map.")
        result

    let ZipAssociationLists lhs rhs =
        let rec zipAssociationLists lhs rhs =
            match lhs
                    , rhs with
                ((lhsHeadKey, lhsHeadValue) as lhsHead :: lhsTail)
                , ((rhsHeadKey, rhsHeadValue) as rhsHead :: rhsTail) ->
                    match compare lhsHeadKey rhsHeadKey with
                        result when result < 0 ->
                            zipAssociationLists lhsTail rhs
                        | result when result > 0 ->
                            zipAssociationLists lhs rhsTail
                        | _ ->
                            (lhsHeadKey, (lhsHeadValue, rhsHeadValue)) :: zipAssociationLists lhsTail rhsTail
              | _ ->
                    []

        zipAssociationLists lhs rhs

    let ZipMaps lhs rhs =
        ZipAssociationLists (Map.toList lhs) (Map.toList rhs)
        |> Map.ofList
