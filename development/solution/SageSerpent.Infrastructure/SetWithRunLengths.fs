namespace SageSerpent.Infrastructure
    open System
    open System.Collections.Generic
    open Microsoft.FSharp.Collections

    module SetWithRunLengthsDetail =
        [<CustomComparison; CustomEquality>]
        type SlotKey =
            Singleton of UInt32
          | Interval of UInt32 * UInt32 // These are the *inclusive* lower- and upper-bounds, respectively.
                                        // No need to worry about empty and singleton intervals, obviously.
            interface IComparable<SlotKey> with
                // NOTE: be careful here; the comparison semantics are deliberately sloppy - they violate the rules for a total ordering, because
                // it is possible to take one slot key A that is less than another slot key B, and then use encompassing or overlapping interval
                // slot keys to 'bridge the gap' between A and B via of chain of intervals that compare equal to each other and to A and B.
                // As long as the client code (which is actually 'SetWithRunLengths') doesn't allow multiple items that compare equal in the same
                // data structure, then this won't cause a problem.
                member this.CompareTo another =
                    match this
                            , another with
                        Singleton lhs
                        , Singleton rhs ->
                            compare lhs rhs
                        | Singleton lhs
                        , Interval (rhsLowerBound
                                    , rhsUpperBound) ->
                            if lhs < rhsLowerBound
                            then
                                -1
                            else if lhs > rhsUpperBound
                            then
                                1
                            else
                                0
                        | Interval (lhsLowerBound
                                    , lhsUpperBound)
                        , Singleton rhs ->
                            if rhs < lhsLowerBound
                            then
                                1
                            else if rhs > lhsUpperBound
                            then
                                -1
                            else
                                0  
                        | Interval (_
                                    , lhsUpperBound)
                        , Interval (rhsLowerBound
                                    , _) when lhsUpperBound < rhsLowerBound ->
                            -1
                        | Interval (lhsLowerBound
                                    , _)
                        , Interval (_
                                    , rhsUpperBound) when rhsUpperBound < lhsLowerBound ->
                            1
                        | _ ->
                            0

            interface IComparable with
                member this.CompareTo another =
                    match another with
                        :? SlotKey as another ->
                            (this :> IComparable<SlotKey>).CompareTo another
                      | _ ->
                            raise (ArgumentException (sprintf "Rhs of comparison must also be of type: %A"
                                                              typeof<SlotKey>.Name))

            override this.Equals another =
                match another with
                    :? SlotKey as another ->
                        0 = compare this
                                    another
                  | _ -> 
                        false

            override this.GetHashCode () =
                match this with
                    Singleton key ->
                        1 + hash key
                  | Interval (_) as pair ->
                        7 * (3 + hash pair)

    open SetWithRunLengthsDetail

    type SetWithRunLengths(representation: C5.ISorted<SlotKey>) =
        do
            for slotKey in representation do
                if Unchecked.defaultof<SlotKey> = slotKey
                then
                    raise (PreconditionViolationException "Null key detected.")
            for slotKey in representation do
                match slotKey with
                    Interval (lowerBound
                              , upperBound) ->
                        if upperBound + 1u < lowerBound
                        then
                            raise (PreconditionViolationException "Malformed interval slot key detected: bounds are ordered in reverse.")
                        else if upperBound + 1u = lowerBound
                        then
                            raise (PreconditionViolationException "Empty interval slot key detected - should have been represented by the absence of the slot key altogether.")
                        else if upperBound = lowerBound
                        then
                            raise (PreconditionViolationException "One-item interval slot key detected - should have been represented by a singleton slot key instead.")
                  | _ ->
                        ()
            for predecessorSlotKey
                , successorSlotKey in representation
                                      |> Seq.pairwise do
                let shouldBeTrue =
                    match predecessorSlotKey
                            , successorSlotKey with
                        Singleton predecessorKey
                        , Singleton successorKey when 1u + predecessorKey = successorKey ->
                            false
                        | Interval (_
                                    , predecessorUpperBound)
                        , Singleton successorKey when 1u + predecessorUpperBound = successorKey ->
                            false
                        | Singleton predecessorKey
                        , Interval (successorLowerBound
                                    , _) when 1u + predecessorKey = successorLowerBound ->
                            false
                        | Interval (_
                                    , predecessorUpperBound)
                        , Interval (successorLowerBound
                                    , _) when 1u + predecessorUpperBound = successorLowerBound ->
                            false
                        | _ ->
                            true
                if not shouldBeTrue
                then
                    raise (PreconditionViolationException "Adjacent slots found that have the same associated value and can be fused together.")

        member this.Contains key =
            representation.Contains (Singleton key)

        member this.Count: int32 =
            representation
            |> Seq.fold (fun count
                             slotKey ->
                                count +
                                match slotKey with
                                    Singleton _ ->
                                        1
                                  | Interval (lowerBound
                                             , upperBound) ->
                                        upperBound + 1u - lowerBound
                                        |> int32)
                        0

        member this.IsEmpty =
            representation.IsEmpty

        static member FuseAndDiscardConflictingAssociations inefficientRepresentation =
            let rec discardConflicts representation
                                     reversedPrefixOfResult =
                match representation with
                    [] ->
                        reversedPrefixOfResult
                  | [singleton] ->
                        singleton :: reversedPrefixOfResult
                  | firstKey as first :: ((secondKey :: _) as nonEmptyTail) when firstKey = secondKey ->
                        discardConflicts nonEmptyTail
                                         reversedPrefixOfResult
                  | first :: ((_ :: _) as nonEmptyList) ->
                        discardConflicts nonEmptyList
                                         (first :: reversedPrefixOfResult)
            let reversedRepresentationWithoutConflicts =
                discardConflicts inefficientRepresentation
                                 []
            let rec fuse representation
                         reversedPrefixOfResult =
                match representation with
                    [] ->
                        reversedPrefixOfResult
                  | [singleton] ->
                        singleton :: reversedPrefixOfResult
                  | firstKey as first :: ((secondKey :: tail) as nonEmptyTail) ->
                        match firstKey
                              , secondKey with
                            Singleton firstKey
                            , Singleton secondKey when 1u + firstKey = secondKey ->
                                fuse (Interval (firstKey
                                                , secondKey) :: tail)
                                     reversedPrefixOfResult
                          | Interval (firstLowerBound
                                      , firstUpperBound)
                            , Interval (secondLowerBound
                                        , secondUpperBound) when 1u + firstUpperBound = secondLowerBound ->
                                fuse (Interval (firstLowerBound
                                                , secondUpperBound) :: tail)
                                     reversedPrefixOfResult
                          | Singleton firstKey
                            , Interval (secondLowerBound
                                        , secondUpperBound) when 1u + firstKey = secondLowerBound ->
                                fuse (Interval (firstKey
                                                , secondUpperBound) :: tail)
                                     reversedPrefixOfResult
                          | Interval (firstLowerBound
                                      , firstUpperBound)
                            , Singleton secondKey when 1u + firstUpperBound = secondKey ->
                                fuse (Interval (firstLowerBound
                                                , secondKey) :: tail)
                                     reversedPrefixOfResult
                          | _ ->
                                fuse nonEmptyTail
                                     (first :: reversedPrefixOfResult)
            let result =
                fuse (reversedRepresentationWithoutConflicts
                      |> List.rev)
                     List.empty
                |> List.rev
            let unwind slotKey =
                match slotKey with
                    Singleton key ->
                        [key]
                  | Interval (lowerBound
                              , upperBound) ->
                        [
                            for key in lowerBound .. upperBound do
                                yield key
                        ]
            let shouldBeTrue =
                (inefficientRepresentation
                 |> List.map unwind
                 |> List.concat
                 |> Set.ofList).IsSupersetOf (result
                                              |> List.map unwind
                                              |> List.concat
                                              |> Set.ofList)
            if not shouldBeTrue
            then
                raise (LogicErrorException "Postcondition violation: the fused representation should only cover key-value pairs that are also covered by the inefficient representation.")
            let shouldBeTrue =
                (inefficientRepresentation
                 |> List.map unwind
                 |> List.concat
                 |> List.length) >= (result
                                     |> List.map unwind
                                     |> List.concat
                                     |> List.length)
            if not shouldBeTrue
            then
                raise (LogicErrorException "Postcondition violation: the fused representation should either more or equally as compact as the inefficient representation.")
            result :> seq<_>

        member this.ToList =
            [
                for slotKey in representation do
                    match slotKey with
                        Singleton key ->
                            yield key
                      | Interval (lowerBound
                                  , upperBound) ->
                            for key in lowerBound .. upperBound do
                                yield key
            ]

        member this.ToSeq =
            seq
                {
                    for slotKey in representation do
                        match slotKey with
                            Singleton key ->
                                yield key
                          | Interval (lowerBound
                                      , upperBound) ->
                                for key in lowerBound .. upperBound do
                                    yield key
                }

        member this.Add (key: UInt32) =
            let locallyMutatedRepresentation =
                new C5.TreeSet<SlotKey> ()
            let liftedKey =
                Singleton key
            if representation.IsEmpty
            then
                locallyMutatedRepresentation.Add liftedKey
                |> ignore
            else
                let greatestLowerBound =
                    ref Unchecked.defaultof<SlotKey>
                let hasGreatestLowerBound =
                    ref false
                let leastUpperBound =
                    ref Unchecked.defaultof<SlotKey>
                let hasLeastUpperBound =
                    ref false
                let entriesWithMatchingKeysToBeAddedIn =
                    if representation.Cut (liftedKey, greatestLowerBound, hasGreatestLowerBound, leastUpperBound, hasLeastUpperBound)
                    then
                        let slotKeyMatchingLiftedKey =
                            ref liftedKey
                        representation.Find(slotKeyMatchingLiftedKey)
                        |> ignore
                        [!slotKeyMatchingLiftedKey]
                    else
                        [liftedKey]
                match !hasGreatestLowerBound
                      , !hasLeastUpperBound with
                    false
                    , false ->
                        locallyMutatedRepresentation.AddSorted (entriesWithMatchingKeysToBeAddedIn
                                                                |> SetWithRunLengths.FuseAndDiscardConflictingAssociations)
                  | true
                    , false ->
                        locallyMutatedRepresentation.AddSorted (representation.RangeTo !greatestLowerBound)
                        locallyMutatedRepresentation.AddSorted ([
                                                                    yield !greatestLowerBound
                                                                    yield! entriesWithMatchingKeysToBeAddedIn
                                                                ]
                                                                |> SetWithRunLengths.FuseAndDiscardConflictingAssociations)
                  | false
                    , true ->
                        locallyMutatedRepresentation.AddSorted ([
                                                                    yield! entriesWithMatchingKeysToBeAddedIn
                                                                    yield !leastUpperBound
                                                                ]
                                                                |> SetWithRunLengths.FuseAndDiscardConflictingAssociations)
                        locallyMutatedRepresentation.AddSorted ((representation.RangeFrom !leastUpperBound)
                                                                |> Seq.skip 1)
                  | true
                    , true ->
                        locallyMutatedRepresentation.AddSorted (representation.RangeTo !greatestLowerBound)
                        locallyMutatedRepresentation.AddSorted ([
                                                                    yield !greatestLowerBound
                                                                    yield! entriesWithMatchingKeysToBeAddedIn
                                                                    yield !leastUpperBound
                                                                ]
                                                                |> SetWithRunLengths.FuseAndDiscardConflictingAssociations)
                        locallyMutatedRepresentation.AddSorted (representation.RangeFrom !leastUpperBound
                                                                |> Seq.skip 1)
            let result = 
                SetWithRunLengths locallyMutatedRepresentation
            let differences =
                (result.ToList |> Set.ofList)
                - (this.ToList |> Set.ofList)
            let noOperationCase =
                this.Contains key
            if noOperationCase
            then
                let shouldBeTrue =
                    differences.IsEmpty
                if not shouldBeTrue
                then
                    raise (LogicErrorException "Postcondition failure: adding in an existing key-value pair (as distinct from overwriting an associated value under an existing key) should result in an identical map.")
            else
                let shouldBeTrue =
                    differences
                    |> Set.count
                     = 1
                if not shouldBeTrue
                then
                    raise (LogicErrorException "Postcondition failure: the result should contain exactly one key-value pair that doesn't exist in the original (NOTE: the converse *may* also be true in the overwritten value case).")
                let shouldBeTrue =
                    differences.Contains key
                if not shouldBeTrue
                then
                    raise (LogicErrorException "Postcondition failure: the key-value pair contained in the result but not in the original should be the one added in.")
            result

        interface IComparable with
            member this.CompareTo another =
                match another with
                    :? SetWithRunLengths as another -> 
                        compare this.ToList
                                another.ToList
                  | _ ->
                        raise (ArgumentException (sprintf "Rhs of comparison must also be of type: %A"
                                                          typeof<SetWithRunLengths>.Name))

        interface ICollection<UInt32> with
//            member this.ContainsKey key =
//                representation.Contains (Singleton key)

            member this.Remove (key: UInt32): bool =
                failwith "The collection is immutable."

            member this.Count =
                this.Count

            member this.IsReadOnly =
                true

            member this.Add key =
                failwith "The collection is immutable."

            member this.Clear () =
                failwith "The collection is immutable."

            member this.Contains key =
                this.Contains key

            member this.CopyTo (keyValuePairs,
                                offsetIndexIntoKeyValuePairs) =
                failwith "Not implemented yet."

            member this.GetEnumerator (): IEnumerator<UInt32> =
                this.ToSeq.GetEnumerator ()

            member this.GetEnumerator (): System.Collections.IEnumerator =
                (this :> seq<UInt32>).GetEnumerator () :> System.Collections.IEnumerator

    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module SetWithRunLengths =
        let inline isEmpty (setWithRunLengths: SetWithRunLengths): bool =
            setWithRunLengths.IsEmpty

        let ofList (list: List<UInt32>): SetWithRunLengths =
            let sortedList =
                list
                |> List.sort
            let locallyMutatedRepresentation =
                new C5.TreeSet<SlotKey> ()
            let fusedList =
                SetWithRunLengths.FuseAndDiscardConflictingAssociations (sortedList
                                                                         |> List.map Singleton)
            locallyMutatedRepresentation.AddSorted fusedList
            SetWithRunLengths locallyMutatedRepresentation

        let inline toList (setWithRunLengths: SetWithRunLengths): List<UInt32> =
            setWithRunLengths.ToList

        let ofSeq (seq: seq<UInt32>): SetWithRunLengths =
            seq
            |> List.ofSeq
            |> ofList

        let inline toSeq (setWithRunLengths: SetWithRunLengths): seq<UInt32> =
            setWithRunLengths.ToSeq

        [<GeneralizableValue>]
        let empty<'Value when 'Value: comparison> =
            SetWithRunLengths (new C5.TreeSet<SlotKey> ())

        let fold (foldOperation: 'State -> UInt32 -> 'State)
                 (state: 'State)
                 (setWithRunLengths: SetWithRunLengths) =
            setWithRunLengths.ToSeq
            |> Seq.fold (fun state
                             key ->
                                foldOperation state
                                              key)
                        state

        let foldBack (foldOperation: UInt32 -> 'State -> 'State)
                     (setWithRunLengths: SetWithRunLengths)
                     (state: 'State): 'State =
            setWithRunLengths.ToList
            |> BargainBasement.Flip (List.foldBack (fun key
                                                        state ->
                                                            foldOperation key
                                                                          state))
                                    state
            

        let add (key: UInt32)
                (setWithRunLengths: SetWithRunLengths) =
            setWithRunLengths.Add key



                    
