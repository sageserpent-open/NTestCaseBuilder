namespace SageSerpent.Infrastructure
    open System
    open System.Collections.Generic
    open Microsoft.FSharp.Collections

    module MapWithRunLengthsDetail =
        [<CustomComparison; CustomEquality>]
        type SlotKey =
            Singleton of UInt32
          | Interval of UInt32 * UInt32 // These are the *inclusive* lower- and upper-bounds, respectively.
                                        // No need to worry about empty and singleton intervals, obviously.
            interface IComparable<SlotKey> with
                // NOTE: be careful here; the comparison semantics are deliberately sloppy - they violate the rules for a total ordering, because
                // it is possible to take one slot key A that is less than another slot key B, and then use encompassing or overlapping interval
                // slot keys to 'bridge the gap' between A and B via of chain of intervals that compare equal to each other and to A and B.
                // As long as the client code (which is actually 'MapWithRunLengths') doesn't allow multiple items that compare equal in the same
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

        let (|KeyValue|) (keyValuePair: C5.KeyValuePair<'Key, 'Value>) =
            keyValuePair.Key
            , keyValuePair.Value

    open MapWithRunLengthsDetail

    type MapWithRunLengths<'Value when 'Value: comparison> (representation: C5.ISortedDictionary<SlotKey, 'Value>) =
        do
            for KeyValue (key
                          , value) in representation do
                if Unchecked.defaultof<SlotKey> = key
                then
                    raise (InvariantViolationException "Null key detected.")
            for slotKey in representation.Keys do
                match slotKey with
                    Interval (lowerBound
                              , upperBound) ->
                        if upperBound + 1u < lowerBound
                        then
                            raise (InvariantViolationException "Malformed interval slot key detected: bounds are ordered in reverse.")
                        else if upperBound + 1u = lowerBound
                        then
                            raise (InvariantViolationException "Empty interval slot key detected - should have been represented by the absence of the slot key altogether.")
                        else if upperBound = lowerBound
                        then
                            raise (InvariantViolationException "One-item interval slot key detected - should have been represented by a singleton slot key instead.")
                  | _ ->
                        ()
            for (KeyValue (predecessorSlotKey
                           , predecessorValue)
                 , KeyValue (successorSlotKey
                             , successorValue)) in representation
                                                   |> Seq.pairwise do
                if predecessorValue = successorValue
                then
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
                        raise (InvariantViolationException "Adjacent slots found that have the same associated value and can be fused together.")

        member this.Keys: ICollection<UInt32> =
            [|
                for slotKey in representation.Keys do
                    match slotKey with
                        Singleton key ->
                            yield key
                      | Interval (lowerBound
                                  , upperBound) ->
                            for key in lowerBound .. upperBound do
                                yield key
            |] :> ICollection<UInt32>

        member this.Values: ICollection<'Value> =
            [|
                for KeyValue (slotKey
                              , value) in representation do
                    match slotKey with
                        Singleton _ ->
                            yield value
                      | Interval (lowerBound
                                  , upperBound) ->
                            for _ in lowerBound .. upperBound do
                                yield value
            |] :> ICollection<'Value>

        member this.Item
            with get (key: UInt32): 'Value =
                representation.[Singleton key]

        member this.Count: int32 =
            representation
            |> Seq.fold (fun count
                             (KeyValue (slotKey
                                       ,_)) ->
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
                  | (firstKey
                     , _) as first :: (((secondKey
                                         , _) :: _) as nonEmptyTail) when firstKey = secondKey ->
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
                  | (firstKey
                     , firstValue) as first :: (((secondKey
                                                  , secondValue) :: tail) as nonEmptyTail) when firstValue = secondValue ->
                        match firstKey
                              , secondKey with
                            Singleton firstKey
                            , Singleton secondKey when 1u + firstKey = secondKey ->
                                fuse ((Interval (firstKey
                                               , secondKey)
                                      , firstValue) :: tail)
                                     reversedPrefixOfResult
                          | Interval (firstLowerBound
                                      , firstUpperBound)
                            , Interval (secondLowerBound
                                        , secondUpperBound) when 1u + firstUpperBound = secondLowerBound ->
                                fuse ((Interval (firstLowerBound
                                                , secondUpperBound)
                                      , firstValue) :: tail)
                                     reversedPrefixOfResult
                          | Singleton firstKey
                            , Interval (secondLowerBound
                                        , secondUpperBound) when 1u + firstKey = secondLowerBound ->
                                fuse ((Interval (firstKey
                                                 , secondUpperBound)
                                       , firstValue) :: tail)
                                     reversedPrefixOfResult
                          | Interval (firstLowerBound
                                      , firstUpperBound)
                            , Singleton secondKey when 1u + firstUpperBound = secondKey ->
                                fuse ((Interval (firstLowerBound
                                                , secondKey)
                                      , firstValue) :: tail)
                                     reversedPrefixOfResult
                          | _ ->
                                fuse nonEmptyTail
                                     (first :: reversedPrefixOfResult)
                  | first :: ((_ :: _) as nonEmptyList) ->
                         fuse nonEmptyList
                              (first :: reversedPrefixOfResult)
            fuse (reversedRepresentationWithoutConflicts
                  |> List.rev)
                 List.empty
            |> List.rev
            |> List.map (fun (slotKey
                              , value) ->
                            C5.KeyValuePair (slotKey, value))
            :> seq<_>

        member this.ToList =
            [
                for KeyValue (slotKey
                              , value) in representation do
                    match slotKey with
                        Singleton key ->
                            yield key
                                  , value
                      | Interval (lowerBound
                                  , upperBound) ->
                            for key in lowerBound .. upperBound do
                                yield key
                                      , value
            ]

        member this.ToSeq =
            seq
                {
                    for KeyValue (slotKey
                                  , value) in representation do
                        match slotKey with
                            Singleton key ->
                                yield key
                                      , value
                          | Interval (lowerBound
                                      , upperBound) ->
                                for key in lowerBound .. upperBound do
                                    yield key
                                          , value
                }

        member this.Add (key: UInt32,
                         value: 'Value) =
            let locallyMutatedRepresentation =
                C5.TreeDictionary<SlotKey, 'Value> ()
            let liftedKey =
                Singleton key
            if representation.IsEmpty
            then
                locallyMutatedRepresentation.Add (liftedKey, value)
            else
                let greatestLowerBound =
                    ref Unchecked.defaultof<C5.KeyValuePair<SlotKey, 'Value>>
                let hasGreatestLowerBound =
                    ref false
                let leastUpperBound =
                    ref Unchecked.defaultof<C5.KeyValuePair<SlotKey, 'Value>>
                let hasLeastUpperBound =
                    ref false
                let entriesWithMatchingKeysToBeAddedIn =
                    if representation.Cut (liftedKey, greatestLowerBound, hasGreatestLowerBound, leastUpperBound, hasLeastUpperBound)
                    then
                        let slotKeyMatchingLiftedKey =
                            ref liftedKey
                        let associatedValue =
                            ref Unchecked.defaultof<'Value>
                        locallyMutatedRepresentation.Find(slotKeyMatchingLiftedKey, associatedValue)
                        |> ignore
                        if value = !associatedValue
                        then
                            match !slotKeyMatchingLiftedKey with
                                Singleton _ ->
                                    [liftedKey
                                     , value]
                              | Interval (lowerBound
                                          , upperBound) when 1u + lowerBound = upperBound ->
                                    if lowerBound = key
                                    then
                                        [(liftedKey
                                          , value); (Singleton upperBound
                                                     , !associatedValue)]
                                    else
                                        [(Singleton lowerBound
                                          , !associatedValue); (liftedKey
                                                                , value)]
                              | Interval (lowerBound
                                          , upperBound) ->
                                    if lowerBound = key
                                    then
                                        [(liftedKey
                                          , value); (Interval (1u + key
                                                               , upperBound)
                                                     , !associatedValue)]
                                    else if upperBound = key
                                    then
                                        [(Interval (lowerBound
                                                    , key - 1u)
                                          , !associatedValue); (liftedKey
                                                                , value)]
                                    else
                                        [(Interval (lowerBound
                                                    , key - 1u)
                                          , !associatedValue); (liftedKey
                                                                , value); (Interval (1u + key
                                                                                     , upperBound)
                                                                           , !associatedValue)]
                        else
                            [!slotKeyMatchingLiftedKey
                             , !associatedValue]
                    else
                        [liftedKey
                         , value]
                match !hasGreatestLowerBound
                      , !hasLeastUpperBound with
                    false
                    , false ->
                        locallyMutatedRepresentation.AddSorted (entriesWithMatchingKeysToBeAddedIn
                                                                |> MapWithRunLengths<'Value>.FuseAndDiscardConflictingAssociations)
                  | true
                    , false ->
                        locallyMutatedRepresentation.AddSorted (representation.RangeTo ((!greatestLowerBound).Key))
                        locallyMutatedRepresentation.AddSorted ([
                                                                    yield match !greatestLowerBound with
                                                                            KeyValue greatestLowerBound ->
                                                                                greatestLowerBound
                                                                    yield! entriesWithMatchingKeysToBeAddedIn
                                                                ]
                                                                |> MapWithRunLengths<'Value>.FuseAndDiscardConflictingAssociations)
                  | false
                    , true ->
                        locallyMutatedRepresentation.AddSorted ([
                                                                    yield! entriesWithMatchingKeysToBeAddedIn
                                                                    yield match !leastUpperBound with
                                                                            KeyValue leastUpperBound ->
                                                                                leastUpperBound
                                                                ]
                                                                |> MapWithRunLengths<'Value>.FuseAndDiscardConflictingAssociations)
                        locallyMutatedRepresentation.AddSorted (representation.RangeFrom ((!leastUpperBound).Key)
                                                                |> Seq.skip 1)
                  | true
                    , true ->
                        locallyMutatedRepresentation.AddSorted (representation.RangeTo ((!greatestLowerBound).Key))
                        locallyMutatedRepresentation.AddSorted ([
                                                                    yield match !greatestLowerBound with
                                                                            KeyValue greatestLowerBound ->
                                                                                greatestLowerBound
                                                                    yield! entriesWithMatchingKeysToBeAddedIn
                                                                    yield match !leastUpperBound with
                                                                            KeyValue leastUpperBound ->
                                                                                leastUpperBound
                                                                ]
                                                                |> MapWithRunLengths<'Value>.FuseAndDiscardConflictingAssociations)
                        locallyMutatedRepresentation.AddSorted (representation.RangeFrom ((!leastUpperBound).Key)
                                                                |> Seq.skip 1)
            
            MapWithRunLengths locallyMutatedRepresentation

        interface IComparable with
            member this.CompareTo another =
                match another with
                    :? MapWithRunLengths<'Value> as another -> 
                        compare this.ToList
                                another.ToList
                  | _ ->
                        raise (ArgumentException (sprintf "Rhs of comparison must also be of type: %A"
                                                          typeof<MapWithRunLengths<'Value>>.Name))

        interface IDictionary<UInt32, 'Value> with
            member this.Item
                with get (key: UInt32): 'Value =
                    this.[key]
                and set (key: UInt32)
                        (value: 'Value): unit =
                    failwith "The collection is immutable."

            member this.Keys =
                this.Keys

            member this.Values =
                this.Values

            member this.ContainsKey key =
                representation.Contains (Singleton key)

            member this.Remove (key: UInt32): bool =
                failwith "The collection is immutable."

            member this.TryGetValue (key,
                                     value) =
                let mutableValue
                    = ref Unchecked.defaultof<'Value>
                if representation.Find (ref (Singleton key), mutableValue)
                then
                    value <- mutableValue.Value
                    true
                else
                    false

            member this.Count =
                this.Count

            member this.IsReadOnly =
                true

            member this.Add (key,
                             value) =
                failwith "The collection is immutable."

            member this.Add keyValuePair =
                failwith "The collection is immutable."

            member this.Clear () =
                failwith "The collection is immutable."

            member this.Contains keyValuePair =
                let mutableValue =
                    ref Unchecked.defaultof<'Value>
                representation.Find (ref (Singleton keyValuePair.Key), mutableValue)
                && !mutableValue = keyValuePair.Value

            member this.CopyTo (keyValuePairs,
                                offsetIndexIntoKeyValuePairs) =
                failwith "Not implemented yet."

            member this.GetEnumerator (): IEnumerator<KeyValuePair<UInt32, 'Value>> =
                (this.ToSeq
                 |> Seq.map (fun (key
                                  , value) -> KeyValuePair (key
                                                            , value))).GetEnumerator ()

            member this.Remove (keyValuePair: KeyValuePair<UInt32, 'Value>): bool =
                failwith "The collection is immutable."

            member this.GetEnumerator (): System.Collections.IEnumerator =
                (this :> seq<KeyValuePair<UInt32, 'Value>>).GetEnumerator () :> System.Collections.IEnumerator

    module MapWithRunLengths =
        let inline isEmpty (mapWithRunLengths: MapWithRunLengths<_>): bool =
            mapWithRunLengths.IsEmpty

        let ofList (list: List<UInt32 * 'Value>): MapWithRunLengths<'Value> =
            let sortedList =
                list
                |> List.sortBy fst
            let locallyMutatedRepresentation =
                C5.TreeDictionary<SlotKey, 'Value> ()
            let fusedList =
                MapWithRunLengths<'Value>.FuseAndDiscardConflictingAssociations (sortedList
                                                                                 |> List.map (fun (key
                                                                                                   , value) ->
                                                                                                Singleton key
                                                                                                , value))
            locallyMutatedRepresentation.AddSorted fusedList
            MapWithRunLengths locallyMutatedRepresentation

        let inline toList (mapWithRunLengths: MapWithRunLengths<'Value>): List<UInt32 * 'Value> =
            mapWithRunLengths.ToList

        let ofSeq (seq: seq<UInt32 * 'Value>): MapWithRunLengths<'Value> =
            seq
            |> List.ofSeq
            |> ofList

        let inline toSeq (mapWithRunLengths: MapWithRunLengths<'Value>): seq<UInt32 * 'Value> =
            mapWithRunLengths.ToSeq

        [<GeneralizableValue>]
        let empty<'Value when 'Value: comparison> =
            MapWithRunLengths<'Value> (C5.TreeDictionary ())

        let fold (foldOperation: 'State -> UInt32 -> 'Value -> 'State)
                 (state: 'State)
                 (mapWithRunLengths: MapWithRunLengths<'Value>) =
            mapWithRunLengths.ToSeq
            |> Seq.fold (fun state
                             (key
                              , value) ->
                                foldOperation state
                                              key
                                              value)
                        state

        let foldBack (foldOperation: UInt32 -> 'Value -> 'State -> 'State)
                     (mapWithRunLengths: MapWithRunLengths<'Value>)
                     (state: 'State): 'State =
            mapWithRunLengths.ToList
            |> BargainBasement.Flip (List.foldBack (fun (key
                                                         , value)
                                                        state ->
                                                            foldOperation key
                                                                          value
                                                                          state))
                                    state
            

        let add (key: UInt32)
                (value: 'Value)
                (mapWithRunLengths: MapWithRunLengths<'Value>) =
            mapWithRunLengths.Add (key,
                                   value)

        let map (transformation: UInt32 -> 'Value -> 'TransformedValue)
                (mapWithRunLengths: MapWithRunLengths<'Value>): MapWithRunLengths<'TransformedValue> =
                mapWithRunLengths
                |> toList
                |> List.map (fun (key
                                  , value) ->
                                key
                                , transformation key
                                                 value)
                |> ofList

        let tryFind (key: UInt32)
                    (mapWithRunLengths: MapWithRunLengths<'Value>): Option<'Value> =
            let mutableValue =
                ref Unchecked.defaultof<'Value>
            match (mapWithRunLengths :> IDictionary<_, _>).TryGetValue (key, mutableValue) with
                true ->
                    Some !mutableValue
              | false ->
                    None

        let iter (operation: UInt32 -> 'Value -> unit)
                 (mapWithRunLengths: MapWithRunLengths<'Value>): unit =
            failwith "Not implemented yet."
                    
