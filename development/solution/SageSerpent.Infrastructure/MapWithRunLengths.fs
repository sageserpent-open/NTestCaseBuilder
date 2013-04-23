namespace SageSerpent.Infrastructure
    open System
    open System.Collections.Generic
    open Microsoft.FSharp.Collections

    module MapWithRunLengthsDetail =
        [<CustomComparison; StructuralEquality>]
        type SlotKey =
            Singleton of UInt32
          | Interval of UInt32 * UInt32 // These are the *inclusive* lower- and upper-bounds, respectively.
                                        // No need to worry about empty and singleton intervals, obviously.
            interface IComparable with
                // NOTE: be careful here; the comparison semantics are deliberately sloppy - they violate the rules for a total ordering, because
                // it is possible to take one slot key A that is less than another slot key B, and then use encompassing or overlapping interval
                // slot keys to 'bridge the gap' between A and B via of chain of intervals that compare equal to each other and to A and B.
                // As long as the client code (which is actually 'MapWithRunLengths') doesn't allow multiple items that compare equal in the same
                // data structure, then this won't cause a problem.
                member this.CompareTo another =
                    match another with
                        :? SlotKey as another ->
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
                      | _ ->
                            raise (ArgumentException (sprintf "Rhs of comparison must also be of type: %A"
                                                              typeof<SlotKey>.Name))

    open MapWithRunLengthsDetail

    type MapWithRunLengths<'Value when 'Value: comparison> (representation: Map<SlotKey, 'Value>) =
        do
            for slotKey
                , _ in Map.toSeq representation do
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

        member this.Keys: seq<UInt32> =
            seq
                {
                    for slotKey
                        , _ in Map.toSeq representation do
                        match slotKey with
                            Singleton key ->
                                yield key
                          | Interval (lowerBound
                                      , upperBound) ->
                                for key in lowerBound .. upperBound do
                                    yield key
                }

        member this.Item
            with get (key: UInt32): 'Value =
                representation.[Singleton key]

        member this.Count: int32 =
            representation
            |> Map.fold (fun count
                             slotKey
                             _ ->
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

        static member Fuse inefficientRepresentation =
            let rec fuse representation
                         reversedPrefixOfResult =
                match representation with
                    [] ->
                        reversedPrefixOfResult
                  | [singleton] ->
                        singleton :: reversedPrefixOfResult
                  | (firstKey
                     , firstValue) as first :: ((secondKey
                                                 , secondValue) :: tail as nonEmptyTail) when firstValue = secondValue ->
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
                                fuse tail
                                     ((Interval (firstKey
                                                , secondUpperBound)
                                      , firstValue) :: reversedPrefixOfResult)
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
                  | first :: (_ :: _) as nonEmptyList ->
                         fuse nonEmptyList
                              (first :: reversedPrefixOfResult)
            fuse inefficientRepresentation
                 List.empty
            |> Map.ofSeq

        member private this.Representation =
            representation

        member this.ToList =
            [
                for slotKey
                    , value in Map.toSeq representation do
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
                    for slotKey
                        , value in Map.toSeq representation do
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
            let liftedKey =
                Singleton key
            MapWithRunLengths (Map.add liftedKey
                                       value
                                       representation
                               |> Map.toList
                               |> MapWithRunLengths<'Value>.Fuse)

        interface IComparable with
            member this.CompareTo another =
                match another with
                    :? MapWithRunLengths<'Value> as another ->
                        compare representation
                                another.Representation
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
                |> Array.ofSeq
                :> ICollection<UInt32>

            member this.Values =
                (representation :> IDictionary<SlotKey, 'Value>).Values

            member this.ContainsKey key =
                representation.ContainsKey (Singleton key)

            member this.Remove (key: UInt32): bool =
                failwith "The collection is immutable."

            member this.TryGetValue (key,
                                     value) =
                (representation :> IDictionary<SlotKey, 'Value>).TryGetValue (Singleton key, ref value)

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
                failwith "Not implemented yet."

            member this.CopyTo (keyValuePairs,
                                offsetIndexIntoKeyValuePairs) =
                failwith "Not implemented yet."

            member this.GetEnumerator (): IEnumerator<KeyValuePair<UInt32, 'Value>> =
                failwith "Not implemented yet."

            member this.Remove (keyValuePair: KeyValuePair<UInt32, 'Value>): bool =
                failwith "Not implemented yet."

            member this.GetEnumerator (): System.Collections.IEnumerator =
                failwith "Not implemented yet."

    module MapWithRunLengths =
        let inline isEmpty (mapWithRunLengths: MapWithRunLengths<_>): bool =
            mapWithRunLengths.IsEmpty

        let ofList (list: List<UInt32 * 'Value>): MapWithRunLengths<'Value> =
            MapWithRunLengths (MapWithRunLengths<'Value>.Fuse (list
                                                               |> List.map (fun (key
                                                                                 , value) ->
                                                                                    Singleton key
                                                                                    , value)))

        let inline toList (mapWithRunLengths: MapWithRunLengths<'Value>): List<UInt32 * 'Value> =
            mapWithRunLengths.ToList

        let ofSeq (seq: seq<UInt32 * 'Value>): MapWithRunLengths<'Value> =
            MapWithRunLengths (MapWithRunLengths<'Value>.Fuse (seq
                                                               |> Seq.map (fun (key
                                                                                , value) ->
                                                                                    Singleton key
                                                                                    , value)
                                                               |> List.ofSeq))

        let inline toSeq (mapWithRunLengths: MapWithRunLengths<'Value>): seq<UInt32 * 'Value> =
            mapWithRunLengths.ToSeq

        [<GeneralizableValue>]
        let empty<'Value when 'Value: comparison> =
            MapWithRunLengths<'Value> (Map.empty)

        let fold (foldOperation: 'State -> UInt32 -> 'Value -> 'State)
                 (state: 'State)
                 (mapWithRunLengths: MapWithRunLengths<'Value>) =
            failwith "Not implemented yet."

        let foldBack (foldOperation: UInt32 -> 'Value -> 'State -> 'State)
                     (mapWithRunLengths: MapWithRunLengths<'Value>)
                     (state: 'State): 'State =
            failwith "Not implemented yet."

        let add (key: UInt32)
                (value: 'Value)
                (mapWithRunLengths: MapWithRunLengths<'Value>) =
            mapWithRunLengths.Add (key,
                                   value)

        let map (transformation: UInt32 -> 'Value -> 'TransformedValue)
                (mapWithRunLengths: MapWithRunLengths<'Value>): MapWithRunLengths<'TransformedValue> =
            failwith "Not implemented yet."

        let tryFind (key: UInt32)
                    (mapWithRunLengths: MapWithRunLengths<'Value>): Option<'Value> =
            failwith "Not implemented yet."

        let iter (operation: UInt32 -> 'Value -> unit)
                 (mapWithRunLengths: MapWithRunLengths<'Value>): unit =
            failwith "Not implemented yet."
                    
