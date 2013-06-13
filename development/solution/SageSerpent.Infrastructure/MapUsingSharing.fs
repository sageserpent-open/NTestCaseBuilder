namespace SageSerpent.Infrastructure
    open System
    open System.Collections.Generic
    open Microsoft.FSharp.Collections
    open ListExtensions

    type MapWithSharing<'Value when 'Value: comparison> (representation: Map<UInt32, 'Value>,
                                                         fixedValue: 'Value,
                                                         keysForFixedValue: Set<UInt32>) =
        do
            let shouldBeTrue =
                (Set.intersect ((representation :> IDictionary<_, _>).Keys
                                |> Set.ofSeq)
                               (keysForFixedValue
                                |> Set.ofSeq)).IsEmpty
            if not shouldBeTrue
            then
                raise (PreconditionViolationException "'MapWithSharing' does not permit the masking of associations from keys to the fixed value by associations in 'representation' - even if the new associations coincidentally refer to the same fixed value.")

        new (representation: Map<UInt32, 'Value>) =
            MapWithSharing (representation,
                            Unchecked.defaultof<'Value>,
                            Set.empty)

        member this.Keys: ICollection<UInt32> =
            List.MergeSortedListsAllowingDuplicates ((representation :> IDictionary<_, _>).Keys
                                                     |> List.ofSeq)
                                                    (keysForFixedValue
                                                     |> Set.toList)
            // OK to allow for duplicates, as there won't be any due to the constructor precondition.
            |> Array.ofList
            :> ICollection<UInt32>

        member this.Values: ICollection<'Value> =
            this.ToList
            |> List.map snd
            |> Array.ofList
            :> ICollection<'Value>

        member this.Item
            with get (key: UInt32): 'Value =
                if keysForFixedValue.Contains key
                then
                    fixedValue
                else
                    representation.[key]

        member this.Count: int32 =
            representation.Count
            + keysForFixedValue.Count

        member this.IsEmpty =
            representation.IsEmpty
            && keysForFixedValue.IsEmpty


        member this.ToList =
            BargainBasement.MergeDisjointSortedAssociationLists (representation
                                                                 |> Map.toList)
                                                                (keysForFixedValue
                                                                 |> Set.toList
                                                                 |> List.map (fun key ->
                                                                                key
                                                                                , fixedValue))

        member this.ToSeq =
            this.ToList
            :> seq<_>

        member this.Add (key: UInt32,
                         value: 'Value) =
            let result = 
                MapWithSharing (Map.add key
                                        value
                                        representation,
                                fixedValue,
                                keysForFixedValue)
            let differences =
                (result.ToList |> Set.ofList)
                - (this.ToList |> Set.ofList)
            let noOperationCase =
                (this :> IDictionary<_, _>).Contains (KeyValuePair (key, value))
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
                    differences.Contains (key, value)
                if not shouldBeTrue
                then
                    raise (LogicErrorException "Postcondition failure: the key-value pair contained in the result but not in the original should be the one added in.")
            result

        interface IComparable with
            member this.CompareTo another =
                match another with
                    :? MapWithSharing<'Value> as another -> 
                        compare this.ToList
                                another.ToList
                  | _ ->
                        raise (ArgumentException (sprintf "Rhs of comparison must also be of type: %A"
                                                          typeof<MapWithSharing<'Value>>.Name))

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
                keysForFixedValue.Contains key
                || representation.ContainsKey key

            member this.Remove (key: UInt32): bool =
                failwith "The collection is immutable."

            member this.TryGetValue (key,
                                     value) =
                if keysForFixedValue.Contains key
                then
                    value <- fixedValue
                    true
                else
                    match representation.TryFind key with
                        Some retrievedValue ->
                            value <- retrievedValue
                            true
                      | None ->
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
                let key =
                    keyValuePair.Key
                let value =
                    keyValuePair.Value
                if keysForFixedValue.Contains key
                then
                    value = fixedValue
                else
                    match representation.TryFind key with
                        Some retrievedValue ->
                            value = retrievedValue
                      | None ->
                            false

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

    module MapWithSharing =
        let inline isEmpty (mapWithSharing: MapWithSharing<_>): bool =
            mapWithSharing.IsEmpty

        let ofList (list: List<UInt32 * 'Value>): MapWithSharing<'Value> =
            MapWithSharing (list
                            |> Map.ofList)

        let inline toList (mapWithSharing: MapWithSharing<'Value>): List<UInt32 * 'Value> =
            mapWithSharing.ToList

        let ofSeq (seq: seq<UInt32 * 'Value>): MapWithSharing<'Value> =
            seq
            |> List.ofSeq
            |> ofList

        let inline toSeq (mapWithSharing: MapWithSharing<'Value>): seq<UInt32 * 'Value> =
            mapWithSharing.ToSeq

        [<GeneralizableValue>]
        let empty<'Value when 'Value: comparison> =
            MapWithSharing<'Value> (Map.empty)

        let fold (foldOperation: 'State -> UInt32 -> 'Value -> 'State)
                 (state: 'State)
                 (mapWithSharing: MapWithSharing<'Value>) =
            mapWithSharing.ToSeq
            |> Seq.fold (fun state
                             (key
                              , value) ->
                                foldOperation state
                                              key
                                              value)
                        state

        let foldBack (foldOperation: UInt32 -> 'Value -> 'State -> 'State)
                     (mapWithSharing: MapWithSharing<'Value>)
                     (state: 'State): 'State =
            mapWithSharing.ToList
            |> BargainBasement.Flip (List.foldBack (fun (key
                                                         , value)
                                                        state ->
                                                            foldOperation key
                                                                          value
                                                                          state))
                                    state
            

        let add (key: UInt32)
                (value: 'Value)
                (mapWithSharing: MapWithSharing<'Value>) =
            mapWithSharing.Add (key,
                                   value)

        let map (transformation: UInt32 -> 'Value -> 'TransformedValue)
                (mapWithSharing: MapWithSharing<'Value>): Map<UInt32, 'TransformedValue> =
                mapWithSharing
                |> toList
                |> List.map (fun (key
                                  , value) ->
                                key
                                , transformation key
                                                 value)
                |> Map.ofList

        let tryFind (key: UInt32)
                    (mapWithSharing: MapWithSharing<'Value>): Option<'Value> =
            let mutableValue =
                ref Unchecked.defaultof<'Value>
            match (mapWithSharing :> IDictionary<_, _>).TryGetValue (key, mutableValue) with
                true ->
                    Some !mutableValue
              | false ->
                    None

        let iter (operation: UInt32 -> 'Value -> unit)
                 (mapWithSharing: MapWithSharing<'Value>): unit =
            failwith "Not implemented yet."
                    
