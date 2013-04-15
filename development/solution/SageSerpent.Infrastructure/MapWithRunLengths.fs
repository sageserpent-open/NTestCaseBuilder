namespace SageSerpent.Infrastructure
    open System
    open System.Collections.Generic
    open Microsoft.FSharp.Collections

    type MapWithRunLengths<'Key, 'Value> () =
        member this.Keys: seq<'Key> =
            failwith "Not implemented yet."

        member this.Item
            with get (key: 'Key): 'Value =
                failwith "Not implemented yet."
            and set (key: 'Key)
                    (value: 'Value): unit =
                failwith "Not implemented yet."

        member this.Count: int32 =
            failwith "Not implemented yet."

        interface IComparable with
            member this.CompareTo another =
                failwith "Not implemented yet."

        interface IDictionary<'Key, 'Value> with
            member this.Item
                with get (key: 'Key): 'Value =
                    failwith "Not implemented yet."
                and set (key: 'Key)
                        (value: 'Value): unit =
                    failwith "Not implemented yet."

            member this.Keys =
                failwith "Not implemented yet."

            member this.Values =
                failwith "Not implemented yet."

            member this.ContainsKey key =
                failwith "Not implemented yet."

            member this.Remove (key: 'Key): bool =
                failwith "Not implemented yet."

            member this.TryGetValue (key,
                                     value) =
                failwith "Not implemented yet."

            member this.Count =
                failwith "Not implemented yet."

            member this.IsReadOnly =
                failwith "Not implemented yet."

            member this.Add (key,
                             value) =
                failwith "Not implemented yet."

            member this.Add keyValuePair =
                failwith "Not implemented yet."

            member this.Clear () =
                failwith "Not implemented yet."

            member this.Contains keyValuePair =
                failwith "Not implemented yet."

            member this.CopyTo (keyValuePairs,
                                offsetIndexIntoKeyValuePairs) =
                failwith "Not implemented yet."

            member this.GetEnumerator (): IEnumerator<KeyValuePair<'Key, 'Value>> =
                failwith "Not implemented yet."

            member this.Remove (keyValuePair: KeyValuePair<'Key, 'Value>): bool =
                failwith "Not implemented yet."

            member this.GetEnumerator (): System.Collections.IEnumerator =
                failwith "Not implemented yet."

    module MapWithRunLengths =
        let isEmpty (mapWithRunLengths: MapWithRunLengths<_, _>): bool =
            failwith "Not implemented yet."

        let ofList (list: List<'Key * 'Value>): MapWithRunLengths<'Key, 'Value> =
            failwith "Not implemented yet."

        let toList (mapWithRunLengths: MapWithRunLengths<'Key, 'Value>): List<'Key * 'Value> =
            failwith "Not implemented yet."

        let ofSeq (seq: seq<'Key * 'Value>): MapWithRunLengths<'Key, 'Value> =
            failwith "Not implemented yet."

        let toSeq (map: MapWithRunLengths<'Key, 'Value>): seq<'Key * 'Value> =
            failwith "Not implemented yet."

        [<GeneralizableValue>]
        let empty<'Key, 'Value> =
            MapWithRunLengths<'Key, 'Value> ()

        let fold (foldOperation: 'State -> 'Key -> 'Value -> 'State)
                 (state: 'State)
                 (mapWithRunLengths: MapWithRunLengths<'Key, 'Value>) =
            failwith "Not implemented yet."

        let foldBack (foldOperation: 'Key -> 'Value -> 'State -> 'State)
                     (mapWithRunLengths: MapWithRunLengths<'Key, 'Value>)
                     (state: 'State): 'State =
            failwith "Not implemented yet."

        let add (key: 'Key)
                (value: 'Value)
                (mapWithRunLengths: MapWithRunLengths<'Key, 'Value>) =
            failwith "Not implemented yet."

        let map (transformation: 'Key -> 'Value -> 'TransformedValue)
                (mapWithRunLengths: MapWithRunLengths<'Key, 'Value>): MapWithRunLengths<'Key, 'TransformedValue> =
            failwith "Not implemented yet."

        let tryFind (key: 'Key)
                    (mapWithRunLengths: MapWithRunLengths<'Key, 'Value>): Option<'Value> =
            failwith "Not implemented yet."

        let iter (operation: 'Key -> 'Value -> unit)
                 (mapWithRunLengths: MapWithRunLengths<'Key, 'Value>): unit =
            failwith "Not implemented yet."
                    
