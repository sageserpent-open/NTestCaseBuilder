#light

// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#r "SageSerpent.Infrastructure.dll"

open System.Collections.Generic
open SageSerpent.Infrastructure

let joinMaps first second =
    let keys map =
        Set.of_seq (seq { for key in (map:> IDictionary<'Key, 'Value>).Keys do yield key })
    if not (Set.intersect (keys first) (keys second)).IsEmpty
    then raise (InternalAssertionViolationException
                    "Maps from test variable indices to levels contributed by separate subtrees should not share common keys.")
    else Seq.append (Map.to_seq first) (Map.to_seq second)
         |> Map.of_seq



