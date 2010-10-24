#light

// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

#r "Z:/SageSerpent/workInProgress/development/solution/SageSerpent.Infrastructure/bin/Debug/SageSerpent.Infrastructure.dll"
#r "Z:/SageSerpent/workInProgress/development/solution/SageSerpent.TestInfrastructure/bin/Debug/SageSerpent.TestInfrastructure.dll"

open System.Collections.Generic
open SageSerpent.Infrastructure
open SageSerpent.TestInfrastructure
open Microsoft.FSharp.Collections
open System


let joinMaps first second =
    let keys map =
        Set.of_seq (seq { for key in (map:> IDictionary<'Key, 'Value>).Keys do yield key })
    if not (Set.intersect (keys first) (keys second)).IsEmpty
    then raise (InternalAssertionViolationException
                    "Maps from test variable indices to levels contributed by separate subtrees should not share common keys.")
    else Seq.append (Map.to_seq first) (Map.to_seq second)
         |> Map.of_seq



let a = TestVariableNode [box 1u; box 2u; box 452u]

let b = TestVariableNode [box "alpha"; box "beta"; box "gamma"]

let c = SynthesizingNode [a; b]

let d = TestVariableNode [box -1; box -89]

let e = InterleavingNode [c; d]

let f = TestVariableNode [box '&'; box '*']

let g = SynthesizingNode [f; e]

let resultsGalore = g.TestVectorRepresentationsGroupedByStrengthUpToAndIncluding 3u

let h = SynthesizingNode [b; g];;

let evenMoreResultsGalore = h.TestVectorRepresentationsGroupedByStrengthUpToAndIncluding 3u

let debug results =
    let printResultsAtStrength strength resultsAtStrength =
        printf "\n**** Strength: %u:-\n\n" strength
        for testVector in resultsAtStrength do
            printf "%A\n" testVector
        
        strength + 1u
    results
    |> List.fold_left printResultsAtStrength 1u
            
[1;4;5]
|> List.scan_left (fun testVariableIndex _ -> testVariableIndex + 1u) 0u            
       
        
open System.Collections

let cache =
    Hashtable ()
    
    
let cachedComputation (computation: unit -> 'a) =
    printf "%A\n" ((box computation).GetHashCode ())
    if cache.ContainsKey (box computation)
    then unbox cache.[computation]
    else let result = computation ()
         cache.Add (computation, box result)
         result
         
         
type 'a Barry (z: 'a list) =
    static let a = ref []
    
    member this.Compute () = a := List.append z !a
                             !a
                             
    
    
let z = Barry [2]

let zz = Barry ["East Enders"];;

printf "%A %A\n" (z.Compute ()) (z.Compute ())

printf "%A %A\n" (zz.Compute ()) (zz.Compute ())


let foooble = BargainBasement.Memoize (fun x -> printf "Calculating for: %A\n" x; 2 * x)

let wazzock = Map<UInt32, option<Object>> []


