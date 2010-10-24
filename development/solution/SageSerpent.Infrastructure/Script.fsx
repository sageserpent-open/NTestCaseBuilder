#light

#r "Z:/SageSerpent/workInProgress/development/solution/SageSerpent.Infrastructure/bin/Debug/SageSerpent.Infrastructure.dll"

open SageSerpent.Infrastructure

// This file is a script that can be executed with the F# Interactive.  
// It can be used to explore and test the library project.
// Note that script files will not be part of the project build.

let a = Map.empty

let b = Map.of_list [(1, "Hi"); (3, "Rabbit!"); (2, "there")]

let o = 2:>obj

let p = BargainBasement.CrossProduct [[1;2;3];[67; -23];[0]]