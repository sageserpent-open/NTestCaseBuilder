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

let q = BargainBasement.MergeSortedListsAllowingDuplicates [1;2;3;4] [2;2;4;78]

let v1 = BargainBasement.CountDuplicatesInSortedList [1;2;3]

let v2 = BargainBasement.CountDuplicatesInSortedList [1;2;2;3]

let v3 = BargainBasement.CountDuplicatesInSortedList [2;2;2;3]

let v4 = BargainBasement.CountDuplicatesInSortedList ([]: int list)

let v5 = BargainBasement.CountDuplicatesInSortedList [1;2;2;2]

let v6 = BargainBasement.CountDuplicatesInSortedList [2;2;2;2]

let v7 = BargainBasement.CountDuplicatesInSortedList [1]

let v8 = BargainBasement.CountDuplicatesInSortedList [1;2]

let q1 = BargainBasement.BreakOff 2u [3;4;7;8];;

let q2 = BargainBasement.ChopUpList [3;4;7;8] [1u;2u];;

let q3 = BargainBasement.BreakOff 4u [3;4;7;8];;

let q4 = BargainBasement.ChopUpList [3;4;7;8] [];;
