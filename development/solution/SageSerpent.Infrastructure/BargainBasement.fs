module SageSerpent.Infrastructure.BargainBasement

    open System.Collections.Generic
    open System

    open RandomExtensions 
   
                            

                            
        
    let NumberOfPermutations originalSize permutationSize =
        if permutationSize > originalSize
        then 0u
        else let numberOfItemsLeftOutOfPermutation =
                originalSize - permutationSize
             let rec productOfPartialResultAndNumberOfSubpermutations originalSize partialResult =
                if originalSize = numberOfItemsLeftOutOfPermutation
                then partialResult
                else productOfPartialResultAndNumberOfSubpermutations (originalSize - 1u) (originalSize * partialResult)
             productOfPartialResultAndNumberOfSubpermutations originalSize 1u
        
    let Factorial x =
        NumberOfPermutations x x
        
    let NumberOfCombinations originalSize combinationSize =
        NumberOfPermutations originalSize combinationSize
        / Factorial combinationSize
        
        
