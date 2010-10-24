#light

namespace SageSerpent.Infrastructure

    open System

    type RandomBehaviour (seed) =
        let randomBehaviour = Random seed
        member this.UnderlyingImplementationForClientUse =
            randomBehaviour
        member this.ChooseAnyNumberFromZeroToOneLessThan =
            int32 >> randomBehaviour.Next >> uint32
        member this.ChooseAnyNumberFromOneTo =
            this.ChooseAnyNumberFromZeroToOneLessThan >> (+) 1u
        member this.HeadsItIs () =
            this.ChooseAnyNumberFromZeroToOneLessThan 2u = 0u
        member this.ChooseOneOf candidates =
            List.nth candidates (int32 (this.ChooseAnyNumberFromZeroToOneLessThan (uint32 (List.length candidates))))
    
    