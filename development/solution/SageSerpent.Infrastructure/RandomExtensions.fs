[<System.Runtime.CompilerServices.Extension>]
module SageSerpent.Infrastructure.RandomExtensions
    open System
    open Wintellect.PowerCollections

    type Random with
        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("ChooseAnyNumberFromZeroToOneLessThan")>]
        member this.ChooseAnyNumberFromZeroToOneLessThan exclusiveLimit =
            exclusiveLimit
            |> int32
            |> this.Next
            |> uint32
            
        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("ChooseAnyNumberFromOneTo")>]
        member this.ChooseAnyNumberFromOneTo inclusiveLimit =
            this.ChooseAnyNumberFromZeroToOneLessThan inclusiveLimit + 1u
            
        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("HeadsItIs")>]
        member this.HeadsItIs () =
            this.ChooseAnyNumberFromZeroToOneLessThan 2u = 0u
            
        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("ChooseSeveralOf")>]
        member this.ChooseSeveralOf ((candidates: #seq<_>), numberToChoose) =
            if numberToChoose > uint32 (Seq.length candidates)
            then raise (PreconditionViolationException "Insufficient number of candidates to satisfy number to choose.")
            else Algorithms.RandomSubset (candidates,
                                          int32 numberToChoose,
                                          this)
                                          
        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("ChooseOneOf")>]
        member this.ChooseOneOf (candidates: #seq<_>) =
            (this.ChooseSeveralOf (candidates, 1u)).[0]
            
        [<System.Runtime.CompilerServices.Extension>]
        [<CompiledName("Shuffle")>]
        member this.Shuffle (items: #seq<_>) =
            Algorithms.RandomShuffle (items,
                                      this)

