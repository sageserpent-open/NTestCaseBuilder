[<System.Runtime.CompilerServices.Extension>]
module SageSerpent.Infrastructure.IEnumerableExtensions

    open System.Collections.Generic


    type IEnumerable<'X> with
        static member IsSorted<'HasComparison when 'HasComparison: comparison> items =
            items
            |> Seq.pairwise
            |> Seq.forall (fun (lhs: 'HasComparison
                                , rhs)
                            -> lhs <= rhs)
