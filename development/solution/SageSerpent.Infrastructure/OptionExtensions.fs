module SageSerpent.Infrastructure.OptionExtensions

    type Microsoft.FSharp.Core.Option<'X> with
        static member MPlus lhs rhs =
            match lhs with
                Some _ ->
                    lhs
              | None ->
                    rhs
        static member LazyMPlus lhs (rhs: Lazy<Option<'X>>) =
            match lhs with
                Some _ ->
                    lhs
              | None ->
                    rhs.Value
