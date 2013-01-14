module SageSerpent.Infrastructure.OptionExtensions

    type Microsoft.FSharp.Core.Option<'X> with
        member inline this.OrElse rhs =
            match this with
                Some _ ->
                    this
              | None ->
                    rhs

        static member GetFromMany many =
            many
            |> List.filter Option.isSome
            |> List.map Option.get

