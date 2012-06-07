module SageSerpent.Infrastructure.OptionWorkflow

    type Builder () =
        member inline this.Bind (lhs: Option<'UnliftedInput>,
                                 rhs: 'UnliftedInput -> Option<'UnliftedOutput>): Option<'UnliftedOutput> =
            lhs
            |> Option.bind rhs

        member inline this.Return (toBeLifted: 'Unlifted) =
            Some toBeLifted

        member inline this.ReturnFrom (alreadyLifted: Option<'Unlifted>) =
            alreadyLifted

        member inline this.Let (lhs: 'UnliftedInput,
                                rhs: 'UnliftedInput -> Option<'UnliftedOutput>): Option<'UnliftedOutput> =
            rhs lhs

        member inline this.Delay (delayedExpression: (Unit -> Option<'Unlifted>)) =
            delayedExpression ()

        member inline this.Zero () =
            None

    let optionWorkflow =
        Builder ()