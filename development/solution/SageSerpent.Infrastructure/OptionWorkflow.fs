module SageSerpent.Infrastructure.OptionWorkflow

    type Builder () =
        member inline this.Bind (lhs: Option<'UnliftedLhsResult>,
                                 rhs: 'UnliftedLhsResult -> Option<'UnliftedRhsResult>): Option<'UnliftedRhsResult> =
            lhs
            |> Option.bind rhs

        member inline this.Return (toBeLifted: 'Unlifted) =
            Some toBeLifted

        member inline this.ReturnFrom (alreadyLifted: Option<'Unlifted>) =
            alreadyLifted

        member inline this.Let (lhs: 'UnliftedLhsResult,
                                rhs: 'UnliftedLhsResult -> Option<'UnliftedRhsResult>): Option<'UnliftedRhsResult> =
            rhs lhs

        member inline this.Delay (delayedExpression: (Unit -> Option<'Unlifted>)) =
            delayedExpression ()

        member inline this.Zero () =
            None

    let optionWorkflow =
        Builder ()