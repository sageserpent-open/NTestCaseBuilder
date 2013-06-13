module SageSerpent.Infrastructure.ContinuationWorkflow

    type ContinuationMonad<'Input, 'ExternalFinalResult> =
        Step of (('Input -> 'ExternalFinalResult) * (unit -> 'ExternalFinalResult) -> 'ExternalFinalResult)

            member inline this.Execute (successContinuation
                                        , failureContinuation) =
                match this with
                    Step computation ->
                        computation (successContinuation
                                     , failureContinuation)

            static member inline Execute (continuationMonad: ContinuationMonad<'FinalResult, 'FinalResult>) =
                continuationMonad.Execute(BargainBasement.Identity, (fun () -> failwith "Unhandled failure: use the '+' operator to introduce an alternative computation for failure."))

            static member inline (+) (lhs: ContinuationMonad<'Input, 'ExternalFinalResult>,
                                      rhs: ContinuationMonad<'Input, 'ExternalFinalResult>) =
                    Step (fun (successContinuation
                               , failureContinuation) ->
                            lhs.Execute(successContinuation,
                                        fun () ->
                                            rhs.Execute(successContinuation,
                                                        failureContinuation)))

            static member inline CallCC (body: ('Result -> ContinuationMonad<_, 'ExternalFinalResult>) -> ContinuationMonad<'Result, 'ExternalFinalResult>) =
                Step (fun (successContinuation
                           , failureContinuation) ->
                    let ejectionSeat fastReturnResult =
                        Step (fun (_, _) ->
                                successContinuation fastReturnResult)
                    (body ejectionSeat).Execute(successContinuation, failureContinuation))

            static member inline CallCC ((exceptionHandler: 'Exception -> ContinuationMonad<'Result, 'ExternalFinalResult>),
                                         (body: ('Exception -> ContinuationMonad<_, 'ExternalFinalResult>) -> ContinuationMonad<'Result, 'ExternalFinalResult>)) =
                Step (fun (successContinuation
                           , failureContinuation) ->
                    let ejectionSeat exceptionReturn =
                        Step (fun (_, _) ->
                                (exceptionHandler exceptionReturn).Execute(successContinuation, failureContinuation))
                    (body ejectionSeat).Execute(successContinuation, failureContinuation))

    type Builder () =
        member inline this.Bind (lhs: ContinuationMonad<'UnliftedLhsResult, 'ExternalFinalResult>,
                                 rhs: 'UnliftedLhsResult -> ContinuationMonad<'UnliftedRhsResult, 'ExternalFinalResult>): ContinuationMonad<'UnliftedRhsResult, 'ExternalFinalResult> =
            Step (fun (successContinuation
                       , failureContinuation) ->
                    lhs.Execute((fun intermediateResult ->
                                    (rhs intermediateResult).Execute(successContinuation, failureContinuation)),
                                failureContinuation))

        member inline this.Return (toBeLifted: 'Unlifted): ContinuationMonad<'Unlifted, 'ExternalFinalResult> =
            Step (fun (successContinuation
                       , _) ->
                    successContinuation toBeLifted)

        member inline this.ReturnFrom (alreadyLifted: ContinuationMonad<'Unlifted, 'ExternalFinalResult>): ContinuationMonad<'Unlifted, 'ExternalFinalResult> =
            alreadyLifted

        member inline this.Let (lhs: 'UnliftedLhsResult,
                                rhs: 'UnliftedLhsResult -> ContinuationMonad<'UnliftedRhsResult, 'ExternalFinalResult>): ContinuationMonad<'UnliftedRhsResult, 'ExternalFinalResult> =
            rhs lhs

        member inline this.Delay (delayedExpression: unit -> ContinuationMonad<'Input, 'ExternalFinalResult>) =
            Step (fun (successContinuation
                       , failureContinuation) ->
                       (delayedExpression ()).Execute(successContinuation, failureContinuation))

        member inline this.Combine (lhs: ContinuationMonad<'Input, 'ExternalFinalResult>,
                                    rhs: ContinuationMonad<'Input, 'ExternalFinalResult>) =
            lhs + rhs

        member inline this.Zero (): ContinuationMonad<_, 'ExternalFinalResult> =
            Step (fun (_
                       , failureContinuation) ->
                    failureContinuation ())

    let continuationWorkflow =
        Builder ()

