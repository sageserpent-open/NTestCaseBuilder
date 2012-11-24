module SageSerpent.Infrastructure.ContinuationWorkflow

    [<AbstractClass>]
    type ContinuationMonad<'Input, 'ExternalFinalResult>() =
        inherit Microsoft.FSharp.Core.FSharpFunc<('Input -> 'ExternalFinalResult) * (unit -> 'ExternalFinalResult), 'ExternalFinalResult>()

        static member inline Execute(continuationMonad: ContinuationMonad<'FinalResult, 'FinalResult>) =
            continuationMonad.Invoke (BargainBasement.Identity
                                      , (fun () -> failwith "Unhandled failure: use the '+' operator to introduce an alternative computation for failure."))

        static member inline (+) (lhs: ContinuationMonad<'Input, 'ExternalFinalResult>,
                                  rhs: ContinuationMonad<'Input, 'ExternalFinalResult>) =
            {
                new ContinuationMonad<'Input, 'ExternalFinalResult>() with
                    member this.Invoke ((successContinuation
                                         , failureContinuation)) =
                        lhs.Invoke (successContinuation,
                                    fun () ->
                                        rhs.Invoke (successContinuation
                                                    , failureContinuation))
            }

        static member inline CallCC(body: ('Result -> ContinuationMonad<_, 'ExternalFinalResult>) -> ContinuationMonad<'Result, 'ExternalFinalResult>) =
            {
                new ContinuationMonad<'Result, 'ExternalFinalResult>() with
                    member this.Invoke ((successContinuation
                                         , failureContinuation)) =
                        let ejectionSeat fastReturnResult =
                            {
                                new ContinuationMonad<_, 'ExternalFinalResult>() with
                                    member this.Invoke ((_
                                                         , _)) =
                                        successContinuation fastReturnResult
                            }
                        (body ejectionSeat).Invoke (successContinuation
                                                    , failureContinuation)
            }

        static member inline CallCC ((exceptionHandler: 'Exception -> ContinuationMonad<'Result, 'ExternalFinalResult>),
                                     (body: ('Exception -> ContinuationMonad<_, 'ExternalFinalResult>) -> ContinuationMonad<'Result, 'ExternalFinalResult>)) =
            {
                new ContinuationMonad<'Result, 'ExternalFinalResult>() with
                    member this.Invoke ((successContinuation
                                         , failureContinuation)) =
                        let ejectionSeat exceptionReturn =
                            {
                                new ContinuationMonad<_, 'ExternalFinalResult>() with
                                    member this.Invoke ((_
                                                         , _)) =
                                        (exceptionHandler exceptionReturn).Invoke (successContinuation
                                                                                   , failureContinuation)
                            }
                        (body ejectionSeat).Invoke (successContinuation
                                                    , failureContinuation)
            }

    type Builder () =
        member inline this.Bind (lhs: ContinuationMonad<'UnliftedLhsResult, 'ExternalFinalResult>,
                                 rhs: 'UnliftedLhsResult -> ContinuationMonad<'UnliftedRhsResult, 'ExternalFinalResult>): ContinuationMonad<'UnliftedRhsResult, 'ExternalFinalResult> =
            {
                new ContinuationMonad<'UnliftedRhsResult, 'ExternalFinalResult>() with
                    member this.Invoke ((successContinuation
                                         , failureContinuation)) =
                        lhs.Invoke ((fun intermediateResult ->
                                        (rhs intermediateResult).Invoke (successContinuation, failureContinuation))
                                    , failureContinuation)
            }

        member inline this.Return (toBeLifted: 'Unlifted): ContinuationMonad<'Unlifted, 'ExternalFinalResult> =
            {
                new ContinuationMonad<'Unlifted, 'ExternalFinalResult>() with
                    member this.Invoke ((successContinuation
                                         , _)) =
                        successContinuation toBeLifted
            }

        member inline this.ReturnFrom (alreadyLifted: ContinuationMonad<'Unlifted, 'ExternalFinalResult>): ContinuationMonad<'Unlifted, 'ExternalFinalResult> =
            alreadyLifted

        member inline this.Let (lhs: 'UnliftedLhsResult,
                                rhs: 'UnliftedLhsResult -> ContinuationMonad<'UnliftedRhsResult, 'ExternalFinalResult>): ContinuationMonad<'UnliftedRhsResult, 'ExternalFinalResult> =
            rhs lhs

        member inline this.Delay (delayedExpression: unit -> ContinuationMonad<'Input, 'ExternalFinalResult>) =
            {
                new ContinuationMonad<'Input, 'ExternalFinalResult>() with
                    member this.Invoke ((successContinuation
                                         , failureContinuation)) =
                        (delayedExpression ()).Invoke(successContinuation
                                                      , failureContinuation)
            }

        member inline this.Combine (lhs: ContinuationMonad<'Input, 'ExternalFinalResult>,
                                    rhs: ContinuationMonad<'Input, 'ExternalFinalResult>) =
            lhs + rhs

        member inline this.Zero (): ContinuationMonad<_, 'ExternalFinalResult> =
            {
                new ContinuationMonad<_, 'ExternalFinalResult>() with
                    member this.Invoke ((_
                                         , failureContinuation)) =
                        failureContinuation ()
            }

    let continuationWorkflow =
        Builder ()

