module SageSerpent.Infrastructure.ContinuationWorkflow

    type ContinuationMonad<'Input, 'ExternalFinalResult> =
        Step of (('Input -> 'ExternalFinalResult) * (unit -> 'ExternalFinalResult) -> 'ExternalFinalResult)
      | Return of 'Input
      | Zero
      | Delayed of (unit -> ContinuationMonad<'Input, 'ExternalFinalResult>)

    let rec liftThroughNestedDelays continuationMonadBuilder
                                    toBeLifted =
        match continuationMonadBuilder () with
            Delayed nestedContinuationMonadBuilder ->
                liftThroughNestedDelays nestedContinuationMonadBuilder
                                        toBeLifted
          | _ as undelayedContinuationMonad ->
                toBeLifted undelayedContinuationMonad

    let rec inline execute continuationMonad
                           successContinuation
                           failureContinuation =
        match continuationMonad with
            Step computation ->
                computation (successContinuation
                             , failureContinuation)
          | Return toBeLifted ->
                successContinuation toBeLifted
          | Zero ->
                failureContinuation ()
          | Delayed continuationMonadBuilder ->
                liftThroughNestedDelays continuationMonadBuilder
                                        (executeInlinedRecursionHack successContinuation
                                                                     failureContinuation)
    and executeInlinedRecursionHack successContinuation
                                    failureContinuation
                                    undelayedContinuationMonad =
            execute undelayedContinuationMonad
                    successContinuation
                    failureContinuation

    let rec inline plus lhs
                        rhs =
        match lhs with
            Step computation ->
                Step (fun (successContinuation
                           , failureContinuation) ->
                        computation (successContinuation,
                                     fun () ->
                                        execute rhs
                                                successContinuation
                                                failureContinuation))
          | Return _ ->
                lhs
          | Zero ->
                rhs
          | Delayed continuationMonadBuilder ->
                liftThroughNestedDelays continuationMonadBuilder
                                        (plusInlinedRecursionHack rhs)
    and plusInlinedRecursionHack rhs
                                 undelayedLhs =
        plus undelayedLhs
             rhs

    let rec inline bind lhs
                        rhs =
        match lhs with
            Step computation ->
                Step (fun (successContinuation
                           , failureContinuation) ->
                        computation ((fun intermediateResult ->
                                        execute (rhs intermediateResult)
                                                successContinuation
                                                failureContinuation)
                                        , failureContinuation))
          | Return toBeLifted ->
                rhs toBeLifted
          | Zero ->
                Zero
          | Delayed continuationMonadBuilder ->
                liftThroughNestedDelays continuationMonadBuilder
                                        (bindInlinedRecursionHack rhs)
    and bindInlinedRecursionHack rhs
                                 undelayedLhs =
        bind undelayedLhs
             rhs

    type ContinuationMonad<'Input, 'ExternalFinalResult> with

            member inline this.Execute (successContinuation,
                                        failureContinuation) =
                execute this
                        successContinuation
                        failureContinuation

            static member inline Execute(continuationMonad: ContinuationMonad<'FinalResult, 'FinalResult>) =
                execute continuationMonad
                        BargainBasement.Identity
                        (fun () -> failwith "Unhandled failure: use the '+' operator to introduce an alternative computation for failure.")

            static member inline (+) (lhs: ContinuationMonad<'Input, 'ExternalFinalResult>,
                                      rhs: ContinuationMonad<'Input, 'ExternalFinalResult>) =
                plus lhs
                     rhs

            static member inline CallCC(body: ('Result -> ContinuationMonad<_, 'ExternalFinalResult>) -> ContinuationMonad<'Result, 'ExternalFinalResult>) =
                Step (fun (successContinuation
                           , failureContinuation) ->
                    let ejectionSeat fastReturnResult =
                        Step (fun (_, _) ->
                                successContinuation fastReturnResult)
                    execute (body ejectionSeat)
                            successContinuation
                            failureContinuation)

            static member inline CallCC ((exceptionHandler: 'Exception -> ContinuationMonad<'Result, 'ExternalFinalResult>),
                                         (body: ('Exception -> ContinuationMonad<_, 'ExternalFinalResult>) -> ContinuationMonad<'Result, 'ExternalFinalResult>)) =
                Step (fun (successContinuation
                           , failureContinuation) ->
                    let ejectionSeat exceptionReturn =
                        Step (fun (_, _) ->
                                execute (exceptionHandler exceptionReturn)
                                        successContinuation
                                        failureContinuation)
                    execute (body ejectionSeat)
                            successContinuation
                            failureContinuation)

    type Builder () =
        member inline this.Bind (lhs: ContinuationMonad<'UnliftedLhsResult, 'ExternalFinalResult>,
                                 rhs: 'UnliftedLhsResult -> ContinuationMonad<'UnliftedRhsResult, 'ExternalFinalResult>): ContinuationMonad<'UnliftedRhsResult, 'ExternalFinalResult> =
            bind lhs
                 rhs

        member inline this.Return (toBeLifted: 'Unlifted): ContinuationMonad<'Unlifted, 'ExternalFinalResult> =
            Return toBeLifted

        member inline this.ReturnFrom (alreadyLifted: ContinuationMonad<'Unlifted, 'ExternalFinalResult>): ContinuationMonad<'Unlifted, 'ExternalFinalResult> =
            alreadyLifted

        member inline this.Let (lhs: 'UnliftedLhsResult,
                                rhs: 'UnliftedLhsResult -> ContinuationMonad<'UnliftedRhsResult, 'ExternalFinalResult>): ContinuationMonad<'UnliftedRhsResult, 'ExternalFinalResult> =
            rhs lhs

        member inline this.Delay (delayedExpression: unit -> ContinuationMonad<'Input, 'ExternalFinalResult>) =
            Delayed delayedExpression

        member inline this.Combine (lhs: ContinuationMonad<'Input, 'ExternalFinalResult>,
                                    rhs: ContinuationMonad<'Input, 'ExternalFinalResult>) =
            lhs + rhs

        member inline this.Zero (): ContinuationMonad<_, 'ExternalFinalResult> =
            Zero

    let continuationWorkflow =
        Builder ()

