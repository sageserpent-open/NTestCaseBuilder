type XTuple<'X, 'Y, 'Result> =
    DataCtr of 'X * ('Y -> 'Result)

let render (DataCtr (data
                     , continuation))
           f =
    f data
    |> continuation

let add (DataCtr (data
                  , continuation))
        x =
    (data
    , (fun resultOfPartialApplication ->
        resultOfPartialApplication x
        |> continuation))
    |> DataCtr

let lift x =
    (x
    , (fun x ->
        x))
    |> DataCtr

let test () =
    let xt1 = lift 2
    let xt2 = add xt1 "Hello"
    let xt3 = add xt2 (false, 1.2)
    render xt3
    //render xt3 (fun x y z -> printf "%u, %A, %A\n" x y z)

type XTuple2<'X, 'Result> =
    DataCtr2 of ('X -> 'Result)

let render2 (DataCtr2 continuation)
            f =
    f |> continuation

let add2 (DataCtr2 continuation)
         x =
    (fun resultOfPartialApplication ->
        (resultOfPartialApplication
         |> continuation) x)
    |> DataCtr2

let lift2 x =
    (fun resultOfPartialApplication ->
        resultOfPartialApplication x)
    |> DataCtr2

let test2 () =
    let xt1 = lift2 2
    let xt2 = add2 xt1 "Hello"
    let xt3 = add2 xt2 (false, 1.2)
    render2 xt3
    |> ignore
    render2 xt3 (fun x y z -> printf "%u, %s, %A\n" x y z)


type XTuple3<'X, 'Result> =
    DataCtr3 of ('X -> 'Result)

let render3 (DataCtr3 continuation)
            f =
    f |> continuation

let add3 (DataCtr3 continuation)
         (x: System.Object) =
    (fun resultOfPartialApplication ->
        (resultOfPartialApplication
         |> continuation) (unbox x))
    |> DataCtr3

let lift3 x =
    (fun resultOfPartialApplication ->
        resultOfPartialApplication x)
    |> DataCtr3

let test3 () =
    let xt1 = lift3 2
    let xt2 = add3 xt1 "Hello"
    let xt3 = add3 xt2 (false, 1.2)
    render3 xt3
    |> ignore
    render3 xt3 (fun x y z -> printf "%u, %s, %A\n" x y z)

let f x =
    match box x with
        :? List<_> as theList ->
            printf "%A`n" (List.head theList)
      | _ ->
            printf "Something else."

let rec x = 1 :: y
    and y = 2 :: x

printf "First 3 items of recursive list: %A\n" (Seq.take 5 x)



type Builder () =
    member inline this.Bind (lhs,
                             rhs) =
        fun continuation ->
            lhs (fun intermediateResult ->
                    rhs intermediateResult continuation)

    member inline this.Return toBeLifted =
        fun continuation ->
            continuation toBeLifted

    member inline this.ReturnFrom alreadyLifted =
        alreadyLifted

    member inline this.Let lhs
                           rhs =
        rhs lhs

let continuationWorkflow =
    Builder ()

let executeContinuation continuation =
    //printf "Executing entire continuation chain....\n"
    continuation (fun result -> result)





open System.Diagnostics

let rec fibonacci x =
    continuationWorkflow
        {
            if x <= 1
            then
                //printf "Base case: %A\n" x
                return x
            else
                let! partial1 = fibonacci (x - 2)
                let! partial2 = fibonacci (x - 1)
                //let numberOfStackFrames = StackTrace().FrameCount
                //printf "Step case: %A, number of stack frames: %A\n" x numberOfStackFrames
                let result = partial1 + partial2

                return result
        }

let rec fibonacci2 x =
    if x <= 1
    then
        //printf "Base case: %A\n" x
        x
    else
        let partial1 = fibonacci2 (x - 2)
        let partial2 = fibonacci2 (x - 1)
        //let numberOfStackFrames = StackTrace().FrameCount
        //printf "Step case: %A, number of stack frames: %A\n" x numberOfStackFrames
        let result = partial1 + partial2

        result

let f1 () =
    printf "%A\n" ([for i in 1 .. 35 do yield i ] |> List.map (fibonacci >> executeContinuation))

let f2 () =
    printf "%A\n" ([for i in 1 .. 35 do yield i ] |> List.map fibonacci2)

let benchmark f =
    let before = System.DateTime.Now
    f ()
    let after = System.DateTime.Now
    let span = after - before
    printf "Time taken: %A\n" span

benchmark f1
benchmark f1
benchmark f1
benchmark f2
benchmark f2
benchmark f2