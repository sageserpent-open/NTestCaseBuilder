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

