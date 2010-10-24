#light

#nowarn "57"

#r "FSharp.PowerPack.dll"
#r "FSharp.PowerPack.Linq"
#r "System.Core"
#r "System"


open System.Reflection.Emit
open Microsoft.FSharp.Linq.QuotationEvaluation
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open System.Linq.Expressions
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Core
open System


type Arrrgh =
    FastFunc<UInt32, UInt32>
    
type ProducerOfArrrgh =
    delegate of Unit -> Arrrgh

   
let tooMuchConceptMan =
    let quotedFunction = <@@ fun x -> x + 1u @@>
    let linqExpression = quotedFunction.ToLinqExpression ()
    let wrappingLinqLambdaExpression = Expression.Lambda (typeof<ProducerOfArrrgh>, linqExpression, [])
    let f = wrappingLinqLambdaExpression.Compile ()
    printf "'f' is a: %A\n" (f.GetType ())
    (f :?> ProducerOfArrrgh).Invoke ()
        
        
printf "'tooMuchConceptMan' is a %A\n" tooMuchConceptMan

let a = <@ () @>
let b = <@ %a, 2 @>
let c = <@@ fun (x, y) -> x + y @@>

printf "%A\n" (c.GetType ())


let tupleTypeString = typeof<int * string>
printf "%A\n" tupleTypeString
    


let tupleArity = 5u

let tupleCondensationDelegate<'X> tupleArity
                                  (listCondensation: List<'X> -> 'X) = 
    let tupleType = FSharpType.MakeTupleType (Array.init (int tupleArity) (fun _ -> typeof<'X>))
    
    printf "Tuple type: %A\n" tupleType

    let tupleVariable = Var ("tupleArgument"
                        , tupleType
                        , false)
                        
    let expressionForTupleVariable = Expr.Var tupleVariable
    
    let listConsConstructor =
        match <@ Unchecked.defaultof<'X> :: [] @> with
            NewUnionCase (unionCaseConstructor
                          , _)
                -> unionCaseConstructor
           | _
                -> failwith "The impossible just happened."
                
    let listEmptyConstructor =
        match <@ []: list<'X> @> with
            NewUnionCase (unionCaseConstructor
                          , _)
                -> unionCaseConstructor
           | _
                -> failwith "The impossible just happened."

    let expressionForLadderSequenceBuildingUpList =            
        let rec expressionForLadderSequenceBuildingUpList argumentIndex =
            if argumentIndex < tupleArity
            then let tail = expressionForLadderSequenceBuildingUpList (argumentIndex + 1u)
                 let expressionForArgumentTakenFromTuple = Expr.TupleGet (expressionForTupleVariable
                                                                          , int argumentIndex)
                 Expr.NewUnionCase (listConsConstructor
                                    , [expressionForArgumentTakenFromTuple; tail])
            else Expr.NewUnionCase (listEmptyConstructor
                                    , [])
        expressionForLadderSequenceBuildingUpList 0u
        
    let condensationAppliedToList = Expr.Application (<@ listCondensation @>
                                                      , expressionForLadderSequenceBuildingUpList)
                                 
    let genericDelegateType = typeof<Converter<_, _>>.GetGenericTypeDefinition ()
    let delegateType = genericDelegateType.MakeGenericType ([|tupleType; typeof<'X>|])

                                     
                                            
    let expressionForDelegate = Expr.NewDelegate (delegateType
                                                  , [tupleVariable]
                                                  , condensationAppliedToList)                                        
                                
    expressionForDelegate.CompileUntyped () ():?> Delegate  // Need to apply this one more time, because the underlying expression
                                                            // actually *creates* a delegate.

let ggg = tupleCondensationDelegate tupleArity (fun list -> List.hd list)


type BloodyDelegate<'X> =
    Converter<'X * 'X * 'X * 'X * 'X, 'X>
    
let zzzzz<'X> = unbox<BloodyDelegate<'X>> ggg




printf "The second list is: %A\n" (zzzzz<obj>.Invoke (box 1, box 2, box 3, box 4, box 5))

let troubleInMind = tupleCondensationDelegate 2u (fun list -> List.hd list)


type Drat<'X> =
    delegate of 'X * 'X * 'X * 'X * 'X -> 'X

let klf = new Drat<int> (fun _ _ a _ _ -> 2 * a)

printf "Type of klf: %A\n" (klf.GetType ())

let methodInfo = klf.Method

let huh = methodInfo.GetParameters ()

printf "Parameters: %A\n" huh
               