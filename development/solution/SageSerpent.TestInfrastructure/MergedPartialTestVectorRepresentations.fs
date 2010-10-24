#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System.Collections.Generic
    open System
    open SageSerpent.Infrastructure
    open Microsoft.FSharp.Collections
    
    type MergedPartialTestVectorRepresentations =
        Foo
      | Bar
      
        interface IEnumerable<Map<UInt32, UInt32>> with
            member this.GetEnumerator () =
                null: IEnumerator<Map<UInt32, UInt32>>
        interface IEnumerable with
            member this.GetEnumerator () =
                null: IEnumerator
      
        member private this.Add partialTestVectorRepresentation =
            Bar
            
        member private this.Remove partialTestVectorRepresentation =
            Bar
        
        static member Empty =
            Foo
            
        member this.MergeOrAdd partialTestVectorRepresentation =
            Bar
            
            

        
