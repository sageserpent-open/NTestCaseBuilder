module SageSerpent.TestInfrastructure.SingletonTestCaseEnumerableFactory

    open System.Collections
    open System
    open SageSerpent.Infrastructure
    
    
    type private TestLevelAdapter<'WrappedSingletonType>(wrapped: 'WrappedSingletonType) =
                 member this.Unwrapped =
                    wrapped
                 override this.Equals(another: Object) =
                        match another with
                            :? TestLevelAdapter<'WrappedSingletonType> as another ->
                                (this:> IEquatable<TestLevelAdapter<'WrappedSingletonType>>).Equals(another)
                          | _ ->
                                false
                 override this.GetHashCode() =
                    0
                 interface IComparable<TestLevelAdapter<'WrappedSingletonType>> with
                    member this.CompareTo(another: TestLevelAdapter<'WrappedSingletonType>) =
                        0
                 interface IComparable with
                    member this.CompareTo(another: Object) =
                        match another with
                            :? TestLevelAdapter<'WrappedSingletonType> as another ->
                                (this:> IComparable<TestLevelAdapter<'WrappedSingletonType>>).CompareTo(another)
                          | _ ->
                                -1
                 interface IEquatable<TestLevelAdapter<'WrappedSingletonType>> with
                    member this.Equals(another: TestLevelAdapter<'WrappedSingletonType>) =
                        true
        
    type private 'WrappedSingletonType Unwrapping =
        delegate of TestLevelAdapter<'WrappedSingletonType> -> 'WrappedSingletonType
        
    let private UnwrappingDelegate<'WrappedSingletonType> =
        Unwrapping<'WrappedSingletonType>(fun wrapped -> wrapped.Unwrapped)

    let Create (singletonTestCase: 'WrappedSingletonType) =
        let singletonTestVariableLevelEumerableFactory =
            TestVariableLevelEnumerableFactory.Create (Seq.singleton (TestLevelAdapter singletonTestCase))
        SynthesizedTestCaseEnumerableFactory.Create (Seq.singleton singletonTestVariableLevelEumerableFactory)
                                                    UnwrappingDelegate<'WrappedSingletonType>
