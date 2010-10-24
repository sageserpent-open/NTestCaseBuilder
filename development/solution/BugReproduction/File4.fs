#light

namespace Foo

    open System.Collections

    [<AbstractClass>]
    type TestCaseEnumeratorFactoryCommonImplementation () as this =
        interface ITestCaseEnumeratorFactory with
            override this.CreateEnumerator desiredStrength =
                (Seq.empty :> IEnumerable).GetEnumerator ()
            override this.MaximumStrength =
                9u



