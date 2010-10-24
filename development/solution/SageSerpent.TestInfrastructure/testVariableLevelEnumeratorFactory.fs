#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System

    type TestVariableLevelEnumeratorFactory(collection: ICollection) = // TODO: see if we can lose this poxy type annotation!
        interface SageSerpent.TestInfrastructure.ITestCaseEnumeratorFactory with
            override this.CreateEnumerator desiredStrength = raise (NotImplementedException())
            override this.MaximumStrength = raise (NotImplementedException())
        