#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    type SynthesizedTestCaseEnumeratorFactory(sequenceOfAlternatives: SageSerpent.TestInfrastructure.ITestCaseEnumeratorFactory seq,
                                              synthesisClosure: Delegate) = // TODO: see if we can lose this poxy type annotation!
        interface SageSerpent.TestInfrastructure.ITestCaseEnumeratorFactory with
            override this.CreateEnumerator desiredStrength = raise (NotImplementedException())
            override this.MaximumStrength = raise (NotImplementedException())
