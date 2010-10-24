#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    type SynthesizedTestCaseEnumeratorFactory(sequenceOfFactoriesProvidingInputsToSynthesis: seq<SageSerpent.TestInfrastructure.ITestCaseEnumeratorFactory>,
                                              synthesisClosure: Delegate) = // TODO: see if we can lose this poxy type annotation!
        // TODO - add a precondition that checks the arity of the synthesis closure againt the number of factories provided.
        interface SageSerpent.TestInfrastructure.ITestCaseEnumeratorFactory with
            override this.CreateEnumerator desiredStrength = raise (NotImplementedException())
            override this.MaximumStrength = raise (NotImplementedException())
