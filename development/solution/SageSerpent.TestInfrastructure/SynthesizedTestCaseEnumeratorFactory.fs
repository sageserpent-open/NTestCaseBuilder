#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    type SynthesizedTestCaseEnumeratorFactory(sequenceOfFactoriesProvidingInputsToSynthesis: seq<SageSerpent.TestInfrastructure.ITestCaseEnumeratorFactory>,
                                              synthesisDelegate: Delegate) = // TODO: see if we can lose this poxy type annotation!
        // TODO - add a precondition that checks the arity of the synthesis closure against the number of factories provided.
        do if Seq.is_empty sequenceOfFactoriesProvidingInputsToSynthesis
           then raise (PreconditionViolationException "Must provide at least one component.")
        interface SageSerpent.TestInfrastructure.ITestCaseEnumeratorFactory with
            override this.CreateEnumerator desiredStrength = raise (NotImplementedException())
            override this.MaximumStrength = raise (NotImplementedException())
