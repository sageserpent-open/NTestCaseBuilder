#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    type InterleavedTestCaseEnumeratorFactory(sequenceOfFactoriesProvidingSubsequencesToInterleave: seq<SageSerpent.TestInfrastructure.ITestCaseEnumeratorFactory>) = // TODO: see if we can lose this poxy type annotation!
        do if Seq.is_empty sequenceOfFactoriesProvidingSubsequencesToInterleave
           then raise (PreconditionViolationException "Must provide at least one alternative.")
        interface SageSerpent.TestInfrastructure.ITestCaseEnumeratorFactory with
            override this.CreateEnumerator desiredStrength = raise (NotImplementedException())
            override this.MaximumStrength = raise (NotImplementedException())

