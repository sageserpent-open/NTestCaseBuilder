#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    type InterleavedTestCaseEnumeratorFactory(sequenceOfFactoriesProvidingSubsequencesToInterleave: seq<SageSerpent.TestInfrastructure.ITestCaseEnumeratorFactory>) = // TODO: see if we can lose this poxy type annotation!
        let alternatives = Seq.to_list sequenceOfFactoriesProvidingSubsequencesToInterleave
        do if alternatives.IsEmpty
           then raise (PreconditionViolationException "Must provide at least one alternative.")
        interface SageSerpent.TestInfrastructure.ITestCaseEnumeratorFactory with
            override this.CreateEnumerator desiredStrength = raise (NotImplementedException())
            override this.MaximumStrength = raise (NotImplementedException())

