#light

namespace SageSerpent.TestInfrastructure

    open System.Collections
    open System
    open SageSerpent.Infrastructure

    type InterleavedTestCaseEnumeratorFactory(sequenceOfAlternatives: SageSerpent.TestInfrastructure.ITestCaseEnumeratorFactory seq) = // TODO: see if we can lose this poxy type annotation!
        let alternatives = Seq.to_list sequenceOfAlternatives
        do if alternatives.IsEmpty
           then raise (PreconditionViolation "Must provide at least one alternative.")
        interface SageSerpent.TestInfrastructure.ITestCaseEnumeratorFactory with
            override this.CreateEnumerator desiredStrength = raise (NotImplementedException())
            override this.MaximumStrength = raise (NotImplementedException())
