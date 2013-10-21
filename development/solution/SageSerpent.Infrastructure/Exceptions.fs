namespace SageSerpent.Infrastructure

    type LogicErrorException(description) =
        inherit System.Exception(description)

    type AdmissibleFailureException(description) =
        inherit System.Exception(description)

    type PreconditionViolationException(description) =
        inherit LogicErrorException("Precondition violation: " + description)

    type InvariantViolationException(description) =
        inherit LogicErrorException("Invariant violation: " + description)

    type InternalAssertionViolationException(description) =
        inherit LogicErrorException("Internal assertion violation: " + description)

