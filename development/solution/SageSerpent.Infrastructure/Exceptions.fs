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

    type StrongGuaranteeExceptionException(description) =
        inherit AdmissibleFailureException(description) // NOTE: there is no textual suffix added here as this exception
                                                        // is only here to serve as a sign for exception propagation layers
                                                        // in clients that they should provided the *strong* exception safety guarantee.

    type BasicGuaranteeExceptionException(description) =
        inherit AdmissibleFailureException(description) // NOTE: there is no textual suffix added here as this exception
                                                        // is only here to serve as a sign for exception propagation layers
                                                        // in clients that they should provided the *basic* exception safety guarantee.
