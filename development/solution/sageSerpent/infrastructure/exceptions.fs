#light

namespace SageSerpent.Infrastructure

    type LogicError(description) =
        inherit System.ApplicationException(description)
        
    type AdmissibleFailure(description) =
        inherit System.ApplicationException(description)
        
    type PreconditionViolation(description) =
        inherit LogicError("Precondition violation: " + description)
        
    type InvariantViolation(description) =
        inherit LogicError("Invariant violation: " + description)
        
    type InternalAssertionViolation(description) =
        inherit LogicError("Internal assertion violation: " + description)
        
    type StrongGuaranteeException(description) =
        inherit AdmissibleFailure(description)  // NOTE: there is no textual suffix added here as this exception
                                                // is only here to serve as a sign for exception propagation layers
                                                // in clients that they should provided the *strong* exception safety guarantee.
        
    type BasicGuaranteeException(description) =
        inherit AdmissibleFailure(description)  // NOTE: there is no textual suffix added here as this exception
                                                // is only here to serve as a sign for exception propagation layers
                                                // in clients that they should provided the *basic* exception safety guarantee.