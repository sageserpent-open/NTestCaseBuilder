#ifndef __EXCEPTIONS_HPP__
#define __EXCEPTIONS_HPP__

namespace SageSerpent
{
    namespace Infrastructure
    {
        public ref class LogicError:
            System::ApplicationException
        {
        public:
            LogicError(System::String ^description):
                System::ApplicationException(description)
            {
            }
        };

        public ref class AdmissibleFailure:
            System::ApplicationException
        {
        public:
            AdmissibleFailure(System::String ^description):
                System::ApplicationException(description)
            {
            }
        };

        public ref class PreconditionViolation:
            LogicError
        {
        public:
            PreconditionViolation(System::String ^description):
                LogicError("Precondition violation: " + description)
            {
            }
        };

        public ref class InvariantViolation:
            LogicError
        {
        public:
            InvariantViolation(System::String ^description):
                LogicError("Invariant violation: " + description)
            {
            }
        };

        public ref class InternalAssertionViolation:
            LogicError
        {
        public:
            InternalAssertionViolation(System::String ^description):
                LogicError("Internal assertion violation: " + description)
            {
            }
        };

        public ref class StrongGuaranteeException:
            AdmissibleFailure
        {
            StrongGuaranteeException(System::String ^description):
                AdmissibleFailure(description)
            {
            }
        };

        public ref class BasicGuaranteeException:
            AdmissibleFailure
        {
            BasicGuaranteeException(System::String ^description):
                AdmissibleFailure(description)
            {
            }
        };
    }
}

#endif