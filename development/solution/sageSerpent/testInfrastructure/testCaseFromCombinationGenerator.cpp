#include "testCaseFromCombinationGenerator.hpp"

#using "SageSerpent.Infrastructure.dll"

namespace SageSerpent
{
    namespace TestInfrastructure
    {
        TestCaseFromCombinationGenerator::TestCaseFromCombinationGenerator(System::Collections::Generic::IEnumerable<ITestCaseGenerator ^> ^componentTestCaseGenerators,
                                                                           System::Delegate ^combiningClosure)
        {
            //throw gcnew System::NotImplementedException("*** Unimplemented stub! ***");
        }

        System::Collections::IEnumerator ^TestCaseFromCombinationGenerator::CreateIterator(System::UInt32 requestedDegreesOfFreedomForCombinationCoverage)
        {
            //throw gcnew System::NotImplementedException("*** Unimplemented stub! ***");
            return nullptr;
        }
        
        System::UInt32 TestCaseFromCombinationGenerator::MaximumDegreesOfFreedom::get()
        {
            return 0U;
        }

        System::Boolean TestCaseFromCombinationGenerator::IsDead::get()
        {
            return false;
        }
    }
}

