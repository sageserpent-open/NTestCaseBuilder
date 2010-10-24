#include "testCaseFromCombinationGenerator.hpp"

#using "infrastructure.dll"

namespace SageSerpent
{
    namespace TestInfrastructure
    {
        TestCaseFromCombinationGenerator::TestCaseFromCombinationGenerator(System::Collections::Generic::IEnumerable<ITestCaseGenerator ^> ^componentTestCaseGenerators,
                                                                           System::Delegate ^combiningClosure)
        {
            //throw gcnew System::NotImplementedException("*** Unimplemented stub! ***");
        }

        System::Collections::IEnumerator ^TestCaseFromCombinationGenerator::CreateIterator()
        {
            //throw gcnew System::NotImplementedException("*** Unimplemented stub! ***");
            return nullptr;
        }
    }
}

