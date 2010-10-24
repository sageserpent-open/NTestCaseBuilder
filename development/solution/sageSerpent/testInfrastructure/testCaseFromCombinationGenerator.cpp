#include "testCaseFromCombinationGenerator.hpp"

#using "infrastructure.dll"

namespace SageSerpent
{
    namespace TestInfrastructure
    {
        generic <typename TestCase>
        TestCaseFromCombinationGenerator<TestCase>::TestCaseFromCombinationGenerator(System::Collections::IEnumerable ^componentTestCaseGenerators,
                                                                                     System::Delegate combiningClosure)
        {
            throw gcnew System::NotImplementedException("*** Unimplemented stub! ***");
        }

        generic <typename TestCase>
        System::Collections::Generic::IEnumerator<TestCase> ^TestCaseFromCombinationGenerator<TestCase>::CreateIterator()
        {
            throw gcnew System::NotImplementedException("*** Unimplemented stub! ***");
        }
    }
}

