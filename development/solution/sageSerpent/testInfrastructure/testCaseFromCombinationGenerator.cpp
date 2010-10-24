#include "testCaseFromCombinationGenerator.hpp"

namespace SageSerpent
{
    namespace TestInfrastructure
    {
        generic <typename TestCase>
        TestCaseFromCombinationGenerator<TestCase>::TestCaseFromCombinationGenerator(System::Collections::Generic::IEnumerable<ITestCaseGenerator<TestCase> ^> ^componentTestCaseGenerators,
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

