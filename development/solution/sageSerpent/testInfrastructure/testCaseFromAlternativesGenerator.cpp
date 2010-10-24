#include "testCaseFromAlternativesGenerator.hpp"

namespace SageSerpent
{
    namespace TestInfrastructure
    {
        generic <typename TestCase>
        TestCaseFromAlternativesGenerator<TestCase>::TestCaseFromAlternativesGenerator(Wintellect::PowerCollections::Set<ITestCaseGenerator<TestCase> ^> ^componentTestCaseGenerators)
        {
            throw gcnew System::NotImplementedException("*** Unimplemented stub! ***");
        }

        generic <typename TestCase>
        System::Collections::Generic::IEnumerator<TestCase> ^TestCaseFromAlternativesGenerator<TestCase>::CreateIterator()
        {
            throw gcnew System::NotImplementedException("*** Unimplemented stub! ***");
        }
    }
}
