#include "testCaseFromCollectionGenerator.hpp"

namespace SageSerpent
{
    namespace TestInfrastructure
    {
        generic <typename TestCase>
        TestCaseFromCollectionGenerator<TestCase>::TestCaseFromCollectionGenerator(System::Collections::Generic::ICollection<TestCase> ^collection)
        {
            throw gcnew System::NotImplementedException("*** Unimplemented stub! ***");
        }

        generic <typename TestCase>
        System::Collections::Generic::IEnumerator<TestCase> ^TestCaseFromCollectionGenerator<TestCase>::CreateIterator()
        {
            throw gcnew System::NotImplementedException("*** Unimplemented stub! ***");
        }
    }
}
